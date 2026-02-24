#  File src/library/stats/R/free1way.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2026 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

# NewtonRaphson

### adopted from rms:::lrm.fit
.NewtonRaphson <- function(start, objective, gradient, hessian, 
                           control = list(iter.max = 150, trace = trace, 
                                          objtol = 5e-4, gradtol = 1e-5, 
                                          paramtol = 1e-5, minstepsize = 1e-2, 
                                          tolsolve = .Machine$double.eps),
                           trace = FALSE)
{

    theta  <- start # Initialize the parameter vector
    oldobj <- Inf
    objthe <- objective(theta)
    if (!is.finite(objthe)) {
        msg <- "Infeasible starting values"
        return(list(par = theta, objective = objthe, convergence = 1, 
                    message = msg)) 
    }

    if(!suppressPackageStartupMessages(requireNamespace("Matrix")))
        stop(gettextf("%s needs package 'Matrix' correctly installed",
                      ".NewtonRaphson"),
                 domain = NA)

    for (iter in seq_len(control$iter.max)) {

        # Newton update
        
        gradthe <- gradient(theta)     # Compute the gradient vector
        hessthe <- hessian(theta)      # Compute the Hessian matrix

        delta <- Matrix::solve(hessthe, gradthe, tol = control$tolsolve)

        if (control$trace)
            cat(iter, ': ', theta, "\n", sep = "")

        step_size <- 1L                # Initialize step size for step-halving
        

        # Step-halving loop
        while (TRUE) {
            # Newton step halving
            
            new_theta <- theta - step_size * delta # Update parameter vector
            objnew_the <- objective(new_theta)

            if (control$trace)
                cat("Old, new, old - new objective:", 
                    objthe, objnew_the, objthe - objnew_the, "\n")

            # Objective function failed to be reduced or is infinite
            if (!is.finite(objnew_the) || (objnew_the > objthe + 1e-6)) {
                step_size <- step_size / 2         # Reduce the step size

                if (control$trace) 
                    cat("Step size reduced to", step_size, "\n")

                if (step_size <= control$minstepsize) {
                    msg <- paste("Step size ", step_size, 
                                 " has reduced below minstepsize")
                    return(list(par = theta, objective = objthe, convergence = 1, 
                                message = msg)) 
                }
            } else {
                theta  <- new_theta # accept the new parameter vector
                oldobj <- objthe
                objthe <- objnew_the
                break
            }
            
        }

        # Newton convergence
        
        # Convergence check - must meet 3 criteria
        if ((objthe <= oldobj + 1e-6 && (oldobj - objthe < control$objtol)) &&
            (max(abs(gradthe)) < control$gradtol) &&
            (max(abs(delta)) < control$paramtol))

            return(list(par            = theta,
                        objective      = objthe,
                        convergence    = 0,
                        message        = "Normal convergence"))
         
    }

    msg <- paste("Reached", control$iter.max, "iterations without convergence")
    return(list(par = theta, objective = objthe, convergence = 1, message = msg)) 
}

# ML estimation

.free1wayML <- function(x, link, mu = 0, start = NULL, fix = NULL, 
                        residuals = TRUE, score = TRUE, hessian = TRUE, 
                        MPL_Jeffreys = FALSE,
                        ### use nlminb for small sample sizes
                        dooptim = c(".NewtonRaphson", "nlminb")[1 + (sum(x) < 20)],                         
                        control = list(
                            "nlminb" = list(trace = trace, iter.max = 200,
                                            eval.max = 200, rel.tol = 1e-10,
                                            abs.tol = 1e-20, xf.tol = 1e-16),
                            ".NewtonRaphson" = list(iter.max = 200, trace = trace, 
                                             objtol = 5e-4, 
                                             gradtol = 1e-5 * sum(x) / 1000, 
                                             paramtol = 1e-5, minstepsize = 1e-2, 
                                             tolsolve = .Machine$double.eps)
                        )[dooptim],
                        trace = FALSE, 
                        tol = sqrt(.Machine$double.eps), ...) 
{

    ### convert to three-way table
    xt <- x
    if (!is.table(x))
      stop(gettextf("Incorrect argument 'x' in %s",
                    "free1way"),
           domain = NA)
    dx <- dim(x)
    dn <- dimnames(x)
    if (length(dx) == 2L) {
        x <- as.table(array(c(x), dim = dx <- c(dx, 1L)))
        dimnames(x) <- dn <- c(dn, list(A = "A"))
    }

    ### short-cuts for link functions
    F <- function(q) .p(link, q = q)
    Q <- function(p) .q(link, p = p)
    f <- function(q) .d(link, x = q)
    fp <- function(q) .dd(link, x = q)

    if(!suppressPackageStartupMessages(requireNamespace("Matrix")))
        stop(gettextf("%s needs package 'Matrix' correctly installed",
                      ".free1wayML"),
                 domain = NA)

    # setup and starting values
    
    # table2list body

    dx <- dim(x)
    if (length(dx) == 1L)
        stop("Incorrect dimensions")
    if (length(dx) == 2L)
        x <- as.table(array(x, dim = c(dx, 1)))
    dx <- dim(x)
    if (length(dx) < 3L)
        stop("Incorrect dimensions")
    C <- dim(x)[1L]
    K <- dim(x)[2L]
    B <- dim(x)[3L]
    if (C < 2L)
        stop("At least two response categories required")
    if (K < 2L)
        stop("At least two groups required")
    xrc <- NULL
    if (length(dx) == 4L) {
        if (dx[4] == 2L) {
            xrc <- array(x[,,,"FALSE", drop = TRUE], dim = dx[1:3])
            x <- array(x[,,,"TRUE", drop = TRUE], dim = dx[1:3])
        } else {
            stop(gettextf("%s currently only allows independent right-censoring",
                          "free1way"),
                 domain = NA)
        }
    }

    # determine steps in blocks

    xlist <- xrclist <- vector(mode = "list", length = B)

    for (b in seq_len(B)) {
        xb <- matrix(x[,,b, drop = TRUE], ncol = K)
        xw <- rowSums(abs(xb)) > 0
        if (sum(xw) > 1L) {
            ### do not remove last parameter if there are corresponding
            ### right-censored observations
            wm <- which(xw)[sum(xw)]
            if (!is.null(xrc) && any(xrc[wm:dx[1],,b,drop = TRUE] > 0))
                xw[length(xw)] <- TRUE
            xlist[[b]] <- xb[xw,,drop = FALSE]
            Cidx <- rep.int(1L, times = C)
            Cidx[xw] <- Cidx[xw] + seq_len(sum(xw))
            attr(xlist[[b]], "idx") <- Cidx
            if (!is.null(xrc)) {
                ### count right-censored observations between distinct event
                ### times
                cs <- apply(xrc[,,b,drop = TRUE] * (!xw), 2, function(x) 
                    diff(c(0, cumsum(x)[xw])))
                xrclist[[b]] <- matrix(xrc[xw,,b,drop = TRUE], ncol = K) + cs
                idx <- seq_len(C)[xw]
                idx <- rep(seq_len(sum(xw)), times = c(idx[1], diff(idx)))
                Cidx <- rep.int(1L, times = C)
                Cidx[seq_along(idx)] <- Cidx[seq_along(idx)] + idx
                attr(xrclist[[b]], "idx") <- Cidx
            }
        }
    }
    ### remove empty blocks
    strata <- !vapply(xlist, is.null, NA)
    xlist <- xlist[strata]
    xrclist <- xrclist[strata]
    
    
    ## allow specification of start = delta and fix = 1:K
    ## for evaluating the likelihood at given delta parameters
    ## without having to specify all intercept parameters
    if (is.null(start))
        start <- rep.int(0, K - 1L)
    NS <- length(start) == (K - 1L)
    lwr <- rep(-Inf, times = K - 1L)
    for (b in seq_len(length(xlist))) {
        bC <- nrow(xlist[[b]]) - 1L
        lwr <- c(lwr, -Inf, rep.int(0, times = bC - 1L))
        if (NS) {
            ecdf0 <- cumsum(rowSums(xlist[[b]]))
            ### ensure that 0 < ecdf0 < 1 such that quantiles exist
            ecdf0 <- pmax(1, ecdf0[-length(ecdf0)]) / (max(ecdf0) + 1)
            Qecdf <- Q(ecdf0)
            start <- c(start, Qecdf)
        }
    }
    
    # negative logLik
    
    .nll <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # parm to prob
        
        bidx <- seq_len(ncol(x) - 1L)
        delta <- c(0, mu + parm[bidx])
        intercepts <- c(-Inf, parm[- bidx], Inf)
        tmb <- intercepts - matrix(delta, nrow = length(intercepts),  
                                          ncol = ncol(x),
                                          byrow = TRUE)
        Ftmb <- F(tmb)
        if (rightcensored) {
            prb <- 1 - Ftmb[- nrow(Ftmb), , drop = FALSE]
        } else {
            prb <- Ftmb[- 1L, , drop = FALSE] - 
                   Ftmb[- nrow(Ftmb), , drop = FALSE]
        } 
        
        if (any(prb < .Machine$double.eps^10)) 
            return(Inf)
        return(- sum(x * log(prb)))
    }
    
    # negative score
    
    .nsc <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # parm to prob
        
        bidx <- seq_len(ncol(x) - 1L)
        delta <- c(0, mu + parm[bidx])
        intercepts <- c(-Inf, parm[- bidx], Inf)
        tmb <- intercepts - matrix(delta, nrow = length(intercepts),  
                                          ncol = ncol(x),
                                          byrow = TRUE)
        Ftmb <- F(tmb)
        if (rightcensored) {
            prb <- 1 - Ftmb[- nrow(Ftmb), , drop = FALSE]
        } else {
            prb <- Ftmb[- 1L, , drop = FALSE] - 
                   Ftmb[- nrow(Ftmb), , drop = FALSE]
        } 
        

        # density prob ratio
        
        ftmb <- f(tmb)
        zu <- x * ftmb[- 1, , drop = FALSE] / prb
        if (rightcensored) zu[] <- 0 ### derivative of a constant
        zl <- x * ftmb[- nrow(ftmb), , drop = FALSE] / prb
        

        ret <- numeric(length(parm))
        ret[bidx] <- .colSums(zl, m = nrow(zl), n = ncol(zl))[-1L] -
                     .colSums(zu[-nrow(zu),,drop = FALSE], 
                              m = nrow(zu) - 1L, n = ncol(zu))[-1L]
        ret[- bidx] <- .rowSums(zu[-nrow(zu),,drop = FALSE] - 
                                zl[-1,,drop = FALSE], 
                                m = nrow(zu) - 1L, n = ncol(zu))
        return(- ret)
    }
    
    # negative score residuals
    
    .nsr <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # parm to prob
        
        bidx <- seq_len(ncol(x) - 1L)
        delta <- c(0, mu + parm[bidx])
        intercepts <- c(-Inf, parm[- bidx], Inf)
        tmb <- intercepts - matrix(delta, nrow = length(intercepts),  
                                          ncol = ncol(x),
                                          byrow = TRUE)
        Ftmb <- F(tmb)
        if (rightcensored) {
            prb <- 1 - Ftmb[- nrow(Ftmb), , drop = FALSE]
        } else {
            prb <- Ftmb[- 1L, , drop = FALSE] - 
                   Ftmb[- nrow(Ftmb), , drop = FALSE]
        } 
        

        # density prob ratio
        
        ftmb <- f(tmb)
        zu <- x * ftmb[- 1, , drop = FALSE] / prb
        if (rightcensored) zu[] <- 0 ### derivative of a constant
        zl <- x * ftmb[- nrow(ftmb), , drop = FALSE] / prb
        

        ret <- .rowSums(zl - zu, m = nrow(zl), n = ncol(zl)) / 
               .rowSums(x, m = nrow(x), n = ncol(x))
        ret[!is.finite(ret)] <- 0
        return(- ret)
    }
    
    # Hessian
    
    .hes <- function(parm, x, mu = 0, rightcensored = FALSE, full = FALSE) 
    {

        # parm to prob
        
        bidx <- seq_len(ncol(x) - 1L)
        delta <- c(0, mu + parm[bidx])
        intercepts <- c(-Inf, parm[- bidx], Inf)
        tmb <- intercepts - matrix(delta, nrow = length(intercepts),  
                                          ncol = ncol(x),
                                          byrow = TRUE)
        Ftmb <- F(tmb)
        if (rightcensored) {
            prb <- 1 - Ftmb[- nrow(Ftmb), , drop = FALSE]
        } else {
            prb <- Ftmb[- 1L, , drop = FALSE] - 
                   Ftmb[- nrow(Ftmb), , drop = FALSE]
        } 
        

        # Hessian prep
        
        ftmb <- f(tmb)
        fptmb <- fp(tmb)

        dl <- ftmb[- nrow(ftmb), , drop = FALSE]
        du <- ftmb[- 1, , drop = FALSE]
        if (rightcensored) du[] <- 0
        dpl <- fptmb[- nrow(ftmb), , drop = FALSE]
        dpu <- fptmb[- 1, , drop = FALSE]
        if (rightcensored) dpu[] <- 0
        dlm1 <- dl[,-1L, drop = FALSE]
        dum1 <- du[,-1L, drop = FALSE]
        dplm1 <- dpl[,-1L, drop = FALSE]
        dpum1 <- dpu[,-1L, drop = FALSE]
        prbm1 <- prb[,-1L, drop = FALSE]

        i1 <- length(intercepts) - 1L
        i2 <- 1L
        

        # off-diagonal elements for Hessian of intercepts
        
        Aoffdiag <- - .rowSums(x * du * dl / prb^2, m = nrow(x), n = ncol(x))[-i2]
        Aoffdiag <- Aoffdiag[-length(Aoffdiag)]
        
        # diagonal elements for Hessian of intercepts
        
        Adiag <- - .rowSums((x * dpu / prb)[-i1,,drop = FALSE] - 
                            (x * dpl / prb)[-i2,,drop = FALSE] - 
                            ((x * du^2 / prb^2)[-i1,,drop = FALSE] + 
                             (x * dl^2 / prb^2)[-i2,,drop = FALSE] ), 
                            m = nrow(x) - length(i1), n = ncol(x)
                           )
                          
        
        # intercept / shift contributions to Hessian
        
        xm1 <- x[,-1L,drop = FALSE] 
        X <- ((xm1 * dpum1 / prbm1)[-i1,,drop = FALSE] - 
              (xm1 * dplm1 / prbm1)[-i2,,drop = FALSE] - 
              ((xm1 * dum1^2 / prbm1^2)[-i1,,drop = FALSE] - 
               (xm1 * dum1 * dlm1 / prbm1^2)[-i2,,drop = FALSE] -
               (xm1 * dum1 * dlm1 / prbm1^2)[-i1,,drop = FALSE] +
               (xm1 * dlm1^2 / prbm1^2)[-i2,,drop = FALSE]
              )
             )

        Z <- - .colSums(xm1 * (dpum1 / prbm1 - 
                               dplm1 / prbm1 -
                               (dum1^2 / prbm1^2 - 
                                2 * dum1 * dlm1 / prbm1^2 +
                                dlm1^2 / prbm1^2
                               )
                              ),
                        m = nrow(xm1), n = ncol(xm1)
                        )
        if (length(Z) > 1L) Z <- diag(Z)
        

        if (length(Adiag) > 1L) {
            if (!isFALSE(full)) {
                A <- list(Adiag = Adiag, Aoffdiag = Aoffdiag)
            } else {
                A <- Matrix::bandSparse(length(Adiag), 
                    k = 0:1, diagonals = list(Adiag, Aoffdiag), 
                    symmetric = TRUE)
            }
        } else {
            if (!isFALSE(full)) {
                A <- list(Adiag = Adiag, Aoffdiag = NULL)
            } else {
                A <- matrix(Adiag)
            }
        }
        return(list(A = A, X = X, Z = Z))
    }
    
    # stratified negative logLik
    
    .snll <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # stratum prep
        
        C <- vapply(x, NROW, 0L) ### might differ by stratum
        K <- unique(do.call("c", lapply(x, ncol))) ### the same
        B <- length(x)
        sidx <- factor(rep(seq_len(B), times = pmax(0, C - 1L)), 
                       levels = seq_len(B))
        bidx <- seq_len(K - 1L)
        delta <- parm[bidx]
        intercepts <- split(parm[-bidx], sidx)
        

        ret <- 0
        for (b in seq_len(B))
            ret <- ret + .nll(c(delta, intercepts[[b]]), x[[b]], mu = mu,
                              rightcensored = rightcensored)
        return(ret)
    }
    
    # stratified negative score
    
    .snsc <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # stratum prep
        
        C <- vapply(x, NROW, 0L) ### might differ by stratum
        K <- unique(do.call("c", lapply(x, ncol))) ### the same
        B <- length(x)
        sidx <- factor(rep(seq_len(B), times = pmax(0, C - 1L)), 
                       levels = seq_len(B))
        bidx <- seq_len(K - 1L)
        delta <- parm[bidx]
        intercepts <- split(parm[-bidx], sidx)
        

        ret <- numeric(length(bidx))
        for (b in seq_len(B)) {
            nsc <- .nsc(c(delta, intercepts[[b]]), x[[b]], mu = mu,
                        rightcensored = rightcensored)
            ret[bidx] <- ret[bidx] + nsc[bidx]
            ret <- c(ret, nsc[-bidx])
        }
        return(ret)
    }
    
    # stratified Hessian
    
    .shes <- function(parm, x, mu = 0, xrc = NULL, full = FALSE, 
                      retMatrix = FALSE) 
    {

        # stratum prep
        
        C <- vapply(x, NROW, 0L) ### might differ by stratum
        K <- unique(do.call("c", lapply(x, ncol))) ### the same
        B <- length(x)
        sidx <- factor(rep(seq_len(B), times = pmax(0, C - 1L)), 
                       levels = seq_len(B))
        bidx <- seq_len(K - 1L)
        delta <- parm[bidx]
        intercepts <- split(parm[-bidx], sidx)
        

        if (!isFALSE(ret <- full)) {
            # full Hessian
            
            for (b in seq_len(B)) {
                H <- .hes(c(delta, intercepts[[b]]), x[[b]], mu = mu, full = full)
                if (!is.null(xrc)) {
                    Hrc <- .hes(c(delta, intercepts[[b]]), xrc[[b]], mu = mu, 
                                rightcensored = TRUE, full = full)
                    H$X <- H$X + Hrc$X
                    H$A$Adiag <- H$A$Adiag + Hrc$A$Adiag
                    H$A$Aoffdiag <- H$A$Aoffdiag + Hrc$A$Aoffdiag
                    H$Z <- H$Z + Hrc$Z
                }
                if (b == 1L) {
                    Adiag <- H$A$Adiag
                    Aoffdiag <- H$A$Aoffdiag
                    X <- H$X
                    Z <- H$Z
                } else {
                    Adiag <- c(Adiag, H$A$Adiag)
                    Aoffdiag <- c(Aoffdiag, 0, H$A$Aoffdiag)
                    X <- rbind(X, H$X)
                    Z <- Z + H$Z
                }
            }

            if (length(Adiag) > 1L) {
                A <- Matrix::bandSparse(length(Adiag),
                                        k = 0:1, diagonals = list(Adiag, Aoffdiag),
                                        symmetric = TRUE)
            } else {
                A <- matrix(Adiag)
            }

            ret <- cbind(Z, t(X))
            ret <- rbind(ret, cbind(X, A))
            if (retMatrix) return(ret)
            return(as.matrix(ret))
            
        }
        ret <- matrix(0, nrow = length(bidx), ncol = length(bidx))
        for (b in seq_len(B)) {
            H <- .hes(c(delta, intercepts[[b]]), x[[b]], mu = mu)
            if (!is.null(xrc)) {
                Hrc <- .hes(c(delta, intercepts[[b]]), xrc[[b]], mu = mu, 
                            rightcensored = TRUE)
                H$X <- H$X + Hrc$X
                H$A <- H$A + Hrc$A
                H$Z <- H$Z + Hrc$Z
            }
            sAH <- tryCatch(Matrix::solve(H$A, H$X), error = function(e) NULL)
            if (is.null(sAH))
                stop(gettextf("Error computing the Hessian in %s",
                              "free1way"),
                     domain = NA)
            ret <- ret + (H$Z - crossprod(H$X, sAH))
        }
        as.matrix(ret)
    }
    
    # stratified negative score residual
    
    .snsr <- function(parm, x, mu = 0, rightcensored = FALSE) 
    {

        # stratum prep
        
        C <- vapply(x, NROW, 0L) ### might differ by stratum
        K <- unique(do.call("c", lapply(x, ncol))) ### the same
        B <- length(x)
        sidx <- factor(rep(seq_len(B), times = pmax(0, C - 1L)), 
                       levels = seq_len(B))
        bidx <- seq_len(K - 1L)
        delta <- parm[bidx]
        intercepts <- split(parm[-bidx], sidx)
        

        ret <- c()
        for (b in seq_len(B)) {
            idx <- attr(x[[b]], "idx")
            ### idx == 1L means zero residual, see definition of idx
            sr <- c(0, .nsr(c(delta, intercepts[[b]]), x[[b]], mu = mu,
                            rightcensored = rightcensored))
            ret <- c(ret, sr[idx])
        }
        return(ret)
    }
    
    # profile
    
    fn <- function(par) 
    {
        ret <- .snll(par, x = xlist, mu = mu)
        if (!is.null(xrc))
            ret <- ret + .snll(par, x = xrclist, mu = mu, 
                               rightcensored = TRUE)
        return(ret)
    }
    gr <- function(par) 
    {
        ret <- .snsc(par, x = xlist, mu = mu)
        if (!is.null(xrc))
            ret <- ret + .snsc(par, x = xrclist, mu = mu, 
                               rightcensored = TRUE)
        return(ret)
    }

    ### allocate memory for hessian
    Hess <- Matrix::Matrix(0, nrow = length(start), ncol = length(start))

    he <- function(par) 
    {
        if (!is.null(xrc)) {
            ret <- .shes(par, x = xlist, mu = mu, xrc = xrclist, full = Hess, 
                         retMatrix = names(control)[1L] == ".NewtonRaphson")
        } else {
            ret <- .shes(par, x = xlist, mu = mu, full = Hess, 
                         retMatrix = names(control)[1L] == ".NewtonRaphson")
        }
        return(ret)
    }

    .profile <- function(start, fix = seq_len(K - 1)) 
    {
        if (!all(fix %in% seq_len(K - 1)))
            stop(gettextf("Incorrect argument 'fix' in %s",
                          "free1way"),
                 domain = NA)
        delta <- start[fix]
        opargs <- list(start = start[-fix], 
                         objective = function(par) {
                             p <- numeric(length(par) + length(fix))
                             p[fix] <- delta
                             p[-fix] <- par
                             fn(p)
                         },
                         gradient = function(par) {
                             p <- numeric(length(par) + length(fix))
                             p[fix] <- delta
                             p[-fix] <- par
                             gr(p)[-fix]
                         },
                         hessian = function(par) {
                             p <- numeric(length(par) + length(fix))
                             p[fix] <- delta
                             p[-fix] <- par
                             he(p)[-fix, -fix, drop = FALSE]
                         })
        opargs$control <- control[[1L]]
        MPL_Jeffreys <- FALSE ### turn off Jeffreys penalisation in .profile

        # do optim
        
        maxit <- control[[1L]]$iter.max
        while(maxit < 10001) {
           ret <- do.call(names(control)[[1L]], opargs)
           maxit <- 5 * maxit
           if (ret$convergence > 0) {
               opargs$control$eval.max <- maxit
               opargs$control$iter.max <- maxit
               opargs$start <- ret$par
           } else {
               break()
           }
        }

        if (isTRUE(MPL_Jeffreys)) {
            # Jeffreys penalisation
            
            .pll_Jeffreys <- function(cf, start) 
            {
                fix <- seq_along(cf)
                start[fix] <- cf
                ### compute profile likelihood w/o warnings
                ret <- suppressWarnings(.profile(start, fix = fix))
                Hfull <- he(ret$par)
                Hfix <- as.matrix(solve(solve(Hfull)[fix, fix]))
                return(ret$value - 
                       .5 * determinant(Hfix, logarithm = TRUE)$modulus)
            }
            if (K == 2) {
                MLcf <- ret$par[seq_len(K - 1)]
                Fret <- optim(MLcf, fn = .pll_Jeffreys, start = ret$par,
                              method = "Brent", lower = MLcf - 5, 
                              upper = MLcf + 5)
            } else {
                ### Nelder-Mead
                Fret <- optim(ret$par[seq_len(K - 1)], fn = .pll_Jeffreys, 
                              start = ret$par)
            }
            if (Fret$convergence == 0) {
                start <- ret$par
                start[seq_len(K - 1)] <- Fret$par
                ret <- .profile(start, fix = seq_len(K - 1))
                ret$objective <- ret$value
            }
            
        } else {
            if (ret$convergence > 0) {
                if (is.na(MPL_Jeffreys)) { ### only after failure
                    warning(gettextf(paste("Jeffreys penalisation was applied in %s because initial optimisation failed with:", 
                                     ret$message),
                                    "free1way"),
                                     domain = NA)
                    MPL_Jeffreys <- TRUE
                    # Jeffreys penalisation
                    
                    .pll_Jeffreys <- function(cf, start) 
                    {
                        fix <- seq_along(cf)
                        start[fix] <- cf
                        ### compute profile likelihood w/o warnings
                        ret <- suppressWarnings(.profile(start, fix = fix))
                        Hfull <- he(ret$par)
                        Hfix <- as.matrix(solve(solve(Hfull)[fix, fix]))
                        return(ret$value - 
                               .5 * determinant(Hfix, logarithm = TRUE)$modulus)
                    }
                    if (K == 2) {
                        MLcf <- ret$par[seq_len(K - 1)]
                        Fret <- optim(MLcf, fn = .pll_Jeffreys, start = ret$par,
                                      method = "Brent", lower = MLcf - 5, 
                                      upper = MLcf + 5)
                    } else {
                        ### Nelder-Mead
                        Fret <- optim(ret$par[seq_len(K - 1)], fn = .pll_Jeffreys, 
                                      start = ret$par)
                    }
                    if (Fret$convergence == 0) {
                        start <- ret$par
                        start[seq_len(K - 1)] <- Fret$par
                        ret <- .profile(start, fix = seq_len(K - 1))
                        ret$objective <- ret$value
                    }
                    
                }
           }
        }
        if (ret$convergence > 0)
            warning(gettextf(paste("Unsuccessful optimisation in %s:", ret$message),
                                   "free1way"),
                                   domain = NA)

        ret$MPL_Jeffreys <- MPL_Jeffreys
        ret$value <- ret$objective
        ret$objective <- NULL
        

        p <- numeric(length(start))
        p[fix] <- delta
        p[-fix] <- ret$par
        ret$par <- p
        ret
    }
    
    # optim
    
    if (!length(fix)) {
        opargs <- list(start = start, 
                       objective = fn, 
                       gradient = gr,
                       hessian = he)
        opargs$control <- control[[1L]]
        # do optim
        
        maxit <- control[[1L]]$iter.max
        while(maxit < 10001) {
           ret <- do.call(names(control)[[1L]], opargs)
           maxit <- 5 * maxit
           if (ret$convergence > 0) {
               opargs$control$eval.max <- maxit
               opargs$control$iter.max <- maxit
               opargs$start <- ret$par
           } else {
               break()
           }
        }

        if (isTRUE(MPL_Jeffreys)) {
            # Jeffreys penalisation
            
            .pll_Jeffreys <- function(cf, start) 
            {
                fix <- seq_along(cf)
                start[fix] <- cf
                ### compute profile likelihood w/o warnings
                ret <- suppressWarnings(.profile(start, fix = fix))
                Hfull <- he(ret$par)
                Hfix <- as.matrix(solve(solve(Hfull)[fix, fix]))
                return(ret$value - 
                       .5 * determinant(Hfix, logarithm = TRUE)$modulus)
            }
            if (K == 2) {
                MLcf <- ret$par[seq_len(K - 1)]
                Fret <- optim(MLcf, fn = .pll_Jeffreys, start = ret$par,
                              method = "Brent", lower = MLcf - 5, 
                              upper = MLcf + 5)
            } else {
                ### Nelder-Mead
                Fret <- optim(ret$par[seq_len(K - 1)], fn = .pll_Jeffreys, 
                              start = ret$par)
            }
            if (Fret$convergence == 0) {
                start <- ret$par
                start[seq_len(K - 1)] <- Fret$par
                ret <- .profile(start, fix = seq_len(K - 1))
                ret$objective <- ret$value
            }
            
        } else {
            if (ret$convergence > 0) {
                if (is.na(MPL_Jeffreys)) { ### only after failure
                    warning(gettextf(paste("Jeffreys penalisation was applied in %s because initial optimisation failed with:", 
                                     ret$message),
                                    "free1way"),
                                     domain = NA)
                    MPL_Jeffreys <- TRUE
                    # Jeffreys penalisation
                    
                    .pll_Jeffreys <- function(cf, start) 
                    {
                        fix <- seq_along(cf)
                        start[fix] <- cf
                        ### compute profile likelihood w/o warnings
                        ret <- suppressWarnings(.profile(start, fix = fix))
                        Hfull <- he(ret$par)
                        Hfix <- as.matrix(solve(solve(Hfull)[fix, fix]))
                        return(ret$value - 
                               .5 * determinant(Hfix, logarithm = TRUE)$modulus)
                    }
                    if (K == 2) {
                        MLcf <- ret$par[seq_len(K - 1)]
                        Fret <- optim(MLcf, fn = .pll_Jeffreys, start = ret$par,
                                      method = "Brent", lower = MLcf - 5, 
                                      upper = MLcf + 5)
                    } else {
                        ### Nelder-Mead
                        Fret <- optim(ret$par[seq_len(K - 1)], fn = .pll_Jeffreys, 
                                      start = ret$par)
                    }
                    if (Fret$convergence == 0) {
                        start <- ret$par
                        start[seq_len(K - 1)] <- Fret$par
                        ret <- .profile(start, fix = seq_len(K - 1))
                        ret$objective <- ret$value
                    }
                    
                }
           }
        }
        if (ret$convergence > 0)
            warning(gettextf(paste("Unsuccessful optimisation in %s:", ret$message),
                                   "free1way"),
                                   domain = NA)

        ret$MPL_Jeffreys <- MPL_Jeffreys
        ret$value <- ret$objective
        ret$objective <- NULL
        
    } else if (length(fix) == length(start)) {
        ret <- list(par = start, 
                    value = fn(start))
    } else {
        ret <- .profile(start, fix = fix)
    }
     
    # post processing
    
    if (is.null(fix) || (length(fix) == length(start)))
        parm <- seq_len(K - 1)
    else 
        parm <- fix
    if (any(parm >= K)) return(ret)

    ret$coefficients <- ret$par[parm]
    dn2 <- dimnames(xt)[2L]
    names(ret$coefficients) <- cnames <- paste0(names(dn2), dn2[[1L]][1L + parm])

    par <- ret$par
    intercepts <- function(parm, x) 
    {

        # stratum prep
        
        C <- vapply(x, NROW, 0L) ### might differ by stratum
        K <- unique(do.call("c", lapply(x, ncol))) ### the same
        B <- length(x)
        sidx <- factor(rep(seq_len(B), times = pmax(0, C - 1L)), 
                       levels = seq_len(B))
        bidx <- seq_len(K - 1L)
        delta <- parm[bidx]
        intercepts <- split(parm[-bidx], sidx)
        

        return(intercepts)
    }
    ret$intercepts <- intercepts(par, x = xlist)

    if (score) {
        ret$negscore <- .snsc(par, x = xlist, mu = mu)[parm]
        if (!is.null(xrc))
            ret$negscore <- ret$negscore + .snsc(par, x = xrclist, mu = mu, 
                                                 rightcensored = TRUE)[parm]
    }
    if (hessian) {
        if (!is.null(xrc)) {
            ret$hessian <- .shes(par, x = xlist, mu = mu, xrc = xrclist)
        } else {
            ret$hessian <- .shes(par, x = xlist, mu = mu)
        }
        ret$vcov <- solve(ret$hessian)
        if (length(parm) != nrow(ret$hessian))
           ret$hessian <- solve(ret$vcov <- ret$vcov[parm, parm, drop = FALSE])
        rownames(ret$vcov) <- colnames(ret$vcov) <- rownames(ret$hessian) <-
            colnames(ret$hessian) <-  cnames
    }
    if (residuals) {
        ret$negresiduals <- .snsr(par, x = xlist, mu = mu)
        if (!is.null(xrc)) {
            rcr <- .snsr(par, x = xrclist, mu = mu, rightcensored = TRUE)
            ret$negresiduals <- c(rbind(matrix(ret$negresiduals, nrow = C),
                                        matrix(rcr, nrow = C)))
         }
    }
    ret$profile <- function(start, fix)
        .free1wayML(xt, link = link, mu = mu, start = start, fix = fix, tol = tol, 
                    ...) 
    ret$table <- xt

    ret$strata <- strata
    ret$mu <- mu
    if (length(ret$mu) == 1) {
        names(ret$mu) <- link$parm
    } else {
        names(ret$mu) <- c(paste(link$parm, cnames[1L], sep = ":"), cnames[-1L])
    }
    

    class(ret) <- "free1wayML"
    ret
}

# free1way generic and table method (main workhorse)

free1way <- function(y, ...)
    UseMethod("free1way")

free1way.table <- function(y, link = c("logit", "probit", "cloglog", "loglog"), 
                           mu = 0, B = 0, exact = FALSE, ...)
{

    cl <- match.call()

    d <- dim(y)
    dn <- dimnames(y)
    DNAME <- NULL
    if (!is.null(dn)) {
        DNAME <- paste(names(dn)[1], "by", names(dn)[2], 
                       paste0("(", paste0(dn[2], collapse = ", "), ")"))
        if (length(dn) == 3L)
            DNAME <- paste(DNAME, "\n\t stratified by", names(dn)[3])
    }

    # link2fun
    
    if (!inherits(link, "linkfun")) {
        link <- match.arg(link)
        link <- do.call(link, list())
    }
    

    if (!(length(mu) == 1L || length(mu) == d[2L] - 1L)) {
        warning(gettextf("Incompatible length of argument 'mu' in %s",
                         "free1way"),
                domain = NA)
        mu <- rep(mu, length.out = d[2L] - 1L)
    }

    ret <- .free1wayML(y, link = link, mu = mu, ...)
    ret$link <- link
    ret$data.name <- DNAME
    ret$call <- cl

    # free1way permutation tests
    
    alias <- link$alias
    if (length(link$alias) == 2L) alias <- alias[1L + (d[2] > 2L)]
    stratified <- FALSE
    if (length(d) == 3L) stratified <- d[3L] > 1
    ret$method <- paste(ifelse(stratified, "Stratified", ""), 
                        paste0(d[2L], "-sample"), alias, 
                        "test against", link$model, "alternatives")

    cf <- ret$par
    ### compute the permutation distribution always
    ### for H0: delta = 0, not delta = mu
    ### otherwise, permutation confidence intervals
    ### are not aligned with permutation p-values
    cf[idx <- seq_len(d[2L] - 1L)] <- -mu
    pr <- ret$profile(cf, idx)
    res <- - pr$negresiduals
    if (d[2L] == 2L)
        res <- res / sqrt(c(pr$hessian))

    # Strasser Weber

    .SW <- function(res, xt) 
    {

        if (length(dim(xt)) == 3L) {
            res <- matrix(res, nrow = dim(xt)[1L], ncol = dim(xt)[3])
            STAT <-  Exp <- Cov <- 0
            for (b in seq_len(dim(xt)[3L])) {
                sw <- .SW(res[,b, drop = TRUE], xt[,,b, drop = TRUE])
                STAT <- STAT + sw$Statistic
                Exp <- Exp + sw$Expectation
                Cov <- Cov + sw$Covariance
            }
            return(list(Statistic = STAT, Expectation = as.vector(Exp),
                        Covariance = Cov))
        }

        Y <- matrix(res, ncol = 1, nrow = length(xt))
        weights <- c(xt)
        x <- gl(ncol(xt), nrow(xt))
        X <- model.matrix(~ x, data = data.frame(x = x))[,-1L,drop = FALSE]

        w. <- sum(weights)
        wX <- weights * X
        wY <- weights * Y
        ExpX <- colSums(wX)
        ExpY <- colSums(wY) / w.
        CovX <- crossprod(X, wX)
        Yc <- t(t(Y) - ExpY)
        CovY <- crossprod(Yc, weights * Yc) / w.
        Exp <- kronecker(ExpY, ExpX)
        Cov <- w. / (w. - 1) * kronecker(CovY, CovX) -
               1 / (w. - 1) * kronecker(CovY, tcrossprod(ExpX))
        STAT <- crossprod(X, wY)
        list(Statistic = STAT, Expectation = as.vector(Exp),
             Covariance = Cov)
    }
    
    # resampling

    .resample <- function(res, xt, B = 10000) 
    {

        if (length(dim(xt)) == 2L)
            xt <- as.table(array(xt, dim = c(dim(xt), 1)))

        res <- matrix(res, nrow = dim(xt)[1L], ncol = dim(xt)[3L])
        stat <- 0
        ret <- .SW(res, xt)
        if (dim(xt)[2L] == 2L) {
            ret$testStat <- c((ret$Statistic - ret$Expectation) / 
                              sqrt(c(ret$Covariance)))
        } else {
            ES <- ret$Statistic - ret$Expectation
            ret$testStat <- sum(ES * solve(ret$Covariance, ES))
        }
        ret$DF <- dim(xt)[2L] - 1L

        if (B) {
            for (j in 1:dim(xt)[3L]) {
               rt <- r2dtable(B, r = rowSums(xt[,,j]), c = colSums(xt[,,j]))
               stat <- stat + vapply(rt, 
                   function(x) .colSums(x[,-1L, drop = FALSE] * res[,j], 
                                        m = nrow(x), n = ncol(x) - 1L), 
                                     FUN.VALUE = rep(0, dim(xt)[[2L]] - 1L))
            }
            if (dim(xt)[2L] == 2L) {
                 ret$permStat <- (stat - ret$Expectation) / 
                                  sqrt(c(ret$Covariance))
            } else {
                ES <- matrix(stat, ncol = B) - ret$Expectation
                ret$permStat <- .colSums(ES * solve(ret$Covariance, ES), 
                                         m = dim(xt)[[2L]] - 1L, n = B)
            }
        }
        ret
    }
    

    if (length(dim(y)) == 3L) y <- y[,,ret$strata, drop = FALSE]
    if (length(dim(y)) == 4L) {
        y <- y[,,ret$strata,, drop = FALSE]
        dy <- dim(y)
        dy[1] <- dy[1] * 2
        y <- apply(y, 3, function(x) rbind(x[,,"TRUE"], x[,,"FALSE"]), simplify = FALSE)
        y <- array(unlist(y), dim = dy[1:3])
    }

    ### exact two-sample Wilcoxon w/o stratification
    if (exact) {
        if (!stratified && link$model == "proportional odds" && d[2L] == 2L) {
            # exact proportional odds
            
            .exact <- function(z, grp, w = rep.int(1, length(z))) 
            {

                z <- rep(z, times = w)
                grp <- rep(grp, times = w)
                x <- rank(z)
                f <- 2 - all(x == floor(x))
                x <- as.integer(x * f)
                x <- x - min(x) + 1L
                sx <- sort(x)

                m <- as.integer(sum(grp > 0))
                stopifnot(m > 1)
                stopifnot(m < length(x))

                d <- .Call(C_dpermdist2, sx, m)
                s <- seq.int(from = 1L, to = sum(rev(sx)[seq_len(m)]), by = 1L)

                STATISTIC <- sum(x[grp > 0])
                F <- cumsum(d)
                S <- rev(cumsum(rev(d)))
                cf <- lm.fit(x = cbind(1, x), y = as.double(z))$coefficients

                z2x <- function(z) round((z - m * cf[1]) / cf[2])

                c(ple = function(z) sum(d[s <= z2x(z)]), # s and STATISTIC are integers
                  pgr = function(z) sum(d[s >= z2x(z)]), 
                  qle = function(q) c(m, max(s[F < q + 1e-08])) %*% cf,
                  qgr = function(q) c(m, min(s[S < q + 1e-08])) %*% cf)
            }
            
            ret$exact <- .exact(c(res, res), grp = unclass(gl(2, d[1L])) - 1L,
                                w = c(y))
            B <- 0
        } else {
            warning(gettextf("Cannot compute exact permutation distribution in %s",
                             "free1way"),
                    domain = NA)
        }
    } 
    ret$perm <- .resample(res, y, B = B)
    

    if (ret$MPL_Jeffreys) 
        ret$method <- paste(ret$method, 
            "with Jeffreys prior penalisation", sep = ", ")

    class(ret) <- "free1way"
    return(ret)
}

# free1way methods

coef.free1way <- function(object, what = c("shift", "PI", "AUC", "OVL"), ...)
{
    what <- match.arg(what)
    cf <- object$coefficients
    return(switch(what, "shift" = cf,
                        "PI" = object$link$parm2PI(cf),
                        "AUC" = object$link$parm2PI(cf),        ### same as PI
                        "OVL" = object$link$parm2OVL(cf)))
}
vcov.free1way <- function(object, ...)
    object$vcov
logLik.free1way <- function(object, ...)
    -object$value
model.frame.free1way <- function(formula, ...) {
    if (!is.null(formula[["data"]])) return(formula[["data"]])
    ret <- as.data.frame(formula$table)
    ret <- ret[rep(seq_len(nrow(ret)), ret$Freq),,drop = FALSE]
    ret
}
### the next two might go into multcomp
terms.free1way <- function(x, ...) 
{
    mf <- model.frame(x)
    terms(as.formula(paste(names(mf)[1:2], collapse = "~")), 
          data = mf)
}
model.matrix.free1way <- function (object, ...) 
{
    mf <- model.frame(object)
    tm <- terms(object)
    mm <- model.matrix(delete.response(tm), data = mf)
    at <- attributes(mm)
    mm <- mm[, -1]
    at$dim[2] <- at$dim[2] - 1
    at$dimnames[[2]] <- at$dimnames[[2]][-1]
    at$assign <- at$assign[-1]
    attributes(mm) <- at
    mm
}

# free1way print

.print.free1way <- function(x, test = c("Permutation", "Wald", "LRT", "Rao"), 
                            alternative = c("two.sided", "less", "greater"), 
                            tol = sqrt(.Machine$double.eps), 
                            mu = 0, ### allow permutation testing non-null hypotheses
                                    ### in alignment with confint(free1way(, B > 0))
                            ...)
{

    test <- match.arg(test)
    alternative <- match.arg(alternative)

    ### global
    cf <- coef(x)
    if ((length(cf) > 1L || test == "LRT") && alternative != "two.sided") 
        stop(gettextf("Cannot compute one-sided p-values in %s",
                      "free1way"),
             domain = NA)

    DF <- NULL
    parm <- seq_along(cf)
    value <- mu

    # statistics
    
    if (test == "Wald") {
        # Wald statistic
        
        if (alternative == "two.sided") {
            STATISTIC <- c("Wald chi-squared" = 
                           c(crossprod(cf, x$hessian %*% cf)))
            DF <- c("df" = length(parm))
            PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
        } else {
            STATISTIC <- c("Wald Z" = unname(c(cf * sqrt(c(x$hessian)))))
            PVAL <- pnorm(STATISTIC, lower.tail = alternative == "less")
        }
        
    } else if (test == "LRT") {
        # LRT
        
        par <- x$par
        par[parm] <- value
        unll <- x$value ### neg logLik
        rnll <- x$profile(par, parm)$value ### neg logLik
        STATISTIC <- c("logLR chi-squared" = - 2 * (unll - rnll))
        DF <- c("df" = length(parm))
        PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
        
    } else if (test == "Rao") {
        # Rao
        
        par <- x$par
        par[parm] <- value
        ret <- x$profile(par, parm)
        if (alternative == "two.sided") {
            STATISTIC <- c("Rao chi-squared" = c(crossprod(ret$negscore, 
                                                           ret$vcov %*%
                                                           ret$negscore)))
            DF <- c("df" = length(parm))
            PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
        } else {
            STATISTIC <- c("Rao Z" = unname(- ret$negscore * 
                                              sqrt(c(ret$vcov))))
            PVAL <- pnorm(STATISTIC, lower.tail = alternative == "less")
        }
        
    } else if (test == "Permutation") {
        # Permutation statistics
        
        par <- x$par
        par[parm] <- value
        ret <- x$profile(par, parm)
        sc <- - ret$negscore
        if (length(cf) == 1L)
            sc <- sc / sqrt(c(ret$hessian))
        if (!is.null(x$exact)) {
            STATISTIC = c("W" = sc)
        } else {
            Esc <- sc - x$perm$Expectation

            if (alternative == "two.sided" && length(cf) > 1L) {
                STATISTIC <- c("Perm chi-squared" = 
                               sum(Esc * solve(x$perm$Covariance, Esc)))
            } else {
                STATISTIC <- c("Perm Z" = Esc / sqrt(c(x$perm$Covariance)))
            }
        }
        
    }
    

    if (test == "Permutation") {
        # Permutation p-values
        
        if (!is.null(x$exact)) {
            PVAL <- switch(alternative,
                           "two.sided" = 2 * min(c(x$exact$ple(sc), 
                                                   x$exact$pgr(sc))),
                           "less" = x$exact$ple(sc),
                           "greater" = x$exact$pgr(sc))
        } else {
            .pm <- function(x) sum(x) / length(x) 
            ps <- x$perm$permStat

            .GE <- function(x, y)
                (y - x) <= sqrt(.Machine$double.eps)

            .LE <- function(x, y)
                (x - y) <= sqrt(.Machine$double.eps)

            if (alternative == "two.sided" && length(cf) > 1L) {
                if (!is.null(ps)) {
                    PVAL <- .pm(.GE(ps, STATISTIC))
                } else {
                    DF <- c("df" = x$perm$DF)
                    PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
                }
            } else {
                if (!is.null(ps)) {
                    PVALle <- .pm(.LE(ps, STATISTIC))
                    PVALge <- .pm(.GE(ps, STATISTIC))
                    if (alternative == "two.sided")
                        PVAL <- 2 * min(c(PVALle, PVALge))
                    else if (alternative == "less")
                        PVAL <- PVALle
                    else
                        PVAL <- PVALge
                } else {
                    if (alternative == "two.sided")
                        PVAL <- pchisq(STATISTIC^2, df = 1, lower.tail = FALSE)
                    else
                        PVAL <- pnorm(STATISTIC, 
                                      lower.tail = alternative == "less")
                }
            }
        }
        
    }

    RVAL <- list(statistic = STATISTIC, parameter = DF, p.value = PVAL, 
        null.value = x$mu, alternative = alternative, method = x$method, 
        data.name = x$data.name)
    class(RVAL) <- "htest"
    return(RVAL)
}

print.free1way <- function(x, ...) 
{
    print(ret <- .print.free1way(x, ...))
    return(invisible(x))
}

# free1way summary

summary.free1way <- function(object, test, 
                             alternative = c("two.sided", "less", "greater"), 
                             tol = .Machine$double.eps, ...)
{

    if (!missing(test))
        return(.print.free1way(object, test = test, 
                               alternative = alternative, tol = tol, ...))
   
    alternative <- match.arg(alternative)

    ESTIMATE <- coef(object)
    SE <- sqrt(diag(vcov(object)))
    STATISTIC <- unname(ESTIMATE / SE)
    if (alternative == "less") {
        PVAL <- pnorm(STATISTIC)
    } else if (alternative == "greater") {
        PVAL <- pnorm(STATISTIC, lower.tail = FALSE)
    } else {
        PVAL <- 2 * pnorm(-abs(STATISTIC))
    }
    cfmat <- cbind(ESTIMATE, SE, STATISTIC, PVAL)
    colnames(cfmat) <- c(object$link$parm, "Std. Error", "z value",
                         switch(alternative, "two.sided" = "P(>|z|)",
                                             "less" = "P(<z)",
                                             "greater" = "P(>z)"))
    ret <- list(call = object$call, coefficients = cfmat)
    class(ret) <- "summary.free1way"
    return(ret)
}

print.summary.free1way <- function(x, ...) 
{
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    cat("Coefficients:\n")
    printCoefmat(x$coefficients)
}

# free1way confint

confint.free1way <- function(object, parm,
    level = .95, test = c("Permutation", "Wald", "LRT", "Rao"), 
    what = c("shift", "PI", "AUC", "OVL"), ...)
{

    test <- match.arg(test)
    conf.level <- 1 - (1 - level) / 2

    cf <- coef(object)
    if (missing(parm)) 
        parm <- seq_along(cf)

    CINT <- confint.default(object, level = level)
    if (test != "Wald") {
        wlevel <- level
        wlevel <- 1 - (1 - level) / 2
        CINT[] <- confint.default(object, level = wlevel)

        sfun <- function(value, parm, quantile) 
        {
            x <- object
            alternative <- "two.sided"
            tol <- .Machine$double.eps
            # statistics
            
            if (test == "Wald") {
                # Wald statistic
                
                if (alternative == "two.sided") {
                    STATISTIC <- c("Wald chi-squared" = 
                                   c(crossprod(cf, x$hessian %*% cf)))
                    DF <- c("df" = length(parm))
                    PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
                } else {
                    STATISTIC <- c("Wald Z" = unname(c(cf * sqrt(c(x$hessian)))))
                    PVAL <- pnorm(STATISTIC, lower.tail = alternative == "less")
                }
                
            } else if (test == "LRT") {
                # LRT
                
                par <- x$par
                par[parm] <- value
                unll <- x$value ### neg logLik
                rnll <- x$profile(par, parm)$value ### neg logLik
                STATISTIC <- c("logLR chi-squared" = - 2 * (unll - rnll))
                DF <- c("df" = length(parm))
                PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
                
            } else if (test == "Rao") {
                # Rao
                
                par <- x$par
                par[parm] <- value
                ret <- x$profile(par, parm)
                if (alternative == "two.sided") {
                    STATISTIC <- c("Rao chi-squared" = c(crossprod(ret$negscore, 
                                                                   ret$vcov %*%
                                                                   ret$negscore)))
                    DF <- c("df" = length(parm))
                    PVAL <- pchisq(STATISTIC, df = DF, lower.tail = FALSE)
                } else {
                    STATISTIC <- c("Rao Z" = unname(- ret$negscore * 
                                                      sqrt(c(ret$vcov))))
                    PVAL <- pnorm(STATISTIC, lower.tail = alternative == "less")
                }
                
            } else if (test == "Permutation") {
                # Permutation statistics
                
                par <- x$par
                par[parm] <- value
                ret <- x$profile(par, parm)
                sc <- - ret$negscore
                if (length(cf) == 1L)
                    sc <- sc / sqrt(c(ret$hessian))
                if (!is.null(x$exact)) {
                    STATISTIC = c("W" = sc)
                } else {
                    Esc <- sc - x$perm$Expectation

                    if (alternative == "two.sided" && length(cf) > 1L) {
                        STATISTIC <- c("Perm chi-squared" = 
                                       sum(Esc * solve(x$perm$Covariance, Esc)))
                    } else {
                        STATISTIC <- c("Perm Z" = Esc / sqrt(c(x$perm$Covariance)))
                    }
                }
                
            }
            
            ### we also could invert p-values, but the
            ### p-value function might be discrete for permutation
            ### tests, in contrast to the test statistic
            return(STATISTIC - quantile)
        }

        if (test == "Permutation") {
            # permutation confint
            
            if (length(cf) > 1L)
                stop(gettextf("Permutation confidence intervals only available for 2-sample comparisons in %s",
                              "confint.free1way"),
                     domain = NA)
            if (!is.null(object$exact)) {
                qu <- c(object$exact$qle(1 - conf.level),
                        object$exact$qgr(1 - conf.level))
            } else {
                if (is.null(object$perm$permStat)) {
                    qu <- qnorm(conf.level) * c(-1, 1)
                } else {
                    .pq <- function(s, alpha) 
                    {
                        su <- sort(unique(s)) 
                        ### F = P(T <= t), S = P(T >= t)
                        Fs <- cumsum(st <- table(match(s, su)))
                        Ss <- length(s) - Fs + st
                        c(max(su[Fs <= alpha * length(s)]),
                          min(su[Ss <= alpha * length(s)]))
                    }
                    ### cf PVAL computation!!!
                    rs <- object$perm$permStat
                    qu <- .pq(round(rs, 10), alpha = 1 - conf.level)
                    att.level <- mean(rs > qu[1] & rs < qu[2])
                    attr(CINT, "Attained level") <- att.level
                }
            }
            
        } else {
            qu <- rep.int(qchisq(level, df = 1), 2) ### always two.sided
        }

        for (p in parm) {
            # confint lower
            
            CINT[p,1] <- max(CINT[p, 1], cf[p] - 1)
            sdlwr <- sign(sfun(cf[p], parm = p, quantile = qu[2]))
            slwr <- try(sfun(CINT[p,1], parm = p, quantile = qu[2]))
            k <- 1
            if (inherits(slwr, "try-error")) {
                CINT[p,1] <- NA
            } else {
                while ((is.na(slwr) || 
                        sign(slwr) == sdlwr) && k < 30) {
                    CINT[p,1] <- CINT[p,1] - 1
                    slwr <- try(sfun(CINT[p,1], parm = p, quantile = qu[2]))
                    if (inherits(slwr, "try-error")) {
                        CINT[p,1] <- NA
                        break()
                    }
                    k <- k + 1
                }
            }
            if (k == 30) {
                CINT[p,1] <- NA
            } else {
                lwr <- try(uniroot(sfun, interval = c(CINT[p,1], cf[p]), 
                                   parm = p, quantile = qu[2])$root)
                if (inherits(lwr, "try-error")) {
                    CINT[p,1] <- NA
                } else {
                    CINT[p,1] <- lwr
                }
            }
            if (is.na(CINT[p,1]))
                warning(gettextf("Failed to compute confidence interval in %s",
                                 "confint.free1way"),
                        domain = NA)

            
            # confint upper
            
            CINT[p,2] <- min(CINT[p, 2], cf[p] + 1)
            sdupr <- sign(sfun(cf[p], parm = p, quantile = qu[1]))
            supr <- try(sfun(CINT[p,2], parm = p, quantile = qu[1]))
            k <- 1
            if (inherits(supr, "try-error")) {
                CINT[p,2] <- NA
            } else {
                while ((is.na(supr) || 
                        sign(supr) == sdupr) && k < 30) {
                    CINT[p,2] <- CINT[p,2] + 1
                    supr <- try(sfun(CINT[p,2], parm = p, quantile = qu[1]))
                    if (inherits(supr, "try-error")) {
                        CINT[p,2] <- NA
                        break()
                    }
                    k <- k + 1
                }
            }
            if (k == 30) {
                CINT[p,2] <- NA 
            } else {
                upr <- try(uniroot(sfun, interval = c(cf[p], CINT[p, 2]), 
                                   parm = p, quantile = qu[1])$root)
                if (inherits(upr, "try-error")) {
                    CINT[p, 2] <- NA
                } else {
                    CINT[p, 2] <- upr
                }
            }
            if (is.na(CINT[p,2]))
                warning(gettextf("Failed to compute confidence interval in %s",
                                 "confint.free1way"),
                        domain = NA)
            
        }
    }

    what <- match.arg(what)
    CINT <- switch(what, "shift" = CINT,
                         "PI" = object$link$parm2PI(CINT),
                         "AUC" = object$link$parm2PI(CINT), ### same as PI 
                         "OVL" = object$link$parm2OVL(CINT))
    return(CINT)
}

# free1way formula

free1way.formula <- function(formula, data, weights, subset, na.action = na.pass, 
                             event = NULL, ...)
{
    cl <- match.call()

    # formula business
    
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")

    if (stratum <- (length(formula[[3L]]) > 1)) {
      if ((length(formula[[3L]]) != 3L) || 
          (formula[[3L]][[1L]] != as.name("|")) || 
          (length(formula[[3L]][[2L]]) !=  1L) || 
          (length(formula[[3L]][[3L]]) != 1L)) 
        stop("incorrect specification for 'formula'")
      formula[[3L]][[1L]] <- as.name("+")
    }

    formula <- terms(formula)
    if (length(attr(formula, "term.labels")) > 1L + stratum)
       stop("'formula' missing or incorrect")
    group <- attr(formula, "term.labels")[1L]

    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    

    response <- attr(attr(mf, "terms"), "response")
    DNAME <- paste(vn <- c(names(mf)[response], group), 
                   collapse = " by ") # works in all cases
    w <- as.vector(model.weights(mf))
    y <- mf[[response]]
    if (inherits(y, "Surv")) {
        if (!is.null(event))
            stop(gettextf("Either 'Surv()'in 'formula' or the 'event' argument can be specified in %s",
                          "free1way"),
                 domain = NA)
        if (attr(y, "type") != "right")
            stop(gettextf("%s currently only allows independent right-censoring",
                          "free1way"),
                 domain = NA)
        event <- (y[,2] > 0)
        y <- y[,1]
    }
    g <- factor(mf[[group]])
    mf[[group]] <- g
    lev <- levels(g)
    DNAME <- paste(DNAME, paste0("(", paste0(lev, collapse = ", "), ")"))
    if (nlevels(g) < 2L)
        stop(gettextf("Incorrect argument 'groups' in %s, at least two groups needed",
                      "free1way"),
             domain = NA)
    if (stratum) {
        st <- factor(mf[[3L]])
        mf[[3L]] <- st
        ### nlevels(st) == 1L is explicitly allowed
        vn <- c(vn, names(mf)[3L])
        RVAL <- free1way(y = y, groups = g, blocks = st, event = event, 
                         weights = w, varnames = vn, ...)
        DNAME <- paste(DNAME, paste("\n\t stratified by", names(mf)[3L]))
    } else {
        ## Call the corresponding method
        RVAL <- free1way(y = y, groups = g, event = event, weights = w, 
                         varnames = vn, ...)
    }
    RVAL$data <- mf
    RVAL$data.name <- DNAME
    RVAL$call <- cl
    RVAL
}

# free1way numeric

free1way.numeric <- function(y, groups, blocks = NULL, event = NULL, 
                             weights = NULL, nbins = 0, varnames = NULL, ...) 
{

    # variable names and checks
    
    cl <- match.call()
    if (is.null(varnames))
        varnames <- c(deparse1(substitute(y)), 
                      deparse1(substitute(groups)), 
                      deparse1(substitute(blocks)))

    DNAME <- paste(varnames[1], "by", varnames[2])
    groups <- factor(groups)
    if (nlevels(groups) < 2L)
        stop(gettextf("Incorrect argument 'groups' in %s, at least two groups needed",
                      "free1way"),
             domain = NA)
    DNAME <- paste(DNAME, paste0("(", paste0(levels(groups), collapse = ", "), 
                                 ")"))

    if (!is.null(blocks)) {
        if (length(unique(blocks)) < 2L) {
            blocks <- NULL
        } else {
            blocks <- factor(blocks)
            DNAME <- paste(DNAME, "\n\t stratified by", varnames[3])
        }
    }
    varnames <- varnames[varnames != "NULL"]
    

    if (!is.null(event)) {
        if (!is.logical(event))
            stop(gettextf("%s currently only allows independent right-censoring",
                          "free1way"),
                 domain = NA)
        uy <- sort(unique(y[event]))
    } else {
        uy <- sort(unique(y))
    }
    if (nbins && nbins < length(uy) && is.null(event)) {
        nbins <- ceiling(nbins)
        breaks <- c(-Inf, quantile(y, probs = seq_len(nbins) / (nbins + 1L)), 
                     Inf)
    } else {
        breaks <- c(-Inf, uy, Inf)
    }
    r <- ordered(cut(y, breaks = breaks, ordered_result = TRUE, 
                     labels = FALSE)) ### avoids costly formatC call
    RVAL <- free1way(y = r, groups = groups, blocks = blocks, 
                     event = event, weights = weights, 
                     varnames = varnames, ...)
    RVAL$data.name <- DNAME
    RVAL$call <- cl
    RVAL
}

# free1way factor

free1way.factor <- function(y, groups, blocks = NULL, event = NULL, 
                            weights = NULL, varnames = NULL, ...) 
{

    # variable names and checks
    
    cl <- match.call()
    if (is.null(varnames))
        varnames <- c(deparse1(substitute(y)), 
                      deparse1(substitute(groups)), 
                      deparse1(substitute(blocks)))

    DNAME <- paste(varnames[1], "by", varnames[2])
    groups <- factor(groups)
    if (nlevels(groups) < 2L)
        stop(gettextf("Incorrect argument 'groups' in %s, at least two groups needed",
                      "free1way"),
             domain = NA)
    DNAME <- paste(DNAME, paste0("(", paste0(levels(groups), collapse = ", "), 
                                 ")"))

    if (!is.null(blocks)) {
        if (length(unique(blocks)) < 2L) {
            blocks <- NULL
        } else {
            blocks <- factor(blocks)
            DNAME <- paste(DNAME, "\n\t stratified by", varnames[3])
        }
    }
    varnames <- varnames[varnames != "NULL"]
    

    if (nlevels(y) > 2L && !is.ordered(y))
        stop(gettextf("%s is not defined for unordered responses",
                              "free1way"),
             domain = NA)
    d <- data.frame(w = 1, y = y, groups = groups)
    if (!is.null(weights)) d$w <- weights
    if (is.null(blocks)) blocks <- gl(1, nrow(d))
    d$blocks <- blocks 
    if (!is.null(event)) {
       if (!is.logical(event))
            stop(gettextf("%s currently only allows independent right-censoring",
                          "free1way"),
                domain = NA)
        d$event <- factor(event, levels = c(FALSE, TRUE), 
                          labels = c("FALSE", "TRUE"))
    }
    tab <- xtabs(w ~ ., data = d)
    dn <- dimnames(tab)
    names(dn)[seq_along(varnames)] <- varnames
    dimnames(tab) <- dn
    RVAL <- free1way(tab, ...)
    RVAL$data.name <- DNAME
    RVAL$call <- cl
    RVAL
}

# plot free1way

plot.free1way <- function(x, ..., block = 1L, cdf = FALSE, model = TRUE,
                          col = seq_len(length(coef(object)) + 1L),
                          lty = 1:2, legend = TRUE) 
{

    # extract plot data
    
    object <- x
    x <- object$table
    if (RC <- (length(dim(x)) == 4L)) {
        x <- x[,,block,,drop = FALSE]
        x <- x[marginSums(x, margin = 1) > 0,,,,drop = FALSE]
    } else {
        x <- x[,,block,drop = FALSE]
        x <- x[marginSums(x, margin = 1) > 0,,,drop = FALSE]
    }
    K <- dim(x)[2L]
    ret0 <- matrix(NA, nrow = dim(x)[1L], ncol = K)
    ln <- object$link
    
    # refit block intercepts
    
    ### refit for this block only
    m1 <- .free1wayML(x, link = ln, start = coef(object), 
                      fix = seq_along(coef(object)),
                      residuals = FALSE, hessian = FALSE)
    intercepts <- m1$intercepts[[1L]]
    j1 <- which(attr(get("xlist", environment(m1$profile))[[1L]], "idx") > 1)
    j1 <- j1[-length(j1)]
    cf <- c(0, coef(object))
    
    # marginal fit
    
    for (k in seq_len(K)) {
        y <- x
        if (RC) {
            y[,-k,1,] <- 0
        } else {
            y[,-k,1] <- 0
        }
        start <- numeric(K - 1)
        m0 <- .free1wayML(y, link = ln, start = start, 
                          fix = seq_len(K - 1), residuals = FALSE, 
                          hessian = FALSE)
        j <- which(attr(get("xlist", environment(m0$profile))[[1L]], "idx") > 1)
        ret0[j[-length(j)],k] <- m0$intercepts[[1L]]
    }
    

    # setup canvas
    
    if (cdf) {
        ylim <- c(0, 1)
        FUN <- function(x) ln$linkinv(x)
    } else {
        ylim <- range(c(ret0, intercepts), na.rm = TRUE)
        FUN <- function(x) x
    }

    idx <- seq_len(nrow(x))
    main <- list(...)$main
    if (is.null(main) && dim(object$table)[3L] > 1L)
        main <- paste(names(dimnames(x))[3L], dimnames(x)[[3L]][1L], sep = "=")
    plot(idx, rep(0, length(idx)), type = "n", ylim = ylim, 
         xlab = paste("Rank(", names(dimnames(x))[1L], ")", sep = ""),
         ylab = ifelse(cdf, "Probability", paste(ln$name, "Link")), 
         main = main, ...)
    
    # marginal plot
    
    out <- sapply(seq_len(K), function(k) 
        lines(which(!is.na(ret0[,k])), FUN(ret0[!is.na(ret0[,k]),k]), 
              type = "s", col = col[k], lty = lty[1]))
    
    # model plot
    
    if (model)
        out <- sapply(seq_len(K), function(k) 
            lines(j1, FUN(intercepts - cf[k]), type = "s", col = col[k], 
                  lty = lty[2]))
    
    # add legend
    
    if (legend) {
            legend("topleft", lty = lty[1], col = col, 
                   legend = paste(names(dimnames(x))[2L], dimnames(x)[[2L]]),
                   title = "Nonparametric", bty = "n")
            if (model) 
                legend("bottomright", lty = lty[2], col = col, 
                       legend = paste(names(dimnames(x))[2L], dimnames(x)[[2L]]),
                       title = "Semiparametric", bty = "n")
        }
    
}

# ppplot

ppplot <- function(x, y, plot.it = TRUE,
                   xlab = paste("Cumulative probabilities for", 
                                deparse1(substitute(x))),
                   ylab = paste("Cumulative probabilities for", 
                                deparse1(substitute(y))), 
                   main = "P-P plot",
                   ..., conf.level = NULL, 
                   conf.args = list(link = "logit", type = "Wald", 
                                    col = NA, border = NULL)) 
{

    force(xlab)
    force(ylab)
    if (xlab == ylab) {
        xlab <- paste0("x = ", xlab)
        ylab <- paste0("y = ", ylab)
    }

    ex <- ecdf(x)
    sy <- sort(unique(c(x, y)))
    py <- ecdf(y)(sy)
    px <- ex(sy)
    ret <- stepfun(px, c(0, py))
    if (!plot.it)
        return(ret)

    plot(ret, xlim = c(0, 1), ylim = c(0, 1), 
         xlab = xlab, ylab = ylab, main = main, 
         verticals = FALSE, ...)

    # ROC bands
    
     if (!is.null(conf.level)) {
        prb <- seq_len(1000) / 1001
        res <- c(x, y)
        grp <- gl(2, 1, labels = c(xlab, ylab))
        grp <- grp[rep(1:2, c(length(x), length(y)))]
        args <- conf.args
        args$y <- res
        args$groups <- grp
        args$border <- args$col <- args$type <- NULL
        f1w <- do.call("free1way", args)

        ci <- confint(f1w, level = conf.level, type = args$type)
        lwr <- .p(f1w$link, .q(f1w$link, prb) - ci[1,1])
        upr <- .p(f1w$link, .q(f1w$link, prb) - ci[1,2])
        x <- c(prb, rev(prb))
        y <- c(lwr, rev(upr))
        xn <- c(x[1L], rep(x[-1L], each = 2))
        yn <- c(rep(y[-length(y)], each = 2), y[length(y)])
        polygon(x = xn, y = yn, col = conf.args$col, border = conf.args$border)
        lines(prb, .p(f1w$link, .q(f1w$link, prb) - coef(f1w)))
    }
    

    plot(ret, add = TRUE, verticals = FALSE, ...)

    return(invisible(ret)) 
}

# rfree1way

.rfree1way <- function(n, delta = 0, link = c("logit", "probit", 
                                              "cloglog", "loglog")) 
{

    logU <- log(ret <- runif(n))

    # link2fun
    
    if (!inherits(link, "linkfun")) {
        link <- match.arg(link)
        link <- do.call(link, list())
    }
    

    trt <- (abs(delta) > 0)
    ret[trt] <- .p(link, .q(link, logU[trt], log.p = TRUE) + delta[trt])

    return(ret)
}

rfree1way <- function(n, prob = NULL, alloc_ratio = 1, 
                      blocks = ifelse(is.null(prob), 1, NCOL(prob)), 
                      strata_ratio = 1, delta = 0, offset = 0, 
                      link = c("logit", "probit", "cloglog", "loglog"))
{

    B <- blocks

    # design args
    
    K <- length(delta) + 1L
    if (is.null(names(delta))) 
        names(delta) <- LETTERS[seq_len(K)[-1]]
    if (length(alloc_ratio) == 1L) 
        alloc_ratio <- rep_len(alloc_ratio, K - 1)
    if (length(alloc_ratio) != K - 1L)
        stop("Incorrect argument 'alloc_ratio'")
    if (length(strata_ratio) == 1L) 
        strata_ratio <- rep_len(strata_ratio, B - 1)
    if (length(strata_ratio) != B - 1L)
        stop("Incorrect argument 'strata_ratio'")
    ### sample size per group (columns) and stratum (rows)
    N <- n * matrix(c(1, alloc_ratio), nrow = B, ncol = K, byrow = TRUE) * 
             matrix(c(1, strata_ratio), nrow = B, ncol = K)
    

    rownames(N) <- paste0("block", seq_len(B))
    ctrl <- "Control"
    colnames(N) <- c(ctrl, names(delta))

    if (length(offset) != K)
        offset <- rep_len(offset, K)

    trt <- gl(K, 1, labels = colnames(N))
    blk <- gl(B, 1, labels = rownames(N))
    ret <- expand.grid(groups = trt, blocks = blk)
    if (B == 1L) ret$blocks <- NULL
    ret <- ret[rep(seq_len(nrow(ret)), times = N), , drop = FALSE]
    ret$y <- .rfree1way(nrow(ret), 
                        delta = offset[ret$groups] + c(0, delta)[ret$groups], 
                        link = link)
    if (is.null(prob)) return(ret)

    ### return discrete distribution
    if (!is.matrix(prob))
        prob <- matrix(prob, nrow = NROW(prob), ncol = B)
    if (ncol(prob) != B)
        stop(gettextf("Incorrect number of columns for 'prob' in %s",
                      "rfree1way"),
             domain = NA)
    prob <- prop.table(prob, margin = 2L)
    ret <- do.call("rbind", lapply(1:ncol(prob), function(b) {
        if (B > 1)
            ret <- subset(ret, blocks == levels(blocks)[b])
        ret$y <- cut(ret$y, breaks = c(-Inf, cumsum(prob[,b])), 
                     labels = paste0("Y", 1:nrow(prob)), ordered_result = TRUE)
        ret
    }))
    return(ret)
}

# power

power.free1way.test <- function(n = NULL, 
                                prob = if (is.null(n)) NULL else 
                                                       rep.int(1 / n, n), 
                                alloc_ratio = 1, 
                                blocks = if (is.null(prob)) 1 else NCOL(prob), 
                                strata_ratio = 1, 
                                delta = NULL, mu = 0, 
                                sig.level = .05, power = NULL,
                                link = c("logit", "probit", "cloglog", "loglog"),
                                alternative = c("two.sided", "less", "greater"), 
                                nsim = 100, seed = NULL, 
                                tol = .Machine$double.eps^0.25) 
{

    # power args check
    
    if (sum(vapply(list(n, delta, power, sig.level), is.null, 
        NA)) != 1) 
        stop("exactly one of 'n', 'delta', 'power', and 'sig.level' must be NULL")
    assert_NULL_or_prob(sig.level)
    assert_NULL_or_prob(power)
    

    # random seed
    
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    if (is.null(seed)) 
        seed <- RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    

    # r2dsim
    
    .r2dsim <- function(n, r, c, delta = 0,
                       link = c("logit", "probit", "cloglog", "loglog")) 
    {

        if (length(n <- as.integer(n)) == 0L || (n < 0) || is.na(n)) 
            stop("invalid argument 'n'")
        colsums <- c
        if (length(colsums[] <- as.integer(c)) <= 1L || 
            any(colsums < 0) || anyNA(colsums)) 
            stop("invalid argument 'c'")

        prob <- r
        if (length(prob[] <- as.double(r / sum(r))) <= 1L || 
            any(prob < 0) || anyNA(prob)) 
            stop("invalid argument 'r'")

        if (is.null(names(prob))) 
            names(prob) <- paste0("i", seq_along(prob))
        
        K <- length(colsums)
        if (is.null(names(colsums))) 
            names(colsums) <- LETTERS[seq_len(K)]
        delta <- rep_len(delta, K - 1L)

        # link2fun
        
        if (!inherits(link, "linkfun")) {
            link <- match.arg(link)
            link <- do.call(link, list())
        }
        

        p0 <- cumsum(prob)
        h0 <- .q(link, p0[-length(p0)]) ### last element of p0 is one

        h1 <- h0 - matrix(delta, nrow = length(prob) - 1L, ncol = K - 1, 
                          byrow = TRUE)
        p1 <- rbind(.p(link, h1), 1)
        p <- cbind(p0, p1)
        ret <- vector(mode = "list", length = n)

        for (i in seq_len(n)) {
            tab <- sapply(seq_len(K), function(k)
                unclass(table(cut(runif(colsums[k]), breaks = c(-Inf, p[,k])))))
            ret[[i]] <- as.table(array(unlist(tab), dim = c(length(prob), K), 
                              dimnames = list(names(prob), 
                                              names(colsums))))
        }
        return(ret)
    }
    

    if (is.null(n)) 
        n <- ceiling(uniroot(function(n) {
                 # power call
                 
                 power.free1way.test(n = n, prob = prob, 
                                     alloc_ratio = alloc_ratio,  
                                     blocks = blocks,
                                     strata_ratio = strata_ratio, 
                                     delta = delta, mu = mu,
                                     sig.level = sig.level, link = link, 
                                     alternative = alternative, 
                                     nsim = nsim, seed = seed, 
                                     tol = tol)$power - power
                 
             }, interval = c(5, 1e+03), tol = tol, extendInt = "upX")$root)
    else if (is.null(delta)) {
        ### 2-sample only
        if (length(alloc_ratio) > 1L)
            stop(gettextf("Effect size can only computed for two sample problems in %s",
                          "power.free1way.test"),
                 domain = NA)   
        delta <- uniroot(function(delta) {
                 # power call
                 
                 power.free1way.test(n = n, prob = prob, 
                                     alloc_ratio = alloc_ratio,  
                                     blocks = blocks,
                                     strata_ratio = strata_ratio, 
                                     delta = delta, mu = mu,
                                     sig.level = sig.level, link = link, 
                                     alternative = alternative, 
                                     nsim = nsim, seed = seed, 
                                     tol = tol)$power - power
                 
    ### <TH> interval depending on alternative, symmetry? </TH>
            }, interval = c(0, 10), tol = tol, extendInt = "upX")$root
        }
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) {
                # power call
                
                power.free1way.test(n = n, prob = prob, 
                                    alloc_ratio = alloc_ratio,  
                                    blocks = blocks,
                                    strata_ratio = strata_ratio, 
                                    delta = delta, mu = mu,
                                    sig.level = sig.level, link = link, 
                                    alternative = alternative, 
                                    nsim = nsim, seed = seed, 
                                    tol = tol)$power - power
                
            }, interval = c(1e-10, 1 - 1e-10), tol = tol, extendInt = "yes")$root

    ### n is available now
    if (is.null(prob)) prob <- rep(1 / n, n)

    # power setup
    

    # link2fun

    if (!inherits(link, "linkfun")) {
        link <- match.arg(link)
        link <- do.call(link, list())
    }
    

    ### matrix means control distributions in different strata
    if (!is.matrix(prob))
        prob <- matrix(prob, nrow = NROW(prob), ncol = blocks)
    prob <- prop.table(prob, margin = 2L)
    C <- nrow(prob)
    B <- ncol(prob)
    if (is.null(colnames(prob))) 
        colnames(prob) <- paste0("stratum", seq_len(B))
    p0 <- apply(prob, 2, cumsum)
    h0 <- .q(link, p0[-nrow(p0),,drop = FALSE])

    # design args

    K <- length(delta) + 1L
    if (is.null(names(delta))) 
        names(delta) <- LETTERS[seq_len(K)[-1]]
    if (length(alloc_ratio) == 1L) 
        alloc_ratio <- rep_len(alloc_ratio, K - 1)
    if (length(alloc_ratio) != K - 1L)
        stop("Incorrect argument 'alloc_ratio'")
    if (length(strata_ratio) == 1L) 
        strata_ratio <- rep_len(strata_ratio, B - 1)
    if (length(strata_ratio) != B - 1L)
        stop("Incorrect argument 'strata_ratio'")
    ### sample size per group (columns) and stratum (rows)
    N <- n * matrix(c(1, alloc_ratio), nrow = B, ncol = K, byrow = TRUE) * 
             matrix(c(1, strata_ratio), nrow = B, ncol = K)
    

    rownames(N) <- colnames(prob)
    ctrl <- "Control"
    dn <- dimnames(prob)
    if (!is.null(names(dn)[1L]))
        ctrl <- names(dn)[1L]
    colnames(N) <- c(ctrl, names(delta))
    
    # estimate Fisher information
    
    he <- 0
    deltamu <- delta - mu
    Nboost <- ifelse(n < 100, ceiling(1000 / n), 1)
    for (i in seq_len(nsim)) {
        parm <- deltamu
        x <- as.table(array(0, dim = c(C, K, B)))
        for (b in seq_len(B)) {
            x[,,b] <- .r2dsim(1L, r = prob[, b], c = Nboost * N[b,], 
                              delta = delta, link = link)[[1L]]
            rs <- which(.rowSums(x[,,b], m = dim(x)[1L], n = dim(x)[2L]) > 0)
            theta <- h0[pmin(nrow(h0), rs), b]
            parm <- c(parm, theta[-length(theta)])
        }
        ### evaluate observed hessian for true parameters parm and x data
        he <- he + .free1wayML(x, link = link, mu = mu, start = parm, 
                               fix = seq_along(parm))$hessian / Nboost
    }
    ### estimate expected Fisher information
    he <- he / nsim
    

    alternative <- match.arg(alternative)
    if (K == 2L) {
        se <- 1 / sqrt(c(he))
        power  <- switch(alternative, 
            "two.sided" = pnorm(qnorm(sig.level / 2) + deltamu / se) + 
                          pnorm(qnorm(sig.level / 2) - deltamu / se),
            "less" = pnorm(qnorm(sig.level) - deltamu / se),
            "greater" = pnorm(qnorm(sig.level) + deltamu / se)
        )
    } else {
        if (alternative != "two.sided")
            stop(gettextf("%s only allows two-sided alternatives in the presence of more than two groups",
                          "power.free1way.test"),
                 domain = NA)
        ncp <- sum((chol(he) %*% deltamu)^2)
        qsig <- qchisq(sig.level, df = K - 1L, lower.tail = FALSE)
        power <- pchisq(qsig, df = K - 1L, ncp = ncp, lower.tail = FALSE)
    }

    # power htest output
    
    ss <- paste(colSums(N), paste0("(", colnames(N), ")"), collapse = " + ")
    ret <- list(n = n, 
                "Total sample size" = paste(ss, "=", sum(N)),
                power = power, 
                sig.level = sig.level)
    if (mu != 0) ret$mu <- mu
    if (K == 2L) ret[["Standard error"]] <- se
    ret[[link$parm]] <- delta
    ret$note <- "'n' is sample size in control group"
    if (B > 1) ret$note <- paste(ret$note, "of first stratum")
    alias <- link$alias
    if (length(link$alias) == 2L) alias <- alias[1L + (K > 2L)]
    ret$method <- paste(ifelse(B > 1L, "Stratified", ""), 
                        paste0(K, "-sample"), alias, 
                        "test against", link$model, "alternatives")
    class(ret) <- "power.htest"
    

    ret
}

