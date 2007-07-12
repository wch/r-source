## mlm := multivariate lm()
summary.mlm <- function(object, ...)
{
    coef <- coef(object)
    ny <- ncol(coef)
    effects <- object$effects
    resid <- object$residuals
    fitted <- object$fitted.values
    ynames <- colnames(coef)
    if(is.null(ynames)) {
	lhs <- object$terms[[2]]
	if(mode(lhs) == "call" && lhs[[1]] == "cbind")
	    ynames <- as.character(lhs)[-1]
	else ynames <- paste("Y", seq_len(ny), sep = "")
    }
    ## we need to ensure that _all_ responses are named
    ind <- ynames == ""
    if(any(ind)) ynames[ind] <-  paste("Y", seq_len(ny), sep = "")[ind]

    value <- vector("list", ny)
    names(value) <- paste("Response", ynames)
    cl <- oldClass(object)
    class(object) <- cl[match("mlm", cl):length(cl)][-1]
    for(i in seq(ny)) {
	object$coefficients <- coef[, i]
	object$residuals <- resid[, i]
	object$fitted.values <- fitted[, i]
	object$effects <- effects[, i]
	object$call$formula[[2]] <- object$terms[[2]] <- as.name(ynames[i])
	value[[i]] <- summary(object, ...)
    }
    class(value) <- "listof"
    value
}


### SSD(object) returns object of class "SSD":
###           $SSD  matrix of sums of squares  & products
###           $df   degrees of freedom.
### estVar(object)returns the estimated covariance matrix
SSD <- function(object, ...) UseMethod("SSD")
estVar <- function(object, ...) UseMethod("estVar")

SSD.mlm <- function(object, ...){
    ## It's not all that hard to incorporate weights, but will
    ## anyone use them?
    if (!is.null(object$weights))
        stop("'mlm' objects with weights are not supported")
    structure(list(SSD=crossprod(residuals(object)),
                   call=object$call,
                   df=object$df.residual), class="SSD")
}
estVar.SSD <- function(object, ...)
    object$SSD/object$df

estVar.mlm <- function(object, ...)
    estVar(SSD(object))


### Convenience functions:
###  Tr: is the trace operator
###  proj: the projection operator possibly generalized to matrices.
###  Rg: matrix rank
###  Thin.row, Thin.col: thin matrix to full (row/column) rank

Tr <- function(matrix) sum(diag(matrix))
proj.matrix <- function(X, orth=FALSE){
    X <- Thin.col(X)
    P <- if (ncol(X) == 0)
        matrix(0,nrow(X),nrow(X))
    else
        ## Brute force. There must be a better way...
        X %*% solve(crossprod(X),t(X))
    if (orth) diag(nrow=nrow(X)) - P else P
}

Rank  <- function(X, tol=1e-7) qr(X, tol=tol, LAPACK=FALSE)$rank

Thin.row <- function(X, tol=1e-7) {
    QR <- qr(t(X), tol=tol, LAPACK=FALSE)
    X[QR$pivot[seq_len(QR$rank)],,drop=FALSE]
}

Thin.col <- function(X, tol=1e-7) {
    QR <- qr(X, tol=tol, LAPACK=FALSE)
    X[,QR$pivot[seq_len(QR$rank)],drop=FALSE]
}


mauchly.test <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq_len(p)),...)
	 UseMethod("mauchly.test")

mauchly.test.mlm <- function(object, ...)
 	mauchly.test(SSD(object), ...)


mauchly.test.SSD <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq_len(p)),...)
{
    p <- ncol(object$SSD)

    Xmis <- missing(X)
    Mmis <- missing(M)
    if (missing(T)){
        orig.X <- X
        orig.M <- M
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    Psi <- T %*% Sigma %*% t(T)
    B <- T %*% object$SSD %*% t(T)
    pp <- nrow(T)
    U <- solve(Psi,B)
    n <- object$df
    logW <- log(det(U)) - pp * log(Tr(U/pp))
    ## Asymptotic mumbojumbo (from TWA)....
    rho <- 1 - (2*pp^2 + pp + 2)/(6*pp*n)
    w2 <- (pp+2)*(pp-1)*(pp-2)*(2*pp^3+6*pp^2+3*p +
                                2)/(288*(n*pp*rho)^2)

    z <- -n * rho * logW
    f <- pp * (pp + 1)/2 - 1

    Pr1 <- pchisq(z, f, lower.tail=FALSE)
    Pr2 <- pchisq(z, f+4, lower.tail=FALSE)
    pval <- Pr1 + w2 * (Pr2 - Pr1)
    transformnote <- if (!missing(T))
        c("\nContrast matrix", apply(format(T),1,paste, collapse=" "))
    else
        c(
          if (!Xmis)
          c("\nContrasts orthogonal to",
            if (is.matrix(orig.X))  apply(format(X), 2,paste, collapse=" ")
            else deparse(formula(orig.X)),"",
            if (!Mmis)
            c("\nContrasts spanned by",
              if (is.matrix(orig.M))  apply(format(M), 2,paste, collapse=" ")
              else deparse(formula(orig.M)),""
              )
            )
          )

    retval <- list(statistic=c(W=exp(logW)),p.value=pval,
                   method=c("Mauchly's test of sphericity", transformnote),
                   data.name=paste("SSD matrix from",
                   deparse(object$call), collapse=" "))
    class(retval) <- "htest"
    retval
}

sphericity <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq_len(p)))
{
    p <- ncol(object$SSD)

    if (missing(T)){
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    Psi <- T %*% Sigma %*% t(T)
    B <- T %*% object$SSD %*% t(T)
    pp <- nrow(T)
    U <- solve(Psi,B)
    sigma <- Tr(U)/pp/object$df
    lambda <- Re(eigen(U)$values)
    GG.eps <- sum(lambda)^2/sum(lambda^2)/pp
    n <- object$df
    HF.eps <- ((n + 1) * pp * GG.eps - 2) / (pp * (n - pp * GG.eps))
    return(list(GG.eps=GG.eps,HF.eps=HF.eps,sigma=sigma))
}

anova.mlm <-
    function(object, ...,
             test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy", "Spherical"),
             Sigma = diag(nrow = p),
             T = Thin.row(proj(M) - proj(X)),
             M = diag(nrow = p),
             X = ~0,
             idata = data.frame(index = seq_len(p)))
{
    if(length(list(object, ...)) > 1){
        cl <- match.call()
        cl[[1]] <- anova.mlmlist
        return(eval.parent(cl))
    } else {
        p <- ncol(SSD(object)$SSD)
        Xmis <- missing(X)
        Mmis <- missing(M)
        if (missing(T)){
            orig.M <- M # keep for printing
            orig.X <- X
            if (class(M) == "formula") M <- model.matrix(M, idata)
            if (class(X) == "formula") X <- model.matrix(X, idata)
            if (Rank(cbind(M,X)) != Rank(M))
                stop("X does not define a subspace of M")
        }
        title <- "Analysis of Variance Table\n"
        transformnote <- if (!missing(T))
            c("\nContrast matrix", apply(format(T),1,paste, collapse=" "))
        else
            c(
              if (!Xmis)
              c("\nContrasts orthogonal to",
                if (is.matrix(orig.X))
                apply(format(X), 2,paste, collapse=" ")
                else deparse(formula(orig.X)),"",
                if (!Mmis)
                c("\nContrasts spanned by",
                  if (is.matrix(orig.M))
                  apply(format(M), 2,paste, collapse=" ")
                  else deparse(formula(orig.M)),""
                  )
                )
              )
        epsnote <- NULL

        ssd <- SSD(object)
        rk <- object$rank
        pp <- nrow(T)
        if(rk > 0) {
            p1 <- 1:rk
            comp <- object$effects[p1, , drop=FALSE]
            asgn <- object$assign[object$qr$pivot][p1]
            nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
            tlabels <- nmeffects[1 + unique(asgn)]
	    ix <- split(seq_len(nrow(comp)), asgn)
            ss <- lapply(ix, function(i) crossprod(comp[i,,drop=FALSE]))
# This was broken. Something similar might work if we implement
#  split.matrix a la split.data.frame
#            ss <- lapply(split(comp,asgn), function(x) crossprod(t(x)))
            df <- sapply(split(asgn,  asgn), length)
        } else {
#            ss <- ssr
#            df <- dfr
#            tlabels <- character(0)
        }
        test <- match.arg(test)
        nmodels <- length(ss)
        if(test == "Spherical"){
            df.res <- ssd$df
            sph <- sphericity(ssd, T=T, Sigma=Sigma)
            epsnote <- c(paste(format(c("Greenhouse-Geisser epsilon:",
                                        "Huynh-Feldt epsilon:")),
                               format(c(sph$GG.eps, sph$HF.eps),digits=4)),
                         "")

            Psi <- T %*% Sigma %*% t(T)
            stats <- matrix(NA, nmodels+1, 6)
            colnames(stats) <- c("F", "num Df", "den Df",
                                 "Pr(>F)", "G-G Pr", "H-F Pr")
            for(i in 1:nmodels) {
                s2 <- Tr(solve(Psi,T %*% ss[[i]] %*% t(T)))/pp/df[i]
                Fval <- s2/sph$sigma
                stats[i,1:3] <- abs(c(Fval, df[i]*pp, df.res*pp))
            }
            stats[,4] <- pf(stats[,1], stats[,2], stats[,3], lower.tail=FALSE)
            stats[,5] <- pf(stats[,1],
                            stats[,2]*sph$GG.eps, stats[,3]*sph$GG.eps,
                            lower.tail=FALSE)
            stats[,6] <- pf(stats[,1],
                            stats[,2]*min(1,sph$HF.eps),
                            stats[,3]*min(1,sph$HF.eps),
                            lower.tail=FALSE)
        } else {
            df.res <- ssd$df
            rss.qr <- qr(T %*% ssd$SSD %*% t(T))
            if(rss.qr$rank < pp)
                stop("residuals have rank ", rss.qr$rank," < ", pp)
            eigs <- array(NA, c(nmodels, pp))
            stats <- matrix(NA, nmodels+1, 5)
            colnames(stats) <- c(test, "approx F", "num Df", "den Df",
                                       "Pr(>F)")

            for(i in 1:nmodels) {
                eigs[i, ] <- Re(eigen(qr.coef(rss.qr,
                                              T %*% ss[[i]] %*% t(T)),
                                      symmetric = FALSE)$values)
                stats[i, 1:4] <-
                    switch(test,
                           "Pillai" = Pillai(eigs[i,  ],
                           df[i], df.res),
                           "Wilks" = Wilks(eigs[i,  ],
                           df[i], df.res),
                           "Hotelling-Lawley" = HL(eigs[i,  ],
                           df[i], df.res),
                           "Roy" = Roy(eigs[i,  ],
                           df[i], df.res))
                ok <- stats[, 2] >= 0 & stats[, 3] > 0 & stats[, 4] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5] <- pf(stats[ok, 2], stats[ok, 3], stats[ok, 4],
                                   lower.tail = FALSE)

            }

        }
        table <- data.frame(Df=c(df,ssd$df), stats, check.names=FALSE)
        row.names(table) <- c(tlabels, "Residuals")
#        if(attr(object$terms,"intercept")) table <- table[-1, ]
        structure(table, heading = c(title, transformnote, epsnote),
                  class = c("anova", "data.frame"))

#        f <- ms/(ssr/dfr)
#        P <- pf(f, df, dfr, lower.tail = FALSE)
#        table <- data.frame(df, ss, ms, f, P)
#        table[length(P), 4:5] <- NA
#        dimnames(table) <- list(c(tlabels, "Residuals"),
#                                c("Df","Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
#        if(attr(object$terms,"intercept")) table <- table[-1, ]
#        structure(table, heading = c("Analysis of Variance Table\n",
#                         paste("Response:", deparse(formula(object)[[2]]))),
#                  class= c("anova", "data.frame"))# was "tabular"
    }
}

Pillai <- function(eig, q, df.res)
{
    test <- sum(eig/(1 + eig))
    p <- length(eig)
    s <- min(p, q)
    n <- 0.5 * (df.res - p - 1)
    m <- 0.5 * (abs(p - q) - 1)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * n + s + 1
    c(test, (tmp2/tmp1 * test)/(s - test), s*tmp1, s*tmp2)
}

Wilks <- function(eig, q, df.res)
{
    test <- prod(1/(1 + eig))
    p <- length(eig)
    tmp1 <- df.res - 0.5 * (p - q + 1)
    tmp2 <- (p * q - 2)/4
    tmp3 <- p^2 + q^2 - 5
    tmp3 <-  if(tmp3 > 0) sqrt(((p*q)^2 - 4)/tmp3) else 1
    c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q,
      p * q, tmp1 * tmp3 - 2 * tmp2)
}

HL <- function(eig, q, df.res)
{
    test <- sum(eig)
    p <- length(eig)
    m <- 0.5 * (abs(p - q) - 1)
    n <- 0.5 * (df.res - p - 1)
    s <- min(p, q)
    tmp1 <- 2 * m + s + 1
    tmp2 <- 2 * (s * n + 1)
    c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
}

Roy <- function(eig, q, df.res)
{
    p <- length(eig)
    test <- max(eig)
    tmp1 <- max(p, q)
    tmp2 <- df.res - tmp1 + q
    c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
}

anova.mlmlist <- function (object, ...,
                           test=c("Pillai", "Wilks",
                           "Hotelling-Lawley", "Roy","Spherical"),
                           Sigma=diag(nrow=p),
                           T = Thin.row(proj(M)-proj(X)),
                           M = diag(nrow=p),
                           X = ~0,
                           idata=data.frame(index=seq_len(p)))
{
    objects <- list(object, ...)
    p <- ncol(SSD(object)$SSD)
    Xmis <- missing(X)
    Mmis <- missing(M)
    if (missing(T)){
        orig.M <- M # keep for printing
        orig.X <- X
        if (class(M) == "formula") M <- model.matrix(M, idata)
        if (class(X) == "formula") X <- model.matrix(X, idata)
        if (Rank(cbind(M,X)) != Rank(M))
            stop("X does not define a subspace of M")
    }
    pp <- nrow(T)
    responses <- as.character(lapply(objects,
				     function(x) deparse(x$terms[[2]])))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("models with response ",
                deparse(responses[!sameresp]),
                " removed because response differs from ", "model 1")
    }

    ns <- sapply(objects, function(x) length(x$residuals))
    if(any(ns != ns[1]))
        stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.mlm(object))

    ## extract statistics

    resdf  <- as.numeric(lapply(objects, df.residual))
    df <- c(NA,diff(resdf))
    resssd <- lapply(objects, SSD)
    deltassd <- mapply(function(x,y) y$SSD - x$SSD,
                       resssd[-nmodels], resssd[-1], SIMPLIFY=FALSE)
    resdet <- sapply(resssd,
                     function(x) det(T %*% (x$SSD/x$df) %*% t(T))^(1/pp))


    ## construct table and title

    table <- data.frame(resdf, df, resdet)
    variables <- lapply(objects, function(x)
                        paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1:nmodels,
                            c("Res.Df", "Df", "Gen.var."))

    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     variables, sep="", collapse="\n")
    transformnote <- if (!missing(T))
        c("\nContrast matrix", apply(format(T),1,paste, collapse=" "))
    else
        c(
          if (!Xmis)
          c("\nContrasts orthogonal to",
            if (is.matrix(orig.X))  apply(format(X), 2,paste, collapse=" ")
            else deparse(formula(orig.X)),"",
            if (!Mmis)
            c("\nContrasts spanned by",
              if (is.matrix(orig.M))  apply(format(M), 2,paste, collapse=" ")
              else deparse(formula(orig.M)),""
              )
            )
          )
    epsnote <- NULL

    ## calculate test statistic

    test <- match.arg(test)
    if(test == "Spherical"){
	bigmodel <- order(resdf)[1]
        df.res <- resdf[bigmodel]
        sph <- sphericity(resssd[[bigmodel]],T=T,Sigma=Sigma)
        epsnote <- c(paste(format(c("Greenhouse-Geisser epsilon:",
                           "Huynh-Feldt epsilon:")),
                         format(c(sph$GG.eps, sph$HF.eps),digits=4)),
                     "")

        Psi <- T %*% Sigma %*% t(T)
        stats <- matrix(NA, nmodels, 6)
        dimnames(stats) <-  list(1:nmodels,
                                 c("F", "num Df", "den Df",
                                   "Pr(>F)", "G-G Pr", "H-F Pr"))
        for(i in 2:nmodels) {
            s2 <- Tr(solve(Psi,T %*% deltassd[[i-1]] %*% t(T)))/pp/df[i]
            Fval <- s2/sph$sigma
            stats[i,1:3] <- abs(c(Fval, df[i]*pp, df.res*pp))
        }
        stats[,4] <- pf(stats[,1], stats[,2], stats[,3], lower.tail=FALSE)
        stats[,5] <- pf(stats[,1],
                        stats[,2]*sph$GG.eps, stats[,3]*sph$GG.eps,
                        lower.tail=FALSE)
        stats[,6] <- pf(stats[,1],
                        stats[,2]*min(1,sph$HF.eps),
                        stats[,3]*min(1,sph$HF.eps),
                        lower.tail=FALSE)
        table <- cbind(table, stats)
    }
    else if(!is.null(test)) {
	bigmodel <- order(resdf)[1]
        df.res <- resdf[bigmodel]
        rss.qr <- qr(T %*% resssd[[bigmodel]]$SSD %*% t(T))
        if(rss.qr$rank < pp)
            stop("residuals have rank ", rss.qr$rank," < ", pp)
        eigs <- array(NA, c(nmodels, pp))
        stats <- matrix(NA, nmodels, 5)
        dimnames(stats) <-  list(1:nmodels,
                                 c(test, "approx F", "num Df", "den Df",
                                   "Pr(>F)"))

        for(i in 2:nmodels) {
            sg <- (df[i] > 0) -  (df[i] < 0)
            eigs[i, ] <- Re(eigen(qr.coef(rss.qr,
                                          sg * T %*% deltassd[[i-1]] %*% t(T)),
                                  symmetric = FALSE)$values)
            stats[i, 1:4] <-
                switch(test,
                       "Pillai" = Pillai(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Wilks" = Wilks(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Hotelling-Lawley" = HL(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]),
                       "Roy" = Roy(eigs[i,  ],
                       sg * df[i], resdf[bigmodel]))
            ok <- stats[, 2] >= 0 & stats[, 3] > 0 & stats[, 4] > 0
            ok <- !is.na(ok) & ok
            stats[ok, 5] <- pf(stats[ok, 2], stats[ok, 3], stats[ok, 4],
                               lower.tail = FALSE)

        }
        table <- cbind(table,stats)

    }
    structure(table, heading = c(title, topnote, transformnote, epsnote),
              class = c("anova", "data.frame"))
}


deviance.mlm <- function(object, ...)
{
    res <-
	if(is.null(w <- object$weights)) object$residuals^2
	else w * object$residuals^2
    drop(rep.int(1, nrow(res)) %*% res)
}

plot.mlm <- function (x, ...) .NotYetImplemented()
