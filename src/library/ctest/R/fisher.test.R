fisher.test <- function(x, y = NULL, workspace = 200000, hybrid = FALSE,
                        alternative = "two.sided", conf.level = 0.95) {
    DNAME <- deparse(substitute(x))

    if(is.data.frame(x))
        x <- as.matrix(x)
    if(is.matrix(x)) {
        if(any(dim(x) < 2))
            stop("x must have at least 2 rows and columns")
        if(any(x < 0) || any(is.na(x))) 
            stop("all entries of x must be nonnegative and finite")
    }
    else {
        if(is.null(y)) 
            stop("if x is not a matrix, y must be given")
        if(length(x) != length(y)) 
            stop("x and y must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if((nlevels(x) < 2) || (nlevels(y) < 2)) 
            stop("x and y must have at least 2 levels")
        x <- table(x, y)
    }

    nr <- nrow(x)
    nc <- ncol(x)

    if((nr == 2) && (nc == 2)) {
        alternative <- char.expand(alternative,
                                   c("two.sided", "less", "greater"))
        if(length(alternative) > 1 || is.na(alternative))
            stop(paste("alternative must be \"two.sided\",",
                       "\"less\" or \"greater\""))
        if(!((length(conf.level) == 1) && is.finite(conf.level) &&
             (conf.level > 0) && (conf.level < 1)))
            stop("conf.level must be a single number between 0 and 1")
    }

    if((nr == 2) && (nc == 2) && (alternative != "two.sided")) {
        ## It is more efficient to compute p-values for the two-sided
        ## 2x2 case via C.  Alternatively, we could do
        ##   lo <- max(0, k - n)
        ##   hi <- min(k, m)
        ##   d <- dhyper(lo : hi, m, n, k)
        ##   PVAL <- sum(d[d <= dhyper(x, m, n, k)])
        m <- sum(x[, 1])
        n <- sum(x[, 2])
        k <- sum(x[1, ])
        x <- x[1, 1]
        PVAL <- switch(alternative,
                       less = phyper(x, m, n, k),
                       greater = 1 - phyper(x - 1, m, n, k))
    } else {
        if(hybrid) {
            warning("p-values may be incorrect")
            PVAL <- .C("fexact",
                       as.integer(nr),
                       as.integer(nc),
                       as.double(x),
                       as.integer(nr),
                       as.double(5),
                       as.double(80),
                       as.double(1),
                       as.double(0),
                       p = as.double(0),
                       as.integer(workspace),
                       PACKAGE = "ctest")$p
        } else
            PVAL <- .C("fexact",
                       as.integer(nr),
                       as.integer(nc),
                       as.double(x),
                       as.integer(nr),
                       as.double(-1),
                       as.double(100),
                       as.double(0),
                       as.double(0),
                       p = as.double(0),
                       as.integer(workspace),
                       PACKAGE = "ctest")$p
    }

    RVAL <- list(p.value = PVAL)

    if((nr == 2) && (nc == 2)) {
        NVAL <- 1
        names(NVAL) <- "odds ratio"
        if(alternative == "two.sided") {
            m <- sum(x[, 1])
            n <- sum(x[, 2])
            k <- sum(x[1, ])
            x <- x[1, 1]
        }
        ## Compute the MLE and confidence intervals for the odds ratio.
        ## Note that in general the conditional distribution of x given
        ## the marginals is a non-central hypergeometric distribution H
        ## with non-centrality parameter ncp, the odds ration.
        lo <- max(0, k - n)
        hi <- min(k, m)
        ## Determine the MLE for ncp by solving E(X) = x, where the
        ## expectation is with respect to H.
        mle <- function(x) {
            if(x == lo)
                return(0)
            if(x == hi)
                return(Inf)
            mnhyper <- function(ncp) {
                if(ncp == 0)
                    return(lo)
                if(ncp == Inf)
                    return(hi)
                q <- lo : hi
                d <- dhyper(q, m, n, k) * ncp ^ (0 : (hi - lo))
                d <- d / sum(d)
                sum(q * d)
            }
            mu <- mnhyper(1)
            if(mu > x)
                uniroot(function(t) mnhyper(t) - x, c(0, 1))$root
            else if(mu < x)
                1 / uniroot(function(t) mnhyper(1/t) - x, c(0, 1))$root
            else
                1
        }
        ESTIMATE <- mle(x)
        names(ESTIMATE) <- "odds ratio"
        ## Determine confidence intervals for the odds ratio.
        pnhyper <- function(q, ncp = 1, upper.tail = FALSE) {
            if(upper.tail) {
                if(ncp == 0)
                    return(as.integer(q <= lo))
                if(ncp == Inf)
                    return(as.integer(q <= hi))
            }
            else {
                if(ncp == 0)
                    return(as.integer(q >= lo))
                if(ncp == Inf)
                    return(as.integer(q >= hi))
            }
            u <- lo : hi
            d <- dhyper(u, m, n, k) * ncp ^ (0 : (hi - lo))
            d <- d / sum(d)
            if(upper.tail)
                sum(d[u >= q])
            else
                sum(d[u <= q])
        }
        ncp.U <- function(x, alpha) {
            if(x == hi)
                return(Inf)
            p <- pnhyper(x, 1)
            if(p < alpha)
                uniroot(function(t) pnhyper(x, t) - alpha, c(0,1))$root
            else if(p > alpha)
                1 / uniroot(function(t) pnhyper(x, 1/t) - alpha,
                            c(0,1))$root
            else
                1
        }
        ncp.L <- function(x, alpha) {
            if(x == lo)
                return(0)
            p <- pnhyper(x, 1, upper = TRUE)
            if(p > alpha)
                uniroot(function(t) pnhyper(x, t, upper = TRUE) - alpha,
                        c(0,1))$root 
            else if (p < alpha)
                1 / uniroot(function(t) pnhyper(x, 1/t, upper = TRUE) -
                            alpha, 
                            c(0,1))$root
            else
                1
        }
        CINT <- switch(alternative,
                       less = c(0, ncp.U(x, 1 - conf.level)),
                       greater = c(ncp.L(x, 1 - conf.level), Inf),
                       two.sided <- {
                           alpha <- (1 - conf.level) / 2
                           c(ncp.L(x, alpha), ncp.U(x, alpha))
                       })
        attr(CINT, "conf.level") <- conf.level
        RVAL <- c(RVAL,
                  list(conf.int = CINT,
                       estimate = ESTIMATE,
                       null.value = NVAL))
    }

    RVAL <- c(RVAL,
              alternative = alternative,
              method = "Fisher's Exact Test for Count Data",
              data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}
