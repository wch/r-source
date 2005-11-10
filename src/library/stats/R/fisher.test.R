fisher.test <-
function(x, y = NULL, workspace = 200000, hybrid = FALSE,
         control = list(), or = 1, alternative = "two.sided",
         conf.int = TRUE, conf.level = 0.95)
{
    DNAME <- deparse(substitute(x))

    if(is.data.frame(x))
        x <- as.matrix(x)
    if(is.matrix(x)) {
        if(any(dim(x) < 2))
            stop("'x' must have at least 2 rows and columns")
        if(!is.numeric(x) || any(x < 0) || any(is.na(x)))
            stop("all entries of 'x' must be nonnegative and finite")
        if(!is.integer(x)) {
            xo <- x
            x <- round(x)
            if(any(x > .Machine$integer.max))
                stop("'x' has entries too large to be integer")
            if(!identical(TRUE, (ax <- all.equal(xo, x))))
                warning("'x' has been rounded to integer: ", ax)
            storage.mode(x) <- "integer"
        }
    }
    else {
        if(is.null(y))
            stop("if 'x' is not a matrix, 'y' must be given")
        if(length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if((nlevels(x) < 2) || (nlevels(y) < 2))
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
    }
    ## x is integer
    con <- list(mult = 30)
    con[names(control)] <- control
    if((mult <- as.integer(con$mult)) < 2)
        stop("'mult' must be integer >= 2, typically = 30")

    nr <- nrow(x)
    nc <- ncol(x)

    if((nr == 2) && (nc == 2)) {
        alternative <- char.expand(alternative,
                                   c("two.sided", "less", "greater"))
        if(length(alternative) > 1 || is.na(alternative))
            stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
        if(!((length(conf.level) == 1) && is.finite(conf.level) &&
             (conf.level > 0) && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
        if(!missing(or) && (length(or) > 1 || is.na(or) || or < 0))
            stop("'or' must be a single number between 0 and Inf")
    }

    PVAL <- NULL
    if(nr != 2  ||  nc != 2
       || (alternative == "two.sided") && (or == 1)) {
        ## Note that it is more efficient to compute p-values in C for
        ## the two-sided 2-by-2 case with odds ratio 1
        if(hybrid) {
            if(nr != 2 || nc != 2)
                warning("p-values may be incorrect")
            else
                warning("hybrid is ignored for a 2 x 2 table")
            PVAL <- .C("fexact",
                       nr,
                       nc,
                       x,
                       nr,
                       ## Cochran condition for asym.chisq. decision:
                       as.double(5),#  expect
                       as.double(80),# percnt
                       as.double(1),#  emin
                       double(1),#  prt
                       p = double(1),
                       as.integer(workspace),
                       mult = as.integer(mult),
                       PACKAGE = "stats")$p
        } else
            PVAL <- .C("fexact",
                       nr,
                       nc,
                       x,
                       nr,
                       as.double(-1),#  expect < 0 : exact
                       as.double(100),
                       as.double(0),
                       double(1),#   prt
                       p = double(1),
                       as.integer(workspace),
                       mult = as.integer(mult),
                       PACKAGE = "stats")$p
        RVAL <- list(p.value = PVAL)
    }

    if((nr == 2) && (nc == 2)) {## conf.int and more only in  2 x 2 case
        m <- sum(x[, 1])
        n <- sum(x[, 2])
        k <- sum(x[1, ])
        x <- x[1, 1]
        lo <- max(0, k - n)
        hi <- min(k, m)
        NVAL <- or
        names(NVAL) <- "odds ratio"

        ## Note that in general the conditional distribution of x given
        ## the marginals is a non-central hypergeometric distribution H
        ## with non-centrality parameter ncp, the odds ratio.
        support <- lo : hi
        ## Density of the *central* hypergeometric distribution on its
        ## support: store for once as this is needed quite a bit.
        logdc <- dhyper(support, m, n, k, log = TRUE)
        dnhyper <- function(ncp) {
            ## Does not work for boundary values for ncp (0, Inf) but it
            ## does not need to.
            d <- logdc + log(ncp) * support
            d <- exp(d - max(d))        # beware of overflow
            d / sum(d)
        }
        mnhyper <- function(ncp) {
            if(ncp == 0)
                return(lo)
            if(ncp == Inf)
                return(hi)
            sum(support * dnhyper(ncp))
        }
        pnhyper <- function(q, ncp = 1, upper.tail = FALSE) {
            if(ncp == 1) {
                if(upper.tail)
                    return(phyper(x - 1, m, n, k, lower.tail = FALSE))
                else
                    return(phyper(x, m, n, k))
            }
            if(ncp == 0) {
                if(upper.tail)
                    return(as.numeric(q <= lo))
                else
                    return(as.numeric(q >= lo))
            }
            if(ncp == Inf) {
                if(upper.tail)
                    return(as.numeric(q <= hi))
                else
                    return(as.numeric(q >= hi))
            }
            d <- dnhyper(ncp)
            if(upper.tail)
                sum(d[support >= q])
            else
                sum(d[support <= q])
        }

        ## Determine the p-value (if still necessary).
        if(is.null(PVAL)) {
            PVAL <-
                switch(alternative,
                       less = pnhyper(x, or),
                       greater = pnhyper(x, or, upper = TRUE),
                       two.sided = {
                           if(or == 0)
                               as.numeric(x == lo)
                           else if(or == Inf)
                               as.numeric(x == hi)
                           else {
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 10 ^ (-7)
                               d <- dnhyper(or)
                               sum(d[d <= d[x - lo + 1] * relErr])
                           }
                       })
            RVAL <- list(p.value = PVAL)
        }

        ## Determine the MLE for ncp by solving E(X) = x, where the
        ## expectation is with respect to H.
        mle <- function(x) {
            if(x == lo)
                return(0)
            if(x == hi)
                return(Inf)
            mu <- mnhyper(1)
            if(mu > x)
                uniroot(function(t) mnhyper(t) - x, c(0, 1))$root
            else if(mu < x)
                1 / uniroot(function(t) mnhyper(1/t) - x,
                            c(.Machine$double.eps, 1))$root
            else
                1
        }
        ESTIMATE <- mle(x)
        names(ESTIMATE) <- "odds ratio"

        if(conf.int) {
            ## Determine confidence intervals for the odds ratio.
            ncp.U <- function(x, alpha) {
                if(x == hi)
                    return(Inf)
                p <- pnhyper(x, 1)
                if(p < alpha)
                    uniroot(function(t) pnhyper(x, t) - alpha,
                            c(0, 1))$root
                else if(p > alpha)
                    1 / uniroot(function(t) pnhyper(x, 1/t) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            ncp.L <- function(x, alpha) {
                if(x == lo)
                    return(0)

                p <- pnhyper(x, 1, upper = TRUE)
                if(p > alpha)
                    uniroot(function(t)
                            pnhyper(x, t, upper = TRUE) - alpha,
                            c(0, 1))$root
                else if(p < alpha)
                    1 / uniroot(function(t)
                                pnhyper(x, 1/t, upper = TRUE) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            CINT <- switch(alternative,
                           less = c(0, ncp.U(x, 1 - conf.level)),
                           greater = c(ncp.L(x, 1 - conf.level), Inf),
                           two.sided = {
                               alpha <- (1 - conf.level) / 2
                               c(ncp.L(x, alpha), ncp.U(x, alpha))
                           })
            attr(CINT, "conf.level") <- conf.level
        }
        RVAL <- c(RVAL,
                  list(conf.int = if(conf.int) CINT,
                       estimate = ESTIMATE,
                       null.value = NVAL))
    } ## end (2 x 2)

    RVAL <- c(RVAL,
              alternative = alternative,
              method = "Fisher's Exact Test for Count Data",
              data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}
