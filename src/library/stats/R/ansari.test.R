ansari.test <- function(x, ...) UseMethod("ansari.test")

ansari.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         exact = NULL, conf.int = FALSE, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    if(conf.int) {
        if(!((length(conf.level) == 1)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
    }
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[complete.cases(x)]
    y <- y[complete.cases(y)]
    m <- length(x)
    if(m < 1)
        stop("not enough 'x' observations")
    n <- length(y)
    if(n < 1)
        stop("not enough 'y' observations")
    N <- m + n

    r <- rank(c(x, y))
    STATISTIC <- sum(pmin(r, N - r + 1)[seq(along = x)])
    TIES <- (length(r) != length(unique(r)))

    if(is.null(exact))
        exact <- ((m < 50) && (n < 50))

    if(exact && !TIES) {
        pansari <- function(q, m, n) {
            .C("pansari",
               as.integer(length(q)),
               p = as.double(q),
               as.integer(m),
               as.integer(n),
               PACKAGE = "stats")$p
        }
        PVAL <-
            switch(alternative,
                   two.sided = {
                       if (STATISTIC > ((m + 1)^2 %/% 4
                                        + ((m * n) %/% 2) / 2))
                           p <- 1 - pansari(STATISTIC - 1, m, n)
                       else
                           p <- pansari(STATISTIC, m, n)
                       min(2 * p, 1)
                   },
                   less = 1 - pansari(STATISTIC - 1, m, n),
                   greater = pansari(STATISTIC, m, n))
        if (conf.int) {
            qansari <- function(p, m, n) {
                .C("qansari",
                   as.integer(length(p)),
                   q = as.double(p),
                   as.integer(m),
                   as.integer(n),
                   PACKAGE = "stats")$q
            }
            alpha <- 1 - conf.level
            x <- sort(x)
            y <- sort(y)
            ab <- function(sig) {
                rab <- rank(c(x/sig, y))
                sum(pmin(rab, N - rab + 1)[seq(along = x)])
            }
            ratio <- outer(x,y,"/")
            aratio <- ratio[ratio >= 0]
            sigma <- sort(aratio)

            cci <- function(alpha) {
              u <- absigma - qansari(alpha/2,  m, n)
              l <- absigma - qansari(1 - alpha/2, m, n)
              ## Check if the statistic exceeds both quantiles first.
              uci <- NULL
              lci <- NULL
              if(length(u[u >= 0]) == 0 || length(l[l > 0]) == 0) {
                  warning("samples differ in location: cannot compute confidence set, returning NA")
                  return(c(NA, NA))
              }
              if (is.null(uci)) {
                  u[u < 0] <- NA
                  uci <- min(sigma[which(u == min(u, na.rm=TRUE))])
              }
              if (is.null(lci)) {
                  l[l <= 0] <- NA
                  lci <- max(sigma[which(l == min(l, na.rm=TRUE))])
              }
              ## The process of the statistics does not need to be
              ## monotone in sigma: check this and interchange quantiles.
              if (uci > lci) {
                  l <- absigma - qansari(alpha/2,  m, n)
                  u <- absigma - qansari(1 - alpha/2, m, n)
                  u[u < 0] <- NA
                  uci <- min(sigma[which(u == min(u, na.rm=TRUE))])
                  l[l <= 0] <- NA
                  lci <- max(sigma[which(l == min(l, na.rm=TRUE))])
               }
               c(uci, lci)
            }

            cint <- if(length(sigma) < 1) {
                warning("cannot compute confidence set, returning NA")
                c(NA, NA)
            }
            else {
                ## Compute statistics directly: computing the steps is
                ## not faster.
                absigma <-
                    sapply(sigma + c(diff(sigma)/2,
                                     sigma[length(sigma)]*1.01), ab)
                switch(alternative, two.sided = {
                    cci(alpha)
                }, greater= {
                    c(cci(alpha*2)[1], Inf)
                }, less= {
                    c(0, cci(alpha*2)[2])
                })
            }
            attr(cint, "conf.level") <- conf.level
            u <- absigma - qansari(0.5, m, n)
            sgr <- sigma[u <= 0]
            if (length(sgr) == 0) sgr <- NA
            else sgr <- max(sgr)
            sle <- sigma[u > 0]
            if (length(sle) == 0) sle <- NA
            else sle <- min(sle)
            ESTIMATE <- mean(c(sle, sgr))
        }
    }
    else {
        EVEN <- ((N %% 2) == 0)
        normalize <- function(s, r, TIES, m=length(x), n=length(y)) {
            z <- if(EVEN)
                s - m * (N + 2)/4
            else
                s - m * (N + 1)^2/(4 * N)
            if (!TIES) {
                SIGMA <- if(EVEN)
                    sqrt((m * n * (N + 2) * (N - 2))/(48 * (N - 1)))
                else
                    sqrt((m * n * (N + 1) * (3 + N^2))/(48 * N^2))
            }
            else {
                r <- rle(sort(pmin(r, N - r + 1)))
                SIGMA <- if(EVEN)
                    sqrt(m * n
                         * (16 * sum(r$l * r$v^2) - N * (N + 2)^2)
                         / (16 * N * (N - 1)))
                else
                    sqrt(m * n
                         * (16 * N * sum(r$l * r$v^2) - (N + 1)^4)
                         / (16 * N^2 * (N - 1)))
            }
            z / SIGMA
        }
        p <- pnorm(normalize(STATISTIC, r, TIES))
        PVAL <- switch(alternative,
                       two.sided = 2 * min(p, 1 - p),
                       less = 1 - p,
                       greater = p)

        if(conf.int && !exact) {
            alpha <- 1 - conf.level
            ab <- function(sig, zq) {
                r <- rank(c(x / sig, y))
                s <- sum(pmin(r, N -r + 1)[seq(along = x)])
                TIES <- (length(r) != length(unique(r)))
                normalize(s, r, TIES, length(x), length(y)) - zq
            }
            ## Use uniroot here.
            ## Compute the range of sigma first.
            srangepos <- NULL
            srangeneg <- NULL
            if (any(x[x > 0]) && any(y[y > 0]))
                srangepos <-
                    c(min(x[x>0], na.rm=TRUE)/max(y[y>0], na.rm=TRUE),
                      max(x[x>0], na.rm=TRUE)/min(y[y>0], na.rm=TRUE))
            if (any(x[x <= 0]) && any(y[y < 0]))
                srangeneg <-
                    c(min(x[x<=0], na.rm=TRUE)/max(y[y<0], na.rm=TRUE),
                      max(x[x<=0], na.rm=TRUE)/min(y[y<0], na.rm=TRUE))
            if (any(is.infinite(c(srangepos, srangeneg)))) {
                warning("cannot compute asymptotic confidence set or estimator")
                conf.int <- FALSE
            } else {
                ccia <- function(alpha) {
                    ## Check if the statistic exceeds both quantiles
                    ## first.
                    statu <- ab(srange[1], zq=qnorm(alpha/2))
                    statl <- ab(srange[2], zq=qnorm(alpha/2, lower=FALSE))
                    if (statu > 0 || statl < 0) {
                        warning("samples differ in location: cannot compute confidence set, returning NA")
                        return(c(NA, NA))
                    }
                    u <- uniroot(ab, srange, tol=1e-4,
                                 zq=qnorm(alpha/2))$root
                    l <- uniroot(ab, srange, tol=1e-4,
                                 zq=qnorm(alpha/2, lower=FALSE))$root
                    ## The process of the statistics does not need to be
                    ## monotone: sort is ok here.
                    sort(c(u, l))
                }
                srange <- range(c(srangepos, srangeneg), na.rm=FALSE)
                cint <- switch(alternative, two.sided = {
                    ccia(alpha)
                }, greater= {
                    c(ccia(alpha*2)[1], Inf)
                }, less= {
                    c(0, ccia(alpha*2)[2])
                })
                attr(cint, "conf.level") <- conf.level
                ## Check if the statistic exceeds both quantiles first.
                statu <- ab(srange[1], zq=0)
                statl <- ab(srange[2], zq=0)
                if (statu > 0 || statl < 0) {
                    ESTIMATE <- NA
                    warning("cannot compute estimate, returning NA")
                } else
                    ESTIMATE <- uniroot(ab, srange, tol=1e-4, zq=0)$root
            }
        }
        if(exact && TIES) {
            warning("cannot compute exact p-value with ties")
            if(conf.int)
                warning("cannot compute exact confidence intervals with ties")
        }
    }

    names(STATISTIC) <- "AB"
    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 null.value = c("ratio of scales" = 1),
                 alternative = alternative,
                 method = "Ansari-Bradley test",
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL,
                  list(conf.int = cint,
                       estimate = c("ratio of scales" = ESTIMATE)))
    class(RVAL) <- "htest"
    return(RVAL)
}

ansari.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1)
       || (length(attr(terms(formula[-3]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("ansari.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
