ansari.test <- function(x, y,
                        alternative = c("two.sided", "less", "greater"),
                        exact = NULL,
                        conf.int = FALSE, conf.level = 0.95) 
{
    alternative <- match.arg(alternative)
    if(conf.int) {
        if(!((length(conf.level) == 1)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("conf.level must be a single number between 0 and 1")
    }
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[complete.cases(x)]
    y <- y[complete.cases(y)]
    m <- length(x)
    if(m < 1)
        stop("not enough x observations")
    n <- length(y)
    if(n < 1)
        stop("not enough y observations")
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
               PACKAGE = "ctest")$p
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
                   PACKAGE = "ctest")$q
            }
            # Bauer defines the CI for y/x, therefore interchange.
            help <- x
            x <- y
            y <- help
            m <- length(x)
            n <- length(y)
            alpha <- 1 - conf.level
            x <- sort(x)
            y <- sort(y)
            ab <- function(sig) {
                rab <- rank(c(y/sig, x))
                ## here follow Bauer directly
                sum(pmin(rab, N - rab + 1)[seq(along = y)])
            }
            xpos <- x[x > 0]
            ypos <- y[y > 0]
            xneg <- x[x <= 0]
            yneg <- y[y <= 0]
            signeg <- NULL
            sigpos <- NULL
            ## compute all stuff for the negative/positive values
            ## separately 
            if(length(xneg) > 0 && length(yneg) > 0) {
                coefn <- function(j, i)
                    - abs(j+i-(N+1)/2) + abs(i+j-1-(N+1)/2)
                signeg <- outer(yneg, xneg, "/")
                coefneg <- outer(1:length(yneg), 1:length(xneg), "coefn")
                coefneg <- coefneg[order(signeg)]
                signeg <- sort(signeg)
            }
            if(length(xpos) > 0 && length(ypos) > 0) {
                coefp <- function(j,i)
                    - abs(j+i-1-(N+1)/2) + abs(i+j-(N+1)/2)
                sigpos <- outer(ypos, xpos, "/")
                mpos <- min(which(x > 0))
                npos <- min(which(y > 0))
                coefpos <- outer(npos:n, mpos:m, "coefp")
                coefpos <- coefpos[order(sigpos)]
                sigpos <- sort(sigpos)
                if(!is.null(signeg)) {
                    sigma <- c(signeg, sigpos)
                    coefs <- c(coefneg, coefpos)
                    coefs <- coefs[order(sigma)]
                    sigma <- sort(sigma)
                } else {
                    sigma <- sigpos
                    coefs <- coefpos[order(sigpos)]
                    sigma <- sort(sigma)
                }
            }
            if(is.null(sigpos) && !is.null(signeg)) {
                sigma <- signeg
                coefs <- coefneg[order(signeg)]
                sigma <- sort(sigma)
            } 
            ## compute step function
            cint <- if(length(sigma) < 1) {
                warning("Cannot compute confidence interval")
                c(0, 0)
            }
            else {
                absigma <- cumsum(c(ab(sigma[1]),
                                    coefs[2:length(coefs)]))
                switch(alternative, two.sided = {
                    u <- absigma - qansari(alpha/2, n, m) 
                    l <- absigma - qansari(1 - alpha/2, n, m) 
                    if(length(u[u >= 0]) == 0)
                        uci <- sigma[1]
                    else {
                        u[u < 0] <- NA
                        uci <- unique(sigma[which(u == min(u, na.rm=TRUE))])
                        if (length(uci) != 1)
                            uci <- uci[1]
                    }
                    if (length(l[l > 0]) == 0)
                        lci <- sigma[length(sigma)]
                    else {                
                        l[l <= 0] <- NA
                        lci <- unique(sigma[which(l == min(l, na.rm=TRUE))])
                        if(length(lci) != 1)
                            lci <- lci[length(lci)]
                    }
                    c(uci, lci)
                }, greater= {
                    u <- absigma - qansari(alpha, n, m)
                    if(length(u[u >= 0]) == 0)
                        uci <- sigma[1]
                    else {
                        u[u < 0] <- NA
                        uci <- unique(sigma[which(u == min(u, na.rm=TRUE))])
                        if(length(uci) != 1)
                            uci <- uci[1]
                    }
                    c(uci, Inf)
                }, less= {
                    l <- absigma - qansari(1 - alpha, n, m)
                    if(length(l[l > 0]) == 0)
                        lci <- sigma[length(sigma)]
                    else {                
                        l[l <= 0] <- NA
                        lci <- unique(sigma[which(l == min(l, na.rm=TRUE))])
                        if (length(lci) != 1)
                            lci <- lci[length(lci)]
                    }
                    c(0, lci)
                })
            }
            attr(cint, "conf.level") <- conf.level	
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
            # Bauer defines the CI for y/x, therefore interchange.
            help <- x
            x <- y
            y <- help
            m <- length(x)
            n <- length(y)

            alpha <- 1 - conf.level
            ab <- function(sig, zq) {
                r <- rank(c(y / sig, x))
                s <- sum(pmin(r, N -r + 1)[seq(along = y)])
                TIES <- (length(r) != length(unique(r)))
                abs(normalize(s, r, TIES, length(y), length(x)) - zq)
            }
            ## optimize is not good here, use Nelder-Mead 
            ## what should we use as initial value?
            ## I think the null hypotheses is right here: use sigma = 1 
            cint <- switch(alternative, two.sided = {
                u <- optim(1, ab, zq=qnorm(alpha/2))$par
                l <- optim(1, ab, zq=qnorm(alpha/2, lower = FALSE))$par
                c(u, l)
            }, greater= {
                u <- optim(1, ab, zq=qnorm(alpha))$par
                c(u, Inf)
            }, less= {
                l <- optim(1, ab, zq=qnorm(alpha, lower = FALSE))$par
                c(0, l)
            })
            attr(cint, "conf.level") <- conf.level
        }

        if(exact && TIES) {
            warning("Cannot compute exact p-value with ties")
            if(conf.int)
                warning(paste("Cannot compute exact confidence",
                              "intervals with ties"))
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
        RVAL$conf.int <- cint
    class(RVAL) <- "htest"
    return(RVAL)
}
