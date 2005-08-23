cor.test <- function(x, ...) UseMethod("cor.test")

cor.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"), exact = NULL,
         conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    method <- match.arg(method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if(length(x) != length(y))
	stop("'x' and 'y' must have the same length")
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    n <- length(x)

    PVAL <- NULL
    NVAL <- 0
    conf.int <- FALSE

    if(method == "pearson") {
	if(n < 3)
	    stop("not enough finite observations")
	method <- "Pearson's product-moment correlation"
	names(NVAL) <- "correlation"
	r <- cor(x, y)
        df <- n - 2
	ESTIMATE <- c(cor = r)
	PARAMETER <- c(df = df)
	STATISTIC <- c(t = sqrt(df) * r / sqrt(1 - r^2))
	p <- pt(STATISTIC, df)
        if(n > 3) { ## confidence int.
            if(!missing(conf.level) &&
               (length(conf.level) != 1 || !is.finite(conf.level) ||
                conf.level < 0 || conf.level > 1))
                stop("'conf.level' must be a single number between 0 and 1")
            conf.int <- TRUE
            z <- atanh(r)
            sigma <- 1 / sqrt(n - 3)
            cint <-
                switch(alternative,
                       less = c(-Inf, z + sigma * qnorm(conf.level)),
                       greater = c(z - sigma * qnorm(conf.level), Inf),
                       two.sided = z +
                       c(-1, 1) * sigma * qnorm((1 + conf.level) / 2))
            cint <- tanh(cint)
            attr(cint, "conf.level") <- conf.level
        }
    }
    else {
	if(n < 2)
	    stop("not enough finite observations")
	PARAMETER <- NULL
	TIES <- (min(length(unique(x)), length(unique(y))) < n)
	if(method == "kendall") {
	    method <- "Kendall's rank correlation tau"
	    names(NVAL) <- "tau"
	    r <- cor(x,y, method = "kendall")
            ESTIMATE <- c(tau = r)

            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(T = NA)
                PVAL <- NA
            }
            else {
                if(is.null(exact))
                    exact <- (n < 50)
                if(exact && !TIES) {
                    q <- round((r + 1) * n * (n - 1) / 4)
                    pkendall <- function(q, n) {
                        .C("pkendall",
                           length(q),
                           p = as.double(q),
                           as.integer(n),
                           PACKAGE = "stats")$p
                    }
                    PVAL <-
                        switch(alternative,
                               "two.sided" = {
                                   if(q > n * (n - 1) / 4)
                                       p <- 1 - pkendall(q - 1, n)
                                   else
                                       p <- pkendall(q, n)
                                   min(2 * p, 1)
                               },
                               "greater" = 1 - pkendall(q - 1, n),
                               "less" = pkendall(q, n))
                    STATISTIC <- c(T = q)
                } else {
                    STATISTIC <- c(z = r / sqrt((4 * n + 10) / (9 * n*(n-1))))
                    p <- pnorm(STATISTIC)
                    if(exact && TIES)
                        warning("Cannot compute exact p-value with ties")
                }
            }
	} else {
	    method <- "Spearman's rank correlation rho"
	    names(NVAL) <- "rho"
	    r <- cor(rank(x), rank(y))
	    ESTIMATE <- c(rho = r)
            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(S = NA)
                PVAL <- NA
            }
            else {
                ## Use the test statistic S = sum(rank(x) - rank(y))^2
                ## and AS 89 for obtaining better p-values than via the
                ## simple normal approximation.
                ## In the case of no ties, S = (1-rho) * (n^3-n)/6.
                pspearman <- function(q, n, lower.tail = TRUE) {
                    if(n <= 1290) # n*(n^2 - 1) does not overflow
                        .C("prho",
                           as.integer(n),
                           as.double(q + 1),
                           p = double(1),
                           integer(1),
                           as.logical(lower.tail),
                           PACKAGE = "stats")$p
		    else { # for large n: aymptotic t_{n-2}
			r <- 1 - 6 * q / (n*(n-1)*(n+1))
			pt(r / sqrt((1 - r^2)/(n-2)), df = n-2,
			   lower.tail= !lower.tail)
		    }
                }
                q <- round((n^3 - n) * (1 - r) / 6)
                STATISTIC <- c(S = q)
                PVAL <-
                    switch(alternative,
                           "two.sided" = {
                               p <- if(q > (n^3 - n) / 6)
                                   pspearman(q - 1, n, lower.tail = FALSE)
                               else
				   pspearman(q, n, lower.tail = TRUE)
			       min(2 * p, 1)
			   },
			   "greater" = pspearman(q, n, lower.tail = TRUE),
			   "less" = pspearman(q - 1, n, lower.tail = FALSE))
                if(TIES)
                    warning("p-values may be incorrect due to ties")
            }
        }
    }

    if(is.null(PVAL)) # for "pearson" only, currently
	PVAL <- switch(alternative,
		       "less" = p,
		       "greater" = 1 - p,
		       "two.sided" = 2 * min(p, 1 - p))

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = as.numeric(PVAL),
                 estimate = ESTIMATE,
                 null.value = NVAL,
                 alternative = alternative,
                 method = method,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL, list(conf.int = cint))
    class(RVAL) <- "htest"
    RVAL
}

cor.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || !inherits(formula, "formula")
       || length(formula) != 2)
        stop("'formula' missing or invalid")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, environment(formula))
    if(length(mf) != 2)
        stop("invalid formula")
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- c("x", "y")
    y <- do.call("cor.test", c(mf, list(...)))
    y$data.name <- DNAME
    y
}
