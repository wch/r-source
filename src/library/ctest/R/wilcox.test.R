wilcox.test <- function(x, y = NULL,
			alternative = c("two.sided", "less", "greater"),
			mu = 0, paired = FALSE, exact = NULL, correct = TRUE)
{
    alternative <- match.arg(alternative)
    if (!missing(mu) && ((length(mu) > 1) || !is.finite(mu)))
	stop("mu must be a single number")

    if (!is.null(y)) {
	DNAME <- paste(deparse(substitute(x)), "and",
		       deparse(substitute(y)))
	if (paired) {
	    if (length(x) != length(y))
		stop("x and y must have the same length")
	    OK <- complete.cases(x, y)
	    x <- x[OK] - y[OK]
	    y <- NULL
	}
	else {
	    x <- x[is.finite(x)]
	    y <- y[is.finite(y)]
	}
    } else {
	DNAME <- deparse(substitute(x))
	if (paired)
	    stop("y missing for paired test")
	x <- x[is.finite(x)]
    }

    if (length(x) < 1)
	stop("not enough (finite) x observations")

    CORRECTION <- 0

    if (is.null(y)) { ## 1 sample
	METHOD <- "Wilcoxon signed rank test"
	x <- x - mu
	ZEROES <- any(x == 0)
	if (ZEROES)
	    x <- x[x != 0]
	n <- length(x)
	if (is.null(exact))
	    exact <- (n < 50)
	r <- rank(abs(x))
	STATISTIC <- sum(r[x > 0])
	names(STATISTIC) <- "V"
	TIES <- (length(r) != length(unique(r)))
	if (exact && !TIES && !ZEROES) {
	    PVAL <-
		switch(alternative,
		       "two.sided" = {
			   if (STATISTIC > (n * (n + 1) / 4))
			       p <- 1 - psignrank(STATISTIC - 1, n)
			   else
			       p <- psignrank(STATISTIC, n)
			   min(2 * p, 1)
		       },
		       "greater" = 1 - psignrank(STATISTIC - 1, n),
		       "less" = psignrank(STATISTIC, n))
	} else {
	    NTIES <- table(r)
	    z <- STATISTIC - n * (n + 1) / 4
	    SIGMA <- sqrt(n * (n + 1) * (2 * n + 1) / 24 -
			  sum(NTIES^3 - NTIES) / 48)
	    if (correct) {
		CORRECTION <- switch(alternative,
				     "two.sided" = sign(z) * 0.5,
				     "greater" = 0.5,
				     "less" = -0.5)
		METHOD <- paste(METHOD, "with continuity correction")
	    }
	    PVAL <- pnorm((z - CORRECTION) / SIGMA)
	    if (alternative == "two.sided")
		PVAL <- 2 * min(PVAL, 1 - PVAL)
	    if (alternative == "greater")
		PVAL <- 1 - PVAL
	    if (exact && TIES)
		warning("Cannot compute exact p-value with ties")
	    if (exact && ZEROES)
		warning("Cannot compute exact p-value with zeroes")
	}
    } else {
	if (length(y) < 1)
	    stop("not enough y observations")
	METHOD <- "Wilcoxon rank sum test"
	r <- rank(c(x - mu, y))
	n.x <- length(x)
	n.y <- length(y)
	if (is.null(exact))
	    exact <- (n.x < 50) && (n.y < 50)
	STATISTIC <- sum(r[seq(along = x)]) - n.x * (n.x + 1) / 2
	## Contrary to BB, we use the symmetric definition
	names(STATISTIC) <- "W"
	TIES <- (length(r) != length(unique(r)))
	if (exact && !TIES) {
	    PVAL <-
		switch(alternative,
		       "two.sided" = {
			   if (STATISTIC > (n.x * n.y / 2))
			       p <- 1 - pwilcox(STATISTIC - 1, n.x, n.y)
			   else
			       p <- pwilcox(STATISTIC, n.x, n.y)
			   min(2 * p, 1)
		       },
		       "greater" = 1 - pwilcox(STATISTIC - 1, n.x, n.y),
		       "less" = pwilcox(STATISTIC, n.x, n.y))
	} else {
	    NTIES <- table(r)
	    z <- STATISTIC - n.x * n.y / 2
	    SIGMA <- sqrt((n.x * n.y / 12) *
			  ((n.x + n.y + 1)
			   - sum(NTIES^3 - NTIES)
			   / ((n.x + n.y) * (n.x + n.y -1))))
	    if (correct) {
		CORRECTION <- switch(alternative,
				     "two.sided" = sign(z) * 0.5,
				     "greater" = 0.5,
				     "less" = -0.5)
		METHOD <- paste(METHOD, "with continuity correction")
	    }
	    PVAL <- pnorm((z - CORRECTION) / SIGMA)
	    if (alternative == "two.sided")
		PVAL <- 2 * min(PVAL, 1 - PVAL)
	    if (alternative == "greater")
		PVAL <- 1 - PVAL
	    if (exact && TIES)
		warning("Cannot compute exact p-value with ties")
	}
    }

    RVAL <- list(statistic = STATISTIC,
		 parameter = NULL,
		 p.value = PVAL,
		 null.value = c(mu = mu),
		 alternative = alternative,
		 method = METHOD,
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
