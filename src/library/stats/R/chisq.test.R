chisq.test <-
function(x, y = NULL, correct = TRUE, p = rep(1 / length(x), length(x)),
         simulate.p.value = FALSE, B = 2000)
{
    DNAME <- deparse(substitute(x))
    if (is.data.frame(x))
        x <- as.matrix(x)
    if (is.matrix(x)) {
	if (min(dim(x)) == 1)
	    x <- as.vector(x)
    }
    if (!is.matrix(x) && !is.null(y)) {
	if (length(x) != length(y))
	    stop("x and y must have the same length")
        DNAME <- c(DNAME, deparse(substitute(y)))
	OK <- complete.cases(x, y)
	x <- factor(x[OK])
	y <- factor(y[OK])
	if ((nlevels(x) < 2) || (nlevels(y) < 2))
	    stop("x and y must have at least 2 levels")
        ## Could also call table() with 'deparse.level = 2', but we need
        ## to deparse ourselves for DNAME anyway ...
	x <- table(x, y)
        names(dimnames(x)) <- DNAME
        DNAME <- paste(DNAME, collapse = " and ")
    }

    if (any(x < 0) || any(is.na(x)))
	stop("all entries of x must be nonnegative and finite")
    if ((n <- sum(x)) == 0)
        stop("at least one entry of x must be positive")

    if (is.matrix(x)) {
	METHOD <- "Pearson's Chi-squared test"
        nr <- nrow(x)
        nc <- ncol(x)
        sr <- rowSums(x)
        sc <- colSums(x)
	E <- outer(sr, sc, "*") / n
	dimnames(E) <- dimnames(x)
        if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
            METHOD <- paste(METHOD,
                            "with simulated p-value\n\t (based on", B,
                            "replicates)")
            tmp <- .C("chisqsim",
                      as.integer(nr),
                      as.integer(nc),
                      as.integer(sr),
                      as.integer(sc),
                      as.integer(n),
                      as.integer(B),
                      as.double(E),
                      integer(nr * nc),
                      double(n + 1),
                      integer(nc),
                      results = double(B),
                      PACKAGE = "stats")
            ## Sorting before summing may look strange, but seems to be
            ## a sensible way to deal with rounding issues (PR#3486):
            STATISTIC <- sum(sort((x - E) ^ 2 / E, decreasing = TRUE))
            PARAMETER <- NA
            ## use correct significance level for a Monte Carlo test
            PVAL <- (1 + sum(tmp$results >= STATISTIC)) / (B + 1)
        }
        else {
            if (simulate.p.value)
                warning("cannot compute simulated p-value with zero marginals")
            if (correct && nrow(x) == 2 && ncol(x) == 2) {
                YATES <- .5
                METHOD <- paste(METHOD, "with Yates' continuity correction")
            }
            else
                YATES <- 0
            STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
            PARAMETER <- (nr - 1) * (nc - 1)
            PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
        }
    }
    else {
	if (length(x) == 1)
	    stop("x must at least have 2 elements")
	if (length(x) != length(p))
	    stop("x and p must have the same number of elements")
	METHOD <- "Chi-squared test for given probabilities"
	E <- n * p
	names(E) <- names(x)
	STATISTIC <- sum((x - E) ^ 2 / E)
	PARAMETER <- length(x) - 1
        PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
    }

    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    if (any(E < 5) && is.finite(PARAMETER))
	warning("Chi-squared approximation may be incorrect")

    structure(list(statistic = STATISTIC,
		   parameter = PARAMETER,
		   p.value = PVAL,
		   method = METHOD,
		   data.name = DNAME,
		   observed = x,
		   expected = E,
                   residuals = (x - E) / sqrt(E)),
	      class = "htest")
}
