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
	DNAME <- paste(DNAME, "and", deparse(substitute(y)))
	OK <- complete.cases(x, y)
	x <- as.factor(x[OK])
	y <- as.factor(y[OK])
	if ((nlevels(x) < 2) || (nlevels(y) < 2))
	    stop("x and y must have at least 2 levels")
	x <- table(x, y)
    }

    if (any(x < 0) || any(is.na(x)))
	stop("all entries of x must be nonnegative and finite")
    if ((n <- sum(x)) == 0)
        stop("at least one entry of x must be positive")

    if (is.matrix(x)) {
	METHOD <- "Pearson's Chi-square test"
        nr <- nrow(x)
        nc <- ncol(x)
        sr <- apply(x, 1, sum)
        sc <- apply(x, 2, sum)
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
                      results = double(B))
            STATISTIC <- sum((x - E) ^ 2 / E)
            PARAMETER <- NA
            PVAL <- sum(tmp$results >= STATISTIC) / B
        }
        else {
            if (simulate.p.value)
                warning(paste("Cannot compute simulated p-value",
                              "with zero marginals"))
            if (correct && nrow(x) == 2 && ncol(x) == 2) {
                YATES <- .5
                METHOD <- paste(METHOD, "with Yates' continuity correction")
            }
            else
                YATES <- 0
            STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
            PARAMETER <- (nr - 1) * (nc - 1)
            PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
        }
    }
    else {
	if (length(x) == 1)
	    stop("x must at least have 2 elements")
	if (length(x) != length(p))
	    stop("x and p must have the same number of elements")
	METHOD <- "Chi-square test for given probabilities"
	E <- n * p
	names(E) <- names(x)
	STATISTIC <- sum((x - E) ^ 2 / E)
	PARAMETER <- length(x) - 1
        PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
    }

    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    if (any(E < 5) && is.finite(PARAMETER))
	warning("Chi-square approximation may be incorrect")

    structure(list(statistic = STATISTIC,
		   parameter = PARAMETER,
		   p.value = PVAL,
		   method = METHOD,
		   data.name = DNAME,
		   observed = x,
		   expected = E),
	      class = "htest")
}
