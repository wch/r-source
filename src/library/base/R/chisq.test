chisq.test <- function(x, y = NULL, correct = TRUE,
		       p = rep(1 / length(x), length(x)))
{
  DNAME <- deparse(substitute(x))
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

  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-square test"    
    E <- outer(apply(x, 1, sum), apply(x, 2, sum), "*") / sum(x)  
    if (correct && nrow(x) == 2 && ncol(x) == 2) {
      YATES <- .5
      METHOD <- paste(METHOD, "with Yates' continuity correction")
    }
    else
      YATES <- 0
    dimnames(E) <- dimnames(x)
    STATISTIC <- sum((abs(x - E) - YATES)^2 / E)
    PARAMETER <- (nrow(x) - 1) * (ncol(x) - 1)
  }
  else {
    if (length(x) == 1)
      stop("x must at least have 2 elements")
    if (length(x) != length(p))
      stop("x and p must have the same number of elements")
    METHOD <- "Chi-square test for given probabilities"
    E <- sum(x) * p
    names(E) <- names(x)
    STATISTIC <- sum((x - E) ^ 2 / E)
    PARAMETER <- length(x) - 1
  }

  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  if (any(E < 5))
    warning("Chi-square approximation may be incorrect") 
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)

  structure(list(statistic = STATISTIC,
		 parameter = PARAMETER,
		 p.value = PVAL,
		 method = METHOD,
		 data.name = DNAME,
		 observed = x,
		 expected = E),
	    class = "htest")
}
