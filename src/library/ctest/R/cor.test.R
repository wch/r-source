cor.test <- function(x, y,
		     alternative = c("two.sided", "less", "greater"),
		     method = c("pearson", "kendall", "spearman"),
		     exact = NULL)
{
    alternative <- match.arg(alternative)
    method <- match.arg(method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if(length(x) != length(y))
	stop("x and y must have the same length")
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    n <- length(x)

    PVAL <- NULL
    NVAL <- 0

    if(method == "pearson") {
	if(n < 3)
	    stop("not enough finite observations")
	method <- "Pearson's product-moment correlation"
	names(NVAL) <- "correlation"
	r <- cor(x, y)
	ESTIMATE <- r
	names(ESTIMATE) <- "cor"
	PARAMETER <- n - 2
	names(PARAMETER) <- "df"
	STATISTIC <- sqrt(PARAMETER) * r / sqrt(1 - r^2)
	names(STATISTIC) <- "t"
	p <- pt(STATISTIC, PARAMETER)
    }
    else {
	if(n < 2)
	    stop("not enough finite observations")
	PARAMETER <- NULL
	TIES <- (min(length(unique(x)), length(unique(y))) < n)
	if(method == "kendall") {
	    method <- "Kendall's rank correlation tau"
	    names(NVAL) <- "tau"
	    x <- rank(x)
	    y <- rank(y)
	    ESTIMATE <- .C("kendall_tau",
			   as.integer(length(x)),
			   as.double(x),
			   as.double(y),
			   tau = as.double(0),
			   PACKAGE = "ctest")$tau
	    names(ESTIMATE) <- "tau"
	    if(is.null(exact))
		exact <- (n < 50)
	    if(exact && !TIES) {
		q <- as.integer((ESTIMATE + 1) * n * (n - 1) / 4)
		pkendall <- function(q, n) {
		    .C("pkendall",
		       as.integer(length(q)),
		       p = as.double(q),
		       as.integer(n),
		       PACKAGE = "ctest")$p
		}
		PVAL <- switch(alternative,
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
		STATISTIC <- c(z = ESTIMATE /
			       sqrt((4 * n + 10) / (9 * n * (n-1))))
		p <- pnorm(STATISTIC)
		if(exact && TIES)
		    warning("Cannot compute exact p-value with ties")
	    }
	} else {
	    method <- "Spearman's rank correlation rho"
	    names(NVAL) <- "rho"
	    ESTIMATE <- c(rho = cor(rank(x), rank(y)))
	    ## Use the test statistic S = sum(rank(x) - rank(y))^2 and
	    ## AS 89 for obtaining better p-values than via the normal
	    ## approximation of S by N((n^3-n)/6, 1/sqrt(n-1)).
	    ## In the case of no ties, S = (1-rho) * (n^3-n)/6.
	    pspearman <- function(q, n, lower.tail = TRUE) {
		.C("prho",
		   as.integer(n),
		   as.integer(q + 1),
		   p = double(1),
		   integer(1),
		   as.logical(lower.tail),
		   PACKAGE = "ctest")$p
	    }
	    q <- as.integer((n^3 - n) * (1 - ESTIMATE) / 6)
	    STATISTIC <- c(S = q)
	    PVAL <- switch(alternative,
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

    if(is.null(PVAL)) # for "pearson" (and when else ??)
	PVAL <- switch(alternative,
		       "less" = p,
		       "greater" = 1 - p,
		       "two.sided" = 2 * min(p, 1 - p))

    structure(list(statistic = STATISTIC,
		   parameter = PARAMETER,
		   p.value = PVAL,
		   estimate = ESTIMATE,
		   null.value = NVAL,
		   alternative = alternative,
		   method = method,
		   data.name = DNAME),
	      class = "htest")
}
