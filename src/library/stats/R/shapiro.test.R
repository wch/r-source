shapiro.test <- function(x) {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if(n < 3 || n > 5000)
	stop("sample size must be between 3 and 5000")
    rng <- x[n] - x[1]
    if(rng == 0)
	stop("all 'x' values are identical")
    if(rng < 1e-10)
	x <- x/rng # rescale to avoid ifault=6
    n2 <- n %/% 2
    ## C Code: Use the first n1 observations as uncensored
    sw <- .C("swilk",
	     init = FALSE,
	     as.single(x),
	     n,
	     n1 = as.integer(n),
	     as.integer(n2),
	     a = single(n2),
	     w	= double(1),
	     pw = double(1),
	     ifault = integer(1), PACKAGE = "stats")
    if (sw$ifault && sw$ifault != 7)# 7 *does* happen (Intel Linux)
	stop(gettextf("ifault=%d. This should not happen", sw$ifault),
             domain = NA)
    RVAL <- list(statistic = c(W = sw$w),
		 p.value = sw$pw,
		 method = "Shapiro-Wilk normality test",
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
