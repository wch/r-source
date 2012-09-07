#  File src/library/stats/R/shapiro.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

shapiro.test <- function(x)
{
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    stopifnot(is.numeric(x))
    n <- length(x)
    if(is.na(n) || n < 3L || n > 5000L)
	stop("sample size must be between 3 and 5000")
    rng <- x[n] - x[1L]
    if(rng == 0)
	stop("all 'x' values are identical")
    if(rng < 1e-10)
	x <- x/rng # rescale to avoid ifault=6
    n2 <- n %/% 2L # integer, too
    ## C Code: Use the first n1 observations as uncensored
    sw <- .C(C_swilk,
	     init = FALSE,
	     as.single(x),
	     n,#      integer
	     n1 = n,#   "
	     n2,#       "
	     a = single(n2),
	     w	= 0, # input/output
	     pw = double(1),
	     ifault = integer(1L))
    if (sw$ifault && sw$ifault != 7L) # 7 *does* happen (Intel Linux)
	stop(gettextf("ifault=%d. This should not happen", sw$ifault),
             domain = NA)
    RVAL <- list(statistic = c(W = sw$w),
		 p.value = sw$pw,
		 method = "Shapiro-Wilk normality test",
		 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
