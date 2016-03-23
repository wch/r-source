#  File src/library/grDevices/R/prettyDate.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
#
# Original code Copyright (C) 2010 Felix Andrews
# Modifications Copyright (C) 2010 The R Core Team
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
#  https://www.R-project.org/Licenses/

##' S3 method =:  pretty.Date() and pretty.POSIXt [in ../NAMESPACE]
prettyDate <- function(x, n = 5, min.n = n %/% 2, sep = " ", ...)
{
    stopifnot(min.n <= n)
    isDate <- inherits(x, "Date")
    x <- as.POSIXct(x)
    if (isDate) # the timezone *does* matter
	attr(x, "tzone") <- "GMT"
    zz <- range(x, na.rm = TRUE)
    D <- diff(nzz <- as.numeric(zz))
    MIN <- 60
    HOUR <- MIN * 60
    DAY <- HOUR * 24
    YEAR <- DAY * 365.25
    MONTH <- YEAR / 12
    makeOutput <- function(at, s, round = TRUE, do) {
	structure(if(isDate)
		      if(round) as.Date(round(at, units = "days")) else at
		  else as.POSIXct(at),
		  labels = format(at, s$format))
    }
    if(isDate && D <= n * DAY) { # D <= 'n days' & Date  ==> use days
	r <- round(n - D/DAY)
	m <- max(0, r %/% 2)
	zz <- as.Date(zz)
 	zz <- seq.Date(zz[1] - m, zz[2] + m + (r %% 2), by = "1 day")
	return(makeOutput(zz, round = FALSE, ## "1 DSTday" from steps:
			  list(format = paste("%b", "%d", sep = sep))))
    }
    else if(D < 1) { # unique values / sub-second ranges: [? or use "1 ms" steps below?]
	m <- min(30, max(D == 0, n/2 - 1)) # "- 1" ==> better match for 'n'
	zz <- structure(c(floor(nzz[1] - m), ceiling(nzz[2] + m)),
			class = class(x), tzone = attr(x, "tzone"))
    }
    xspan <- as.numeric(diff(zz), units = "secs")
    ## specify the set of pretty timesteps
    steps <-
        list("1 sec" = list(1, format = "%S", start = "mins"),
             "2 secs" = list(2),
             "5 secs" = list(5),
             "10 secs" = list(10),
             "15 secs" = list(15),
             "30 secs" = list(30, format = "%H:%M:%S"),
             "1 min" = list(1*MIN, format = "%H:%M"),
             "2 mins" = list(2*MIN, start = "hours"),
             "5 mins" = list(5*MIN),
             "10 mins" = list(10*MIN),
             "15 mins" = list(15*MIN),
             "30 mins" = list(30*MIN),
             ## "1 hour" = list(1*HOUR),
             "1 hour" = list(1*HOUR, format = if (xspan <= DAY) "%H:%M" else paste("%b %d", "%H:%M", sep = sep)),
             "3 hours" = list(3*HOUR, start = "days"),
             "6 hours" = list(6*HOUR, format = paste("%b %d", "%H:%M", sep = sep)),
             "12 hours" = list(12*HOUR),
             "1 DSTday" = list(1*DAY, format = paste("%b", "%d", sep = sep)),
             "2 DSTdays" = list(2*DAY),
             "1 week" = list(7*DAY, start = "weeks"),
             "halfmonth" = list(MONTH/2, start = "months"),
             ## "1 month" = list(1*MONTH, format = "%b"),
             "1 month" = list(1*MONTH, format = if (xspan < YEAR) "%b" else paste("%b", "%Y", sep = sep)),
             "3 months" = list(3*MONTH, start = "years"),
             "6 months" = list(6*MONTH, format = "%Y-%m"),
             "1 year" = list(1*YEAR, format = "%Y"),
             "2 years" = list(2*YEAR, start = "decades"),
             "5 years" = list(5*YEAR),
             "10 years" = list(10*YEAR),
             "20 years" = list(20*YEAR, start = "centuries"),
             "50 years" = list(50*YEAR),
             "100 years" = list(100*YEAR),
             "200 years" = list(200*YEAR),
             "500 years" = list(500*YEAR),
             "1000 years" = list(1000*YEAR))
    ## carry forward 'format' and 'start' to following steps
    for (i in seq_along(steps)) {
        if (is.null(steps[[i]]$format))
            steps[[i]]$format <- steps[[i-1]]$format
        if (is.null(steps[[i]]$start))
            steps[[i]]$start <- steps[[i-1]]$start
        steps[[i]]$spec <- names(steps)[i]
    }
    ## crudely work out number of steps in the given interval
    nsteps <- xspan / vapply(steps, `[[`, numeric(1), 1L)
    init.i <- which.min(abs(nsteps - n))
    ## calculate actual number of ticks in the given interval
    calcSteps <- function(s) {
        startTime <- trunc_POSIXt(min(zz), units = s$start) ## FIXME: should be trunc() eventually
	at <- if (identical(s$spec, "halfmonth")) {
		  at <- seq(startTime, max(zz), by = "months")
		  at2 <- as.POSIXlt(at)
		  at2$mday <- 15L
		  structure(sort(c(as.POSIXct(at), as.POSIXct(at2))),
			    tzone = attr(at, "tzone"))
	      } else
		  seq(startTime, max(zz), by = s$spec)
	at[min(zz) <= at & at <= max(zz)]
    }
    init.at <- calcSteps(steps[[init.i]])
    ## bump it up if below acceptable threshold
    while ((init.n <- length(init.at) - 1L) < min.n) {
	if(init.i == 1L)
	    break # and live with it instead of error
	init.i <- init.i - 1L
	init.at <- calcSteps(steps[[init.i]])
    }
    if (init.n == n) ## perfect
        return(makeOutput(init.at, steps[[init.i]]))
    new.i <- if (init.n > n) ## too many ticks
		 min(init.i + 1L, length(steps))
	     else ## too few ticks
		 max(init.i - 1L, 1L)
    new.at <- calcSteps(steps[[new.i]])
    new.n <- length(new.at) - 1L
    ## work out whether new.at or init.at is better
    if (new.n < min.n)
        new.n <- -Inf
    if (abs(new.n - n) < abs(init.n - n))
	makeOutput(new.at, steps[[new.i]])
    else
	makeOutput(init.at, steps[[init.i]])
}

## utility function, extending the base function trunc.POSIXt.
## Ideally this should replace the original, but that should be done
## with a little more thought (what about round.POSIXt etc.?)

trunc_POSIXt <-
    function(x, units = c("secs", "mins", "hours", "days",
                "weeks", "months", "years", "decades", "centuries"),
             start.on.monday = TRUE)
{
    x <- as.POSIXlt(x)
    ## Why is base:: here?  The namespace implicitly imports base
    if (units %in% c("secs", "mins", "hours", "days"))
        return(base::trunc.POSIXt(x, units))
    x <- base::trunc.POSIXt(x, "days")
    if (length(x$sec))
        switch(units,
               weeks = {
                   x$mday <- x$mday - x$wday
                   if (start.on.monday)
                       x$mday <- x$mday + ifelse(x$wday > 0L, 1L, -6L)
               },
               months = {
                   x$mday <- 1
               },
               years = {
                   x$mday <- 1
                   x$mon <- 0
               },
               decades = {
                   x$mday <- 1
                   x$mon <- 0
                   x$year <- (x$year %/% 10) * 10
               },
               centuries = {
                   x$mday <- 1
                   x$mon <- 0
                   x$year <- (x$year %/% 100) * 100
               })
    x
}
