#  File src/library/stats/R/ecdf.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

#### Empirical Cumulative Distribution Functions :  "ecdf"
##--  inherit from  "stepfun"

## Constructor
ecdf <- function (x)
{
    x <- sort(x) # drops NAs
    n <- length(x)
    if(n < 1) stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
		      method = "constant", yleft = 0, yright = 1, f = 0,
                      ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    assign("nobs", n, envir=environment(rval))# e.g. to reconstruct rank(x)
    attr(rval, "call") <- sys.call()
    rval
}

print.ecdf <- function (x, digits = getOption("digits") - 2L, ...)
{
    numform <- function(x)
        paste(formatC(x, digits = digits, decimal.mark = getOption("OutDec")),
              collapse = ", ")
    cat("Empirical CDF \nCall: ")
    print(attr(x, "call"), ...)
    n <- length(xx <- environment(x)$"x")
    i1 <- 1L:min(3L,n)
    i2 <- if(n >= 4L) max(4L, n-1L):n else integer()
    cat(" x[1:",n,"] = ", numform(xx[i1]),
	if(n>3L) ", ", if(n>5L) " ..., ", numform(xx[i2]), "\n", sep = "")
    invisible(x)
}

summary.ecdf <- function(object, ...)
{
    n <- length(eval(expression(x), envir = environment(object)))
    header <- paste("Empirical CDF:	 ", n,
                    "unique values with summary\n")
    structure(summary(knots(object), ...),
              header = header, class = "summary.ecdf")
}

print.summary.ecdf <- function(x, ...)
{
    cat(attr(x, "header"))
    y <- unclass(x); attr(y, "header") <- NULL
    print(y, ...)
    invisible(x)
}

## add  conf.int = 0.95
## and  conf.type = c("none", "KS")
## (these argument names are compatible to Kaplan-Meier survfit() !)
## and use ./KS-confint.R 's  code !!!

plot.ecdf <- function(x, ..., ylab="Fn(x)", verticals = FALSE,
		      col.01line = "gray70", pch = 19)
{
    plot.stepfun(x, ..., ylab = ylab, verticals = verticals, pch = pch)
    abline(h = c(0,1), col = col.01line, lty = 2)
}

utils::globalVariables("y", add = TRUE)
quantile.ecdf <- function (x, ...)
    ## == quantile( sort( <original sample> ) ) :
    quantile(evalq(rep.int(x, diff(c(0, round(nobs*y)))), environment(x)), ...)

