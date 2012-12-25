#  File src/library/stats/R/htest.R
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

print.htest <- function(x, digits = 4L, quote = TRUE, prefix = "", ...)
{
    cat("\n")
    cat(strwrap(x$method, prefix = "\t"), sep = "\n")
    cat("\n")
    cat("data:  ", x$data.name, "\n", sep = "")
    out <- character()
    if(!is.null(x$statistic))
	out <- c(out, paste(names(x$statistic), "=",
			    format(round(x$statistic, 4))))
    if(!is.null(x$parameter))
	out <- c(out, paste(names(x$parameter), "=",
			    format(round(x$parameter, 3))))
    if(!is.null(x$p.value)) {
	fp <- format.pval(x$p.value, digits = digits)
	out <- c(out, paste("p-value",
			    if(substr(fp, 1L, 1L) == "<") fp else paste("=",fp)))
    }
    cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
    if(!is.null(x$alternative)) {
	cat("alternative hypothesis: ")
	if(!is.null(x$null.value)) {
	    if(length(x$null.value) == 1L) {
		alt.char <-
		    switch(x$alternative,
			   two.sided = "not equal to",
			   less = "less than",
			   greater = "greater than")
		cat("true ", names(x$null.value), " is ", alt.char, " ",
		    x$null.value, "\n", sep = "")
	    }
	    else {
		cat(x$alternative, "\nnull values:\n", sep = "")
		print(x$null.value, ...)
	    }
	}
	else cat(x$alternative, "\n", sep = "")
    }
    if(!is.null(x$conf.int)) {
	cat(format(100 * attr(x$conf.int, "conf.level")),
	    " percent confidence interval:\n", " ",
	    paste(format(c(x$conf.int[1L], x$conf.int[2L])), collapse = " "),
            "\n", sep = "")
    }
    if(!is.null(x$estimate)) {
	cat("sample estimates:\n")
	print(x$estimate, ...)
    }
    cat("\n")
    invisible(x)
}
