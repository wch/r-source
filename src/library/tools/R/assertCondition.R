#  File src/library/tools/R/assertCondition.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2013 The R Core Team
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

assertCondition <- function(expr, ..., verbose=getOption("verbose"),
			    ignoreWarning = is.na(match("warning", conds))) {
    fe <- function(e)e
    d.expr <- deparse(substitute(expr), width.cutoff = 30L)
    if(length(d.expr) > 1)
	d.expr <- paste(d.expr[[1]], "...")
    conds <- if(nargs() > 1) c(...) else "condition"
    .Wanted <- paste(conds, collapse = " | ")
    if(ignoreWarning)
	res <- tryCatch(expr, error = fe, finally = fe)
    else
	res <- tryCatch(expr, error = fe, warning = fe, finally = fe)
    if(inherits(res, "condition")) {
	ii <- class(res) %in% conds
	if(any(ii)) {
	    if(verbose) cat(sprintf("Asserted %s: %s\n",
				    class(res)[which.max(ii)],
				    conditionMessage(res)))
	    invisible(res)
	}
	else
	    stop(gettextf("Expected %s, got %s in evaluating %s",
			 .Wanted, class(res)[[1]], d.expr))
    }
    else
	stop(gettextf("Failed to get expected %s in evaluating %s",
		     .Wanted, d.expr))
}
