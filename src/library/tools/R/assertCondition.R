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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

assertCondition <- function(expr, ..., verbose = getOption("verbose")) {
    fe <- function(e)e
    getCondClasses <- function(expr) {
	conds <- character()
	lastCond <- NULL
	tryCatch(withCallingHandlers(expr,
				     warning = function(w) {
					 conds <<- c(conds, class(w))
					 lastCond <<- w
					 invokeRestart("muffleWarning")
				     },
				     condition = function(cond) {
					 conds <<- c(conds, class(cond))
					 lastCond <<- cond}),
		 error = function(e) {
		     conds <<- c(conds, class(e))
		     lastCond <<- e})
	list(unique(conds), lastCond)
    }
    d.expr <- deparse(substitute(expr), width.cutoff = 30L)
    if(length(d.expr) > 1)
	d.expr <- paste(d.expr[[1]], "...")
    conds <- if(nargs() > 1) c(...) # else NULL
    .Wanted <- (if(length(conds))
		paste("expected", paste(conds, collapse = " | "))
		else "any condition")
    res <- getCondClasses(expr)
    lastCond <- res[[2]]
    res <- res[[1]]
    if(length(res)) {
	if(is.null(conds)) {
	    if(verbose) cat(sprintf("Got condition \"%s\": %s\n",
				    class(lastCond)[[1]],
				    conditionMessage(lastCond)))
	    invisible(res)
	}
	else {
	    ii <- res %in% conds
	    if(any(ii)) {
		if(verbose) {
		    got <- res[which.max(ii)]
		    text <- if(got %in% class(lastCond))
			paste(":", conditionMessage(lastCond)) else ""
		    cat(sprintf("Asserted %s%s\n", got, text))
		}
		invisible(res)
	    }
	    else
		stop(gettextf("Got %s (%s) in evaluating %s; %s",
			      class(lastCond)[[1]], conditionMessage(lastCond),
			      d.expr, .Wanted))
	}
    }
    else
	stop(gettextf("Failed to get %s in evaluating %s",
		      .Wanted, d.expr))
}
