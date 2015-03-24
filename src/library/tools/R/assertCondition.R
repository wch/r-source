#  File src/library/tools/R/assertCondition.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2013-2014 The R Core Team
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

assertCondition <-
    function(expr, ...,
             .exprString = .deparseTrim(substitute(expr), cutoff = 30L),
             verbose = FALSE)
{
    getConds <- function(expr) {
	conds <- list()
	tryCatch(withCallingHandlers(expr,
				     warning = function(w) {
					 conds <<- c(conds, list(w))
					 invokeRestart("muffleWarning")
				     },
				     condition = function(cond)
					 conds <<- c(conds, list(cond))),
		 error = function(e)
		     conds <<- c(conds, list(e)))
	conds
    }
    conds <- if(nargs() > 1) c(...) # else NULL
    .Wanted <- if(nargs() > 1) paste(c(...), collapse = " or ") else "any condition"
    res <- getConds(expr)
    if(length(res)) {
	if(is.null(conds)) {
            if(verbose)
                message("assertConditon: Successfully caught a condition\n")
	    invisible(res)
        }
	else {
	    ii <- sapply(res, function(cond) any(class(cond) %in% conds))
	    if(any(ii)) {
                if(verbose) {
                    found <-
                        unique(sapply(res, function(cond) class(cond)[class(cond) %in% conds]))
                    message(sprintf("assertCondition: caught %s",
                                    paste(dQuote(found), collapse =", ")), domain = NA)
                }
		invisible(res)
            }
	    else {
                .got <- paste(unique((sapply(res, function(obj)class(obj)[[1]]))),
                                     collapse = ", ")
		stop(gettextf("Got %s in evaluating %s; wanted %s",
			      .got, .exprString, .Wanted))
            }
	}
    }
    else
	stop(gettextf("Failed to get %s in evaluating %s",
		      .Wanted, .exprString))
}

assertError <- function(expr, verbose = FALSE) {
    d.expr <- .deparseTrim(substitute(expr), cutoff = 30L)
    tryCatch(res <- assertCondition(expr, "error", .exprString = d.expr),
             error = function(e)
                 stop(gettextf("Failed to get error in evaluating %s", d.expr),
                      call. = FALSE)
             )
    if(verbose) {
        error <- res[ sapply(res, function(cond) "error" %in% class(cond)) ]
        message(sprintf("Asserted error: %s", error[[1]]$message))
    }
    invisible(res)
}

assertWarning <- function(expr, verbose = FALSE) {
    d.expr <- .deparseTrim(substitute(expr), cutoff = 30L)
    res <- assertCondition(expr, "warning", .exprString = d.expr)
    if(any(sapply(res, function(cond) "error" %in% class(cond))))
        stop(gettextf("Got warning in evaluating %s, but also an error", d.expr))
    if(verbose) {
        warning <- res[ sapply(res, function(cond) "warning" %in% class(cond)) ]
        message(sprintf("Asserted warning: %s", warning[[1]]$message))
    }
    invisible(res)
}

.deparseTrim <- function(expr, cutoff = 30L) {
    res <- deparse(expr)
    if(length(res) > 1) {
        if(res[[1]] == "{") {
            exprs <- sub("^[ \t]*", "", res[c(-1, -length(res))])
            res <- paste0("{", paste(exprs, collapse = "; "), "}")
        }
        else
            res <- paste(res[[1]], " ...")
    }
    if(nchar(res) > cutoff)
        paste(substr(res, 1, cutoff), " ...")
    else
        res
}
