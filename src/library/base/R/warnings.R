#  File src/library/base/R/warnings.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

warnings <- function(...)
{
    if(!exists("last.warning", envir=baseenv())) return()
    last.warning <- get("last.warning", envir = baseenv())
    if(!(length(last.warning))) return()
    structure(last.warning, dots = list(...), class = "warnings")
}

`[.warnings` <- function(x, ...)
    structure(NextMethod("["), class = "warnings")

c.warnings <- function (..., recursive = FALSE)
    structure(NextMethod("c"), class = "warnings")

unique.warnings <- function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

print.warnings <- function(x, ...)
{
    if(n <- length(x)) {
        cat(ngettext(n, "Warning message:\n", "Warning messages:\n"))
        msgs <- names(x)
        for(i in seq_len(n)) {
            ind <- if(n == 1L) "" else paste0(i, ": ")
            out <- if(length(x[[i]])) {
                ## deparse can overshoot cutoff
                temp <- deparse(x[[i]], width.cutoff = 50L, nlines = 2L)
                ## Put on one line if narrow enough.
                sm <- strsplit(msgs[i], "\n")[[1L]]
                nl <- if(nchar(ind, "w") + nchar(temp[1L], "w") +
                         nchar(sm[1L], "w") <= 75L)
                    " " else "\n  "
                paste(ind, "In ",
                      temp[1L], if(length(temp) > 1L) " ...",
                      " :", nl, msgs[i], sep = "")
            } else paste0(ind, msgs[i])
            do.call("cat", c(list(out), attr(x, "dots"), fill=TRUE))
        }
    }
    invisible(x)
}

##' @title Warn about extraneous arguments in the "..."	 (of its caller).
##' @author Martin Maechler, June 2012, May 2014
##' @param ...
##' @param which.call passed to sys.call().  A caller may use -2 if the message should
##' mention *its* caller
##' @param allowed not yet implemented: character vector of *named* elements in '...'
##' which are \dQuote{allowed} and hence not warned about
chkDots <- function(..., which.call = -1, allowed = character(0)) {
    if(nx <- length(list(...)))
	warning(sprintf(ngettext(nx,
				 "In %s :\n extra argument %s will be disregarded",
				 "In %s :\n extra arguments %s will be disregarded"),
			deparse(sys.call(which.call), control=c()),
			## sub(")$", '', sub("^list\\(", '', deparse(list(...), control=c())))
			paste(sQuote(names(list(...))), collapse = ", ")),
		call. = FALSE, domain=NA)
}
