#  File src/library/base/R/traceback.R
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

traceback <-
function(x = NULL, max.lines = getOption("deparse.max.lines"))
{
    if(is.null(x) && !is.null(x <- get0(".Traceback", envir = baseenv())))
	{}
    else if (is.numeric(x))
    	x <- .Internal(traceback(x))
    n <- length(x)
    if(n == 0L)
        cat(gettext("No traceback available"), "\n")
    else {
        for(i in 1L:n) {
            label <- paste0(n-i+1L, ": ")
            m <- length(x[[i]])
            if (!is.null(srcref <- attr(x[[i]], "srcref"))) {
            	srcfile <- attr(srcref, "srcfile")
            	x[[i]][m] <- paste0(x[[i]][m], " at ",
				    basename(srcfile$filename), "#", srcref[1L])
            }
            if(m > 1)
                label <- c(label, rep(substr("          ", 1L,
                                             nchar(label, type="w")),
                                      m - 1L))
            if(is.numeric(max.lines) && max.lines > 0L && max.lines < m) {
                cat(paste0(label[1L:max.lines], x[[i]][1L:max.lines]),
                    sep = "\n")
                cat(label[max.lines+1L], " ...\n")
            } else
            	cat(paste0(label, x[[i]]), sep="\n")
        }
    }
    invisible(x)
}
