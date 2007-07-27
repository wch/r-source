#  File src/library/utils/R/capture.output.R
#  Part of the R package, http://www.R-project.org
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

capture.output <- function(..., file=NULL, append=FALSE)
{
    args <- substitute(list(...))[-1]

    if (is.null(file)){
        file <- textConnection("rval", ifelse(append, "a", "w"), local = TRUE)
        sink(file)
        on.exit({sink();close(file)})
    } else if (inherits(file, "connection")) {
	rval <- invisible(NULL)
	if (!isOpen(file)){
            open(file, ifelse(append, "a", "w"))
            sink(file)
            on.exit({sink();close(file)})
	} else{
            sink(file)
            on.exit(sink())
	}
    } else {
        file <- file(file, ifelse(append, "a", "w"))
        rval <- invisible(NULL)
        sink(file)
        on.exit({sink();close(file)})
    }

    pf <- parent.frame()
    evalVis <- function(expr)
        .Internal(eval.with.vis(expr, pf, baseenv()))

    for(i in seq_along(args)) {
        expr <- args[[i]]
        if(mode(expr) == "expression")
            tmp <- lapply(expr, evalVis)
        else if (mode(expr) == "call")
            tmp <- list(evalVis(expr))
        else if (mode(expr) == "name")
            tmp <- list(evalVis(expr))
        else stop("bad argument")

        for(item in tmp)
            if (item$visible) print(item$value)
    }
    rval
}
