#  File src/library/utils/R/capture.output.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

capture.output <- function(..., file=NULL, append=FALSE,
                           type = c("output", "message"), split = FALSE)
{
    type <- match.arg(type)
    rval <- NULL; closeit <- TRUE
    if (is.null(file))
        file <- textConnection("rval", "w", local = TRUE)
    else if (is.character(file))
        file <- file(file, if(append) "a" else "w")
    else if (inherits(file, "connection")) {
	if (!isOpen(file)) open(file, if(append) "a" else "w")
	else closeit <- FALSE
    } else
        stop("'file' must be NULL, a character string or a connection")

    sink(file, type=type, split=split)
    ## for error recovery: all output will be lost if file=NULL
    on.exit({sink(type=type, split=split); if(closeit) close(file)})

    for(i in seq_len(...length())) {
	out <- withVisible(...elt(i))
	if (out$visible)
	    print(out$value)
    }

    ## we need to close the text connection before returning 'rval'
    on.exit()
    sink(type=type, split=split)
    if(closeit) close(file)
    rval %||% invisible(NULL)
}
