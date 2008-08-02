#  File src/library/base/R/data.matrix.R
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

data.matrix <- function(frame, rownames.force = NA)
{
    if(!is.data.frame(frame)) return(as.matrix(frame))

    d <- dim(frame)
    if(d[2] > 0) {
	log <- unlist(lapply(frame, is.logical))
	num <- unlist(lapply(frame,
                             function(x) is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXct")
                             ))
	fac <- unlist(lapply(frame, is.factor))

	if(!all(log|fac|num))
	    stop("non-numeric data type in frame")
        cl <- sapply(frame[log|num], function(x) {
            cl <- class(x)
            length(cl) > 1 || ! (cl %in% c("numeric", "integer", "logical"))
        })
        if(length(cl) && any(cl))
            warning("class information lost from one or more columns")
    }
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(frame)
    else {if(.row_names_info(frame) <= 0) NULL else row.names(frame)}
    x <- matrix(NA_integer_, nrow = d[1], ncol = d[2],
		dimnames = list(rn, names(frame)) )
    for(i in seq_len(d[2]) ) {
	xi <- frame[[i]]
	x[,i] <- if(is.logical(xi) || is.factor(xi)) as.integer(xi) else xi
    }
    x
}
