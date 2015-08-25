#  File src/library/base/R/labels.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 1998-2012 The R Core Team
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

labels <- function(object, ...) UseMethod("labels")

labels.default <- function(object, ...)
{
    if(length(d <- dim(object))) {	# array or data frame
	nt <- dimnames(object)
	if(is.null(nt)) nt <- vector("list", length(d))
	for(i in seq_along(d))
	    if(!length(nt[[i]])) nt[[i]] <- as.character(seq_len(d[i]))
    } else {
	nt <- names(object)
	if(!length(nt)) nt <- as.character(seq_along(object))
    }
    nt
}
