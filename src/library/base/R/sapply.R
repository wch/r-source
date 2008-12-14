#  File src/library/base/R/sapply.R
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

sapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    answer <- lapply(X, FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
                names(answer) <- X
    if(simplify && length(answer) &&
       length(common.len <- unique(unlist(lapply(answer, length)))) == 1L) {
	if(common.len == 1L)
	    unlist(answer, recursive = FALSE)
	else if(common.len > 1L)
	    array(unlist(answer, recursive = FALSE),
		  dim= c(common.len, length(X)),
		  dimnames= if(!(is.null(n1 <- names(answer[[1L]])) &
			         is.null(n2 <- names(answer)))) list(n1,n2))
	else answer
    } else answer
}

