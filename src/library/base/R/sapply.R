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
	else if(common.len > 1L) {
	    ## make sure that array(*) will not call rep() {e.g. for 'call's}:
	    r <- as.vector(unlist(answer, recursive = FALSE))
	    if(prod(d <- c(common.len, length(X))) == length(r))
		array(r, dim = d,
		      dimnames= if(!(is.null(n1 <- names(answer[[1L]])) &
				     is.null(n2 <- names(answer)))) list(n1,n2))
	    else answer
	}
	else answer
    } else answer
}

vapply <- function(X, FUN, FUN.VALUE, ...,  USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
}


