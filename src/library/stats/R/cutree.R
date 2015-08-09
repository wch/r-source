#  File src/library/stats/R/cutree.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

cutree <- function(tree, k=NULL, h=NULL)
{
    if(is.null(n1 <- nrow(tree$merge)) || n1 < 1)
        stop("invalid 'tree' ('merge' component)")
    n <- n1 + 1
    if(is.null(k) && is.null(h))
        stop("either 'k' or 'h' must be specified")
    if(is.null(k)) {
        if(is.unsorted(tree$height))
            stop("the 'height' component of 'tree' is not sorted (increasingly)")
        ## h |--> k
        ## S+6 help(cutree) says k(h) = k(h+), but does k(h-) [continuity]
        ## h < min() should give k = n;
        k <- n+1L - apply(outer(c(tree$height,Inf), h, ">"), 2, which.max)
        if(getOption("verbose")) message("cutree(): k(h) = ", k, domain = NA)
    }
    else {
        k <- as.integer(k)
        if(min(k) < 1 || max(k) > n)
            stop(gettextf("elements of 'k' must be between 1 and %d", n),
                 domain = NA)
    }

    ans <- .Call(C_cutree, tree$merge, k)

    if(length(k) == 1L) {
        ans <- setNames(as.vector(ans), tree$labels)
    }
    else{
        colnames(ans) <- if(!is.null(h)) h else k
        rownames(ans) <- tree$labels
    }
    return(ans)
}
