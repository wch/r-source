#  File src/library/stats/R/identify.hclust.R
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

rect.hclust <- function(tree, k=NULL, which=NULL,
                        x=NULL, h=NULL, border=2, cluster=NULL)
{
    if(length(h) > 1L | length(k) > 1L)
        stop("'k' and 'h' must be a scalar")

    if(!is.null(h)){
        if(!is.null(k))
            stop("specify exactly one of 'k' and 'h'")
        k <- min(which(rev(tree$height)<h))
        k <- max(k, 2)
    }
    else
        if(is.null(k))
            stop("specify exactly one of 'k' and 'h'")

    if(k < 2 | k > length(tree$height))
        stop(gettextf("k must be between 2 and %d", length(tree$height)),
             domain = NA)

    if(is.null(cluster))
        cluster <- cutree(tree, k=k)
    ## cutree returns classes sorted by data, we need classes
    ## as occurring in the tree (from left to right)
    clustab <- table(cluster)[unique(cluster[tree$order])]
    m <- c(0, cumsum(clustab))

    if(!is.null(x)){
        if(!is.null(which))
            stop("specify exactly one of 'which' and 'x'")
        which <- x
        for(n in seq_along(x))
            which[n] <- max(which(m<x[n]))
    }
    else
        if(is.null(which))
            which <- 1L:k

    if(any(which>k))
        stop(gettextf("all elements of 'which' must be between 1 and %d", k),
             domain = NA)

    border <- rep_len(border, length(which))

    retval <- list()
    for(n in seq_along(which)) {
        rect(m[which[n]]+0.66, par("usr")[3L],
             m[which[n]+1]+0.33, mean(rev(tree$height)[(k-1):k]),
             border = border[n])
        retval[[n]] <- which(cluster==as.integer(names(clustab)[which[n]]))
    }
    invisible(retval)
}

identify.hclust <- function(x, FUN = NULL, N = 20, MAXCLUSTER = 20,
                            DEV.FUN = NULL, ...)
{
    cluster <- cutree(x, k = 2:MAXCLUSTER)

    retval <- list()
    oldk <- NULL
    oldx <- NULL
    DEV.x <- dev.cur()

    for(n in 1L:N){

        dev.set(DEV.x)
        X <- locator(1)
        if(is.null(X))
            break

        k <- min(which(rev(x$height) < X$y), MAXCLUSTER)
        k <- max(k, 2)
        if(!is.null(oldx)){
            rect.hclust(x, k = oldk, x = oldx, cluster = cluster[, oldk-1],
                        border = "grey")
        }
        retval[[n]] <- unlist(rect.hclust(x, k = k, x = X$x,
                                          cluster = cluster[, k-1],
                                          border = "red"))
        if(!is.null(FUN)){
            if(!is.null(DEV.FUN)){
                dev.set(DEV.FUN)
            }
            retval[[n]] <- FUN(retval[[n]], ...)
        }

        oldx <- X$x
        oldk <- k
    }
    dev.set(DEV.x)
    invisible(retval)
}
