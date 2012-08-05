#  File src/library/stats/R/kmeans.R
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

kmeans <-
function(x, centers, iter.max = 10, nstart = 1,
         algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
{
    do_one <- function(nmeth) {
        Z <-
            switch(nmeth,
                   { # 1
                       Z <- .Fortran(C_kmns, x, m, p,
                                centers = centers,
                                as.integer(k), c1 = integer(m), integer(m),
                                nc = integer(k), double(k), double(k), integer(k),
                                double(m), integer(k), integer(k),
                                iter.max, wss = double(k),
                                ifault = 0L)
                       switch(Z$ifault,
                              stop("empty cluster: try a better set of initial centers",
                                   call.=FALSE),
                              warning(gettextf("did not converge in %d iterations",
                                               iter.max), call.=FALSE, domain =NA),
                              stop("number of cluster centres must lie between 1 and nrow(x)",
                                   call.=FALSE)
                              )
                       Z
                   },
                   { # 2
                       Z <- .C(C_kmeans_Lloyd, x, m, p,
                               centers = centers, k,
                               c1 = integer(m), iter = iter.max,
                               nc = integer(k), wss = double(k))
                       if(Z$iter > iter.max)
                           warning("did not converge in ",
                                  iter.max, " iterations", call.=FALSE)
                       if(any(Z$nc == 0))
                           warning("empty cluster: try a better set of initial centers", call.=FALSE)
                       Z
                   },
                   { # 3
                       Z <- .C(C_kmeans_MacQueen, x, m, p,
                               centers = as.double(centers), k,
                               c1 = integer(m), iter = iter.max,
                               nc = integer(k), wss = double(k))
                       if(Z$iter > iter.max)
                           warning("did not converge in ",
                                   iter.max, " iterations", call.=FALSE)
                       if(any(Z$nc == 0))
                           warning("empty cluster: try a better set of initial centers", call.=FALSE)
                       Z
                    })
        Z
    }
    x <- as.matrix(x)
    m <- as.integer(nrow(x))
    if(is.na(m)) stop("invalid nrow(x)")
    p <- as.integer(ncol(x))
    if(is.na(p)) stop("invalid ncol(x)")
    if(missing(centers))
	stop("'centers' must be a number or a matrix")
    nmeth <- switch(match.arg(algorithm),
                    "Hartigan-Wong" = 1,
                    "Lloyd" = 2, "Forgy" = 2,
                    "MacQueen" = 3)
    if(length(centers) == 1L) {
	if (centers == 1) nmeth <- 3
	k <- centers
        ## we need to avoid duplicates here
        if(nstart == 1)
            centers <- x[sample.int(m, k), , drop = FALSE]
        if(nstart >= 2 || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if(mm < k)
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample.int(mm, k), , drop=FALSE]
        }
    } else {
	centers <- as.matrix(centers)
        if(any(duplicated(centers)))
            stop("initial centers are not distinct")
        cn <- NULL
	k <- nrow(centers)
        if(m < k)
            stop("more cluster centers than data points")
    }
    k <- as.integer(k)
    if(is.na(k)) stop("'invalid value of 'k'")
    iter.max <- as.integer(iter.max)
    if(is.na(iter.max) || iter.max < 1) stop("'iter.max' must be positive")
    if(ncol(x) != ncol(centers))
	stop("must have same number of columns in 'x' and 'centers'")
    if(!is.double(x)) storage.mode(x) <- "double"
    if(!is.double(centers)) storage.mode(centers) <- "double"
    Z <- do_one(nmeth)
    best <- sum(Z$wss)
    if(nstart >= 2 && !is.null(cn))
	for(i in 2:nstart) {
	    centers <- cn[sample.int(mm, k), , drop=FALSE]
	    ZZ <- do_one(nmeth)
	    if((z <- sum(ZZ$wss)) < best) {
		Z <- ZZ
		best <- z
	    }
	}
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1L:k, dimnames(x)[[2L]])
    cluster <- Z$c1
    if(!is.null(rn <- rownames(x)))
        names(cluster) <- rn
    totss <- sum(scale(x, scale = FALSE)^2)
    structure(list(cluster = cluster, centers = centers, totss = totss,
                   withinss = Z$wss, tot.withinss = best,
                   betweenss = totss - best, size = Z$nc),
	      class = "kmeans")
}

## modelled on print methods in the cluster package
print.kmeans <- function(x, ...)
{
    cat("K-means clustering with ", length(x$size), " clusters of sizes ",
        paste(x$size, collapse=", "), "\n", sep="")
    cat("\nCluster means:\n")
    print(x$centers, ...)
    cat("\nClustering vector:\n")
    print(x$cluster, ...)
    cat("\nWithin cluster sum of squares by cluster:\n")
    print(x$withinss, ...)
    cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n",
		100 * x$betweenss/x$totss),
	"Available components:\n", sep="\n")
    print(names(x))
    invisible(x)
}

fitted.kmeans <- function(object, method = c("centers", "classes"), ...)
{
	method <- match.arg(method)
	if (method == "centers") object$centers[object$cl, , drop=FALSE]
	else object$cl
}

