kmeans <-
function(x, centers, iter.max = 10, nstart = 1,
         algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
{
    do_one <- function(nmeth) {
        Z <-
            switch(nmeth,
                   { # 1
                       Z <- .Fortran("kmns", as.double(x), as.integer(m),
                                as.integer(ncol(x)),
                                centers = as.double(centers),
                                as.integer(k), c1 = integer(m), integer(m),
                                nc = integer(k), double(k), double(k), integer(k),
                                double(m), integer(k), integer(k),
                                as.integer(iter.max), wss = double(k),
                                ifault = as.integer(0), PACKAGE="stats")
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
                       Z <- .C("kmeans_Lloyd", as.double(x), as.integer(m),
                               as.integer(ncol(x)),
                               centers = as.double(centers), as.integer(k),
                               c1 = integer(m), iter = as.integer(iter.max),
                               nc = integer(k), wss = double(k),
                               PACKAGE="stats")
                       if(Z$iter > iter.max)
                           warning("did not converge in ",
                                  iter.max, " iterations", call.=FALSE)
                       if(any(Z$nc == 0))
                           warning("empty cluster: try a better set of initial centers", call.=FALSE)
                       Z
                   },
                   { # 3
                       Z <- .C("kmeans_MacQueen", as.double(x), as.integer(m),
                               as.integer(ncol(x)),
                               centers = as.double(centers), as.integer(k),
                               c1 = integer(m), iter = as.integer(iter.max),
                               nc = integer(k), wss = double(k),
                               PACKAGE="stats")
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
    m <- nrow(x)
    if(missing(centers))
	stop("'centers' must be a number or a matrix")
    algorithm <- match.arg(algorithm)
    nmeth <- switch(algorithm,
                    "Hartigan-Wong" = 1,
                    "Lloyd" = 2, "Forgy" = 2,
                    "MacQueen" = 3)
    if(length(centers) == 1) {
	k <- centers
        ## we need to avoid duplicates here
        if(nstart == 1)
            centers <- x[sample(1 : m, k), , drop = FALSE]
        if(nstart >= 2 || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if(mm < k)
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample(1:mm, k), , drop=FALSE]
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
    if(iter.max < 1) stop("'iter.max' must be positive")
    if(ncol(x) != ncol(centers))
	stop("must have same number of columns in 'x' and 'centers'")
    Z <- do_one(nmeth)
    if(nstart >= 2 && !is.null(cn)) {
        best <- sum(Z$wss)
        for(i in 2:nstart) {
            centers <- cn[sample(1:mm, k), , drop=FALSE]
            ZZ <- do_one(nmeth)
            if((z <- sum(ZZ$wss)) < best) {
                Z <- ZZ
                best <- z
            }
        }
    }
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1:k, dimnames(x)[[2]])
    cluster <- Z$c1
    if(!is.null(rn <- rownames(x)))
        names(cluster) <- rn
    out <- list(cluster = cluster, centers = centers, withinss = Z$wss,
                size = Z$nc)
    class(out) <- "kmeans"
    out
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
    cat("\nAvailable components:\n")
    print(names(x))
    invisible(x)
}
