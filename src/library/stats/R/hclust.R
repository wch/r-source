#  File src/library/stats/R/hclust.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

## Hierarchical clustering, on raw input data; we will use Euclidean
## distance.  A range of criteria are supported; also there is a
## storage-economic option.
##
## We use the general routine, `hc', which caters for 7 criteria,
## using a half dissimilarity matrix; (BTW, this uses the very efficient
## nearest neighbor chain algorithm, which makes this algorithm of
## O(n^2) computational time, and differentiates it from the less
## efficient -- i.e. O(n^3) -- implementations in all commercial
## statistical packages -- as far as I am aware -- except Clustan.)
##
## Clustering Methods:
##
## 1. Ward's minimum variance or error sum of squares method (using raw d) -> "ward.D"
## 2. single linkage or nearest neighbor method.
## 3. complete linkage or diameter.
## 4. average linkage, group average, or UPGMA method.
## 5. McQuitty's or WPGMA method.
## 6. median, Gower's or WPGMC method.
## 7. centroid or UPGMC method (7).
## 8. Ward's ... "correct" method using d^2 (in Fortran) -> "ward.D2"
##
## Original author: F. Murtagh, May 1992
## R Modifications: Ross Ihaka, Dec 1996
##		    Friedrich Leisch, Apr 1998, Jun 2000
## "ward.D" and "ward.D2" from suggestions by Pierre Legendre,
## by Martin Maechler, mostly in the Fortran part.

hclust <- function(d, method="complete", members=NULL)
{
    ## order of METHODS --> i.meth -> Fortran's  iOpt  codes
    METHODS <- c("ward.D", "single", # 1, 2,
                 "complete", "average", "mcquitty", # 3, 4, 5,
                 "median", "centroid", "ward.D2") # 6, 7, 8
    if(method == "ward") { # do not deprecate earlier than 2015!
	message("The \"ward\" method has been renamed to \"ward.D\"; note new \"ward.D2\"")
	method <- "ward.D"
    }
    i.meth <-  pmatch(method, METHODS)
    if(is.na(i.meth))
        ## TODO: use gettextf() [-> translation string change]
	stop("invalid clustering method", paste("", method))
    if(i.meth == -1)
	stop("ambiguous clustering method", paste("", method))

    n <- as.integer(attr(d, "Size"))
    if(is.null(n))
	stop("invalid dissimilarities")
    if(is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536")
    if(n < 2)
        stop("must have n >= 2 objects to cluster")
    len <- as.integer(n*(n-1)/2)
    if(length(d) != len)
        (if (length(d) < len) stop else warning
         )("dissimilarities of improper length")

    if(is.null(members))
        members <- rep(1, n)
    else if(length(members) != n)
        stop("invalid length of members")

    storage.mode(d) <- "double"
    hcl <- .Fortran(C_hclust,
		    n = n,
		    len = len,
		    method = as.integer(i.meth),
		    ia = integer(n),
		    ib = integer(n),
		    crit = double(n),
		    members = as.double(members),
		    nn = integer(n),
		    disnn = double(n),
		    flag = logical(n),
		    diss = d)

    ## 2nd step: interpret the information that we now have
    ## as merge, height, and order lists.

    hcass <- .Fortran(C_hcass2,
		      n = n, # checked above.
		      ia = hcl$ia,
		      ib = hcl$ib,
		      order = integer(n),
		      iia = integer(n),
		      iib = integer(n))

    structure(list(merge = cbind(hcass$iia[1L:(n-1)], hcass$iib[1L:(n-1)]),
		   height = hcl$crit[1L:(n-1)],
		   order = hcass$order,
		   labels = attr(d, "Labels"),
		   method = METHODS[i.meth],
		   call = match.call(),
		   dist.method = attr(d, "method")),
	      class = "hclust")
}

plot.hclust <-
    function (x, labels = NULL, hang = 0.1,
              axes = TRUE, frame.plot = FALSE, ann = TRUE,
              main = "Cluster Dendrogram",
              sub = NULL, xlab = NULL, ylab = "Height", ...)
{
    merge <- x$merge
    if (!is.matrix(merge) || ncol(merge) != 2)
	stop("invalid dendrogram")
    ## merge should be integer but might not be after dump/restore.
    if (any(as.integer(merge) != merge))
        stop("'merge' component in dendrogram must be integer")
    storage.mode(merge) <- "integer"
    n <- nrow(merge)
    height <- as.double(x$height)
    labels <-
	if(missing(labels) || is.null(labels)) {
	    if (is.null(x$labels))
		paste(1L:(n+1L))
	    else
		as.character(x$labels)
	} else {
	    if(is.logical(labels) && !labels)# FALSE
		character(n+1L)
	    else
		as.character(labels)
	}

    dev.hold(); on.exit(dev.flush())
    plot.new()
    graphics:::plotHclust(n, merge, height, order(x$order), hang, labels, ...)
    if(axes)
        axis(2, at=pretty(range(height)), ...)
    if (frame.plot)
        box(...)
    if (ann) {
        if(!is.null(cl <- x$call) && is.null(sub))
            sub <- paste0(deparse(cl[[1L]])," (*, \"", x$method,"\")")
        if(is.null(xlab) && !is.null(cl))
            xlab <- deparse(cl[[2L]])
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    invisible()
}



as.hclust <- function(x, ...) UseMethod("as.hclust")
## need *.default for idempotency:
as.hclust.default <- function(x, ...) {
    if(inherits(x, "hclust")) x
    else
	stop(gettextf("argument 'x' cannot be coerced to class %s",
                      dQuote("hclust")),
             if(!is.null(oldClass(x)))
             gettextf("\n Consider providing an as.hclust.%s() method",
                      oldClass(x)[1L]),
             domain = NA)
}

as.hclust.twins <- function(x, ...)
{
    r <- list(merge = x$merge,
	      height = sort(x$height),
	      order = x$order,
	      labels = if(!is.null(lb <- x$order.lab)) {
                  lb[sort.list(x$order)] } else rownames(x$data),# may be NULL
	      call = if(!is.null(cl <- x$call)) cl else match.call(),
	      method = if(!is.null(mt <- x$method)) mt else NA,
	      dist.method = attr(x$diss, "Metric"))
    class(r) <- "hclust"
    r
}

print.hclust <- function(x, ...)
{
    if(!is.null(x$call))
        cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if(!is.null(x$method))
        cat("Cluster method   :", x$method, "\n")
    if(!is.null(x$dist.method))
        cat("Distance         :", x$dist.method, "\n")
    cat("Number of objects:", length(x$height)+1, "\n")
    cat("\n")
    invisible(x)
}

cophenetic <-
function(x)
    UseMethod("cophenetic")
cophenetic.default <-
function(x)
{
    x <- as.hclust(x)
    nobs <- length(x$order)
    ilist <- vector("list", length = nobs)
    out <- matrix(0, nrow = nobs, ncol = nobs)
    for(i in 1 : (nobs - 1)) {
        inds <- x$merge[i,]
        ids1 <- if(inds[1L] < 0L) -inds[1L] else ilist[[inds[1L]]]
        ids2 <- if(inds[2L] < 0L) -inds[2L] else ilist[[inds[2L]]]
        ilist[[i]] <- c(ids1, ids2)
        out[cbind(rep.int(ids1, rep.int(length(ids2), length(ids1))),
                  rep.int(ids2, length(ids1)))] <- x$height[i]
    }
    rownames(out) <- x$labels
    as.dist(out + t(out))
}
cophenetic.dendrogram <-
function(x)
{
    ## Obtain cophenetic distances from a dendrogram by recursively
    ## doing the following:
    ## * if not a leaf, then for all children call ourselves, create
    ##   a block diagonal matrix from this, and fill the rest with the
    ##   current height (as everything in different children is joined
    ##   at the current split) ...
    ## * if a leaf, height and result are 0.
    ## Actually, we need to return something of class "dist", so things
    ## are a bit more complicated, and we might be able to make this
    ## more efficient by avoiding matrices ...
    if(is.leaf(x)) {
        ## If there is no label, we cannot recover the (names of the)
        ## objects the distances are for, and hence abort.
        if(is.null(label <- attr(x, "label")))
            stop("need dendrograms where all leaves have labels")
        return(as.dist(matrix(0, dimnames = list(label, label))))
    }
    children <- vector("list", length(x))
    for(i in seq_along(x))
        children[[i]] <- Recall(x[[i]])
    lens <- sapply(children, attr, "Size")
    m <- matrix(attr(x, "height"), sum(lens), sum(lens))
    ## This seems a bit slower:
    ##    inds <- split(seq(length = sum(lens)),
    ##                  rep.int(seq_along(lens), lens))
    ##    for(i in seq_along(inds))
    ##         m[inds[[i]], inds[[i]]] <- as.matrix(children[[i]])
    hi <- cumsum(lens)
    lo <- c(0L, hi[-length(hi)]) + 1L
    for(i in seq_along(x))
        m[lo[i] : hi[i], lo[i] : hi[i]] <- as.matrix(children[[i]])
    rownames(m) <- colnames(m) <- unlist(lapply(children, attr, "Labels"))
    as.dist(m)
}
