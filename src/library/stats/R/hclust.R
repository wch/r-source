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
## 1. Ward's minimum variance or error sum of squares method.
## 2. single linkage or nearest neighbor method.
## 3. complete linkage or diameter.
## 4. average linkage, group average, or UPGMA method.
## 5. McQuitty's or WPGMA method.
## 6. median, Gower's or WPGMC method.
## 7. centroid or UPGMC method (7).
##
## Original author: F. Murtagh, May 1992
## R Modifications: Ross Ihaka, Dec 1996
##		    Friedrich Leisch, Apr 1998, Jun 2000

hclust <- function(d, method="complete", members=NULL)
{
    METHODS <- c("ward", "single",
                 "complete", "average", "mcquitty",
                 "median", "centroid")
    method <-  pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid clustering method")
    if(method == -1)
	stop("ambiguous clustering method")

    n <- as.integer(attr(d, "Size"))
    if(is.null(n))
	stop("invalid dissimilarities")
    if(n < 2)
        stop("Must have n >= 2 objects to cluster")
    len <- as.integer(n*(n-1)/2)
    if(length(d) != len)
        (if (length(d) < len) stop else warning
         )("dissimilarities of improper length")

    if(is.null(members))
        members <- rep(1, n)
    else if(length(members) != n)
        stop("Invalid length of members")

    hcl <- .Fortran("hclust",
		    n = n,
		    len = len,
		    method = as.integer(method),
		    ia = integer(n),
		    ib = integer(n),
		    crit = double(n),
		    members = as.double(members),
		    nn = integer(n),
		    disnn = double(n),
		    flag = logical(n),
		    diss = as.double(d), PACKAGE="stats")

    ## 2nd step: interpret the information that we now have
    ## as merge, height, and order lists.

    hcass <- .Fortran("hcass2",
		      n = as.integer(n),
		      ia = as.integer(hcl$ia),
		      ib = as.integer(hcl$ib),
  		      order = integer(n),
		      iia = integer(n),
		      iib = integer(n), PACKAGE="stats")

    tree <- list(merge = cbind(hcass$iia[1:(n-1)], hcass$iib[1:(n-1)]),
		 height= hcl$crit[1:(n-1)],
		 order = hcass$order,
		 labels=attr(d, "Labels"),
                 method=METHODS[method],
                 call = match.call(),
                 dist.method = attr(d, "method"))
    class(tree) <- "hclust"
    tree
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
        stop("merge component in dendrogram must be integer")
    storage.mode(merge) <- "integer"
    n <- nrow(merge)
    height <- as.double(x$height)
    labels <-
	if(missing(labels) || is.null(labels)) {
	    if (is.null(x$labels))
		paste(1:(n+1))
	    else
		as.character(x$labels)
	} else {
	    if(is.logical(labels) && !labels)# FALSE
		character(n+1)
	    else
		as.character(labels)
	}

    plot.new()
    .Internal(dend.window(n, merge, height,                 hang, labels, ...))
    .Internal(dend       (n, merge, height, order(x$order), hang, labels, ...))
    if(axes)
        axis(2, at=pretty(range(height)))
    if (frame.plot)
        box(...)
    if (ann) {
        if(!is.null(cl <- x$call) && is.null(sub))
            sub <- paste(deparse(cl[[1]])," (*, \"", x$method,"\")",sep="")
        if(is.null(xlab) && !is.null(cl))
            xlab <- deparse(cl[[2]])
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    invisible()
}

## For S ``compatibility'': was in cluster just as
## plclust <- plot.hclust ## .Alias
plclust <- function(tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0,
                    square = TRUE, labels = NULL, plot. = TRUE,
                    axes = TRUE, frame.plot = FALSE, ann = TRUE,
                    main = "", sub = NULL, xlab = NULL, ylab = "Height")
{
    if(!missing(level) && level)	.NotYetUsed("level", error = FALSE)
    if(!missing(hmin) && hmin != 0)	.NotYetUsed("hmin",  error = FALSE)
    if(!missing(square) && !square)	.NotYetUsed("square",error = FALSE)
    if(!missing(plot.) && !plot.)	.NotYetUsed("plot.", error = TRUE)
    if(!missing(hmin)) tree$height <- pmax(tree$height, hmin)
    if(unit) tree$height <- rank(tree$height)
    plot.hclust(x = tree, labels = labels, hang = hang,
                axes = axes, frame.plot = frame.plot, ann = ann,
                main = main, sub = sub, xlab = xlab, ylab = ylab)
}


as.hclust <- function(x, ...) UseMethod("as.hclust")
## need *.default for idempotency:
as.hclust.default <- function(x, ...) {
    if(inherits(x, "hclust")) x
    else
	stop(paste("argument", sQuote("x"),
                   "cannot be coerced to class",
                   dQuote("hclust")),
             if(!is.null(oldClass(x)))
             "\n Consider providing an as.hclust.",oldClass(x)[1],"() method")
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
        cat("\nCall:\n",deparse(x$call),"\n\n",sep="")
    if(!is.null(x$method))
        cat("Cluster method   :", x$method, "\n")
    if(!is.null(x$dist.method))
        cat("Distance         :", x$dist.method, "\n")
    cat("Number of objects:", length(x$height)+1, "\n")
    cat("\n")
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
    out <- matrix(0, nr = nobs, nc = nobs)
    for(i in 1 : (nobs - 1)) {
        inds <- x$merge[i,]
        ids1 <- if(inds[1] < 0) -inds[1] else ilist[[inds[1]]]
        ids2 <- if(inds[2] < 0) -inds[2] else ilist[[inds[2]]]
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
            stop("Need dendrograms where all leaves have labels.")
        return(as.dist(matrix(0, dimnames = list(label, label))))
    }
    children <- vector("list", length(x))
    for(i in seq(along = x))
        children[[i]] <- Recall(x[[i]])
    lens <- sapply(children, attr, "Size")
    m <- matrix(attr(x, "height"), sum(lens), sum(lens))
    ## This seems a bit slower:
    ##    inds <- split(seq(length = sum(lens)),
    ##                  rep.int(seq(along = lens), lens))
    ##    for(i in seq(along = inds))
    ##         m[inds[[i]], inds[[i]]] <- as.matrix(children[[i]])
    hi <- cumsum(lens)
    lo <- c(0, hi[-length(hi)]) + 1
    for(i in seq(along = x))
        m[lo[i] : hi[i], lo[i] : hi[i]] <- as.matrix(children[[i]])
    rownames(m) <- colnames(m) <-
        unlist(sapply(children, attr, "Labels"))
    as.dist(m)
}
