cutree <- function(tree, k=NULL, h=NULL)
{
    if(is.null(n1 <- nrow(tree$merge)) || n1 < 1)
        stop("invalid 'tree' (merge component)")
    n <- n1 + 1
    if(is.null(k) && is.null(h))
        stop("either 'k' or 'h' must be specified")
    if(is.null(k)) {
        if(is.unsorted(tree$height))
            stop("the 'height' component of 'tree' is not sorted\n(increasingly); consider applying as.hclust() first")
        ## h |--> k
        k <- integer(length(h))
        ## S+6 help(cutree) says k(h) = k(h+), but does k(h-) [continuity]
        ## h < min() should give k = n;
        k <- n+1 - apply(outer(c(tree$height,Inf), h, ">"), 2, which.max)
        if(getOption("verbose")) cat("cutree(): k(h) = ",k,"\n")
    }
    else {
        k <- as.integer(k)
        if(min(k) < 1 || max(k) > n)
            stop(gettextf("elements of 'k' must be between 1 and %d", n),
                 domain = NA)
    }

    ans <- .Call("R_cutree", tree$merge, k, PACKAGE = "stats")

    if(length(k) == 1) {
        ans <- as.vector(ans)
        names(ans) <- tree$labels
    }
    else{
        colnames(ans) <- if(!is.null(h)) h else k
        rownames(ans) <- tree$labels
    }
    return(ans)
}
