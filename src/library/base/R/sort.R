sort <- function(x, decreasing = FALSE, ...)
{
    if(!is.logical(decreasing) || length(decreasing) != 1)
        stop("'decreasing' must be a length-1 logical vector.\nDid you intend to set 'partial'?")
    UseMethod("sort")
}

sort.default <- function(x, decreasing = FALSE, na.last = NA, ...)
{
    ## The first case includes factors.
    if(is.object(x)) x[order(x, na.last = na.last, decreasing = decreasing)]
    else sort.int(x, na.last = na.last, decreasing = decreasing, ...)
}

sort.int <-
    function(x, partial = NULL, na.last = NA, decreasing = FALSE,
             method = c("shell", "quick"), index.return = FALSE)
{
    if(isfact <- is.factor(x)) {
        if(index.return) stop("'index.return' only for non-factors")
	lev <- levels(x)
	nlev <- nlevels(x)
 	isord <- is.ordered(x)
        x <- c(x)
    } else if(!is.atomic(x))
        stop("'x' must be atomic")

    if(has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <-  x[!ina]
    }
    if(index.return && !is.na(na.last))
        stop("'index.return' only for 'na.last = NA'")
    if(!is.null(partial)) {
        if(index.return || decreasing || isfact || !missing(method))
	    stop("unsupported options for partial sorting")
        if(!all(is.finite(partial))) stop("non-finite 'partial'")
        y <- if(length(partial) <= 10) {
            partial <- .Internal(qsort(partial, FALSE))
            .Internal(psort(x, partial))
        } else .Internal(qsort(x, FALSE))
    }
    else {
        nms <- names(x)
        method <- if(is.numeric(x)) match.arg(method) else "shell"
        switch(method,
               "quick" = {
                   if(!is.null(nms)) {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, TRUE))
                       if(decreasing) y$x <- -y$x
                       names(y$x) <- nms[y$ix]
                       if (!index.return) y <- y$x
                   } else {
                       if(decreasing) x <- -x
                       y <- .Internal(qsort(x, index.return))
                       if(decreasing)
                           if(index.return) y$x <- -y$x else y <- -y
                   }
               },
               "shell" = {
                   if(index.return || !is.null(nms)) {
                       o <- sort.list(x, decreasing = decreasing)
                       y <- if (index.return) list(x = x[o], ix = o) else x[o]
                       ## names(y) <- nms[o] # pointless!
                   }
                   else
                       y <- .Internal(sort(x, decreasing))
               })
    }
    if(!is.na(na.last) && has.na)
	y <- if(!na.last) c(nas, y) else c(y, nas)
    if(isfact)
        y <- (if (isord) ordered else factor)(y, levels=seq_len(nlev),
                                              labels=lev)
    y
}

order <- function(..., na.last = TRUE, decreasing = FALSE)
{
    if(!is.na(na.last))
        .Internal(order(na.last, decreasing, ...))
    else{ ## remove nas
        z <- list(...)
        if(any(diff(sapply(z, length)) != 0))
            stop("argument lengths differ")
        ans <- sapply(z, is.na)
        if(is.list(ans)) return(integer(0)) # happens for 0-length input
        ok <- if(is.matrix(ans)) !apply(ans, 1, any) else !any(ans)
        if(all(!ok)) return(integer(0))
        z[[1]][!ok] <- NA
        ans <- do.call("order", c(z, decreasing=decreasing))
        keep <- seq_along(ok)[ok]
        ans[ans %in% keep]
    }
}

sort.list <- function(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
                      method = c("shell", "quick", "radix"))
{
    method <- match.arg(method)
    if(!is.atomic(x))
        stop("'x' must be atomic for 'sort.list'\nHave you called 'sort' on a list?")
    if(!is.null(partial))
        .NotYetUsed("partial != NULL")
    if(method == "quick") {
        if(is.factor(x)) x <- as.integer(x) # sort the internal codes
        if(is.numeric(x))
            return(sort(x, na.last = na.last, decreasing = decreasing,
                        method = "quick", index.return = TRUE)$ix)
        else stop("method=\"quick\" is only for numeric 'x'")
    }
    if(method == "radix") {
        if(!typeof(x) == "integer") # do want to allow factors here
            stop("method=\"radix\" is only for integer 'x'")
        if(is.na(na.last))
            return(.Internal(radixsort(x[!is.na(x)], TRUE, decreasing)))
        else
            return(.Internal(radixsort(x, na.last, decreasing)))
    }
    ## method == "shell"
    if(is.na(na.last)) .Internal(order(TRUE, decreasing, x[!is.na(x)]))
    else .Internal(order(na.last, decreasing, x))
}
