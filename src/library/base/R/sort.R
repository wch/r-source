sort <- function(x, partial=NULL, na.last=NA, decreasing = FALSE,
                 method = c("shell", "quick"), index.return = FALSE)
{
    if(isfact <- is.factor(x)) {
        if(index.return) stop("index.return only for non-factors")
	lev <- levels(x)
	nlev <- nlevels(x)
        x <- c(x)
    } else if(!is.atomic(x))
        stop("`x' must be atomic")
    if(has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <-  x[!ina]
    }
    if(index.return && !is.na(na.last))
        stop("index.return only for na.last = NA")
    if(!is.null(partial)) {
        if(!all(is.finite(partial))) stop("non-finite `partial'")
	y <- .Internal(psort(x, partial))
    }
    else {
        nms <- names(x)
        method <- if(is.numeric(x)) match.arg(method) else "shell"
        switch(method,
               "quick" = {
                   if(decreasing)
                       stop("qsort only handles increasing sort")
                   if(!is.null(nms)) {
                       y <- .Internal(qsort(x, TRUE))
                       names(y$x) <- nms[y$ix]
                       if (!index.return) y <- y$x
                   } else
                       y <- .Internal(qsort(x, index.return))
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
        y <- factor(y, levels=1:nlev, labels=lev)
    y
}

order <- function(..., na.last = TRUE, decreasing = FALSE)
{
    if(!is.na(na.last))
        .Internal(order(na.last, decreasing, ...))
    else{ ## remove nas
        z <- list(...)
        if(any(diff(sapply(z, length)) != 0))
            stop("Argument lengths differ")
        ans <- sapply(z, is.na)
        ok <- if(is.matrix(ans)) !apply(ans, 1, any) else !any(ans)
        if(all(!ok)) return(integer(0))
        z[[1]][!ok] <- NA
        ans <- do.call("order", c(z, decreasing=decreasing))
        keep <- seq(along=ok)[ok]
        ans[ans %in% keep]
    }
}

sort.list <- function(x, partial = NULL, na.last = TRUE, decreasing = FALSE)
{
    if(!is.atomic(x))
        stop("`x' must be atomic")
    if(!is.null(partial))
        .NotYetUsed("partial != NULL")
    if(is.na(na.last)) .Internal(order(TRUE, decreasing, x[!is.na(x)]))
    else .Internal(order(na.last, decreasing, x))
}
