sort <- function(x, partial=NULL, na.last=NA, decreasing = FALSE,
                 method = c("shell", "quick"), index.return = FALSE)
{
    if(isfact <- is.factor(x)) {
	lev <- levels(x)
	nlev <- nlevels(x)
        x <- c(x)
    } else if(!is.atomic(x))
        stop("`x' must be atomic")
    if(has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <-  x[!ina]
    }
    if(!is.null(partial)) {
        if(!all(is.finite(partial))) stop("non-finite `partial'")
	y <- .Internal(psort(x, partial))
    }
    else {
        nms <- names(x)
        meth <- if(is.null(nms) && is.numeric(x))
            match.arg(method) else "shell"
        ## work around sys.function() {called from match.arg(.)} bug :
        if(is.character(meth) && length(meth)== 1)
            method <- meth
        else { ## Bug happened! -- e.g. example(mosaicplot)
            if(missing(method) || substr(method,1,1)=="q")
                method <- "quick"
            else method <- "shell"
            warning("`meth' is wrong -- bug from match.arg():",
                    meth,"\n setting method to", method)
        }
        switch(method,
               "quick" = {
                   y <- .Internal(qsort(x, index.return))
               },
               "shell" = {
                   if(!is.null(nms)) {
                       o <- sort.list(x, decreasing=decreasing)
                       y <- x[o]
                       names(y) <- nms[o]
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
        ok <- !apply(sapply(z, is.na), 1, any)
        if(all(!ok)) stop("all elements contain an NA")
        z[[1]][!ok] <- NA
        ans <- do.call("order", z)
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
