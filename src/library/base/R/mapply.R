mapply<-function(FUN,..., MoreArgs=NULL, SIMPLIFY=TRUE, USE.NAMES=TRUE)
{
    FUN <- match.fun(FUN)
    dots <- list(...)

    answer<-.Call("do_mapply", FUN, dots, MoreArgs, environment(),
                  PACKAGE="base")

    if (USE.NAMES && length(dots) && is.character(dots[[1]]) &&
        is.null(names(answer))) names(answer) <- dots[[1]]
    if (SIMPLIFY && length(answer) &&
        length(common.len <- unique(unlist(lapply(answer, length)))) == 1) {
        if (common.len == 1)
            unlist(answer, recursive = FALSE)
        else if (common.len > 1)
            array(unlist(answer, recursive = FALSE),
                  dim = c(common.len, max(sapply(dots,length))),
                  dimnames = list(names(answer[[1]]), names(answer)))
        else answer
    }
    else answer
}

