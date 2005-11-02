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

Vectorize <- function(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, 
		USE.NAMES = TRUE) {
    arg.names <- as.list(formals(FUN))
    arg.names[["..."]] <- NULL
    arg.names <- names(arg.names)

    vectorize.args <- as.character(vectorize.args)

    if (!length(vectorize.args)) return(FUN)

    if (!all(vectorize.args %in% arg.names)) 
    	stop("must specify formal argument names to vectorize")

    FUNV <- function() { # will set the formals below
        args <- lapply(as.list(match.call())[-1], eval, parent.frame())
        names <- if(is.null(names(args))) character(length(args)) 
        	 else names(args)
        dovec <- names %in% vectorize.args
        do.call("mapply", c(FUN = FUN,
                            args[dovec],
                            MoreArgs = list(args[!dovec]),
                            SIMPLIFY = SIMPLIFY,
                            USE.NAMES = USE.NAMES))
    }
    formals(FUNV) <- formals(FUN)
    FUNV
}