subset.data.frame <-
    function (dfr, subset, select)
{
    if(missing(subset))
	r <- TRUE
    else {
	e <- substitute(subset)
	r <- eval(e, dfr, sys.frame(sys.parent()))
	r <- r & !is.na(r)
    }
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1:ncol(dfr))
	names(nl) <- names(dfr)
	vars <- eval(substitute(select),nl, sys.frame(sys.parent()))
    }
    dfr[r,vars,drop=FALSE]
}

subset<-
    function(x,...)
    UseMethod("subset")

subset.default <-
    function(x,subset)
    x[subset & !is.na(subset)]

transform.data.frame <-
    function (dfr, ...)
{
    e <- eval(substitute(list(...)), dfr, sys.frame(sys.parent()))
    tags <- names(e)
    inx <- match(tags, names(dfr))
    matched <- !is.na(inx)
    if (any(matched)) {
	dfr[inx[matched]] <- e[matched]
	dfr <- data.frame(dfr)
    }
    if (!all(matched))
	data.frame(dfr, e[!matched])
    else dfr
}

transform <-
    function(x,...)
    UseMethod("transform")

## Actually, I have no idea what to transform(), except dataframes.
## The default converts its argument to a dataframe and transforms
## that. This is probably marginally useful at best. --pd
transform.default <-
    function(x,...)
    transform.data.frame(data.frame(x),...)

stack.data.frame <-
    function(x, select)
{
    if (!missing(select)) {
	nl <- as.list(1:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, sys.frame(sys.parent()))
        x <- x[, vars, drop=FALSE]
    }
    x <- x[, unlist(lapply(x, is.vector)), drop = FALSE]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep(names(x), lapply(x, length))))
}

stack <-
    function(x, ...)
    UseMethod("stack")

stack.default <-
    function(x, ...)
{
    x <- as.list(x)
    x <- x[unlist(lapply(x, is.vector))]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep(names(x), lapply(x, length))))
}

unstack.data.frame <-
    function(x, form = formula(x))
{
    form <- as.formula(form)
    if (length(form) < 3)
        stop("form must be a two-sided formula")
    res <- c(tapply(eval(form[[2]], x), eval(form[[3]], x), as.vector))
    if (length(res) < 2 || any(diff(unlist(lapply(res, length))) != 0))
        return(res)
    data.frame(res)
}

unstack <-
    function(x, ...)
    UseMethod("unstack")

unstack.default <-
    function(x, form)
{
    x <- as.list(x)
    form <- as.formula(form)
    if (length(form) < 3)
        stop("form must be a two-sided formula")
    res <- c(tapply(eval(form[[2]], x), eval(form[[3]], x), as.vector))
    if (length(res) < 2 || any(diff(unlist(lapply(res, length))) != 0))
        return(res)
    data.frame(res)
}
