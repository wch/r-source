subset.data.frame <- function (x, subset, select, drop = FALSE, ...)
{
    if(missing(subset))
	r <- TRUE
    else {
	e <- substitute(subset)
	r <- eval(e, x, parent.frame())
        if(!is.logical(r)) stop("'subset' must evaluate to logical")
	r <- r & !is.na(r)
    }
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select), nl, parent.frame())
    }
    x[r, vars, drop = drop]
}

subset <- function(x, ...) UseMethod("subset")

subset.default <- function(x, subset, ...) {
    if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset)]
}

subset.matrix <- function(x, subset, select, drop = FALSE, ...)
{
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1:ncol(x))
	names(nl) <- colnames(x)
	vars <- eval(substitute(select), nl, parent.frame())
    }
    if(missing(subset)) subset <- TRUE
    else if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset), vars, drop = drop]
}

transform.data.frame <- function (x, ...)
{
    e <- eval(substitute(list(...)), x, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(x))
    matched <- !is.na(inx)
    if (any(matched)) {
	x[inx[matched]] <- e[matched]
	x <- data.frame(x)
    }
    if (!all(matched))
	data.frame(x, e[!matched])
    else x
}

transform <- function(x,...) UseMethod("transform")

## Actually, I have no idea what to transform(), except dataframes.
## The default converts its argument to a dataframe and transforms
## that. This is probably marginally useful at best. --pd
transform.default <- function(x,...)
    transform.data.frame(data.frame(x),...)

stack.data.frame <- function(x, select, ...)
{
    if (!missing(select)) {
	nl <- as.list(1:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, parent.frame())
        x <- x[, vars, drop=FALSE]
    }
    x <- x[, unlist(lapply(x, is.vector)), drop = FALSE]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep.int(names(x), lapply(x, length))))
}

stack <- function(x, ...) UseMethod("stack")

stack.default <- function(x, ...)
{
    x <- as.list(x)
    x <- x[unlist(lapply(x, is.vector))]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep.int(names(x), lapply(x, length))))
}

unstack.data.frame <- function(x, form = formula(x), ...)
{
    form <- as.formula(form)
    if (length(form) < 3)
        stop("form must be a two-sided formula")
    res <- c(tapply(eval(form[[2]], x), eval(form[[3]], x), as.vector))
    if (length(res) < 2 || any(diff(unlist(lapply(res, length))) != 0))
        return(res)
    data.frame(res)
}

unstack <- function(x, ...) UseMethod("unstack")

unstack.default <- function(x, form, ...)
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
