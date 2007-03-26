stack <- function(x, ...) UseMethod("stack")

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

stack.default <- function(x, ...)
{
    x <- as.list(x)
    x <- x[unlist(lapply(x, is.vector))]
    data.frame(values = unlist(unname(x)),
               ind = factor(rep.int(names(x), lapply(x, length))))
}

unstack <- function(x, ...) UseMethod("unstack")

unstack.data.frame <- function(x, form, ...)
{
    form <- if(missing(form)) stats::formula(x) else stats::as.formula(form)
    if (length(form) < 3)
        stop("'form' must be a two-sided formula")
    res <- c(tapply(eval(form[[2]], x), eval(form[[3]], x), as.vector))
    if (length(res) < 2 || any(diff(unlist(lapply(res, length))) != 0))
        return(res)
    data.frame(res)
}

unstack.default <- function(x, form, ...)
{
    x <- as.list(x)
    form <- stats::as.formula(form)
    if (length(form) < 3)
        stop("'form' must be a two-sided formula")
    res <- c(tapply(eval(form[[2]], x), eval(form[[3]], x), as.vector))
    if (length(res) < 2 || any(diff(unlist(lapply(res, length))) != 0))
        return(res)
    data.frame(res)
}
