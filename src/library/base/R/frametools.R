subset.data.frame <-
    function (x, subset, select, ...)
{
    if(missing(subset))
	r <- TRUE
    else {
	e <- substitute(subset)
	r <- eval(e, x, parent.frame())
	r <- r & !is.na(r)
    }
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, parent.frame())
    }
    x[r,vars,drop=FALSE]
}

subset<-
    function(x, ...)
    UseMethod("subset")

subset.default <-
    function(x, subset, ...)
    x[subset & !is.na(subset)]

transform.data.frame <-
    function (x, ...)
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
    function(x, select, ...)
{
    if (!missing(select)) {
	nl <- as.list(1:ncol(x))
	names(nl) <- names(x)
	vars <- eval(substitute(select),nl, parent.frame())
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
    function(x, form = formula(x), ...)
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
    function(x, form, ...)
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
reshapeLong <-
    function(x,
             jvars,
             ilev = row.names(x),
             jlev = names(x)[jvars],
             iname = "reshape.i",
             jname = "reshape.j",
             vname = "reshape.v")
{
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    jvars <- eval(substitute(jvars), nl, parent.frame())
    n <- nrow(x)
    k <- length(jvars)
    if (k == 0) stop("no j variables")
    t1 <- x[,-jvars,drop=FALSE]
    t2 <- as.matrix(x[,jvars])
    i <- gl(n, k, labels = ilev)
    j <- gl(k, 1, length = n*k, labels = jlev)
    t2 <- data.frame(foo=i, bar=j, baz=as.vector(t(t2)))
    names(t2) <- c(iname, jname, vname)
    if(ncol(t1)==0) return(t2)
    t1 <- t1[i,,drop=FALSE]
    rownames(t1) <- 1:(n*k)
    cbind(t1, t2)
}

reshapeWide <-
    function(x,
             i = reshape.i,
             j = reshape.j,
             val = reshape.v,
             jnames = levels(j))
{
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    ijv <- eval(substitute(c(i,j,val)), nl, parent.frame())
    i <- eval(substitute(as.factor(i)), envir=x)
    j <- eval(substitute(as.factor(j)), envir=x)
    val <- eval(substitute(val), envir=x)
    if (any(table(i,j) != 1)) stop("data frame cannot be reshaped")
    xr <- x[,-ijv,drop=FALSE]
    resp <- tapply(val,list(i,j),as.vector)
    resp <- as.data.frame(resp)
    names(resp) <- jnames
    if (ncol(xr) == 0) return(resp)
    reduced <- xr[as.numeric(j)==1,,drop=FALSE]
    cbind(reduced,resp)
}
