table <- function (..., exclude = c(NA, NaN),
   dnn = list.names(...), deparse.level = 1)
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq(along = l)
        else nm == ""
        dep <- sapply(l[fixup], function(x)
	    switch (deparse.level + 1,
		"",
		if (is.symbol(x)) as.character(x) else "",
		deparse(x)[1]
	    )
        )
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }

    args <- list(...)
    if (length(args) == 0)
	stop("nothing to tabulate")
    if (length(args) == 1 && is.list(args[[1]])) {
	args <- args[[1]]
	if (length(dnn) != length(args))
	    dnn <- if (!is.null(argn <- names(args)))
	         argn
	    else
                 paste(dnn[1],1:length(args),sep='.')
    }
    bin <- 0
    lens <- NULL
    dims <- integer(0)
    pd <- 1
    dn <- NULL
    for (a in args) {
	if (is.null(lens)) lens <- length(a)
	else if (length(a) != lens)
	    stop("all arguments must have the same length")
	if (is.factor(a))
	    cat <- a
	else
	    cat <- factor(a, exclude = exclude)
	nl <- length(l <- levels(cat))
	dims <- c(dims, nl)
	dn <- c(dn, list(l))
	## requiring   all(unique(as.integer(cat)) == 1:nlevels(cat))  :
	bin <- bin + pd * (as.integer(cat) - 1)
	pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) bin <- bin + 1 # otherwise, that makes bin NA
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}

print.table <- function(x, digits = getOption("digits"), quote = FALSE,
                        na.print = "", ...)
{
    print.default(unclass(x), digits = digits, quote = quote,
                  na.print = na.print, ...)
}


summary.table <- function(object, ...)
{
    if(!inherits(object, "table"))
	stop("object must inherit from class table")
    n.cases <- sum(object)
    n.vars <- length(dim(object))
    y <- list(n.vars = n.vars,
	      n.cases = n.cases)
    if(n.vars > 1) {
        m <- vector("list", length = n.vars)
        for(k in seq(along = m)) {
            m[[k]] <- apply(object, k, sum) / n.cases
        }
        expected <- apply(do.call("expand.grid", m), 1, prod) * n.cases
        statistic <- sum((c(object) - expected)^2 / expected)
        parameter <- prod(sapply(m, length) - 1)
        y <- c(y, list(statistic = statistic,
                       parameter = parameter,
                       approx.ok = all(expected >= 5),
                       p.value = pchisq(statistic, parameter,
                       lower.tail = FALSE),
                       call = attr(object, "call")))
    }
    class(y) <- "summary.table"
    y
}

print.summary.table <-
function(x, digits = max(1, getOption("digits") - 3), ...)
{
    if(!inherits(x, "summary.table"))
	stop("x must inherit from class `summary.table'")
    if(!is.null(x$call)) {
        cat("Call: "); print(x$call)
    }
    cat("Number of cases in table:", x$n.cases, "\n")
    cat("Number of factors:", x$n.vars, "\n")
    if(x$n.vars > 1) {
        cat("Test for independence of all factors:\n")
        ch <- x$statistic
        cat("\tChisq = ",
            format(round(ch, max(0, digits - log10(ch)))),
            ", df = ",
            x$parameter,
            ", p-value = ",
            format.pval(x$p.value, digits, eps = 0),
            "\n", sep = "")
        if(!x$approx.ok)
            cat("\tChi-squared approximation may be incorrect\n")
    }
    invisible(x)
}

as.data.frame.table <- function(x, row.names = NULL, optional = FALSE, ...)
{
    x <- as.table(x)
    data.frame(do.call("expand.grid", dimnames(x)), Freq = c(x),
               row.names = row.names)
}

is.table <- function(x) inherits(x, "table")
as.table <- function(x, ...) UseMethod("as.table")
as.table.default <- function(x, ...)
{
    if(is.table(x))
        return(x)
    else if(is.array(x) || is.numeric(x)) {
        x <- as.array(x)
        if(any(dim(x) == 0))
            stop("cannot coerce into a table")
        ## Try providing dimnames where missing.
        dnx <- dimnames(x)
        if(is.null(dnx))
            dnx <- vector("list", length(dim(x)))
        for(i in which(sapply(dnx, is.null)))
            dnx[[i]] <- LETTERS[seq(length = dim(x)[i])]
        dimnames(x) <- dnx
        class(x) <- c("table", class(x))
        return(x)
    }
    else
        stop("cannot coerce into a table")
}

prop.table <- function(x, margin = NULL)
{
    if(length(margin))
        sweep(x, margin, margin.table(x, margin), "/")
    else
    	x / sum(x)
}

margin.table <- function(x, margin = NULL)
{
    if(!is.array(x)) stop("x is not an array")
    if (length(margin)) {
        z <- apply(x, margin, sum)
        dim(z) <- dim(x)[margin]
        dimnames(z) <- dimnames(x)[margin]
    }
    else return(sum(x))
    class(z) <- class(x)
    z
}
