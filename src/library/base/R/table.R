#  File src/library/base/R/table.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

table <- function (..., exclude = if (useNA=="no") c(NA, NaN),
                   useNA = c("no", "ifany", "always"),
		   dnn = list.names(...), deparse.level = 1)
{
    list.names <- function(...) {
	l <- as.list(substitute(list(...)))[-1L]
	nm <- names(l)
	fixup <- if (is.null(nm)) seq_along(l) else nm == ""
	dep <- vapply(l[fixup], function(x)
		      switch(deparse.level + 1,
			     "", ## 0
			     if (is.symbol(x)) as.character(x) else "", ## 1
			     deparse(x, nlines=1)[1L] ## 2
			     ),
		      "")
	if (is.null(nm))
	    dep
	else {
	    nm[fixup] <- dep
	    nm
	}
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"

    useNA <- match.arg(useNA)
    args <- list(...)
    if (!length(args))
	stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
	args <- args[[1L]]
	if (length(dnn) != length(args))
	    dnn <- if (!is.null(argn <- names(args)))
		 argn
	    else
		 paste(dnn[1L], seq_along(args), sep = ".")
    }
    # 0L, 1L, etc: keep 'bin' and 'pd' integer - as long as tabulate() requires it
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
	if (is.null(lens)) lens <- length(a)
	else if (length(a) != lens)
	    stop("all arguments must have the same length")
        cat <-
            if (is.factor(a)) {
                if (any(is.na(levels(a)))) # Don't touch this!
                    a
                else {
                    ## The logic here is tricky because it tries to do
                    ## something sensible if both 'exclude' and
                    ## 'useNA' are set.
                    ##
                    ## A non-null setting of 'exclude' sets the
                    ## excluded levels to missing, which is different
                    ## from the <NA> factor level. Excluded levels are
                    ## NOT tabulated, even if 'useNA' is set.
                    if (is.null(exclude) && useNA != "no")
                        addNA(a, ifany = (useNA == "ifany"))
                    else {
                        if (useNA != "no")
                            a <- addNA(a, ifany = (useNA == "ifany"))
                        ll <- levels(a)
                        a <- factor(a, levels = ll[!(ll %in% exclude)],
                               exclude = if (useNA == "no") NA)
                    }
                }
            }
            else { # NB: this excludes first, unlike the case above.
                a <- factor(a, exclude = exclude)
                if (useNA != "no")
                    addNA(a, ifany = (useNA == "ifany"))
                else
                    a
            }

	nl <- length(ll <- levels(cat))
	dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max)
            stop("attempt to make a table with >= 2^31 elements")
	dn <- c(dn, list(ll))
	## requiring   all(unique(as.integer(cat)) == 1L:nlevels(cat))  :
	bin <- bin + pd * (as.integer(cat) - 1L)
	pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) bin <- bin + 1L # otherwise, that makes bin NA
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}


## NB: NA in dimnames should be printed.
print.table <-
function (x, digits = getOption("digits"), quote = FALSE, na.print = "",
	  zero.print = "0",
	  justify = "none", ...)
{
    ## tables with empty extents have no contents and are hard to
    ## output in a readable way, so just say something descriptive and
    ## return.
    d <- dim(x)
    if (any(d == 0)) {
        cat ("< table of extent", paste(d, collapse=" x "), ">\n")
        return ( invisible(x) )
    }

    xx <- format(unclass(x), digits = digits, justify = justify)
    ## na.print handled here
    if(any(ina <- is.na(x)))
	xx[ina] <- na.print

    if(zero.print != "0" && any(i0 <- !ina & x == 0))
	## MM thinks this should be an option for many more print methods...
	xx[i0] <- zero.print ## keep it simple;  was sub(..., xx[i0])

    ## Numbers get right-justified by format(), irrespective of 'justify'.
    ## We need to keep column headers aligned.
    if (is.numeric(x) || is.complex(x))
        print(xx, quote = quote, right = TRUE, ...)
    else
        print(xx, quote = quote, ...)
    invisible(x)
}

summary.table <- function(object, ...)
{
    if(!inherits(object, "table"))
	stop(gettextf("'object' must inherit from class %s",
                      dQuote("table")),
             domain = NA)
    n.cases <- sum(object)
    n.vars <- length(dim(object))
    y <- list(n.vars = n.vars,
	      n.cases = n.cases)
    if(n.vars > 1) {
	m <- vector("list", length = n.vars)
	relFreqs <- object / n.cases
	for(k in 1L:n.vars)
	    m[[k]] <- apply(relFreqs, k, sum)
	expected <- apply(do.call("expand.grid", m), 1L, prod) * n.cases
	statistic <- sum((c(object) - expected)^2 / expected)
	lm <- vapply(m, length, 1L)
	parameter <- prod(lm) - 1L - sum(lm - 1L)
	y <- c(y, list(statistic = statistic,
		       parameter = parameter,
		       approx.ok = all(expected >= 5),
		       p.value = stats::pchisq(statistic, parameter, lower.tail=FALSE),
		       call = attr(object, "call")))
    }
    class(y) <- "summary.table"
    y
}

print.summary.table <-
function(x, digits = max(1L, getOption("digits") - 3L), ...)
{
    if(!inherits(x, "summary.table"))
	stop(gettextf("'x' must inherit from class %s",
                      dQuote("summary.table")),
             domain = NA)
    if(!is.null(x$call)) {
	cat("Call: "); print(x$call)
    }
    cat("Number of cases in table:", x$n.cases, "\n")
    cat("Number of factors:", x$n.vars, "\n")
    if(x$n.vars > 1) {
	cat("Test for independence of all factors:\n")
	ch <- x$statistic
	cat("\tChisq = ",	format(round(ch, max(0, digits - log10(ch)))),
	    ", df = ",		x$parameter,
	    ", p-value = ",	format.pval(x$p.value, digits, eps = 0),
	    "\n", sep = "")
	if(!x$approx.ok)
	    cat("\tChi-squared approximation may be incorrect\n")
    }
    invisible(x)
}

as.data.frame.table <-
    function(x, row.names = NULL, ..., responseName = "Freq",
             stringsAsFactors = TRUE, sep="", base = list(LETTERS))
{
    ex <- quote(data.frame(do.call("expand.grid",
				   c(dimnames(provideDimnames(x, sep=sep, base=base)),
				     KEEP.OUT.ATTRS = FALSE,
                                     stringsAsFactors = stringsAsFactors)),
                           Freq = c(x),
                           row.names = row.names))
    names(ex)[3L] <- responseName
    eval(ex)
}

is.table <- function(x) inherits(x, "table")
as.table <- function(x, ...) UseMethod("as.table")
as.table.default <- function(x, ...)
{
    if(is.table(x)) return(x)
    else if(is.array(x) || is.numeric(x)) {
	x <- as.array(x)
	if(any(dim(x) == 0L)) stop("cannot coerce to a table")
	structure(class = c("table", oldClass(x)), provideDimnames(x))
    } else stop("cannot coerce to a table")
}

prop.table <- function(x, margin = NULL)
{
    if(length(margin))
	sweep(x, margin, margin.table(x, margin), "/", check.margin=FALSE)
    else
	x / sum(x)
}

margin.table <- function(x, margin = NULL)
{
    if(!is.array(x)) stop("'x' is not an array")
    if (length(margin)) {
	z <- apply(x, margin, sum)
	dim(z) <- dim(x)[margin]
	dimnames(z) <- dimnames(x)[margin]
    }
    else return(sum(x))
    class(z) <- oldClass(x) # avoid adding "matrix"
    z
}
