#  File src/library/base/R/dataframe.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

# Statlib code by John Chambers, Bell Labs, 1994
# Changes Copyright (C) 1998-2019 The R Core Team


## As from R 2.4.0, row.names can be either character or integer.
## row.names() will always return character.
## attr(, "row.names") will return either character or integer.
##
## Do not assume that the internal representation is either, since
## 1L:n is stored as the integer vector c(NA, n) to save space (and
## the C-level code to get/set the attribute makes the appropriate
## translations.
##
## As from 2.5.0 c(NA, n > 0) indicates deliberately assigned row names,
## and c(NA, n < 0) automatic row names.

## We cannot allow long vectors as elements until we can handle
## duplication of row names.

.row_names_info <- function(x, type = 1L)
    .Internal(shortRowNames(x, type))

row.names <- function(x) UseMethod("row.names")
row.names.data.frame <- function(x) as.character(attr(x, "row.names"))
row.names.default <- function(x) if(!is.null(dim(x))) rownames(x)# else NULL

.set_row_names <- function(n)
    if(n > 0) c(NA_integer_, -n) else integer()


##_H Hack around the fact that other packages fail with a newly improved `row.names<-`:
##_H
##_H `row.names<-` <- function(x, make.names = FALSE, value) UseMethod("row.names<-")
`row.names<-` <- function(x, value) UseMethod("row.names<-")

##_H `row.names<-.data.frame` <-
`.rowNamesDF<-` <- function(x, make.names = FALSE, value)
{
    if (!is.data.frame(x)) x <- as.data.frame(x)
    n <- .row_names_info(x, 2L)
    if(is.null(value)) { # set automatic row.names
        attr(x, "row.names") <- .set_row_names(n)
        return(x)
    }
    ## do this here, as e.g. POSIXlt changes length when coerced.
    if( is.object(value) || !is.integer(value) )
        value <- as.character(value)
    if(n == 0L) {
        ## we have to be careful here.  This could be a
        ## 0-row data frame or an invalid one being constructed.
        if(!is.null(attr(x, "row.names")) && length(value) > 0L)
           stop("invalid 'row.names' length")
    }
    else if (length(value) != n) {
	if(isFALSE(make.names)) stop("invalid 'row.names' length")
        else if(is.na(make.names)) { # automatic row.names
            attr(x, "row.names") <- .set_row_names(n)
            return(x)
        }
        else if(!isTRUE(make.names)) stop("invalid 'make.names'")
        ## else  make.names = TRUE: amend 'value' to correct ones:
        else if((nv <- length(value)) < n)
            value <- c(value, rep_len(value[nv], n-nv))
        else # length(value) > n
            value <- value[seq_len(n)]
    }
    if (anyDuplicated(value)) {
        if(isFALSE(make.names)) {
            nonuniq <- sort(unique(value[duplicated(value)]))
            ## warning + stop ?? FIXME: s/warning/stop/ and drop (2nd) stop ??
            warning(ngettext(length(nonuniq),
                             sprintf("non-unique value when setting 'row.names': %s",
                                     sQuote(nonuniq[1L])),
                             sprintf("non-unique values when setting 'row.names': %s",
                                     paste(sQuote(nonuniq), collapse = ", "))),
                domain = NA, call. = FALSE)
            stop("duplicate 'row.names' are not allowed")
        }
        else if(is.na(make.names)) { # automatic row.names
            value <- .set_row_names( # find nrow(.) in case 'n' is not usable:
                if(n == 0L && is.null(.row_names_info(x, 0L)) && length(x) > 0L)
                    length(x[[1L]])
                else n)
        }
        else if(!isTRUE(make.names)) stop("invalid 'make.names'")
        else # make.names = TRUE: amend 'value' to correct ones:
            value <- make.names(value, unique=TRUE)
        ## NB: 'value' is now guaranteed to have no NA's ==> can use 'else if' :
    }
    else if (anyNA(value)) {
        if(isFALSE(make.names))
            stop("missing values in 'row.names' are not allowed")
        if(is.na(make.names)) # automatic row.names
            value <- .set_row_names(n)
        else if(!isTRUE(make.names)) stop("invalid 'make.names'")
        else # make.names = TRUE: amend 'value' to correct ones:
            value <- make.names(value, unique=TRUE)
    }
    attr(x, "row.names") <- value
    x
}

`row.names<-.data.frame` <- function(x, value) `.rowNamesDF<-`(x, value=value)

##_H `row.names<-.default` <- function(x, ..., value) `rownames<-`(x, value)
`row.names<-.default` <- function(x, value) `rownames<-`(x, value)

is.na.data.frame <- function (x)
{
    ## need to special-case no columns
    y <- if (length(x)) {
        do.call("cbind", lapply(x, "is.na")) # gives a matrix
    } else matrix(FALSE, length(row.names(x)), 0)
    if(.row_names_info(x) > 0L) rownames(y) <- row.names(x)
    y
}

## not needed, as  anyNA() works recursively on list()s :
## anyNA.data.frame <- function(x) any(vapply(x, anyNA, NA, USE.NAMES=FALSE))

is.data.frame <- function(x) inherits(x, "data.frame")

I <- function(x) { structure(x, class = unique(c("AsIs", oldClass(x)))) }

print.AsIs <- function (x, ...)
{
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    NextMethod("print")
    invisible(x)
}


t.data.frame <- function(x)
{
    x <- as.matrix(x)
    NextMethod("t")
}

dim.data.frame <- function(x) c(.row_names_info(x, 2L), length(x))

dimnames.data.frame <- function(x) list(row.names(x), names(x))

`dimnames<-.data.frame` <- function(x, value)
{
    d <- dim(x)
    if(!is.list(value) || length(value) != 2L)
	stop("invalid 'dimnames' given for data frame")
    ## do the coercion first, as might change length
    value[[1L]] <- as.character(value[[1L]])
    value[[2L]] <- as.character(value[[2L]])
    if(d[[1L]] != length(value[[1L]]) || d[[2L]] != length(value[[2L]]))
	stop("invalid 'dimnames' given for data frame")
    row.names(x) <- value[[1L]] # checks validity
    names(x) <- value[[2L]]
    x
}

as.data.frame <- function(x, row.names = NULL, optional = FALSE, ...)
{
    if(is.null(x))			# can't assign class to NULL
	return(as.data.frame(list()))
    UseMethod("as.data.frame")
}

as.data.frame.default <- function(x, ...)
    stop(gettextf("cannot coerce class %s to a data.frame",
                  sQuote(deparse(class(x))[1L])),
         domain = NA)

###  Here are methods ensuring that the arguments to "data.frame"
###  are in a form suitable for combining into a data frame.

as.data.frame.data.frame <- function(x, row.names = NULL, ...)
{
    cl <- oldClass(x)
    i <- match("data.frame", cl)
    if(i > 1L)
	class(x) <- cl[ - (1L:(i-1L))]
    if(!is.null(row.names)){
        nr <- .row_names_info(x, 2L)
	if(length(row.names) == nr)
	    attr(x, "row.names") <- row.names
	else
            stop(sprintf(ngettext(nr,
                                  "invalid 'row.names', length %d for a data frame with %d row",
                                  "invalid 'row.names', length %d for a data frame with %d rows"),
                         length(row.names), nr), domain = NA)
    }
    x
}

## prior to 1.8.0 this coerced names - PR#3280
as.data.frame.list <-
    function(x, row.names = NULL, optional = FALSE, ...,
	     cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
             stringsAsFactors = default.stringsAsFactors())
{
    ## need to protect names in x.
    ## truncate any of more than 256 (or cut.names) bytes:
    new.nms <- !missing(col.names)
    if(cut.names) {
	maxL <- if(is.logical(cut.names)) 256L else as.integer(cut.names)
	if(any(long <- nchar(col.names, "bytes", keepNA = FALSE) > maxL))
	    col.names[long] <- paste(substr(col.names[long], 1L, maxL - 6L), "...")
	else cut.names <- FALSE
    }
    m <- match(names(formals(data.frame))[-1L],
	       ## c("row.names", "check.rows", ...., "stringsAsFactors"),
	       col.names, 0L)
    if(any.m <- any(m)) col.names[m] <- paste0("..adfl.", col.names[m])
    if(new.nms || any.m || cut.names) names(x) <- col.names
    if(is.null(check.n <- list(...)$check.names)) check.n <- !optional
    ## data.frame() is picky with its 'row.names':
    alis <- c(list(check.names = check.n, fix.empty.names = fix.empty.names,
		   stringsAsFactors = stringsAsFactors),
	      if(!is.null(row.names)) list(row.names = row.names))
    x <- do.call(data.frame, c(x, alis))
    if(any.m) names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
    x
}

as.data.frame.vector <- function(x, row.names = NULL, optional = FALSE, ...,
				 nm = paste(deparse(substitute(x),
						    width.cutoff = 500L),
					    collapse = " "))
{
    force(nm)
    nrows <- length(x)
    ## ## row.names -- for now warn about and "forget" illegal row.names
    ## ##           -- can simplify much (move this *after* the is.null(.) case) once we stop() !
### FIXME: allow  integer [of full length]
    if(!(is.null(row.names) || (is.character(row.names) && length(row.names) == nrows))) {
	warning(gettextf(
	    "'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
	    nrows), domain = NA)
	row.names <- NULL
    }
    if(is.null(row.names)) {
	if (nrows == 0L)
	    row.names <- character()
	else if(length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
	    row.names <- .set_row_names(nrows)
    }
    ## else if(length(row.names) != nrows) # same behavior as the 'matrix' method
    ##     row.names <- .set_row_names(nrows)
    if(!is.null(names(x))) names(x) <- NULL # remove names as from 2.0.0
    value <- list(x)
    if(!optional) names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}

as.data.frame.ts <- function(x, ...)
{
    if(is.matrix(x))
	as.data.frame.matrix(x, ...)
    else
	as.data.frame.vector(x, ...)
}

as.data.frame.raw  <- as.data.frame.vector
as.data.frame.factor  <- as.data.frame.vector
as.data.frame.ordered <- as.data.frame.vector
as.data.frame.integer <- as.data.frame.vector
as.data.frame.logical <- as.data.frame.vector
as.data.frame.numeric <- as.data.frame.vector
as.data.frame.complex <- as.data.frame.vector


default.stringsAsFactors <- function()
{
    val <- getOption("stringsAsFactors")
    if(is.null(val)) val <- TRUE
    if(!is.logical(val) || is.na(val) || length(val) != 1L)
        stop('options("stringsAsFactors") not set to TRUE or FALSE')
    val
}

## in case someone passes 'nm'
as.data.frame.character <-
    function(x, ..., stringsAsFactors = default.stringsAsFactors())
{
    nm <- paste(deparse(substitute(x), width.cutoff=500L), collapse = " ")# as in  as.DF.vector()
    if(stringsAsFactors) x <- factor(x)
    if(!"nm" %in% names(list(...)))
        as.data.frame.vector(x, ..., nm = nm)
    else as.data.frame.vector(x, ...)
}

as.data.frame.matrix <- function(x, row.names = NULL, optional = FALSE, make.names = TRUE, ...,
                                 stringsAsFactors = default.stringsAsFactors())
{
    d <- dim(x)
    nrows <- d[[1L]]
    ncols <- d[[2L]]
    ic <- seq_len(ncols)
    dn <- dimnames(x)
    ## surely it cannot be right to override the supplied row.names?
    ## changed in 1.8.0
    if(is.null(row.names)) row.names <- dn[[1L]]
    collabs <- dn[[2L]]
    ## These might be NA
    if(any(empty <- !nzchar(collabs)))
	collabs[empty] <- paste0("V", ic)[empty]
    value <- vector("list", ncols)
    if(mode(x) == "character" && stringsAsFactors) {
	for(i in ic)
	    value[[i]] <- as.factor(x[,i])
    } else {
	for(i in ic)
	    value[[i]] <- as.vector(x[,i])
    }
    ## Explicitly check for NULL in case nrows==0
    autoRN <- (is.null(row.names) || length(row.names) != nrows)
    if(length(collabs) == ncols)
	names(value) <- collabs
    else if(!optional)
	names(value) <- paste0("V", ic)
    class(value) <- "data.frame"
    if(autoRN)
        attr(value, "row.names") <- .set_row_names(nrows)
    else
        .rowNamesDF(value, make.names=make.names) <- row.names
    value
}

as.data.frame.model.matrix <-
    function(x, row.names = NULL, optional = FALSE, make.names = TRUE, ...)
{
    d <- dim(x)
    nrows <- d[[1L]]
    dn <- dimnames(x)
    row.names <- dn[[1L]]
    value <- list(x)
    if(!optional) names(value) <- deparse(substitute(x))[[1L]]
    class(value) <- "data.frame"
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != nrows)
            stop(sprintf(ngettext(length(row.names),
                                  "supplied %d row name for %d rows",
                                  "supplied %d row names for %d rows"),
                          length(row.names), nrows), domain = NA)
        .rowNamesDF(value, make.names=make.names) <- row.names
    }
    else attr(value, "row.names") <- .set_row_names(nrows)
    value
}

as.data.frame.array <- function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    if(length(d) == 1L) { ## same as as.data.frame.vector, but deparsed here
	## c(): better than drop() or as.vector() !
	value <- as.data.frame.vector( c(x), row.names, optional, ...)
        if(!optional) names(value) <- deparse(substitute(x))[[1L]]
        value
    } else if (length(d) == 2L) {
        ## for explicit "array" class; otherwise *.matrix() is dispatched
        as.data.frame.matrix(x, row.names, optional, ...)
    } else {
        dn <- dimnames(x)
        dim(x) <- c(d[1L], prod(d[-1L]))
        if(!is.null(dn)) {
            if(length(dn[[1L]])) rownames(x) <- dn[[1L]]
            for(i in 2L:length(d))
                if(is.null(dn[[i]])) dn[[i]] <- seq_len(d[i])
            colnames(x) <- interaction(expand.grid(dn[-1L]))
        }
        as.data.frame.matrix(x, row.names, optional, ...)
    }
}

## Allow extraction method to have changed the underlying class,
## so re-assign the class based on the result.
`[.AsIs` <- function(x, i, ...) I(NextMethod("["))


## NB: this is called relatively often from data.frame() itself, ...
as.data.frame.AsIs <- function(x, row.names = NULL, optional = FALSE, ...)
{
    if(length(dim(x)) == 2L)
	as.data.frame.model.matrix(x, row.names, optional)
    else { # as.data.frame.vector without removing names
        nrows <- length(x)
        nm <- paste(deparse(substitute(x), width.cutoff=500L), collapse=" ")
        if(is.null(row.names)) {
            autoRN <- FALSE
            if (nrows == 0L)
                row.names <- character()
            else if(length(row.names <- names(x)) == nrows &&
                    !anyDuplicated(row.names)) {
            }
            else {
                autoRN <- TRUE
                row.names <- .set_row_names(nrows)
            }
        } else
            autoRN <- is.integer(row.names) && length(row.names) == 2L &&
                is.na(rn1 <- row.names[[1L]]) && rn1 < 0
        value <- list(x)
        if(!optional) names(value) <- nm
        class(value) <- "data.frame"
        ## FIXME -- Need to comment the  'row.names(.) <-'  case
        ## if(autoRN)
            attr(value, "row.names") <- row.names
        ## else
        ##     row.names(value) <- row.names
        value
    }

}

###  This is the real "data.frame".
###  It does everything by calling the methods presented above.

data.frame <-
    function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
	     fix.empty.names = TRUE,
             stringsAsFactors = default.stringsAsFactors())
{
    data.row.names <-
	if(check.rows && is.null(row.names))
	    function(current, new, i) {
		if(is.character(current)) new <- as.character(new)
		if(is.character(new)) current <- as.character(current)
		if(anyDuplicated(new))
		    return(current)
		if(is.null(current))
		    return(new)
		if(all(current == new) || all(current == ""))
		    return(new)
		stop(gettextf(
		    "mismatch of row names in arguments of 'data.frame\', item %d", i),
		    domain = NA)
	    }
	else function(current, new, i) {
	    if(is.null(current)) {
		if(anyDuplicated(new)) {
		    warning(gettextf(
                        "some row.names duplicated: %s --> row.names NOT used",
                        paste(which(duplicated(new)), collapse=",")),
                        domain = NA)
		    current
		} else new
	    } else current
	}
    object <- as.list(substitute(list(...)))[-1L]
    mirn <- missing(row.names) # record before possibly changing
    mrn  <- is.null(row.names) # missing or NULL
    x <- list(...)
    n <- length(x)
    if(n < 1L) {
        if(!mrn) {
            if(is.object(row.names) || !is.integer(row.names))
                row.names <- as.character(row.names)
            if(anyNA(row.names))
                stop("row names contain missing values")
            if(anyDuplicated(row.names))
                stop(gettextf("duplicate row.names: %s",
                              paste(unique(row.names[duplicated(row.names)]),
                                    collapse = ", ")),
                     domain = NA)
        } else row.names <- integer()
	return(structure(list(), names = character(),
                         row.names = row.names,
			 class = "data.frame"))
    }
    vnames <- names(x)
    if(length(vnames) != n)
	vnames <- character(n)
    no.vn <- !nzchar(vnames)
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for(i in seq_len(n)) {
        ## do it this way until all as.data.frame methods have been updated
	xi <- if(is.character(x[[i]]) || is.list(x[[i]]))
		  as.data.frame(x[[i]], optional = TRUE,
				stringsAsFactors = stringsAsFactors)
	      else as.data.frame(x[[i]], optional = TRUE)

        nrows[i] <- .row_names_info(xi) # signed for now
	ncols[i] <- length(xi)
	namesi <- names(xi)
	if(ncols[i] > 1L) {
	    if(length(namesi) == 0L) namesi <- seq_len(ncols[i])
	    vnames[[i]] <- if(no.vn[i]) namesi
			   else paste(vnames[[i]], namesi, sep=".")
	} else if(length(namesi)) {
	    vnames[[i]] <- namesi
	} else if (fix.empty.names && no.vn[[i]]) {
	    tmpname <- deparse(object[[i]], nlines = 1L)[1L]
	    if(startsWith(tmpname, "I(") && endsWith(tmpname, ")")) {
                ## from 'I(*)', only keep '*':
		ntmpn <- nchar(tmpname, "c")
                tmpname <- substr(tmpname, 3L, ntmpn - 1L)
	    }
	    vnames[[i]] <- tmpname
	} ## else vnames[[i]] are not changed
	if(mirn && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            ## Avoid all-blank names
            if(any(nzchar(rowsi)))
                row.names <- data.row.names(row.names, rowsi, i)
        }
        nrows[i] <- abs(nrows[i])
	vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for(i in seq_len(n)[nrows < nr]) {
	xi <- vlist[[i]]
	if(nrows[i] > 0L && (nr %% nrows[i] == 0L)) {
            ## make some attempt to recycle column i
            xi <- unclass(xi) # avoid data-frame methods
            fixed <- TRUE
            for(j in seq_along(xi)) {
                xi1 <- xi[[j]]
                if(is.vector(xi1) || is.factor(xi1))
                    xi[[j]] <- rep(xi1, length.out = nr)
		else if(is.character(xi1) && inherits(xi1, "AsIs"))
                    xi[[j]] <- structure(rep(xi1, length.out = nr),
                                         class = class(xi1))
                else if(inherits(xi1, "Date") || inherits(xi1, "POSIXct"))
                    xi[[j]] <- rep(xi1, length.out = nr)
                else {
                    fixed <- FALSE
                    break
                }
            }
            if (fixed) {
                vlist[[i]] <- xi
                next
            }
        }
        stop(gettextf("arguments imply differing number of rows: %s",
                      paste(unique(nrows), collapse = ", ")),
             domain = NA)
    }
    value <- unlist(vlist, recursive=FALSE, use.names=FALSE)
    ## unlist() drops i-th component if it has 0 columns
    vnames <- as.character(unlist(vnames[ncols > 0L]))
    if(fix.empty.names && any(noname <- !nzchar(vnames)))
	vnames[noname] <- paste0("Var.", seq_along(vnames))[noname]
    if(check.names) {
	if(fix.empty.names)
	    vnames <- make.names(vnames, unique=TRUE)
	else { ## do not fix ""
	    nz <- nzchar(vnames)
	    vnames[nz] <- make.names(vnames[nz], unique=TRUE)
	}
    }
    names(value) <- vnames
    if(!mrn) { # non-null row.names arg was supplied
        if(length(row.names) == 1L && nr != 1L) {  # one of the variables
            if(is.character(row.names))
                row.names <- match(row.names, vnames, 0L)
            if(length(row.names) != 1L ||
               row.names < 1L || row.names > length(vnames))
                stop("'row.names' should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[ - i]
        } else if ( !is.null(row.names) && length(row.names) != nr )
            stop("row names supplied are of the wrong length")
    } else if( !is.null(row.names) && length(row.names) != nr ) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    class(value) <- "data.frame"
    if(is.null(row.names))
        attr(value, "row.names") <- .set_row_names(nr) #seq_len(nr)
    else {
        if(is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if(anyNA(row.names))
            stop("row names contain missing values")
        if(anyDuplicated(row.names))
            stop(gettextf("duplicate row.names: %s",
                          paste(unique(row.names[duplicated(row.names)]),
                                collapse = ", ")),
                 domain = NA)
        row.names(value) <- row.names
    }
    value
}


###  Subsetting and mutation methods
###  These are a little less general than S

`[.data.frame` <-
    function(x, i, j, drop = if(missing(i)) TRUE else length(cols) == 1)
{
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop  # number of arg from x,i,j that were specified
    has.j <- !missing(j)
    if(!all(names(sys.call()) %in% c("", "drop"))
       && !isS4(x)) # at least don't warn for callNextMethod!
        warning("named arguments other than 'drop' are discouraged")

    if(Narg < 3L) {  # list-like indexing or matrix indexing
        if(!mdrop) warning("'drop' argument will be ignored")
	if(missing(i)) return(x)
	if(is.matrix(i))
	    return(as.matrix(x)[i])  # desperate measures
        ## zero-column data frames prior to 2.4.0 had no names.
        nm <- names(x); if(is.null(nm)) nm <- character()
        ## if we have NA names, character indexing should always fail
        ## (for positive index length)
        if(!is.character(i) && anyNA(nm)) { # less efficient version
            names(nm) <- names(x) <- seq_along(x)
            y <- NextMethod("[")
            cols <- names(y)
            if(anyNA(cols)) stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        } else {
            y <- NextMethod("[")
            cols <- names(y)
            if(!is.null(cols) && anyNA(cols))
                stop("undefined columns selected")
        }
        ## added in 1.8.0
        if(anyDuplicated(cols)) names(y) <- make.unique(cols)
        ## since we have not touched the rows, copy over the raw row.names
        ## Claimed at one time at least one fewer copies: PR#15274
        attr(y, "row.names") <- .row_names_info(x, 0L)
        attr(y, "class") <- oldClass(x)
        return(y)
    }

    if(missing(i)) { # df[, j] or df[ , ]
        ## not quite the same as the 1/2-arg case, as 'drop' is used.
        if(drop && !has.j && length(x) == 1L) return(.subset2(x, 1L))
        nm <- names(x); if(is.null(nm)) nm <- character()
        if(has.j && !is.character(j) && anyNA(nm)) {
            ## less efficient version
            names(nm) <- names(x) <- seq_along(x)
            y <- .subset(x, j)
            cols <- names(y)
            if(anyNA(cols)) stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        } else {
            y <- if(has.j) .subset(x, j) else x
            cols <- names(y)
            if(anyNA(cols)) stop("undefined columns selected")
        }
        if(drop && length(y) == 1L) return(.subset2(y, 1L))
        if(anyDuplicated(cols)) names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if(drop && !mdrop && nrow == 1L)
            return(structure(y, class = NULL, row.names = NULL))
        else {
            ## Claimed at one time at least one fewer copies: PR#15274
            attr(y, "class") <- oldClass(x)
            attr(y, "row.names") <- .row_names_info(x, 0L)
            return(y)
        }
    }

    ### df[i, j] or df[i , ]
    ## rewritten for R 2.5.0 to avoid duplicating x.
    xx <- x
    cols <- names(xx)  # needed for computation of 'drop' arg
    ## make a shallow copy
    x <- vector("list", length(x))
    ## attributes(x) <- attributes(xx) expands row names
    x <- .Internal(copyDFattr(xx, x))
    oldClass(x) <- attr(x, "row.names") <- NULL

    if(has.j) { # df[i, j]
        nm <- names(x); if(is.null(nm)) nm <- character()
        if(!is.character(j) && anyNA(nm))
            names(nm) <- names(x) <- seq_along(x)
        x <- x[j]
        cols <- names(x)  # needed for 'drop'
        if(drop && length(x) == 1L) {
            ## for consistency with [, <length-1>]
            if(is.character(i)) {
                rows <- attr(xx, "row.names")
                i <- pmatch(i, rows, duplicates.ok = TRUE)
            }
            ## need to figure which col was selected:
            ## cannot use .subset2 directly as that may
            ## use recursive selection for a logical index.
            xj <- .subset2(.subset(xx, j), 1L)
            return(if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE])
        }
        if(anyNA(cols)) stop("undefined columns selected")
        ## fix up names if we altered them.
        if(!is.null(names(nm))) cols <- names(x) <- nm[cols]
        ## sxx <- match(cols, names(xx)) fails with duplicate names
        nxx <- structure(seq_along(xx), names=names(xx))
        sxx <- match(nxx[j], seq_along(xx))
    } else sxx <- seq_along(x)

    rows <- NULL # placeholder: only create row names when needed
                 # as this can be expensive.
    if(is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
    }
    for(j in seq_along(x)) {
        xj <- xx[[ sxx[j] ]]
        ## had drop = drop prior to 1.8.0
        x[[j]] <- if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE]
    }

    if(drop) {
	n <- length(x)
	if(n == 1L) return(x[[1L]]) # drops attributes
	if(n > 1L) {
	    xj <- x[[1L]]
	    nrow <- if(length(dim(xj)) == 2L) dim(xj)[1L] else length(xj)
            ## for consistency with S: don't drop (to a list)
            ## if only one row, unless explicitly asked for
            drop <- !mdrop && nrow == 1L
	} else drop <- FALSE ## for n == 0
    }

    if(!drop) { # not else as previous section might reset drop
        ## row names might have NAs.
        if(is.null(rows)) rows <- attr(xx, "row.names")
        rows <- rows[i]
	if((ina <- anyNA(rows)) | (dup <- anyDuplicated(rows))) {
	    ## both will coerce integer 'rows' to character:
	    if (!dup && is.character(rows)) dup <- "NA" %in% rows
	    if(ina)
		rows[is.na(rows)] <- "NA"
	    if(dup)
		rows <- make.unique(as.character(rows))
	}
        ## new in 1.8.0  -- might have duplicate columns
	if(has.j && anyDuplicated(nm <- names(x)))
            names(x) <- make.unique(nm)
        if(is.null(rows)) rows <- attr(xx, "row.names")[i]
	attr(x, "row.names") <- rows
	oldClass(x) <- oldClass(xx)
    }
    x
}

`[[.data.frame` <- function(x, ..., exact=TRUE)
{
    ## use in-line functions to refer to the 1st and 2nd ... arguments
    ## explicitly. Also will check for wrong number or empty args
    na <- nargs() - !missing(exact)
    if(!all(names(sys.call()) %in% c("", "exact")))
        warning("named arguments other than 'exact' are discouraged")

    if(na < 3L)
	(function(x, i, exact)
	  if(is.matrix(i)) as.matrix(x)[[i]]
 	  else .subset2(x, i, exact=exact))(x, ..., exact=exact)
    else {
        col <- .subset2(x, ..2, exact=exact)
        i <- if(is.character(..1))
            pmatch(..1, row.names(x), duplicates.ok = TRUE)
        else ..1
        ## we do want to dispatch on methods for a column.
        ## .subset2(col, i, exact=exact)
        col[[i, exact = exact]]
    }
}

`[<-.data.frame` <- function(x, i, j, value)
{
    if(!all(names(sys.call()) %in% c("", "value")))
        warning("named arguments are discouraged")

    nA <- nargs() # 'value' is never missing, so 3 or 4.
    if(nA == 4L) { ## df[,] or df[i,] or df[, j] or df[i,j]
	has.i <- !missing(i)
	has.j <- !missing(j)
    }
    else if(nA == 3L) {
        ## this collects both df[] and df[ind]
        if (is.atomic(value) && !is.null(names(value)))
            names(value) <- NULL
        if(missing(i) && missing(j)) { # case df[]
            i <- j <- NULL
            has.i <- has.j <- FALSE
            ## added in 1.8.0
            if(is.null(value)) return(x[logical()])
        } else { # case df[ind]
            ## really ambiguous, but follow common use as if list
            ## except for two column numeric matrix or full-sized logical matrix
            if(is.numeric(i) && is.matrix(i) && ncol(i) == 2) {
                # Rewrite i as a logical index
                index <- rep.int(FALSE, prod(dim(x)))
                dim(index) <- dim(x)
                tryCatch(index[i] <- TRUE,
                         error = function(e) stop(conditionMessage(e), call.=FALSE))
                # Put values in the right order
                o <- order(i[,2], i[,1])
                N <- length(value)
                if (length(o) %% N != 0L)
                    warning("number of items to replace is not a multiple of replacement length")
                if (N < length(o))
                    value <- rep(value, length.out=length(o))
                value <- value[o]
                i <- index
            }
            if(is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm=TRUE)
                if(!nreplace) return(x) # nothing to replace
                ## allow replication of length(value) > 1 in 1.8.0
                N <- length(value)
                if(N > 1L && N < nreplace && (nreplace %% N) == 0L)
                    value <- rep(value, length.out = nreplace)
                if(N > 1L && (length(value) != nreplace))
                    stop("'value' is the wrong length")
                n <- 0L
                nv <- nrow(x)
                for(v in seq_len(dim(i)[2L])) {
                    thisvar <- i[, v, drop = TRUE]
                    nv <- sum(thisvar, na.rm = TRUE)
                    if(nv) {
                        if(is.matrix(x[[v]]))
                            x[[v]][thisvar, ] <- if(N > 1L) value[n+seq_len(nv)] else value
                        else
                            x[[v]][thisvar] <- if(N > 1L) value[n+seq_len(nv)] else value
                    }
                    n <- n+nv
                }
                return(x)
            }  # end of logical matrix
            if(is.matrix(i))
                stop("unsupported matrix index in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else # nargs() <= 2
	stop("need 0, 1, or 2 subscripts")

    if ((has.j && !length(j)) ||	# "no", i.e. empty columns specified
        (has.i && !length(i) && !has.j))# empty rows and no col.   specified
	return(x)

    cl <- oldClass(x)
    ## delete class: S3 idiom to avoid any special methods for [[, etc
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if(has.i && length(i)) { # df[i, ] or df[i, j]
        rows <- NULL  # indicator that it is not yet set
        if(anyNA(i))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
	    ii <- match(i, rows)
	    nextra <- sum(new.rows <- is.na(ii))
	    if(nextra > 0L) {
		ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
		new.rows <- i[new.rows]
	    }
	    i <- ii
	}
	if(!is.logical(i) &&
	   (char.i && nextra  ||  all(i >= 0L) && (nn <- max(i)) > nrows)) {
	    ## expand
            if(is.null(rows)) rows <- attr(x, "row.names")
	    if(!char.i) {
		nrr <- (nrows + 1L):nn
		if(inherits(value, "data.frame") &&
		   (dim(value)[1L]) >= length(nrr)) {
		    new.rows <- attr(value, "row.names")[seq_along(nrr)]
		    repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
		    if(any(repl)) new.rows[repl] <- nrr[repl]
		}
		else new.rows <- nrr
	    }
	    x <- xpdrows.data.frame(x, rows, new.rows)
	    rows <- attr(x, "row.names")
	    nrows <- length(rows)
	}
	iseq <- seq_len(nrows)[i]
	if(anyNA(iseq)) stop("non-existent rows not allowed")
    }
    else iseq <- NULL

    if(has.j) {
        if(anyNA(j))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(is.character(j)) {
            if("" %in% j) stop("column name \"\" cannot match any column")
	    jseq <- match(j, names(x))
	    if(anyNA(jseq)) {
		n <- is.na(jseq)
		jseq[n] <- nvars + seq_len(sum(n))
		new.cols <- j[n]
	    }
	}
	else if(is.logical(j) || min(j) < 0L)
	    jseq <- seq_along(x)[j]
	else {
	    jseq <- j
	    if(max(jseq) > nvars) {
		new.cols <- paste0("V",
                                   seq.int(from = nvars + 1L, to = max(jseq)))
		if(length(new.cols)  != sum(jseq > nvars))
		    stop("new columns would leave holes after existing columns")
                ## try to use the names of a list `value'
                if(is.list(value) && !is.null(vnm <- names(value))) {
                    p <- length(jseq)
                    if(length(vnm) < p) vnm <- rep_len(vnm, p)
                    new.cols <- vnm[jseq > nvars]
                }
	    }
	}
    }
    else jseq <- seq_along(x)

    ## empty rows and not (a *new* column as in  d[FALSE, "new"] <- val )  :
    if(has.i && !length(iseq) && all(1L <= jseq & jseq <= nvars))
	return(`class<-`(x, cl))

    ## addition in 1.8.0
    if(anyDuplicated(jseq))
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if(n == 0L) n <- nrows
    p <- length(jseq)
    if (is.null(value)) {
        value <- list(NULL)
    }
    m <- length(value)
    if(!is.list(value)) {
        if(p == 1L) {
            N <- NROW(value)
            if(N > n)
                stop(sprintf(ngettext(N,
                                      "replacement has %d row, data has %d",
                                      "replacement has %d rows, data has %d"),
                             N, n), domain = NA)
            if(N < n && N > 0L)
                if(n %% N == 0L && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = n)
                else
                    stop(sprintf(ngettext(N,
                                          "replacement has %d row, data has %d",
                                          "replacement has %d rows, data has %d"),
                                 N, nrows), domain = NA)
            if (!is.null(names(value))) names(value) <- NULL
            value <- list(value)
         } else {
            if(m < n*p && (m == 0L || (n*p) %% m))
                stop(sprintf(ngettext(m,
                                      "replacement has %d item, need %d",
                                      "replacement has %d items, need %d"),
                             m, n*p), domain = NA)
            value <- matrix(value, n, p)  ## will recycle
            ## <FIXME split.matrix>
            value <- split(c(value), col(value))
        }
	dimv <- c(n, p)
    } else { # a list
        ## careful, as.data.frame turns things into factors.
	## value <- as.data.frame(value)
        value <- unclass(value) # to avoid data frame indexing
        lens <- vapply(value, NROW, 1L)
        for(k in seq_along(lens)) {
            N <- lens[k]
            if(n != N && length(dim(value[[k]])) == 2L)
                stop(sprintf(ngettext(N,
                                      "replacement element %d is a matrix/data frame of %d row, need %d",
                                      "replacement element %d is a matrix/data frame of %d rows, need %d"),
                             k, N, n),
                     domain = NA)
            if(N > 0L && N < n && n %% N)
                stop(sprintf(ngettext(N,
                                      "replacement element %d has %d row, need %d",
                                      "replacement element %d has %d rows, need %d"),
                             k, N, n), domain = NA)
            ## these fixing-ups will not work for matrices
            if(N > 0L && N < n) value[[k]] <- rep(value[[k]], length.out = n)
            if(N > n) {
                warning(sprintf(ngettext(N,
                                         "replacement element %d has %d row to replace %d rows",
                                         "replacement element %d has %d rows to replace %d rows"),
                                k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
	dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if(nrowv < n && nrowv > 0L) {
	if(n %% nrowv == 0L)
	    value <- value[rep_len(seq_len(nrowv), n),,drop = FALSE]
	else
            stop(sprintf(ngettext(nrowv,
                                  "%d row in value to replace %d rows",
                                  "%d rows in value to replace %d rows"),
                         nrowv, n), domain = NA)
    }
    else if(nrowv > n)
        warning(sprintf(ngettext(nrowv,
                                 "replacement data has %d row to replace %d rows",
                                 "replacement data has %d rows to replace %d rows"),
                        nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
    if(ncolv < p) jvseq <- rep_len(seq_len(ncolv), p)
    else if(p != 0L && ncolv > p) {
        warning(sprintf(ngettext(ncolv,
                                 "provided %d variable to replace %d variables",
                                 "provided %d variables to replace %d variables"),
                        ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if(length(new.cols)) {
        ## extend and name now, as assignment of NULL may delete cols later.
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x); a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if(has.i)
	for(jjj in seq_len(p)) {
	    jj <- jseq[jjj]
	    vjj <- value[[ jvseq[[jjj]] ]]
            if(jj <= nvars) {
                ## if a column exists, preserve its attributes
                if(length(dim(x[[jj]])) != 2L)
                     x[[jj]][iseq  ] <- vjj
                else x[[jj]][iseq, ] <- vjj
            } else {
                ## try to make a new column match in length: may be an error
                x[[jj]] <- vjj[FALSE]
                if(length(dim(vjj)) == 2L) {
                    length(x[[jj]]) <- nrows * ncol(vjj)
                    dim(x[[jj]])  <- c(nrows,  ncol(vjj))
                    x[[jj]][iseq, ] <- vjj
                } else {
                    length(x[[jj]]) <- nrows
                    x[[jj]][iseq] <- vjj
                }
            }
	}
    else if(p > 0L)
      for(jjj in p:1L) { # we might delete columns with NULL
        ## ... and for that reason, we'd better ensure that jseq is increasing!
        o <- order(jseq)
        jseq <- jseq[o]
        jvseq <- jvseq[o]

        jj <- jseq[jjj]
        v <- value[[ jvseq[[jjj]] ]]
        ## This is consistent with the have.i case rather than with
        ## [[<- and $<- (which throw an error).  But both are plausible.
        if (!is.null(v) && nrows > 0L && !length(v)) length(v) <- nrows
	x[[jj]] <- v
        if (!is.null(v) && is.atomic(x[[jj]]) && !is.null(names(x[[jj]])))
            names(x[[jj]]) <- NULL
    }
    if(length(new.cols) > 0L) {
        new.cols <- names(x) # we might delete columns with NULL
        ## added in 1.8.0
        if(anyDuplicated(new.cols)) names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
}

`[[<-.data.frame` <- function(x, i, j, value)
{
    if(!all(names(sys.call()) %in% c("", "value")))
        warning("named arguments are discouraged")

    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if(is.atomic(value) && !is.null(names(value))) names(value) <- NULL
    if(nargs() < 4L) {
	## really ambiguous, but follow common use as if list
        nc <- length(x)
	if(!is.null(value)) {
            N <- NROW(value)
            if(N > nrows)
                stop(sprintf(ngettext(N,
                                      "replacement has %d row, data has %d",
                                      "replacement has %d rows, data has %d"),
                             N, nrows), domain = NA)
            if(N < nrows)
                if(N > 0L && (nrows %% N == 0L) && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = nrows)
                else
                    stop(sprintf(ngettext(N,
                                          "replacement has %d row, data has %d",
                                          "replacement has %d rows, data has %d"),
                                 N, nrows), domain = NA)
	}
	x[[i]] <- value
        ## added in 1.8.0 -- make sure there is a name
        if(length(x) > nc) {
            nc <- length(x)
            if(names(x)[nc] == "") names(x)[nc] <- paste0("V", nc)
            names(x) <- make.unique(names(x))
        }
	class(x) <- cl
	return(x)
    }
    if(missing(i) || missing(j))
	stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
    rows <- attr(x, "row.names")
    nvars <- length(x)
    if(n <- is.character(i)) {
	ii <- match(i, rows)
	n <- sum(new.rows <- is.na(ii))
	if(n > 0L) {
	    ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
	    new.rows <- i[new.rows]
	}
	i <- ii
    }
    if(all(i >= 0L) && (nn <- max(i)) > nrows) {
	## expand
	if(n == 0L) {
	    nrr <- (nrows + 1L):nn
	    if(inherits(value, "data.frame") &&
	       (dim(value)[1L]) >= length(nrr)) {
		new.rows <- attr(value, "row.names")[seq_len(nrr)]
		repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
		if(any(repl)) new.rows[repl] <- nrr[repl]
	    }
	    else new.rows <- nrr
	}
	x <- xpdrows.data.frame(x, rows, new.rows)
	rows <- attr(x, "row.names")
	nrows <- length(rows)
    }

    ## FIXME: this is wasteful and probably unnecessary
    iseq <- seq_len(nrows)[i]
    if(anyNA(iseq))
	stop("non-existent rows not allowed")

    if(is.character(j)) {
        if("" %in% j) stop("column name \"\" cannot match any column")
	jseq <- match(j, names(x))
	if(anyNA(jseq))
            stop(gettextf("replacing element in non-existent column: %s",
                          j[is.na(jseq)]), domain = NA)
    }
    else if(is.logical(j) || min(j) < 0L)
	jseq <- seq_along(x)[j]
    else {
	jseq <- j
	if(max(jseq) > nvars)
            stop(gettextf("replacing element in non-existent column: %s",
                          jseq[jseq > nvars]), domain = NA)
    }
    if(length(iseq) > 1L || length(jseq) > 1L)
	stop("only a single element should be replaced")
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
}

## added in 1.8.0
`$<-.data.frame` <- function(x, name, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    ## This forces a copy, but we are going to need one anyway
    ## and NAMED=1 prevents any further copying.
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if(!is.null(value)) {
        N <- NROW(value)
        if(N > nrows)
            stop(sprintf(ngettext(N,
                                  "replacement has %d row, data has %d",
                                  "replacement has %d rows, data has %d"),
                         N, nrows), domain = NA)
        if (N < nrows)
            if (N > 0L && (nrows %% N == 0L) && length(dim(value)) <= 1L)
                value <- rep(value, length.out = nrows)
            else
                stop(sprintf(ngettext(N,
                                      "replacement has %d row, data has %d",
                                      "replacement has %d rows, data has %d"),
                             N, nrows), domain = NA)
        if(is.atomic(value) && !is.null(names(value))) names(value) <- NULL
    }
    x[[name]] <- value
    class(x) <- cl
    return(x)
}


xpdrows.data.frame <- function(x, old.rows, new.rows)
{
    nc <- length(x)
    nro <- length(old.rows)
    nrn <- length(new.rows)
    nr <- nro + nrn
    for (i in seq_len(nc)) {
	y <- x[[i]]
	dy <- dim(y)
	cy <- oldClass(y)
	class(y) <- NULL
	if (length(dy) == 2L) {
	    dny <- dimnames(y)
	    if (length(dny[[1L]]) > 0L)
		dny[[1L]] <- c(dny[[1L]], new.rows)
	    z <- array(y[1L], dim = c(nr, nc), dimnames = dny)
	    z[seq_len(nro), ] <- y
	    class(z) <- cy
	    x[[i]] <- z
	}
	else {
	    ay <- attributes(y)
	    if (length(names(y)) > 0L)
		ay$names <- c(ay$names, new.rows)
	    length(y) <- nr
	    attributes(y) <- ay
	    class(y) <- cy
	    x[[i]] <- y
	}
    }
    nm <- c(old.rows, new.rows)
    if (any(duplicated(nm))) nm <- make.unique(as.character(nm))
    attr(x, "row.names") <- nm
    x
}


### Here are the methods for rbind and cbind.

cbind.data.frame <- function(..., deparse.level = 1)
    data.frame(..., check.names = FALSE)

rbind.data.frame <- function(..., deparse.level = 1, make.row.names = TRUE,
                             stringsAsFactors = default.stringsAsFactors(),
                             factor.exclude = TRUE)
{
    match.names <- function(clabs, nmi)
    {
	if(identical(clabs, nmi)) NULL
	else if(length(nmi) == length(clabs) && all(nmi %in% clabs)) {
            ## we need 1-1 matches here
	    m <- pmatch(nmi, clabs, 0L)
            if(any(m == 0L))
                stop("names do not match previous names")
            m
	} else stop("names do not match previous names")
    }
    allargs <- list(...)
    allargs <- allargs[lengths(allargs) > 0L]
    if(length(allargs)) {
        ## drop any zero-row data frames, as they may not have proper column
        ## types (e.g. NULL).
        nr <- vapply(allargs, function(x)
                     if(is.data.frame(x)) .row_names_info(x, 2L)
                     else if(is.list(x)) length(x[[1L]])
					# mismatched lists are checked later
                     else length(x), 1L)
	if(any(n0 <- nr == 0L)) {
	    if(all(n0)) return(allargs[[1L]]) # pretty arbitrary
	    allargs <- allargs[!n0]
	}
    }
    n <- length(allargs)
    if(n == 0L)
	return(list2DF())
    nms <- names(allargs)
    if(is.null(nms))
	nms <- character(n)
    cl <- NULL
    perm <- rows <- vector("list", n)
    if(make.row.names) {
	rlabs <- rows
	autoRnms <- TRUE # result with 1:nrow(.) row names? [efficiency!]
	Make.row.names <- function(nmi, ri, ni, nrow)
	{
	    if(nzchar(nmi)) {
		if(autoRnms) autoRnms <<- FALSE
		if(ni == 0L) character()  # PR#8506
		else if(ni > 1L) paste(nmi, ri, sep = ".")
		else nmi
	    }
	    else if(autoRnms && nrow > 0L && identical(ri, seq_len(ni)))
		as.integer(seq.int(from = nrow + 1L, length.out = ni))
	    else {
		if(autoRnms && (nrow > 0L || !identical(ri, seq_len(ni))))
		    autoRnms <<- FALSE
		ri
	    }
	}
    }
    smartX <- isTRUE(factor.exclude)

    ## check the arguments, develop row and column labels
    nrow <- 0L
    value <- clabs <- NULL
    all.levs <- list()
    for(i in seq_len(n)) { ## check and treat arg [[ i ]]  -- part 1
	xi <- allargs[[i]]
	nmi <- nms[i]
        ## coerce matrix to data frame
        if(is.matrix(xi)) allargs[[i]] <- xi <-
            as.data.frame(xi, stringsAsFactors = stringsAsFactors)
	if(inherits(xi, "data.frame")) {
	    if(is.null(cl))
		cl <- oldClass(xi)
	    ri <- attr(xi, "row.names")
	    ni <- length(ri)
	    if(is.null(clabs)) ## first time
		clabs <- names(xi)
	    else {
                if(length(xi) != length(clabs))
                    stop("numbers of columns of arguments do not match")
		pi <- match.names(clabs, names(xi))
		if( !is.null(pi) ) perm[[i]] <- pi
	    }
	    rows[[i]] <- seq.int(from = nrow + 1L, length.out = ni)
	    if(make.row.names) rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    nrow <- nrow + ni
	    if(is.null(value)) { ## first time ==> setup once:
		value <- unclass(xi)
		nvar <- length(value)
		all.levs <- vector("list", nvar)
		has.dim <- facCol <- ordCol <- logical(nvar)
		if(smartX) NA.lev <- ordCol
		for(j in seq_len(nvar)) {
		    xj <- value[[j]]
                    facCol[j] <- fac <-
                        if(!is.null(lj <- levels(xj))) {
                            all.levs[[j]] <- lj
                            TRUE # turn categories into factors
                        } else
                            is.factor(xj)
		    if(fac) {
			ordCol[j] <- is.ordered(xj)
			if(smartX && !NA.lev[j])
			    NA.lev[j] <- anyNA(lj)
		    }
		    has.dim[j] <- length(dim(xj)) == 2L
		}
	    }
	    else for(j in seq_len(nvar)) {
                xij <- xi[[j]]
                if(is.null(pi) || is.na(jj <- pi[[j]])) jj <- j
                if(facCol[jj]) {
                    if(length(lij <- levels(xij))) {
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], lij))
			if(ordCol[jj])
			    ordCol[jj] <- is.ordered(xij)
			if(smartX && !NA.lev[jj])
			    NA.lev[jj] <- anyNA(lij)
                    } else if(is.character(xij))
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], xij))
                }
            }
	} ## end{data.frame}
	else if(is.list(xi)) {
	    ni <- range(lengths(xi))
	    if(ni[1L] == ni[2L])
		ni <- ni[1L]
	    else stop("invalid list argument: all variables should have the same length")
	    rows[[i]] <- ri <-
                as.integer(seq.int(from = nrow + 1L, length.out = ni))
	    nrow <- nrow + ni
	    if(make.row.names) rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    if(length(nmi <- names(xi)) > 0L) {
		if(is.null(clabs))
		    clabs <- nmi
		else {
                    if(length(xi) != length(clabs))
                        stop("numbers of columns of arguments do not match")
		    pi <- match.names(clabs, nmi)
		    if( !is.null(pi) ) perm[[i]] <- pi
		}
	    }
	}
	else if(length(xi)) { # 1 new row
	    rows[[i]] <- nrow <- nrow + 1L
            if(make.row.names)
		rlabs[[i]] <- if(nzchar(nmi)) nmi else as.integer(nrow)
	}
    } # for(i .)

    nvar <- length(clabs)
    if(nvar == 0L)
	nvar <- max(lengths(allargs)) # only vector args
    if(nvar == 0L)
	return(list2DF())
    pseq <- seq_len(nvar)
    if(is.null(value)) { # this happens if there has been no data frame
	value <- list()
	value[pseq] <- list(logical(nrow)) # OK for coercion except to raw.
        all.levs <- vector("list", nvar)
	has.dim <- facCol <- ordCol <- logical(nvar)
	if(smartX) NA.lev <- ordCol
    }
    names(value) <- clabs
    for(j in pseq)
	if(length(lij <- all.levs[[j]]))
            value[[j]] <-
		factor(as.vector(value[[j]]), levels = lij,
		       exclude = if(smartX) {
				     if(!NA.lev[j]) NA # else NULL
				 } else factor.exclude,
		       ordered = ordCol[j])

    if(any(has.dim)) { # some col's are matrices or d.frame's
        jdim <- pseq[has.dim]
        if(!all(df <- vapply(jdim, function(j) inherits(value[[j]],"data.frame"), NA))) {
            ## Ensure matrix columns can be filled in  for(i ...) below
            rmax <- max(unlist(rows))
            for(j in jdim[!df]) {
		dn <- dimnames(vj <- value[[j]])
		rn <- dn[[1L]]
		if(length(rn) > 0L) length(rn) <- rmax
		pj <- dim(vj)[2L]
		length(vj) <- rmax * pj
		value[[j]] <- array(vj, c(rmax, pj), list(rn, dn[[2L]]))
	    }
        }
    }

    for(i in seq_len(n)) { ## add arg [[i]] to result
	xi <- unclass(allargs[[i]])
	if(!is.list(xi))
	    if(length(xi) != nvar)
		xi <- rep(xi, length.out = nvar)
	ri <- rows[[i]]
	pi <- perm[[i]]
	if(is.null(pi)) pi <- pseq
	for(j in pseq) {
	    jj <- pi[j]
            xij <- xi[[j]]
	    if(has.dim[jj]) {
		value[[jj]][ri,	 ] <- xij
                ## copy rownames
                if(!is.null(r <- rownames(xij))) rownames(value[[jj]])[ri] <- r
	    } else {
                ## coerce factors to vectors, in case lhs is character or
                ## level set has changed
                value[[jj]][ri] <- if(is.factor(xij)) as.vector(xij) else xij
                ## copy names if any
                if(!is.null(nm <- names(xij))) names(value[[jj]])[ri] <- nm
            }
	}
    }
    rlabs <- if(make.row.names && !autoRnms) {
		 rlabs <- unlist(rlabs)
		 if(anyDuplicated(rlabs))
		     make.unique(as.character(rlabs), sep = "")
		 else
		     rlabs
	     } # else NULL
    if(is.null(cl)) {
	as.data.frame(value, row.names = rlabs, fix.empty.names = TRUE,
		      stringsAsFactors = stringsAsFactors)
    } else {
	structure(value, class = cl,
		  row.names = if(is.null(rlabs)) .set_row_names(nrow) else rlabs)
    }
}


### coercion and print methods

print.data.frame <-
    function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
	     row.names = TRUE, max = NULL)
{
    n <- length(row.names(x))
    if(length(x) == 0L) {
	cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
			     "data frame with 0 columns and %d rows"),
		    n), "\n", sep = "")
    } else if(n == 0L) {
        ## FIXME: header format is inconsistent here
	print.default(names(x), quote = FALSE)
	cat(gettext("<0 rows> (or 0-length row.names)\n"))
    } else {
	if(is.null(max)) max <- getOption("max.print", 99999L)
        if(!is.finite(max)) stop("invalid 'max' / getOption(\"max.print\"): ", max)
	## format.<*>() : avoiding picking up e.g. format.AsIs
	omit <- (n0 <- max %/% length(x)) < n
	m <- as.matrix(
	    format.data.frame(if(omit) x[seq_len(n0), , drop=FALSE] else x,
			      digits = digits, na.encode = FALSE))
	if(!isTRUE(row.names))
	    dimnames(m)[[1L]] <-
		if(isFALSE(row.names)) rep.int("", if(omit) n0 else n)
		else row.names
	print(m, ..., quote = quote, right = right, max = max)
	if(omit)
	    cat(" [ reached 'max' / getOption(\"max.print\") -- omitted",
		n - n0, "rows ]\n")
    }
    invisible(x)
}

as.matrix.data.frame <- function (x, rownames.force = NA, ...)
{
    dm <- dim(x)
    rn <- if(rownames.force %in% FALSE) NULL
	  else if(rownames.force %in% TRUE || .row_names_info(x) > 0L)
              row.names(x) # else NULL
    dn <- list(rn, names(x))
    if(any(dm == 0L))
	return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2L] # >= 1
    pseq <- seq_len(p)
    n <- dm[1L]
    X <- unclass(x) # will contain the result;
    ## the "big question" is if we return a numeric or a character matrix
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in pseq) {
	xj <- X[[j]]
	if(inherits(xj, "data.frame"))# && ncol(xj) > 1L)
	    X[[j]] <- xj <- as.matrix(xj)
        j.logic <- is.logical(xj)
        if(all.logical && !j.logic) all.logical <- FALSE
	if(length(levels(xj)) > 0L || !(j.logic || is.numeric(xj) || is.complex(xj))
	   || (!is.null(cl <- attr(xj, "class")) && # numeric classed objects to format:
	       any(cl %in% c("Date", "POSIXct", "POSIXlt"))))
	    non.numeric <- TRUE
	if(!is.atomic(xj) && !inherits(xj, "POSIXlt"))
	    non.atomic <- TRUE
    }
    if(non.atomic) {
	for (j in pseq) {
	    xj <- X[[j]]
	    if(!is.recursive(xj))
		X[[j]] <- as.list(as.vector(xj))
	}
    } else if(all.logical) {
        ## do nothing for logical columns if a logical matrix will result.
    } else if(non.numeric) {
	for (j in pseq) {
	    if (is.character(X[[j]]))
		next
	    else if(is.logical(xj <- X[[j]]))
		xj <- as.character(xj) # not format(), takes care of NAs too
	    else {
		miss <- is.na(xj)
		xj <- if(length(levels(xj))) as.vector(xj) else format(xj)
		is.na(xj) <- miss
	    }
            X[[j]] <- xj
	}
    }
    ## These coercions could have changed the number of columns
    ## (e.g. class "Surv" coerced to character),
    ## so only now can we compute collabs.
    collabs <- as.list(dn[[2L]])
    for (j in pseq) {
        xj <- X[[j]]
        dj <- dim(xj)
        if(length(dj) == 2L && dj[2L] > 0L) { # matrix with > 0 col
            if(!length(dnj <- colnames(xj))) dnj <- seq_len(dj[2L])
            collabs[[j]] <-
                if(length(collabs)) {
                    if(dj[2L] > 1L)
                        paste(collabs[[j]], dnj, sep = ".")
                    else if(is.character(collabs[[j]])) collabs[[j]]
                    else dnj
                }
                else dnj
        }
    }
    nc <- vapply(X, NCOL, numeric(1), USE.NAMES=FALSE)
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(dn[[1L]], unlist(collabs[nc > 0], use.names = FALSE))
    X
}

Math.data.frame <- function (x, ...)
{
    mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
    if (all(mode.ok)) {
	x[] <- lapply(X = x, FUN = .Generic, ...)
	return(x)
    } else {
	vnames <- names(x)
	if (is.null(vnames)) vnames <- seq_along(x)
	stop("non-numeric variable(s) in data frame: ",
	     paste(vnames[!mode.ok], collapse = ", "))
    }
}

Ops.data.frame <- function(e1, e2 = NULL)
{
    isList <- function(x) !is.null(x) && is.list(x)
    unary <- nargs() == 1L
    lclass <- nzchar(.Method[1L])
    rclass <- !unary && (nzchar(.Method[2L]))
    value <- list()
    rn <- NULL
    ## set up call as op(left, right)
    ## These are used, despite
    ## _R_CHECK_CODETOOLS_PROFILE_="suppressLocalUnused=FALSE"
    FUN <- get(.Generic, envir = parent.frame(), mode = "function")
    f <- if (unary) quote(FUN(left)) else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if(lclass && rclass) {
        nr <- .row_names_info(e1, 2L)
	if(.row_names_info(e1) > 0L) rn <- attr(e1, "row.names")
	cn <- names(e1)
	if(any(dim(e2) != dim(e1)))
	    stop(gettextf("%s only defined for equally-sized data frames",
                          sQuote(.Generic)), domain = NA)
    } else if(lclass) {
	## e2 is not a data frame, but e1 is.
        nr <- .row_names_info(e1, 2L)
	if(.row_names_info(e1) > 0L) rn <- attr(e1, "row.names")
	cn <- names(e1)
	rscalar <- length(e2) <= 1L # e2 might be null
	if(isList(e2)) {
	    if(rscalar) e2 <- e2[[1L]]
	    else if(length(e2) != ncol(e1))
		stop(gettextf("list of length %d not meaningful", length(e2)),
                     domain = NA)
	} else {
	    if(!rscalar)
		e2 <- split(rep_len(as.vector(e2), prod(dim(e1))),
			    rep.int(seq_len(ncol(e1)),
                                    rep.int(nrow(e1), ncol(e1))))
	}
    } else {
	## e1 is not a data frame, but e2 is.
        nr <- .row_names_info(e2, 2L)
	if(.row_names_info(e2) > 0L) rn <- attr(e2, "row.names")
	cn <- names(e2)
	lscalar <- length(e1) <= 1L
	if(isList(e1)) {
	    if(lscalar) e1 <- e1[[1L]]
	    else if(length(e1) != ncol(e2))
		stop(gettextf("list of length %d not meaningful", length(e1)),
                     domain = NA)
	} else {
	    if(!lscalar)
		e1 <- split(rep_len(as.vector(e1), prod(dim(e2))),
			    rep.int(seq_len(ncol(e2)),
                                    rep.int(nrow(e2), ncol(e2))))
	}
    }
    for(j in seq_along(cn)) {
	left  <- if(!lscalar) e1[[j]] else e1
	right <- if(!rscalar) e2[[j]] else e2
	value[[j]] <- eval(f)
    }
    if(.Generic %in% c("+","-","*","^","%%","%/%","/")) {## == 'Arith'
	if(length(value)) {
	    names(value) <- cn
	    data.frame(value, row.names = rn, check.names = FALSE)
	} else
	    data.frame(       row.names = rn, check.names = FALSE)
    }
    else { ## 'Logic' ("&","|")  and  'Compare' ("==",">","<","!=","<=",">=") :
	value <- unlist(value, recursive = FALSE, use.names = FALSE)
	matrix(if(is.null(value)) logical() else value,
	       nrow = nr, dimnames = list(rn,cn))
    }
}

Summary.data.frame <- function(..., na.rm)
{
    args <- list(...)
    args <- lapply(args, function(x) {
        x <- as.matrix(x)
        if(!is.numeric(x) && !is.complex(x))
            stop("only defined on a data frame with all numeric variables")
        x
    })
    do.call(.Generic, c(args, na.rm=na.rm))
}

list2DF <-
function(x = list(), nrow = NULL)
{
    stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)
    if(n <- length(x)) {
        if(is.null(nrow))
            nrow <- max(lengths(x), 0L)
        x <- lapply(x, rep_len, nrow)
    } else {
        if(is.null(nrow))
            nrow <- 0L
    }
    if(is.null(names(x)))
        names(x) <- character(n)
    class(x) <- "data.frame"
    attr(x, "row.names") <- .set_row_names(nrow)
    x
}
