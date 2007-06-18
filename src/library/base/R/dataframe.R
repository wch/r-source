## As from R 2.4.0, row.names can be either character or integer.
## row.names() will always return character.
## attr(, "row.names") will return either character or integer.
##
## Do not assume that the internal representation is either, since
## 1:n is stored as the integer vector c(NA, n) to save space (and
## the C-level code to get/set the attribute makes the appropriate
## translations.
##
## As from 2.5.0 c(NA, n > 0) indicates deliberately assigned row names,
## and c(NA, n < 0) automatic row names.

.row_names_info <- function(x, type = 1L)
    .Call("R_shortRowNames", x, type, PACKAGE = "base")

row.names <- function(x) UseMethod("row.names")
row.names.data.frame <- function(x) as.character(attr(x, "row.names"))
row.names.default <- function(x) if(!is.null(dim(x))) rownames(x)# else NULL

.set_row_names <- function(n)
    if(n > 0) c(NA_integer_, -n) else integer(0)

"row.names<-" <- function(x, value) UseMethod("row.names<-")
"row.names<-.data.frame" <- function(x, value) {
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
    else if (length(value) != n)
	stop("invalid 'row.names' length")
    if (any(duplicated(value)))
	stop("duplicate 'row.names' are not allowed")
    if (any(is.na(value)))
	stop("missing values in 'row.names' are not allowed")
    attr(x, "row.names") <- value
    x
}

"row.names<-.default" <- function(x, value) "rownames<-"(x, value)

is.na.data.frame <- function (x)
{
    y <- do.call("cbind", lapply(x, "is.na")) # gives a matrix
    if(.row_names_info(x) > 0L) rownames(y) <- row.names(x)
    y
}

is.data.frame <- function(x) inherits(x, "data.frame")

I <- function(x) { structure(x, class = unique(c("AsIs", oldClass(x)))) }

print.AsIs <- function (x, ...)
{
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    NextMethod("print")
    invisible(x)
}


t.data.frame <- function(x) {
    x <- as.matrix(x)
    NextMethod("t")
}

dim.data.frame <- function(x) c(.row_names_info(x, 2L), length(x))

dimnames.data.frame <- function(x) list(row.names(x), names(x))

"dimnames<-.data.frame" <- function(x, value)
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

as.data.frame <- function(x, row.names = NULL, optional = FALSE, ...) {
    if(is.null(x))			# can't assign class to NULL
	return(as.data.frame(list()))
    UseMethod("as.data.frame")
}

as.data.frame.default <- function(x, ...)
    stop(gettextf("cannot coerce class \"%s\" into a data.frame", class(x)),
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
	else stop(gettextf("invalid 'row.names', length %d for a data frame with %d rows",
                           length(row.names), nr), domain = NA)
    }
    x
}

## prior to 1.8.0 this coerced names - PR#3280
as.data.frame.list <-
    function(x, row.names = NULL, optional = FALSE, ...,
             stringsAsFactors = default.stringsAsFactors())
{
    ## need to protect names in x.
    cn <- names(x)
    m <- match(c("row.names", "check.rows", "check.names", "stringsAsFactors"),
               cn, 0L)
    if(any(m > 0L)) {
        cn[m] <- paste("..adfl.", cn[m], sep="")
        names(x) <- cn
    }
    x <- eval(as.call(c(expression(data.frame), x, check.names = !optional,
                        stringsAsFactors = stringsAsFactors)))
    if(any(m > 0L)) names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
    if(!is.null(row.names)) {
	# row.names <- as.character(row.names)
	if(length(row.names) != dim(x)[[1L]])
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), dim(x)[[1L]]), domain = NA)
	attr(x, "row.names") <- row.names
    }
    x
}

as.data.frame.vector <- function(x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse=" ")
    if(is.null(row.names)) {
	if (nrows == 0L)
	    row.names <- character(0L)
	else if(length(row.names <- names(x)) == nrows &&
		!any(duplicated(row.names))) {}
	else row.names <- .set_row_names(nrows)
    }
    names(x) <- NULL # remove names as from 2.0.0
    value <- list(x)
    if(!optional) names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
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
as.data.frame.numeric <- as.data.frame.vector
as.data.frame.complex <- as.data.frame.vector

default.stringsAsFactors <- function()
{
    val <- getOption("stringsAsFactors")
    if(is.null(val)) val <- TRUE
    if(!is.logical(val) || is.na(val) || length(val) != 1L)
        stop("options('stringsAsFactors') not set to TRUE or FALSE")
    val
}


as.data.frame.character <-
    function(x, ..., stringsAsFactors = default.stringsAsFactors())
    as.data.frame.vector(if(stringsAsFactors) factor(x) else x, ...)

as.data.frame.logical <- as.data.frame.vector

as.data.frame.matrix <- function(x, row.names = NULL, optional = FALSE, ...,
                                 stringsAsFactors = default.stringsAsFactors())
{
    d <- dim(x)
    nrows <- d[1L]; ir <- seq_len(nrows)
    ncols <- d[2L]; ic <- seq_len(ncols)
    dn <- dimnames(x)
    ## surely it cannot be right to override the supplied row.names?
    ## changed in 1.8.0
    if(is.null(row.names)) row.names <- dn[[1L]]
    collabs <- dn[[2L]]
    if(any(empty <- nchar(collabs) == 0L))
	collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if(mode(x) == "character" && stringsAsFactors) {
	for(i in ic)
	    value[[i]] <- as.factor(x[,i])
    } else {
	for(i in ic)
	    value[[i]] <- as.vector(x[,i])
    }
    if(length(row.names) != nrows)
	row.names <- .set_row_names(nrows)
    if(length(collabs) == ncols)
	names(value) <- collabs
    else if(!optional)
	names(value) <- paste("V", ic, sep="")
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.model.matrix <-
    function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    nrows <- d[1L]
    dn <- dimnames(x)
    row.names <- dn[[1L]]
    value <- list(x)
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != nrows)
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), nrows), domain = NA)
    }
    else row.names <- .set_row_names(nrows)
    if(!optional) names(value) <- deparse(substitute(x))[[1L]]
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.array <- function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    if(length(d) == 1L) { ## same as as.data.frame.vector, but deparsed here
        value <- as.data.frame.vector(drop(x), row.names, optional, ...)
        if(!optional) names(value) <- deparse(substitute(x))[[1L]]
        value
    } else if (length(d) == 2L) {
        as.data.frame.matrix(x, row.names, optional, ...)
    } else {
        dn <- dimnames(x)
        dim(x) <- c(d[1L], prod(d[-1]))
        if(!is.null(dn)) {
            if(length(dn[[1L]])) rownames(x) <- dn[[1L]]
            for(i in 2L:length(d))
                if(is.null(dn[[i]])) dn[[i]] <- seq_len(d[i])
            colnames(x) <- interaction(expand.grid(dn[-1]))
        }
        as.data.frame.matrix(x, row.names, optional, ...)
    }
}

## will always have a class here
"[.AsIs" <- function(x, i, ...) structure(NextMethod("["), class = class(x))

as.data.frame.AsIs <- function(x, row.names = NULL, optional = FALSE, ...)
{
    ## why not remove class and NextMethod here?
    if(length(dim(x)) == 2L)
	as.data.frame.model.matrix(x, row.names, optional)
    else { # as.data.frame.vector without removing names
        nrows <- length(x)
        nm <- paste(deparse(substitute(x), width.cutoff=500), collapse=" ")
        if(is.null(row.names)) {
            if (nrows == 0L)
                row.names <- character(0L)
            else if(length(row.names <- names(x)) == nrows &&
                    !any(duplicated(row.names))) {}
            else row.names <- .set_row_names(nrows)
        }
        value <- list(x)
        if(!optional) names(value) <- nm
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        value
    }

}

###  This is the real "data.frame".
###  It does everything by calling the methods presented above.

data.frame <-
    function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
             stringsAsFactors = default.stringsAsFactors())
{
    data.row.names <-
	if(check.rows && is.null(row.names))
	    function(current, new, i) {
		if(is.character(current)) new <- as.character(new)
		if(is.character(new)) current <- as.character(current)
		if(any(duplicated(new)))
		    return(current)
		if(is.null(current))
		    return(new)
		if(all(current == new) || all(current == ""))
		    return(new)
		stop(gettextf("mismatch of row names in arguments of 'data.frame\', item %d", i), domain = NA)
	    }
	else function(current, new, i) {
	    if(is.null(current)) {
		if(any(dup <- duplicated(new))) {
		    warning("some row.names duplicated: ",
                            paste(which(dup), collapse=","),
                            " --> row.names NOT used")
		    current
		} else new
	    } else current
	}
    object <- as.list(substitute(list(...)))[-1]
    mrn <- is.null(row.names) # missing or NULL
    x <- list(...)
    n <- length(x)
    if(n < 1L) {
        if(!mrn) {
            if(is.object(row.names) || !is.integer(row.names))
                row.names <- as.character(row.names)
            if(any(is.na(row.names)))
                stop("row names contain missing values")
            if(any(duplicated(row.names)))
                stop("duplicate row.names: ",
                     paste(unique(row.names[duplicated(row.names)]),
                           collapse = ", "))
        } else row.names <- integer(0)
	return(structure(list(), names = character(0L),
                         row.names = row.names,
			 class = "data.frame"))
    }
    vnames <- names(x)
    if(length(vnames) != n)
	vnames <- character(n)
    no.vn <- nchar(vnames) == 0L
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
	    if(no.vn[i]) vnames[[i]] <- namesi
	    else vnames[[i]] <- paste(vnames[[i]], namesi, sep=".")
	}
	else {
            if(length(namesi) > 0L) vnames[[i]] <- namesi
            else if (no.vn[[i]]) {
                tmpname <- deparse(object[[i]])[1L]
                if( substr(tmpname, 1L, 2L) == "I(" ) {
                    ntmpn <- nchar(tmpname)
                    if(substr(tmpname, ntmpn, ntmpn) == ")")
                        tmpname <- substr(tmpname, 3L, ntmpn - 1L)
                }
                vnames[[i]] <- tmpname
            }
        } # end of ncols[i] <= 1
	if(is.null(row.names) && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            ## old way to mark optional names
            if(!(rowsi[[1L]] %in% ""))
                row.names <- data.row.names(row.names, rowsi, i)
        }
        nrows[i] <- abs(nrows[i])
	vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for(i in seq_len(n)[nrows < nr]) {
	xi <- vlist[[i]]
	if(length(xi) == 1L && nrows[i] > 0L && nr %% nrows[i] == 0L) {
            xi1 <- xi[[1L]]
            if(is.vector(xi1) || is.factor(xi1)) {
                vlist[[i]] <- list(rep(xi1, length.out = nr))
                next
            }
            if(is.character(xi1) && class(xi1) == "AsIs") {
                ## simple char vectors only
                cl <- class(xi1) # `methods' adds a class -- Eh?
                vlist[[i]] <- list(structure(rep(xi1, length.out = nr), class=cl))
                next
            }
        }
	stop("arguments imply differing number of rows: ",
             paste(unique(nrows), collapse = ", "))
    }
    value <- unlist(vlist, recursive=FALSE, use.names=FALSE)
    ## unlist() drops i-th component if it has 0 columns
    vnames <- unlist(vnames[ncols > 0L])
    noname <- nchar(vnames) == 0L
    if(any(noname))
	vnames[noname] <- paste("Var", seq_along(vnames), sep = ".")[noname]
    if(check.names)
	vnames <- make.names(vnames, unique=TRUE)
    names(value) <- vnames
    if(!mrn) { # non-null row.names arg was supplied
        if(length(row.names) == 1L && nr != 1L) {  # one of the variables
            if(is.character(row.names))
                row.names <- match(row.names, vnames, 0L)
            if(length(row.names) != 1L ||
               row.names < 1L || row.names > length(vnames))
                stop("row.names should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[ - i]
        } else if ( !is.null(row.names) && length(row.names) != nr )
            stop("row names supplied are of the wrong length")
    } else if( !is.null(row.names) && length(row.names) != nr ) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    if(is.null(row.names))
        row.names <- .set_row_names(nr) #seq_len(nr)
    else {
        if(is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if(any(is.na(row.names)))
            stop("row names contain missing values")
        if(any(duplicated(row.names)))
            stop("duplicate row.names: ",
                 paste(unique(row.names[duplicated(row.names)]),
                       collapse = ", "))
    }
    attr(value, "row.names") <- row.names
    attr(value, "class") <- "data.frame"
    value
}


###  Subsetting and mutation methods
###  These are a little less general than S

"[.data.frame" <-
    function(x, i, j, drop = if(missing(i)) TRUE else length(cols) == 1)
{
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop  # number of arg from x,i,j that were specified

    if(Narg < 3) {  # list-like indexing or matrix indexing
        if(!mdrop) warning("drop argument will be ignored")
	if(missing(i)) return(x)
	if(is.matrix(i))
	    return(as.matrix(x)[i])  # desperate measures
	y <- NextMethod("[")
        cols <- names(y)
        ## zero-column data frames prior to 2.4.0 had no names.
	if(!is.null(cols) && any(is.na(cols)))
            stop("undefined columns selected")
        ## added in 1.8.0
        if(any(duplicated(cols))) names(y) <- make.unique(cols)
        ## since we have not touched the rows, copy over the raw row.names
	return(structure(y, class = oldClass(x),
                         row.names = .row_names_info(x, 0L)))
    }

    if(missing(i)) { # df[, j] or df[ , ]
        ## not quite the same as the 1/2-arg case, as 'drop' is used.
        if(missing(j) && drop && length(x) == 1L) return(.subset2(x, 1L))
        y <- if(missing(j)) x else .subset(x, j)
        if(drop && length(y) == 1L) return(.subset2(y, 1L))
	cols <- names(y)
	if(any(is.na(cols))) stop("undefined columns selected")
        if(any(duplicated(cols))) names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if(drop && !mdrop && nrow == 1L)
            return(structure(y, class = NULL, row.names = NULL))
        else
            return(structure(y, class = oldClass(x),
                             row.names = .row_names_info(x, 0L)))
    }

    ### df[i, j] or df[i , ]
    ## rewritten for R 2.5.0 to avoid duplicating x.
    xx <- x
    cols <- names(xx)  # needed for 'drop'
    ## make a shallow copy
    x <- vector("list", length(x))
    ## attributes(x) <- attributes(xx) expands row names
    x <- .Call("R_copyDFattr", xx, x, PACKAGE="base")
    oldClass(x) <- attr(x, "row.names") <- NULL

    if(!missing(j)) { # df[i, j]
        x <- x[j]
        cols <- names(x)  # also needed for 'drop'
        if(any(is.na(cols))) stop("undefined columns selected")
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
	if((ina <- any(is.na(rows))) | (dup <- any(duplicated(rows)))) {
	    ## both will coerce integer 'rows' to character:
	    if(ina)
		rows[is.na(rows)] <- "NA"
	    if(dup)
		rows <- make.unique(as.character(rows))
	}
        ## new in 1.8.0  -- might have duplicate columns
        if(any(duplicated(nm <- names(x)))) names(x) <- make.unique(nm)
        if(is.null(rows)) rows <- attr(xx, "row.names")[i]
	attr(x, "row.names") <- rows
	oldClass(x) <- oldClass(xx)
    }
    x
}

"[[.data.frame" <- function(x, ...)
{
    ## use in-line functions to refer to the 1st and 2nd ... arguments
    ## explicitly. Also will check for wrong number or empty args
    if(nargs() < 3)
	(function(x, i)
	  if(is.matrix(i)) as.matrix(x)[[i]]
 	  else .subset2(x, i))(x, ...)
    else
        .subset2(.subset2(x, ..2), ..1)
}

"[<-.data.frame" <- function(x, i, j, value)
{
    nA <- nargs() # value is never missing, so 3 or 4.
    if(nA == 4) { ## df[,] or df[i,] or df[, j] or df[i,j]
	has.i <- !missing(i)
	has.j <- !missing(j)
    }
    else if(nA == 3) {
        ## this collects both df[] and df[ind]
        if(is.atomic(value)) names(value) <- NULL
        if(missing(i) && missing(j)) { # case df[]
            i <- j <- NULL
            has.i <- has.j <- FALSE
            ## added in 1.8.0
            if(is.null(value)) return(x[logical(0L)])
        } else { # case df[ind]
            ## really ambiguous, but follow common use as if list
            ## except for a full-sized logical matrix
            if(is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm=TRUE)
                if(!nreplace) return(x) # nothing to replace
                ## allow replication of length(value) > 1 in 1.8.0
                N <- length(value)
                if(N > 1L && N < nreplace && (nreplace %% N) == 0L)
                    value <- rep(value, length.out = nreplace)
                if(N > 1L && (length(value) != nreplace))
                    stop("rhs is the wrong length for indexing by a logical matrix")
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
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else {
	stop("need 0, 1, or 2 subscripts")
    }
    ## no columns specified
    if(has.j && length(j) == 0L) return(x)

    cl <- oldClass(x)
    ## delete class: S3 idiom to avoid any special methods for [[, etc
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if(has.i) { # df[i, ] or df[i, j]
        rows <- NULL  # indicator that it is not yet set
        if(any(is.na(i)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
	    ii <- match(i, rows)
	    nextra <- sum(new.rows <- is.na(ii))
	    if(nextra > 0L) {
		ii[new.rows] <- seq.int(from = nrows + 1L, length = nextra)
		new.rows <- i[new.rows]
	    }
	    i <- ii
	}
	if(all(i >= 0L) && (nn <- max(i)) > nrows) {
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
	if(any(is.na(iseq))) stop("non-existent rows not allowed")
    }
    else iseq <- NULL

    if(has.j) {
        if(any(is.na(j)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(is.character(j)) {
	    jj <- match(j, names(x))
	    nnew <- sum(is.na(jj))
	    if(nnew > 0L) {
		n <- is.na(jj)
		jj[n] <- nvars + seq_len(nnew)
		new.cols <- j[n]
	    }
	    jseq <- jj
	}
	else if(is.logical(j) || min(j) < 0L)
	    jseq <- seq_along(x)[j]
	else {
	    jseq <- j
	    if(max(jseq) > nvars) {
		new.cols <- paste("V", seq.int(from = nvars + 1L, to = max(jseq)),
                                  sep = "")
		if(length(new.cols)  != sum(jseq > nvars))
		    stop("new columns would leave holes after existing columns")
                ## try to use the names of a list `value'
                if(is.list(value) && !is.null(vnm <- names(value))) {
                    p <- length(jseq)
                    if(length(vnm) < p) vnm <- rep(vnm, length.out = p)
                    new.cols <- vnm[jseq > nvars]
                }
	    }
	}
    }
    else jseq <- seq_along(x)

    ## addition in 1.8.0
    if(any(duplicated(jseq)))
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if(n == 0L) n <- nrows
    p <- length(jseq)
    m <- length(value)
    if(!is.list(value)) {
        if(p == 1L) {
            N <- NROW(value)
            if(N > n)
                stop(gettextf("replacement has %d rows, data has %d", N, n),
                     domain = NA)
            if(N < n && N > 0L)
                if(n %% N == 0L && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = n)
                else
                    stop(gettextf("replacement has %d rows, data has %d", N, n),
                         domain = NA)
            names(value) <- NULL
            value <- list(value)
         } else {
            if(m < n*p && (m == 0L || (n*p) %% m))
                stop(gettextf("replacement has %d items, need %d", m, n*p),
                     domain = NA)
            value <- matrix(value, n, p)  ## will recycle
            value <- split(value, col(value))
        }
	dimv <- c(n, p)
    } else { # a list
        ## careful, as.data.frame turns things into factors.
	## value <- as.data.frame(value)
        value <- unclass(value) # to avoid data frame indexing
        lens <- sapply(value, NROW)
        for(k in seq_along(lens)) {
            N <- lens[k]
            if(n != N && length(dim(value[[k]])) == 2L)
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", k, N, n),
                     domain = NA)
            if(N > 0L && N < n && n %% N)
                stop(gettextf("replacement element %d has %d rows, need %d",
                              k, N, n), domain = NA)
            ## these fixing-ups will not work for matrices
            if(N > 0L && N < n) value[[k]] <- rep(value[[k]], length.out = n)
            if(N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows",
                                 k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
	dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if(nrowv < n && nrowv > 0L) {
	if(n %% nrowv == 0L)
	    value <- value[rep(seq_len(nrowv), length.out = n),,drop = FALSE]
	else stop(gettextf("%d rows in value to replace %d rows", nrowv, n),
                  domain = NA)
    }
    else if(nrowv > n)
	warning(gettextf("replacement data has %d rows to replace %d rows",
                         nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
    if(ncolv < p) jvseq <- rep(seq_len(ncolv), length.out = p)
    else if(ncolv > p)
	warning(gettextf("provided %d variables to replace %d variables",
                         ncolv, p), domain = NA)
    if(length(new.cols)) {
        ## extend and name now, as assignment of NULL may delete cols later.
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        x <- c(x, vector("list", length(new.cols)))
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
    }
    if(has.i)
	for(jjj in seq_len(p)) {
	    jj <- jseq[jjj]
	    vjj <- value[[ jvseq[[jjj]] ]]
            if(jj <= nvars) {
                ## if a column exists, preserve its attributes
                if(length(dim(x[[jj]])) != 2L) x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            } else {
                ## try to make a new column match in length: may be an error
                x[[jj]] <- vjj[FALSE]
                if(length(dim(vjj)) == 2L) {
                    length(x[[j]]) <- nrows * ncol(vjj)
                    dim(x[[j]]) <- c(nrows, ncol(vjj))
                    x[[jj]][iseq, ] <- vjj
                } else {
                    length(x[[j]]) <- nrows
                    x[[jj]][iseq] <- vjj
                }
            }
	}
    else if(p > 0L) for(jjj in p:1L) { # we might delete columns with NULL
	jj <- jseq[jjj]
        v <- value[[ jvseq[[jjj]] ]]
	x[[jj]] <- v
        if(!is.null(v) && is.atomic(x[[jj]])) names(x[[jj]]) <- NULL
    }
    if(length(new.cols) > 0L) {
        new.cols <- names(x) # we might delete columns with NULL
        ## added in 1.8.0
        if(any(duplicated(new.cols))) names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x
}

"[[<-.data.frame"<- function(x, i, j, value)
{
    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[<-
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if(is.atomic(value)) names(value) <- NULL
    if(nargs() < 4) {
	## really ambiguous, but follow common use as if list
        nc <- length(x)
	if(!is.null(value)) {
            N <- NROW(value)
            if(N > nrows)
                stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                     domain = NA)
            if(N < nrows && N > 0L)
                if(nrows %% N == 0L && length(dim(value)) <= 1L)
                    value <- rep(value, length.out = nrows)
                else
                    stop(gettextf("replacement has %d rows, data has %d",
                                  N, nrows), domain = NA)
	}
	x[[i]] <- value
        ## added in 1.8.0 -- make sure there is a name
        if(length(x) > nc) {
            nc <- length(x)
            if(names(x)[nc] == "") names(x)[nc] <- paste("V", nc, sep="")
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
	    ii[new.rows] <- seq.int(from = nrows + 1L, length = n)
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
    if(any(is.na(iseq)))
	stop("non-existent rows not allowed")

    if(is.character(j)) {
	jseq <- match(j, names(x))
	if(any(is.na(jseq)))
	    stop("replacing element in non-existent column: ",
                 j[is.na(jseq)])
    }
    else if(is.logical(j) || min(j) < 0L)
	jseq <- seq_along(x)[j]
    else {
	jseq <- j
	if(max(jseq) > nvars)
	    stop("replacing element in non-existent column: ",
                 jseq[jseq>nvars])
    }
    if(length(iseq) > 1L || length(jseq) > 1L)
	stop("only a single element should be replaced")
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
}

## added in 1.8.0
"$<-.data.frame"<- function(x, i, value)
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
            stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                 domain = NA)
        if(N < nrows && N > 0L)
            if(nrows %% N == 0L && length(dim(value)) <= 1L)
                value <- rep(value, length.out = nrows)
            else
                stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                     domain = NA)
        if(is.atomic(value)) names(value) <- NULL
    }
    x[[i]] <- value
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
    attr(x, "row.names") <- c(old.rows, new.rows)
    x
}


### Here are the methods for rbind and cbind.

cbind.data.frame <- function(..., deparse.level = 1)
    data.frame(..., check.names = FALSE)

rbind.data.frame <- function(..., deparse.level = 1)
{
    match.names <- function(clabs, nmi)
    {
	if(identical(clabs, nmi)) NULL
	else if(length(nmi) == length(clabs) && all(match(nmi, clabs, 0L))) {
            ## we need unique matches here
	    m <- pmatch(nmi, clabs, 0L)
            if(any(m == 0L))
                stop("names do not match previous names")
            m
	} else stop("names do not match previous names")
    }
    Make.row.names <- function(nmi, ri, ni, nrow)
    {
	if(nchar(nmi) > 0L) {
            if(ni == 0L) character(0L)  # PR8506
	    else if(ni > 1L) paste(nmi, ri, sep = ".")
	    else nmi
	}
	else if(nrow > 0L && identical(ri, seq_len(ni)))
	    as.integer(seq.int(from = nrow + 1L, length = ni))
	else ri
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0L]
    if(length(allargs)) {
    ## drop any zero-row data frames, as they may not have proper column
    ## types (e.g. NULL).
        nr <- sapply(allargs, function(x)
                     if(is.data.frame(x)) .row_names_info(x, 2L)
                     else if(is.list(x)) length(x[[1]]) # mismatched lists are checked later
                     else length(x))
        if(any(nr > 0L)) allargs <- allargs[nr > 0L]
        else return(allargs[[1]]) # pretty arbitrary
    }
    n <- length(allargs)
    if(n == 0L)
	return(structure(list(),
			 class = "data.frame",
			 row.names = integer()))
    nms <- names(allargs)
    if(is.null(nms))
	nms <- character(length(allargs))
    cl <- NULL
    perm <- rows <- rlabs <- vector("list", n)
    nrow <- 0L
    value <- clabs <- NULL
    all.levs <- list()
    for(i in seq_len(n)) {
	## check the arguments, develop row and column labels
	xi <- allargs[[i]]
	nmi <- nms[i]
        ## coerce matrix to data frame
        if(is.matrix(xi)) allargs[[i]] <- xi <- as.data.frame(xi)
	if(inherits(xi, "data.frame")) {
	    if(is.null(cl))
		cl <- oldClass(xi)
	    ri <- attr(xi, "row.names")
	    ni <- length(ri)
	    if(is.null(clabs))
		clabs <- names(xi)
	    else {
		pi <- match.names(clabs, names(xi))
		if( !is.null(pi) )
		    perm[[i]] <- pi
	    }
	    rows[[i]] <- seq.int(from = nrow + 1L, length = ni)
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    nrow <- nrow + ni
	    if(is.null(value)) {
		value <- unclass(xi)
		nvar <- length(value)
		all.levs <- vector("list", nvar)
		has.dim <- logical(nvar)
                facCol <- logical(nvar)
                ordCol <- logical(nvar)
		for(j in seq_len(nvar)) {
		    xj <- value[[j]]
		    if( !is.null(levels(xj)) ) {
			all.levs[[j]] <- levels(xj)
                        facCol[j] <- TRUE # turn categories into factors
                    } else facCol[j] <- is.factor(xj)
                    ordCol[j] <- is.ordered(xj)
		    has.dim[j] <- length(dim(xj)) == 2L
		}
	    }
	    else for(j in seq_len(nvar)) {
                xij <- xi[[j]]
                if(is.null(pi) || is.na(jj <- pi[[j]])) jj <- j
                if(facCol[jj]) {
                    if(length(lij <- levels(xij)) > 0L) {
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], lij))
                        ordCol[jj] <- ordCol[jj] & is.ordered(xij)
                    } else if(is.character(xij))
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], xij))
                }
            }
	}
	else if(is.list(xi)) {
	    ni <- range(sapply(xi, length))
	    if(ni[1L] == ni[2L])
		ni <- ni[1L]
	    else stop("invalid list argument: all variables should have the same length")
	    rows[[i]] <- ri <-
                as.integer(seq.int(from = nrow + 1L, length = ni))
	    nrow <- nrow + ni
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    if(length(nmi <- names(xi)) > 0L) {
		if(is.null(clabs))
		    clabs <- nmi
		else {
		    tmp<-match.names(clabs, nmi)
		    if( !is.null(tmp) )
			perm[[i]] <- tmp
		}
	    }
	}
	else if(length(xi) > 0L) {
	    rows[[i]] <- nrow <- nrow + 1L
	    rlabs[[i]] <- if(nchar(nmi) > 0L) nmi else as.integer(nrow)
	}
    }
    nvar <- length(clabs)
    if(nvar == 0L)
	nvar <- max(sapply(allargs, length))	# only vector args
    if(nvar == 0L)
	return(structure(list(), class = "data.frame",
			 row.names = integer()))
    pseq <- seq_len(nvar)
    if(is.null(value)) { # this happens if there has been no data frame
	value <- list()
	value[pseq] <- list(logical(nrow)) # OK for coercion except to raw.
        all.levs <- vector("list", nvar)
        has.dim <- logical(nvar)
        facCol <- logical(nvar)
        ordCol <- logical(nvar)
    }
    names(value) <- clabs
    for(j in pseq)
	if(length(lij <- all.levs[[j]]) > 0L)
            value[[j]] <-
                factor(as.vector(value[[j]]), lij, ordered = ordCol[j])
    if(any(has.dim)) {
	rmax <- max(unlist(rows))
	for(i in pseq[has.dim])
	    if(!inherits(xi <- value[[i]], "data.frame")) {
		dn <- dimnames(xi)
		rn <- dn[[1L]]
		if(length(rn) > 0L) length(rn) <- rmax
		pi <- dim(xi)[2L]
		length(xi) <- rmax * pi
		value[[i]] <- array(xi, c(rmax, pi), list(rn, dn[[2L]]))
	    }
    }
    for(i in seq_len(n)) {
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
                rownames(value[[jj]])[ri] <- rownames(xij)
	    } else {
                ## coerce factors to vectors, in case lhs is character or
                ## level set has changed
                value[[jj]][ri] <- if(is.factor(xij)) as.vector(xij) else xij
                ## copy names if any
                if(!is.null(nm <- names(xij))) names(value[[jj]])[ri] <- nm
            }
	}
    }
    rlabs <- unlist(rlabs)
    if(any(duplicated(rlabs)))
        rlabs <- make.unique(as.character(unlist(rlabs)), sep = "")
    if(is.null(cl)) {
	as.data.frame(value, row.names = rlabs)
    } else {
	class(value) <- cl
	attr(value, "row.names") <- rlabs
	value
    }
}


### coercion and print methods

print.data.frame <-
    function(x, ..., digits = NULL, quote = FALSE, right = TRUE)
{
    if(length(x) == 0L) {
	cat("NULL data frame with", length(row.names(x)), "rows\n")
    } else if(length(row.names(x)) == 0L) {
	print.default(names(x), quote = FALSE)
	cat("<0 rows> (or 0-length row.names)\n")
    } else {
	## avoiding picking up e.g. format.AsIs
	print(as.matrix(format.data.frame(x, digits=digits, na.encode=FALSE)),
              ..., quote = quote, right = right)
    }
    invisible(x)
}

as.matrix.data.frame <- function (x, rownames.force = NA, ...)
{
    dm <- dim(x)
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(x)
    else {if(.row_names_info(x) <= 0) NULL else row.names(x)}
    dn <- list(rn, names(x))
    if(any(dm == 0L))
	return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2L]
    pseq <- seq_len(p)
    n <- dm[1L]
    collabs <- as.list(dn[[2L]])
    X <- x # will contain the result;
    ## the "big question" is if we return a numeric or a character matrix
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in pseq) {
	xj <- X[[j]]
	if(length(dj <- dim(xj)) == 2L && dj[2L] > 1L) {# matrix with >=2 col
	    if(inherits(xj, "data.frame"))
		xj <- X[[j]] <- as.matrix(X[[j]])
	    dnj <- dimnames(xj)[[2L]]
	    collabs[[j]] <- paste(collabs[[j]],
				  if(length(dnj) > 0L) dnj else seq_len(dj[2L]),
				  sep = ".")
	}
        j.logic <- is.logical(xj)
        if(all.logical && !j.logic) all.logical <- FALSE
	if(length(levels(xj)) > 0L || !(j.logic || is.numeric(xj) || is.complex(xj))
	   || (!is.null(cl <- attr(xj, "class")) && # numeric classed objects to format:
	       any(cl %in% c("Date", "POSIXct", "POSIXlt"))))
	    non.numeric <- TRUE
	if(!is.atomic(xj))
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
	    xj <- X[[j]]
            miss <- is.na(xj)
	    xj <- if(length(levels(xj))) as.vector(xj) else format(xj)
            is.na(xj) <- miss
            X[[j]] <- xj
	}
    }
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(dn[[1L]], unlist(collabs, use.names = FALSE))
    ##NO! don't copy buggy S-plus!  either all matrices have class or none!!
    ##NO class(X) <- "matrix"
    X
}

Math.data.frame <- function (x, ...)
{
    f <- get(.Generic, mode = "function")
    if (is.null(formals(f)))
	f <- function(x, ...) {
	}
    call <- match.call(f, sys.call())
    call[[1L]] <- as.name(.Generic)
    arg <- names(formals(f))[1L]
    call[[arg]] <- as.name("xx")
    encl <- parent.frame()
    var.f <- function(x) eval(call, list(xx = x), encl)
    mode.ok <- sapply(x, is.numeric) & !sapply(x, is.factor) |
	sapply(x, is.complex)
    if (all(mode.ok)) {
	r <- lapply(x, var.f)
	class(r) <- oldClass(x)
	attr(r, "row.names") <- attr(x, "row.names")
	return(r)
    }
    else {
	vnames <- names(x)
	if (is.null(vnames)) vnames <- seq_along(x)
	stop("non-numeric variable in data frame: ", vnames[!mode.ok])
    }
}

Ops.data.frame <- function(e1, e2 = NULL)
{
    isList <- function(x) !is.null(x) && is.list(x)
    unary <- nargs() == 1L
    lclass <- nchar(.Method[1L]) > 0L
    rclass <- !unary && (nchar(.Method[2L]) > 0L)
    value <- list()
    rn <- NULL
    ## set up call as op(left, right)
    FUN <- get(.Generic, envir = parent.frame(), mode = "function")
    f <- if (unary) quote(FUN(left)) else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if(lclass && rclass) {
        nr <- .row_names_info(e1, 2L)
	if(.row_names_info(e1) > 0L) rn <- attr(e1, "row.names")
	cn <- names(e1)
	if(any(dim(e2) != dim(e1)))
	    stop(.Generic, " only defined for equally-sized data frames")
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
		e2 <- split(rep(as.vector(e2), length.out = prod(dim(e1))),
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
		e1 <- split(rep(as.vector(e1), length.out = prod(dim(e2))),
			    rep.int(seq_len(ncol(e2)),
                                    rep.int(nrow(e2), ncol(e2))))
	}
    }
    for(j in seq_along(cn)) {
	left <- if(!lscalar) e1[[j]] else e1
	right <-if(!rscalar) e2[[j]] else e2
	value[[j]] <- eval(f)
    }
    if(.Generic %in% c("+","-","*","/","%%","%/%") ) {
	names(value) <- cn
	data.frame(value, row.names = rn, check.names = FALSE,
                   check.rows = FALSE)
    }
    else matrix(unlist(value, recursive = FALSE, use.names = FALSE),
		nrow = nr, dimnames = list(rn,cn))
}

Summary.data.frame <- function(..., na.rm)
{
    args <- list(...)
    args <- lapply(args, function(x) {
        x <- as.matrix(x)
        if(!is.numeric(x) && !is.complex(x))
            stop("only defined on a data frame with all numeric or complex variables")
        x
    })
    do.call(.Generic, c(args, na.rm=na.rm))
}
