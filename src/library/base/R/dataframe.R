## As from R 2.4.0, row.names can be either character or integer.
## row.names() will always return character.
## attr(, "row.names") will return either character or integer.
##
## Do not assume that the internal representation is either, since
## 1:n is stored as the integer vector c(NA, n) to save space (and
## the C-level code to get/set the attribute makes the appropriate
## translations (whereas attributes does not).
## .Call("R_shortRowNames", x) gives +nrow(x) internal c(NA, n) is used;
##                                   -nrow(.) otherwise.
## Consider changing the C function name, and switching the sign of the result

row.names <- function(x) UseMethod("row.names")
row.names.data.frame <- function(x) as.character(attr(x, "row.names"))
row.names.default <- function(x) if(!is.null(dim(x))) rownames(x)# else NULL

"row.names<-" <- function(x, value) UseMethod("row.names<-")
"row.names<-.data.frame" <- function(x, value) {
    if (!is.data.frame(x))
	x <- as.data.frame(x)
    n <- abs(.Call("R_shortRowNames", x, PACKAGE = "base"))
    if(is.null(value)) { # set integer row.names
        attr(x, "row.names") <-
            if(n > 2) c(as.integer(NA), n) else seq_len(n)
        return(x)
    }
    ## do this here, as e.g. POSIXlt changes length when coerced.
    if(is.object(value) || !(is.integer(value)) )
        value <- as.character(value)
    if (n != 0 && length(value) != n)
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
    y <- do.call("cbind", lapply(x, "is.na"))
    rownames(y) <- row.names(x)
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

dim.data.frame <- function(x)
    c(abs(.Call("R_shortRowNames", x, PACKAGE = "base")), length(x))

if(FALSE) # Martin's proposal
dimnames.data.frame <- function(x, maybeNULL = FALSE, ...)
    list(if(maybeNULL && .Call("R_shortRowNames", x, PACKAGE = "base") >= 0)
         NULL else row.names(x),
	 names(x))
## an alternative which leaves dimnames() generic with one single argument:
dimnames.data.frame <- function(x) list(row.names(x), names(x))


"dimnames<-.data.frame" <- function(x, value)
{
    d <- dim(x)
    if(!is.list(value) || length(value) != 2)
	stop("invalid 'dimnames' given for data frame")
    ## do the coercion first, as might change length
    value[[1]] <- as.character(value[[1]])
    value[[2]] <- as.character(value[[2]])
    if(d[[1]] != length(value[[1]]) || d[[2]] != length(value[[2]]))
	stop("invalid 'dimnames' given for data frame")
    row.names(x) <- value[[1]] # checks validity
    names(x) <- value[[2]]
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
    if(i > 1)
	class(x) <- cl[ - (1:(i-1))]
    if(!is.null(row.names)){
	if(length(row.names) == length(attr(x, "row.names")))
	    attr(x, "row.names") <- row.names
	else stop(gettextf("invalid 'row.names', length %d for a data frame with %d rows",
                           length(row.names), length(attr(x, "row.names"))),
                  domain = NA)
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
               cn, 0)
    if(any(m > 0)) {
        cn[m] <- paste("..adfl.", cn[m], sep="")
        names(x) <- cn
    }
    x <- eval(as.call(c(expression(data.frame), x, check.names = !optional,
                        stringsAsFactors = stringsAsFactors)))
    if(any(m > 0)) names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
    if(!is.null(row.names)) {
	# row.names <- as.character(row.names)
	if(length(row.names) != dim(x)[[1]])
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), dim(x)[[1]]), domain = NA)
	attr(x, "row.names") <- row.names
    }
    x
}

as.data.frame.vector <- function(x, row.names = NULL, optional = FALSE, ...)
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse=" ")
    if(is.null(row.names)) {
	if (nrows == 0)
	    row.names <- character(0)
	else if(length(row.names <- names(x)) == nrows &&
		!any(duplicated(row.names))) {}
	else if(optional) row.names <- character(nrows)
	else row.names <- seq_len(nrows)
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
    if(!is.logical(val) || is.na(val) || length(val) != 1)
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
    nrows <- d[1]; ir <- seq_len(nrows)
    ncols <- d[2]; ic <- seq_len(ncols)
    dn <- dimnames(x)
    ## surely it cannot be right to override the supplied row.names?
    ## changed in 1.8.0
    if(is.null(row.names)) row.names <- dn[[1]]
    collabs <- dn[[2]]
    if(any(empty <- nchar(collabs)==0))
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
	row.names <- if(optional) character(nrows) else ir
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
    nrows <- d[1]
    dn <- dimnames(x)
    row.names <- dn[[1]]
    value <- list(x)
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != nrows)
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), nrows), domain = NA)
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- seq_len(nrows)
    if(!optional) names(value) <- deparse(substitute(x))[[1]]
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.array <- function(x, row.names = NULL, optional = FALSE, ...)
{
    d <- dim(x)
    if(length(d) == 1) { ## same as as.data.frame.vector, but deparsed here
        value <- as.data.frame.vector(drop(x), row.names, optional, ...)
        if(!optional) names(value) <- deparse(substitute(x))[[1]]
        value
    } else if (length(d) == 2) {
        as.data.frame.matrix(x, row.names, optional, ...)
    } else {
        dn <- dimnames(x)
        dim(x) <- c(d[1], prod(d[-1]))
        if(!is.null(dn)) {
            if(length(dn[[1]])) rownames(x) <- dn[[1]]
            for(i in 2:length(d))
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
    if(length(dim(x))==2)
	as.data.frame.model.matrix(x, row.names, optional)
    else { # as.data.frame.vector without removing names
        nrows <- length(x)
        nm <- paste(deparse(substitute(x), width.cutoff=500), collapse=" ")
        if(is.null(row.names)) {
            if (nrows == 0)
                row.names <- character(0)
            else if(length(row.names <- names(x)) == nrows &&
                    !any(duplicated(row.names))) {}
            else if(optional) row.names <- character(nrows)
            else row.names <- seq_len(nrows)
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
	if(check.rows && missing(row.names))
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
    mrn <- missing(row.names)
    x <- list(...)
    n <- length(x)
    if(n < 1)
	return(structure(list(), names = character(0),
                         row.names = character(0),
			 class = "data.frame"))
    vnames <- names(x)
    if(length(vnames) != n)
	vnames <- character(n)
    no.vn <- nchar(vnames) == 0
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for(i in 1:n) {
        ## do it this way until all as.data.frame methods have been updated
	xi <- if(is.character(x[[i]]) || is.list(x[[i]]))
                 as.data.frame(x[[i]], optional = TRUE,
                               stringsAsFactors = stringsAsFactors)
        else as.data.frame(x[[i]], optional = TRUE)

	rowsi <- attr(xi, "row.names")
	ncols[i] <- length(xi)
	namesi <- names(xi)
	if(ncols[i] > 1) {
	    if(length(namesi) == 0) namesi <- 1:ncols[i]
	    if(no.vn[i]) vnames[[i]] <- namesi
	    else vnames[[i]] <- paste(vnames[[i]], namesi, sep=".")
	}
	else {
            if(length(namesi) > 0) vnames[[i]] <- namesi
            else if (no.vn[[i]]) {
                tmpname <- deparse(object[[i]])[1]
                if( substr(tmpname,1,2) == "I(" ) {
                    ntmpn <- nchar(tmpname)
                    if(substr(tmpname, ntmpn, ntmpn) == ")")
                        tmpname <- substr(tmpname,3,ntmpn-1)
                }
                vnames[[i]] <- tmpname
            }
        } # end of ncols[i] <= 1
	nrows[i] <- length(rowsi)
	if(missing(row.names) && (nrows[i] > 0) && !(rowsi[[1]] %in% ""))
	    row.names <- data.row.names(row.names, rowsi, i)
	vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for(i in (1:n)[nrows < nr]) {
	xi <- vlist[[i]]
	if(length(xi)==1 && nrows[i] > 0 && nr%%nrows[i]==0) {
            xi1 <- xi[[1]]
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
    vnames <- unlist(vnames[ncols > 0])
    noname <- nchar(vnames) == 0
    if(any(noname))
	vnames[noname] <- paste("Var", 1:length(vnames), sep = ".")[noname]
    if(check.names)
	vnames <- make.names(vnames, unique=TRUE)
    names(value) <- vnames
    if(!mrn) { # row.names arg was supplied
        if(length(row.names) == 1 && nr != 1) {  # one of the variables
            if(is.character(row.names))
                row.names <- match(row.names, vnames, 0)
            if(length(row.names)!=1 ||
               row.names < 1 || row.names > length(vnames))
                stop("row.names should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[ - i]
        } else if (length(row.names) > 0 && length(row.names) != nr)
            stop("row names supplied are of the wrong length")
    } else if(length(row.names) > 0 && length(row.names) != nr) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    if(length(row.names) == 0) row.names <- seq_len(nr)
    else if(is.object(row.names) || !is.integer(row.names))
        row.names <- as.character(row.names)
    if(any(is.na(row.names)))
        stop("row names contain missing values")
    if(any(duplicated(row.names)))
	stop("duplicate row.names: ",
             paste(unique(row.names[duplicated(row.names)]), collapse = ", "))
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
	if(missing(i))
	    return(x)
	if(is.matrix(i))
	    return(as.matrix(x)[i])  # desperate measures
	y <- NextMethod("[")
        nm <- names(y)
        ## zero-column data frames prior to 2.4.0 had no names.
	if(!is.null(nm) && any(is.na(nm))) stop("undefined columns selected")
        ## added in 1.8.0
        if(any(duplicated(nm))) names(y) <- make.unique(nm)
	return(structure(y, class = oldClass(x),
                         row.names = attr(x, "row.names")))
    }

    ## preserve the attributes for later use ...

### FIXME: memory-hog for large x with trivial rownames
    rows <- attr(x, "row.names")
    cols <- names(x)
    cl <- oldClass(x) # doesn't really matter unless called directly
    class(x) <- attr(x, "row.names") <- NULL


    if(missing(i)) { # df[, j] or df[ , ]
        ## handle the column only subsetting ...
        if(!missing(j)) x <- x[j]
	cols <- names(x)
	if(any(is.na(cols))) stop("undefined columns selected")
    }
    else { # df[i, j] or df[i , ]
	if(is.character(i))
	    i <- pmatch(i, as.character(rows), duplicates.ok = TRUE)
	rows <- rows[i]
	if(!missing(j)) { # df[i, j]
	    x <- x[j]
	    cols <- names(x)
	    if(any(is.na(cols))) stop("undefined columns selected")
	}
	for(j in seq_along(x)) {
	    xj <- x[[j]]
            ## had drop = drop prior to 1.8.0
	    x[[j]] <- if(length(dim(xj)) != 2) xj[i] else xj[i, , drop = FALSE]
	}
    }
    if(drop) {
	n <- length(x)
	if(n == 1) {
	    x <- x[[1]]
	    drop <- TRUE
	}
	else if(n > 1) {
	    xj <- x[[1]]
	    nrow <- if(length(dim(xj)) == 2) dim(xj)[1] else length(xj)
            ## for consistency with S: don't drop (to a list)
            ## if only one row unless explicitly asked for
            drop <- !mdrop && nrow == 1
            if(drop) {
		names(x) <- cols
		attr(x, "row.names") <- NULL
	    }
	} else drop <- FALSE ## for n == 0
    }
    if(!drop) { # not else as previous section might reset drop
	names(x) <- cols
        ## row names might have NAs.
	if((ina <- any(is.na(rows))) | (dup <- any(duplicated(rows)))) {
	    ## both will coerce integer 'rows' to character:
	    if(ina)
		rows[is.na(rows)] <- "NA"
	    if(dup)
		rows <- make.unique(as.character(rows))
	}
        ## new in 1.8.0  -- might have duplicate columns
        if(any(duplicated(nm <- names(x)))) names(x) <- make.unique(nm)
	attr(x, "row.names") <- rows
	class(x) <- cl
    }
    x
}

"[[.data.frame" <- function(x, ...)
{
    ## use in-line functions to refer to the 1st and 2nd ... arguments
    ## explicitly. Also will check for wrong number or empty args
    if(nargs() < 3)
	(function(x, i)
	  if(is.matrix(i))
	  as.matrix(x)[[i]]
 	  else .subset2(x,i))(x, ...)
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
            if(is.null(value)) return(x[logical(0)])
        } else { # case df[ind]
            ## really ambiguous, but follow common use as if list
            ## except for a full-sized logical matrix
            if(is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm=TRUE)
                if(!nreplace) return(x) # nothing to replace
                ## allow replication of length(value) > 1 in 1.8.0
                N <- length(value)
                if(N > 1 && N < nreplace && (nreplace %% N) == 0)
                    value <- rep(value, length.out = nreplace)
                if(N > 1 && (length(value) != nreplace))
                    stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0
                nv <- nrow(x)
                for(v in seq_len(dim(i)[2])) {
                    thisvar <- i[, v, drop = TRUE]
                    nv <- sum(thisvar, na.rm = TRUE)
                    if(nv) {
                        if(is.matrix(x[[v]]))
                            x[[v]][thisvar, ] <- if(N > 1) value[n+(1:nv)] else value
                        else
                            x[[v]][thisvar] <- if(N > 1) value[n+(1:nv)] else value
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
    if(has.j && length(j) ==0) return(x)

    cl <- oldClass(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[, etc
    class(x) <- NULL
    rows <- attr(x, "row.names")
    new.cols <- NULL
    nvars <- length(x)
    nrows <- length(rows)
    if(has.i) { # df[i, ] or df[i, j]
        if(any(is.na(i)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(char.i <- is.character(i)) {
	    ii <- match(i, rows)
	    nextra <- sum(new.rows <- is.na(ii))
	    if(nextra > 0) {
		ii[new.rows] <- seq.int(from = nrows + 1, length = nextra)
		new.rows <- i[new.rows]
	    }
	    i <- ii
	}
	if(all(i >= 0) && (nn <- max(i)) > nrows) {
	    ## expand
	    if(!char.i) {
		nrr <- as.character((nrows + 1):nn)
		if(inherits(value, "data.frame") &&
		   (dim(value)[1]) >= length(nrr)) {
		    new.rows <- attr(value, "row.names")[1:length(nrr)]
		    repl <- duplicated(new.rows) | match(new.rows, rows, 0)
		    if(any(repl))
			new.rows[repl] <- nrr[repl]
		}
		else new.rows <- nrr
	    }
	    x <- xpdrows.data.frame(x, rows, new.rows)
	    rows <- attr(x, "row.names")
	    nrows <- length(rows)
	}
	iseq <- seq_along(rows)[i]
	if(any(is.na(iseq)))
	    stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if(has.j) {
        if(any(is.na(j)))
            stop("missing values are not allowed in subscripted assignments of data frames")
	if(is.character(j)) {
	    jj <- match(j, names(x))
	    nnew <- sum(is.na(jj))
	    if(nnew > 0) {
		n <- is.na(jj)
		jj[n] <- nvars + 1:nnew
		new.cols <- j[n]
	    }
	    jseq <- jj
	}
	else if(is.logical(j) || min(j) < 0)
	    jseq <- seq_along(x)[j]
	else {
	    jseq <- j
	    if(max(jseq) > nvars) {
		new.cols <- paste("V", seq.int(from = nvars + 1, to = max(jseq)),
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
    if(n == 0) n <- nrows
    p <- length(jseq)
    m <- length(value)
    if(!is.list(value)) {
        if(p == 1) {
            N <- NROW(value)
            if(N > n)
                stop(gettextf("replacement has %d rows, data has %d", N, n),
                     domain = NA)
            if(N < n && N > 0)
                if(n %% N == 0 && length(dim(value)) <= 1)
                    value <- rep(value, length.out = n)
                else
                    stop(gettextf("replacement has %d rows, data has %d", N, n),
                         domain = NA)
            names(value) <- NULL
            value <- list(value)
         } else {
            if(m < n*p && (m == 0 || (n*p) %% m))
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
            if(n != N && length(dim(value[[k]])) == 2)
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", k, N, n),
                     domain = NA)
            if(N > 0 && N < n && n %% N)
                stop(gettextf("replacement element %d has %d rows, need %d",
                              k, N, n), domain = NA)
            ## these fixing-ups will not work for matrices
            if(N > 0 && N < n) value[[k]] <- rep(value[[k]], length.out = n)
            if(N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows",
                                 k, N, n), domain = NA)
                value[[k]] <- value[[k]][1:n]
            }
        }
	dimv <- c(n, length(value))
    }
    nrowv <- dimv[1]
    if(nrowv < n && nrowv > 0) {
	if(n %% nrowv == 0)
	    value <- value[rep(1:nrowv, length.out = n),,drop = FALSE]
	else stop(gettextf("%d rows in value to replace %d rows", nrowv, n),
                  domain = NA)
    }
    else if(nrowv > n)
	warning(gettextf("replacement data has %d rows to replace %d rows",
                         nrowv, n), domain = NA)
    ncolv <- dimv[2]
    jvseq <- seq_len(p)
    if(ncolv < p) jvseq <- rep(1:ncolv, length.out = p)
    else if(ncolv > p)
	warning(gettextf("provided %d variables to replace %d variables",
                         ncolv, p), domain = NA)
    if(length(new.cols)) {
        ## extend and name now, as assignment of NULL may delete cols later.
        nm <- names(x)
        rows <- attr(x, "row.names")
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
                if(length(dim(x[[jj]])) != 2) x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            } else {
                ## try to make a new column match in length: may be an error
                x[[jj]] <- vjj[FALSE]
                if(length(dim(vjj)) == 2) {
                    length(x[[j]]) <- nrows * ncol(vjj)
                    dim(x[[j]]) <- c(nrows, ncol(vjj))
                    x[[jj]][iseq, ] <- vjj
                } else {
                    length(x[[j]]) <- nrows
                    x[[jj]][iseq] <- vjj
                }
            }
	}
    else if(p > 0) for(jjj in p:1) { # we might delete columns with NULL
	jj <- jseq[jjj]
	x[[jj]] <- value[[ jvseq[[jjj]] ]]
        if(is.atomic(x[[jj]])) names(x[[jj]]) <- NULL
    }
    if(length(new.cols) > 0) {
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
    rows <- attr(x, "row.names")
    nrows <- length(rows)
    if(is.atomic(value)) names(value) <- NULL
    if(nargs() < 4) {
	## really ambiguous, but follow common use as if list
        nc <- length(x)
	if(!is.null(value)) {
            N <- NROW(value)
            if(N > nrows)
                stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                     domain = NA)
            if(N < nrows && N > 0)
                if(nrows %% N == 0 && length(dim(value)) <= 1)
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
    nvars <- length(x)
    if(n <- is.character(i)) {
	ii <- match(i, rows)
	n <- sum(new.rows <- is.na(ii))
	if(n > 0) {
	    ii[new.rows] <- seq.int(from = nrows + 1, length = n)
	    new.rows <- i[new.rows]
	}
	i <- ii
    }
    if(all(i >= 0) && (nn <- max(i)) > nrows) {
	## expand
	if(n == 0) {
	    nrr <- as.character((nrows + 1):nn)
	    if(inherits(value, "data.frame") &&
	       (dim(value)[1]) >= length(nrr)) {
		new.rows <- attr(value, "row.names")[1:length(nrr)]
		repl <- duplicated(new.rows) | match(new.rows, rows, 0)
		if(any(repl))
		    new.rows[repl] <- nrr[repl]
	    }
	    else new.rows <- nrr
	}
	x <- xpdrows.data.frame(x, rows, new.rows)
	rows <- attr(x, "row.names")
	nrows <- length(rows)
    }
    iseq <- seq_along(rows)[i]
    if(any(is.na(iseq)))
	stop("non-existent rows not allowed")
    if(is.character(j)) {
	jseq <- match(j, names(x))
	if(any(is.na(jseq)))
	    stop("replacing element in non-existent column: ", j[is.na(jseq)])
    }
    else if(is.logical(j) || min(j) < 0)
	jseq <- seq_along(x)[j]
    else {
	jseq <- j
	if(max(jseq) > nvars)
	    stop("replacing element in non-existent column: ", jseq[jseq>nvars])
    }
    if(length(iseq) > 1 || length(jseq) > 1)
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
    class(x) <- NULL
    nrows <- length(attr(x, "row.names"))
    if(!is.null(value)) {
        N <- NROW(value)
        if(N > nrows)
            stop(gettextf("replacement has %d rows, data has %d", N, nrows),
                 domain = NA)
        if(N < nrows && N > 0)
            if(nrows %% N == 0 && length(dim(value)) <= 1)
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
    for (i in 1:nc) {
	y <- x[[i]]
	dy <- dim(y)
	cy <- oldClass(y)
	class(y) <- NULL
	if (length(dy) == 2) {
	    dny <- dimnames(y)
	    if (length(dny[[1]]) > 0)
		dny[[1]] <- c(dny[[1]], new.rows)
	    z <- array(y[1], dim = c(nr, nc), dimnames = dny)
	    z[1 : nro, ] <- y
	    class(z) <- cy
	    x[[i]] <- z
	}
	else {
	    ay <- attributes(y)
	    if (length(names(y)) > 0)
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
	if(all(clabs == nmi))
	    NULL
	else if(length(nmi) == length(clabs) &&
                all(nii <- match(nmi, clabs, 0))) {
            ## we need unique matches here
	    m <- pmatch(nmi, clabs, 0)
            if(any(m == 0))
                stop("names do not match previous names")
            m
	} else stop("names do not match previous names:\n\t",
                  paste(nmi[nii == 0], collapse = ", "))
    }
    Make.row.names <- function(nmi, ri, ni, nrow)
    {
	if(nchar(nmi) > 0) {
            if(ni == 0) character(0)  # PR8506
	    else if(ni > 1) paste(nmi, ri, sep = ".")
	    else nmi
	}
	else if(nrow > 0 && identical(ri, 1:ni))
	    as.integer(seq.int(from = nrow + 1, length = ni))
	else ri
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0]
    n <- length(allargs)
    if(n == 0)
	return(structure(list(),
			 class = "data.frame",
			 row.names = integer()))
    nms <- names(allargs)
    if(is.null(nms))
	nms <- character(length(allargs))
    cl <- NULL
    perm <- rows <- rlabs <- vector("list", n)
    nrow <- 0
    value <- clabs <- NULL
    all.levs <- list()
    for(i in 1:n) {
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
	    rows[[i]] <- seq.int(from = nrow + 1, length = ni)
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    nrow <- nrow + ni
	    if(is.null(value)) {
		value <- unclass(xi)
		nvar <- length(value)
		all.levs <- vector("list", nvar)
		has.dim <- logical(nvar)
                facCol <- logical(nvar)
                ordCol <- logical(nvar)
		for(j in 1:nvar) {
		    xj <- value[[j]]
		    if( !is.null(levels(xj)) ) {
			all.levs[[j]] <- levels(xj)
                        facCol[j] <- TRUE # turn categories into factors
                    } else facCol[j] <- is.factor(xj)
                    ordCol[j] <- is.ordered(xj)
		    has.dim[j] <- length(dim(xj)) == 2
		}
	    }
	    else for(j in 1:nvar) {
                xij <- xi[[j]]
                if(is.null(pi) || is.na(jj <- pi[[j]])) jj <- j
                if(facCol[jj]) {
                    if(length(lij <- levels(xij)) > 0) {
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], lij))
                        ordCol[jj] <- ordCol[jj] & is.ordered(xij)
                    } else if(is.character(xij))
                        all.levs[[jj]] <- unique(c(all.levs[[jj]], xij))
                }
            }
	}
	else if(is.list(xi)) {
	    ni <- range(sapply(xi, length))
	    if(ni[1] == ni[2])
		ni <- ni[1]
	    else stop("invalid list argument: all variables should have the same length")
	    rows[[i]] <- ri <-
                as.integer(seq.int(from = nrow + 1, length = ni))
	    nrow <- nrow + ni
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    if(length(nmi <- names(xi)) > 0) {
		if(is.null(clabs))
		    clabs <- nmi
		else {
		    tmp<-match.names(clabs, nmi)
		    if( !is.null(tmp) )
			perm[[i]] <- tmp
		}
	    }
	}
	else if(length(xi) > 0) {
	    rows[[i]] <- nrow <- nrow + 1
	    rlabs[[i]] <- if(nchar(nmi) > 0) nmi else as.integer(nrow)
	}
    }
    nvar <- length(clabs)
    if(nvar == 0)
	nvar <- max(sapply(allargs, length))	# only vector args
    if(nvar == 0)
	return(structure(list(), class = "data.frame",
			 row.names = integer()))
    pseq <- 1:nvar
    if(is.null(value)) {
	value <- list()
	value[pseq] <- list(logical(nrow))
    }
    names(value) <- clabs
    for(j in 1:nvar)
	if(length(lij <- all.levs[[j]]) > 0)
            value[[j]] <-
                factor(as.vector(value[[j]]), lij, ordered = ordCol[j])
    if(any(has.dim)) {
	rmax <- max(unlist(rows))
	for(i in (1:nvar)[has.dim])
	    if(!inherits(xi <- value[[i]], "data.frame")) {
		dn <- dimnames(xi)
		rn <- dn[[1]]
		if(length(rn) > 0) length(rn) <- rmax
		pi <- dim(xi)[2]
		length(xi) <- rmax * pi
		value[[i]] <- array(xi, c(rmax, pi), list(rn, dn[[2]]))
	    }
    }
    for(i in 1:n) {
	xi <- unclass(allargs[[i]])
	if(!is.list(xi))
	    if(length(xi) != nvar)
		xi <- rep(xi, length.out = nvar)
	ri <- rows[[i]]
	pi <- perm[[i]]
	if(is.null(pi))
	    pi <- pseq
	for(j in 1:nvar) {
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
    if(length(x) == 0) {
	cat("NULL data frame with", length(row.names(x)), "rows\n")
    } else if(length(row.names(x)) == 0) {
	print.default(names(x), quote = FALSE)
	cat("<0 rows> (or 0-length row.names)\n")
    } else {
	## avoiding picking up e.g. format.AsIs
	print(as.matrix(format.data.frame(x, digits=digits, na.encode=FALSE)),
              ..., quote = quote, right = right)
    }
    invisible(x)
}

as.matrix.data.frame <- function (x, rownames.force = FALSE)
{
    .dimnames <- function(x, maybeNULL = FALSE)
        list(if(maybeNULL &&
                .Call("R_shortRowNames", x, PACKAGE = "base") >= 0)
             NULL else row.names(x),
             names(x))

    dm <- dim(x)
    dn <- .dimnames(x, maybeNULL = !rownames.force)
    if(any(dm == 0))
	return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2]
    n <- dm[1]
    collabs <- as.list(dn[[2]])
    X <- x # will contain the result;
    ## the "big question" is if we return a numeric or a character matrix
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    all.logical <- TRUE
    for (j in 1:p) {
	xj <- X[[j]]
	if(length(dj <- dim(xj)) == 2 && dj[2] > 1) {# matrix with >=2 col
	    if(inherits(xj, "data.frame"))
		xj <- X[[j]] <- as.matrix(X[[j]])
	    dnj <- dimnames(xj)[[2]]
	    collabs[[j]] <- paste(collabs[[j]],
				  if(length(dnj) > 0) dnj else 1:dj[2],
				  sep = ".")
	}
        j.logic <- is.logical(xj)
        if(all.logical && !j.logic) all.logical <- FALSE
	if(length(levels(xj)) > 0 || !(j.logic || is.numeric(xj) || is.complex(xj))
	   || (!is.null(cl <- attr(xj, "class")) && # numeric classed objects to format:
	       any(cl %in% c("Date", "POSIXct", "POSIXlt"))))
	    non.numeric <- TRUE
	if(!is.atomic(xj))
	    non.atomic <- TRUE
    }
    if(non.atomic) {
	for (j in 1:p) {
	    xj <- X[[j]]
	    if(!is.recursive(xj))
		X[[j]] <- as.list(as.vector(xj))
	}
    } else if(all.logical) {
        ## do nothing for logical columns if a logical matrix will result.
    } else if(non.numeric) {
	for (j in 1:p) {
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
    dimnames(X) <- list(dn[[1]], unlist(collabs, use.names = FALSE))
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
    call[[1]] <- as.name(.Generic)
    arg <- names(formals(f))[1]
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
    unary <- nargs() == 1
    lclass <- nchar(.Method[1]) > 0
    rclass <- !unary && (nchar(.Method[2]) > 0)
    value <- list()
    ## set up call as op(left, right)
    FUN <- get(.Generic, envir = parent.frame(), mode="function")
    f <- if (unary) quote(FUN(left)) else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if(lclass && rclass) {
	rn <- row.names(e1)
	cn <- names(e1)
	if(any(dim(e2) != dim(e1)))
	    stop(.Generic, " only defined for equally-sized data frames")
    } else if(lclass) {
	## e2 is not a data frame, but e1 is.
	rn <- row.names(e1)
	cn <- names(e1)
	rscalar <- length(e2) <= 1 # e2 might be null
	if(isList(e2)) {
	    if(rscalar) e2 <- e2[[1]]
	    else if(length(e2) != ncol(e1))
		stop(gettextf("list of length %d not meaningful", length(e2)),
                     domain = NA)
	} else {
	    if(!rscalar)
		e2 <- split(rep(as.vector(e2), length.out = prod(dim(e1))),
			    rep.int(1:ncol(e1), rep.int(nrow(e1), ncol(e1))))
	}
    } else {
	## e1 is not a data frame, but e2 is.
	rn <- row.names(e2)
	cn <- names(e2)
	lscalar <- length(e1) <= 1
	if(isList(e1)) {
	    if(lscalar) e1 <- e1[[1]]
	    else if(length(e1) != ncol(e2))
		stop(gettextf("list of length %d not meaningful", length(e1)),
                     domain = NA)
	} else {
	    if(!lscalar)
		e1 <- split(rep(as.vector(e1), length.out = prod(dim(e2))),
			    rep.int(1:ncol(e2), rep.int(nrow(e2), ncol(e2))))
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
    else matrix(unlist(value,recursive = FALSE, use.names=FALSE),
		nrow=length(rn), dimnames=list(rn,cn))
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
