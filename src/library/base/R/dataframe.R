row.names <- function(x) UseMethod("row.names")
row.names.data.frame <- function(x) attr(x, "row.names")
row.names.default <- function(x) if(!is.null(dim(x))) rownames(x)# else NULL

"row.names<-" <- function(x, value) UseMethod("row.names<-")
"row.names<-.data.frame" <- function(x, value) {
    if (!is.data.frame(x))
	x <- as.data.frame(x)
    old <- attr(x, "row.names")
    if (!is.null(old) && length(value) != length(old))
	stop("invalid row.names length")
    value <- as.character(value)
    if (any(duplicated(value)))
	stop("duplicate row.names are not allowed")
    if (any(is.na(value)))
	stop("missing row.names are not allowed")
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

I <- function(x) { structure(x, class = unique(c("AsIs", class(x)))) }

print.AsIs <- function (x, ...)
{
    cl <- class(x)
    class(x) <- cl[cl != "AsIs"]
    NextMethod("print")
    invisible(x)
}

plot.data.frame <- function (x, ...) {
    if(!is.data.frame(x))
	stop("plot.data.frame applied to non data frame")
    x <- data.matrix(x)
    if(ncol(x) == 1) {
	stripchart(x, ...)
    }
    else if(ncol(x) == 2) {
	plot(x, ...)
    }
    else {
	pairs(x, ...)
    }
}

t.data.frame <- function(x) {
    x <- as.matrix(x)
    NextMethod("t")
}

dim.data.frame <- function(x) c(length(attr(x,"row.names")), length(x))

dimnames.data.frame <- function(x) list(attr(x,"row.names"), names(x))

"dimnames<-.data.frame" <- function(x, value) {
    d <- dim(x)
    if(!is.list(value) || length(value) != 2
       || d[[1]] != length(value[[1]])
       || d[[2]] != length(value[[2]]))
	stop("invalid dimnames given for data frame")
    attr(x, "row.names") <- as.character(value[[1]])
    attr(x, "names") <- as.character(value[[2]])
    x
}

as.data.frame <- function(x, row.names = NULL, optional = FALSE) {
    if(is.null(x))			# can't assign class to NULL
	return(as.data.frame(list()))
    if(is.null(attr(x, "class"))) class(x) <- data.class(x)
    UseMethod("as.data.frame", x, row.names, optional)
}
as.data.frame.default <- function(x, row.names = NULL, optional = FALSE)
    stop(paste("can't coerce", data.class(x), "into a data.frame"))


###  Here are methods ensuring that the arguments to "data.frame"
###  are in a form suitable for combining into a data frame.

as.data.frame.data.frame <- function(x, row.names = NULL, optional = FALSE)
{
    cl <- class(x)
    i <- match("data.frame", cl)
    if(i > 1)
	class(x) <- cl[ - (1:(i-1))]
    if(is.character(row.names)){
	if(length(row.names) == length(attr(x, "row.names")))
	    attr(x, "row.names") <- row.names
	else stop(paste("invalid row.names, length", length(row.names),
			"for a data frame with", length(attr(x, "row.names")),
			"rows"))
    }
    x
}

as.data.frame.list <- function(x, row.names = NULL, optional = FALSE)
{
    x <- eval(as.call(c(expression(data.frame), x)))
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != dim(x)[[1]]) stop(paste(
		 "supplied", length(row.names), "row names for",
		 dim(x)[[1]], "rows"))
	attr(x, "row.names") <- row.names
    }
    x
}

as.data.frame.vector <- function(x, row.names = NULL, optional = FALSE)
{
    nrows <- length(x)
    if(is.null(row.names)) {
	if (nrows == 0)
	    row.names <- character(0)
	else if(length(row.names <- names(x)) == nrows &&
		!any(duplicated(row.names))) {}
	else if(optional) row.names <- character(nrows)
	else row.names <- as.character(1:nrows)
    }
    value <- list(x)
    if(!optional) names(value) <- deparse(substitute(x))[[1]]
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.ts <- function(x, row.names=NULL, optional=FALSE)
{
    if(is.matrix(x))
	as.data.frame.matrix(x, row.names, optional)
    else
	as.data.frame.vector(x, row.names, optional)
}

as.data.frame.factor  <- as.data.frame.vector
as.data.frame.ordered <- as.data.frame.vector
as.data.frame.integer <- as.data.frame.vector
as.data.frame.numeric <- as.data.frame.vector
as.data.frame.complex <- as.data.frame.vector

as.data.frame.character <- function(x, row.names = NULL, optional = FALSE)
    as.data.frame.vector(factor(x), row.names, optional)

as.data.frame.logical <- as.data.frame.vector

as.data.frame.matrix <- function(x, row.names = NULL, optional = FALSE)
{
    d <- dim(x)
    nrows <- d[1]; ir <- seq(length = nrows)
    ncols <- d[2]; ic <- seq(length = ncols)
    dn <- dimnames(x)
    row.names <- dn[[1]]
    collabs <- dn[[2]]
    if(any(empty <- nchar(collabs)==0))
	collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if(mode(x) == "character") {
	for(i in ic)
	    value[[i]] <- as.factor(x[,i])
    } else {
	for(i in ic)
	    value[[i]] <- as.vector(x[,i])
    }
    if(length(row.names) != nrows)
	row.names <- if(optional) character(nrows) else as.character(ir)
    if(length(collabs) == ncols)
	names(value) <- collabs
    else if(!optional)
	names(value) <- paste("V", ic, sep="")
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

as.data.frame.model.matrix <- function(x, row.names = NULL, optional = FALSE)
{
    d <- dim(x)
    nrows <- d[1]
    dn <- dimnames(x)
    row.names <- dn[[1]]
    value <- list(x)
    if(!is.null(row.names)) {
	row.names <- as.character(row.names)
	if(length(row.names) != nrows) stop(paste("supplied",
		 length(row.names), "names for a data frame with",
		 nrows, "rows"))
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- as.character(1:nrows)
    if(!optional) names(value) <- deparse(substitute(x))[[1]]
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

"[.AsIs" <- function(x, i, ...) structure(NextMethod("["), class = class(x))

as.data.frame.AsIs <- function(x, row.names = NULL, optional = FALSE)
{
    if(length(dim(x))==2)
	as.data.frame.model.matrix(x, row.names, optional)
    else
	as.data.frame.vector(x, row.names, optional)
}

###  This is the real "data.frame".
###  It does everything by calling the methods presented above.

data.frame <-
    function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE)
{
    data.row.names <-
	if(check.rows && missing(row.names))
	    function(current, new, i) {
		new <- as.character(new)
		if(any(duplicated(new)))
		    return(current)
		if(is.null(current))
		    return(new)
		if(all(current == new) || all(current == ""))
		    return(new)
		stop(paste("mismatch of row names in elements of",
			   "\"data.frame\", item", i))
	    }
	else function(current, new, i) {
	    if(is.null(current)) {
		if(adup <- any(dup <- duplicated(new <- as.character(new)))) {
		    warning(paste("some row.names duplicated:",
				  paste(which(dup),collapse=","),
				  " --> row.names NOT used."))
		    current
		} else new
	    } else current
	}
    object <- as.list(substitute(list(...)))[-1]
    x <- list(...)
    n <- length(x)
    if(n < 1)
	return(structure(list(), row.names = character(0),
			 class = "data.frame"))
    vnames <- names(x)
    if(length(vnames) != n)
	vnames <- character(n)
    no.vn <- nchar(vnames) == 0
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for(i in 1:n) {
	xi <- as.data.frame(x[[i]], optional=TRUE)
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
        }## ncols[i] <= 1
	nrows[i] <- length(rowsi)
	if(missing(row.names) && (nrows[i] > 0) && !(rowsi[[1]] %in% ""))
	    row.names <- data.row.names(row.names, rowsi, i)
	vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for(i in (1:n)[nrows < nr]) {
	xi <- vlist[[i]]
	if(length(xi)==1 && nr%%nrows[i]==0) {
            xi1 <- xi[[1]]
            if(is.vector(xi1) || is.factor(xi1)) {
                vlist[[i]] <- list(rep(xi1, length=nr))
                next
            }
            if(is.character(xi1) && class(xi1) == "AsIs") {
                ## simple char vectors only
                cl <- class(xi1) # `methods' adds a class
                vlist[[i]] <- list(structure(rep(xi1, length=nr), class=cl))
                next
            }
        }
	stop(paste("arguments imply differing number of rows:",
                   paste(unique(nrows), collapse = ", ")))
    }
    value <- unlist(vlist, recursive=FALSE, use.names=FALSE)
    ## unlist() drops i-th component if it has 0 columns
    vnames <- unlist(vnames[ncols > 0])
    noname <- nchar(vnames) == 0
    if(any(noname))
	vnames[noname] <- paste("Var", 1:length(vnames), sep = ".")[noname]
    if(check.names)
	vnames <- make.names(vnames)
    names(value) <- vnames
    if(length(row.names) == 0)
	row.names <- seq(length = nr)
    else if(length(row.names) != nr) {
	if(is.character(row.names))
	    row.names <- match(row.names, vnames, 0)
	if(length(row.names)!=1 ||
	   row.names < 1 || row.names > length(vnames))
	    stop("row.names should specify one of the variables")
	i <- row.names
	row.names <- value[[i]]
	value <- value[ - i]
    }
    row.names <- as.character(row.names)
    if(any(is.na(row.names)))
        stop("row names contain missing values")
    if(any(duplicated(row.names)))
	stop(paste("duplicate row.names:",
		   paste(unique(row.names[duplicated(row.names)]),
			 collapse = ", ")))
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
    if(nargs() < 3) {
	if(missing(i))
	    return(x)
	if(is.matrix(i))
	    return(as.matrix(x)[i])
	y <- NextMethod("[")
	if(any(is.na(names(y))))
	    stop("undefined columns selected")
	return(structure(y, class = class(x), row.names = row.names(x)))
    }

    ## preserve the attributes for later use ...

    rows <- attr(x, "row.names")
    cols <- names(x)
    cl <- class(x)
    class(x) <- attr(x, "row.names") <- NULL

    ## handle the column only subsetting ...

    if(missing(i)) {
        if(!missing(j))
            x <- x[j]
	cols <- names(x)
	if(is.null(cols) || any(nchar(cols) == 0))
	    stop("not all specified columns exist")
    }
    else {
	if(is.character(i))
	    i <- pmatch(i, rows, duplicates.ok = TRUE)
	rows <- rows[i]
	if(!missing(j)) {
	    x <- x[j]
	    cols <- names(x)
	    if(is.null(cols) || any(nchar(cols) == 0))
		stop("undefined columns selected")
	}
	n <- length(x)
	jj <- seq(length = n)
	for(j in jj) {
	    xj <- x[[j]]
	    x[[j]] <- if(length(dim(xj)) != 2) xj[i] else xj[i, , drop = drop]
	}
    }
    if(drop) {
	drop <- FALSE
	n <- length(x)
	if(n == 1) {
	    x <- x[[1]]
	    drop <- TRUE
	}
	else if(n > 1) {
	    xj <- x[[1]]
	    if(length(dim(xj)) == 2)
		nrow <- dim(xj)[1]
	    else nrow <- length(xj)
            ## for consistency with S: don't drop (to a list)
            ## if only one row unless explicitly asked for
	    if(!mdrop && nrow == 1) {
		drop <- TRUE
		names(x) <- cols
		attr(x, "row.names") <- NULL
	    }
	}
    }
    if(!drop) {
	names(x) <- cols
        ## might have NAs, which make.names eliminates.
	if(any(is.na(rows) | duplicated(rows)))
	    rows <- make.names(rows, unique = TRUE)
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
        .subset2(.subset2(x,..1),..2)
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
        if(missing(i) && missing(j)) {
            ## case df[]
            i <- j <- NULL
            has.i <- has.j <- FALSE
        } else {
            ## case df[ind]
            ## really ambiguous, but follow common use as if list
            if(is.matrix(i))
                stop("matrix subscripts not allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    }
    else {
	stop("Need 0, 1, or 2 subscripts")
    }
    cl <- class(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[, etc
    class(x) <- NULL
    rows <- attr(x, "row.names")
    new.cols <- NULL
    nvars <- length(x)
    nrows <- length(rows)
    if(has.i) {
	if(char.i <- is.character(i)) {
	    ii <- match(i, rows)
	    nextra <- sum(new.rows <- is.na(ii))
	    if(nextra > 0) {
		ii[new.rows] <- seq(from = nrows + 1, length = nextra)
		new.rows <- i[new.rows]
	    }
	    i <- ii
	}
	if(all(i >= 0) && (nn <- max(i)) > nrows) {
	    ## expand
	    if(!char.i) {
		nrr <- as.character((nrows + 1):nn)
		if(inherits(value, "data.frame") &&
		   (nrv <- dim(value)[1]) >= length(nrr)) {
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
	iseq <- seq(along = rows)[i]
	if(any(is.na(iseq)))
	    stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if(has.j) {
	if(is.character(j)) {
	    jj <- match(j, names(x))
	    nnew <- sum(is.na(jj))
	    if(nnew > 0) {
		n <- is.na(jj)
		jj[n] <- nvars + 1:nnew
		new.cols <- c(names(x), j[n])
	    }
	    jseq <- jj
	}
	else if(is.logical(j) || min(j) < 0)
	    jseq <- seq(along = x)[j]
	else {
	    jseq <- j
	    if(max(jseq) > nvars) {
		new.cols <- c(names(x),
			      paste("V", seq(from = nvars + 1,
					     to = max(jseq)),
				    sep = ""))
		if(length(new.cols) - nvars != sum(jseq > nvars))
		    stop(paste("new columns would leave holes",
			       "after existing columns"))
	    }
	}
    }
    else jseq <- seq(along = x)
    n <- length(iseq)
    if(n == 0)
	n <- nrows
    p <- length(jseq)
    m <- length(value)
## careful, as.data.frame turns things into factors.
##    value <- as.data.frame(value)
    if(!is.list(value) && (missing(j) || !missing(i))) { # [i, ] or [i,j]
	value <- matrix(value, n, p)
	dimv <- c(n, p)
	value <- split(value, col(value))
    } else {
	value <- as.data.frame(value)
	dimv <- dim(value)
    }
    nrowv <- dimv[[1]]
    if(nrowv < n) {
	if(n %% nrowv == 0)
	    value <- value[rep(1:nrowv, length=n),,drop = FALSE]
	else stop(paste(nrowv, "rows in value to replace", n, "rows"))
    }
    else if(nrowv > n)
	warning(paste("replacement data has", nrowv, "rows to replace",
		      n, "rows"))
    vseq <- 1:n
    ncolv <- dimv[[2]]
    jvseq <- 1:p
    if(ncolv < p) jvseq <- rep(1:ncolv, length=p)
    else if(ncolv > p)
	warning(paste("provided", ncolv, "variables to replace", p,
		      "variables"))
    if(has.i)
	for(jjj in 1:p) {
	    jj <- jseq[jjj]
	    vjj <- value[[jvseq[[jjj]] ]]
	    xj <- x[[jj]]
	    if(length(dim(xj)) != 2)
		xj[iseq] <- vjj
	    else xj[iseq,  ] <- vjj
	    x[[jj]] <- xj
	}
    else for(jjj in 1:p) {
	jj <- jseq[jjj]
	x[[jj]] <- value[[jvseq[[jjj]] ]]
    }
    if(length(new.cols) > 0)
	names(x) <- new.cols
    class(x) <- cl
    x
}

"[[<-.data.frame"<- function(x, i, j, value)
{
    cl <- class(x)
    ## delete class: Version 3 idiom
    ## to avoid any special methods for [[, etc
    class(x) <- NULL
    rows <- attr(x, "row.names")
    nrows <- length(rows)
    if(nargs() < 4) {
	## really ambiguous, but follow common use as if list
	if(!is.null(value)) {
#	    if(!inherits(value, "data.frame"))
# 		value <- as.data.frame(value)
# 	    if(length(value) != 1)
# 		stop(paste("trying to replace one column with",
# 			   length(value)))
# 	    if(length(row.names(value)) != nrows)
# 		stop(paste("replacement has", length(value),
# 			   "rows, data has", nrows))
# 	    class(value) <- NULL
# 	    value <- value[[1]]
            N <- NROW(value)
            if(N < nrows)
                if(nrows %% N == 0 && length(dim(value)) <= 1)
                    value <- rep(value, length = nrows)
                else
                    stop(paste("replacement has", N, "rows, data has", nrows))
	}
	x[[i]] <- value
	class(x) <- cl
	return(x)
    }
    if(missing(i) || missing(j))
	stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
    nvars <- length(x)
    if(n <- is.character(i)) {
	ii <- match(i, rows)
	n <- sum(new.rows <- is.na(ii))
	if(any(n > 0)) {# drop any(.)?
	    ii[new.rows] <- seq(from = nrows + 1, length = n)
	    new.rows <- i[new.rows]
	}
	i <- ii
    }
    if(all(i >= 0) && (nn <- max(i)) > nrows) {
	## expand
	if(n==0) {
	    nrr <- as.character((nrows + 1):nn)
	    if(inherits(value, "data.frame") &&
	       (nrv <- dim(value)[1]) >= length(nrr)) {
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
    iseq <- seq(along = rows)[i]
    if(any(is.na(iseq)))
	stop("non-existent rows not allowed")
    if(is.character(j)) {
	jseq <- match(j, names(x))
	if(any(is.na(jseq)))
	    stop(paste("replacing element in non-existent column:",
		       j[is.na(jseq)]))
    }
    else if(is.logical(j) || min(j) < 0)
	jseq <- seq(along = x)[j]
    else {
	jseq <- j
	if(max(jseq) > nvars)
	    stop(paste("replacing element in non-existent column:",
		       jseq[jseq>nvars]))
    }
    if(length(iseq) > 1 || length(jseq) > 1)
	stop("only a single element should be replaced")
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
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
	cy <- class(y)
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
    attr(x, "row.names") <- as.character(c(old.rows, new.rows))
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
	else if(all(nii <- match(nmi, clabs, 0)))
	    nii
	else stop(paste("names don't match previous names:\n\t",
			paste(nmi[nii == 0], collapse = ", ")))
    }
    Make.row.names <- function(nmi, ri, ni, nrow)
    {
	if(nchar(nmi) > 0) {
	    if(ni > 1)
		paste(nmi, ri, sep = ".")
	    else nmi
	}
	else if(nrow > 0 && identical(ri, 1:ni))
	    seq(from = nrow + 1, length = ni)
	else ri
    }
    allargs <- list(...)
    allargs <- allargs[sapply(allargs, length) > 0]
    n <- length(allargs)
    if(n == 0)
	return(structure(list(),
			 class = "data.frame",
			 row.names = character()))
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
	if(inherits(xi, "data.frame")) {
	    if(is.null(cl))
		cl <- class(xi)
	    ri <- row.names(xi)
	    ni <- length(ri)
	    if(is.null(clabs))
		clabs <- names(xi)
	    else {
		pi <- match.names(clabs, names(xi))
		if( !is.null(pi) )
		    perm[[i]] <- pi
	    }
	    rows[[i]] <- nii <- seq(from = nrow + 1, length = ni)
	    rlabs[[i]] <- Make.row.names(nmi, ri, ni, nrow)
	    nrow <- nrow + ni
	    if(is.null(value)) {
		value <- unclass(xi)
		nvar <- length(value)
		all.levs <- vector("list", nvar)
		has.dim <- logical(nvar)
		for(j in 1:nvar) {
		    xj <- value[[j]]
		    if( !is.null(levels(xj)) )
			all.levs[[j]] <- levels(xj)
		    has.dim[j] <- length(dim(xj)) == 2
		}
	    }
	    else for(j in 1:nvar)
		if(length(lij <- levels(xi[[j]])) > 0) {
		    if(is.null(pi) || is.na(jj <- pi[[j]]))
			jj <- j
		    all.levs[[jj]] <- unique(c(all.levs[[jj]],
					       lij))
		}
	}
	else if(is.list(xi)) {
	    ni <- range(sapply(xi, length))
	    if(ni[1] == ni[2])
		ni <- ni[1]
	    else stop("invalid list argument: all variables should have the same length")
	    rows[[i]] <- ri <- seq(from = nrow + 1, length = ni)
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
	    rlabs[[i]] <- if(nchar(nmi) > 0) nmi else nrow
	}
    }
    nvar <- length(clabs)
    if(nvar == 0)
	nvar <- max(sapply(allargs, length))	# only vector args
    if(nvar == 0)
	return(structure(list(), class = "data.frame",
			 row.names = character()))
    pseq <- 1:nvar
    if(is.null(value)) {
	value <- list()
	value[pseq] <- list(logical(nrow))
    }
    names(value) <- clabs
    for(j in 1:nvar)
	if(length(lij <- all.levs[[j]]) > 0)
	    value[[j]] <- factor(as.vector(value[[j]]), lij)
    if(any(has.dim)) {
	rmax <- max(unlist(rows))
	for(i in (1:nvar)[has.dim])
	    if(!inherits(xi <- value[[i]], "data.frame")) {
		dn <- dimnames(xi)
		row.names <- dn[[1]]
		if(length(row.names) > 0)
		    length(row.names) <- rmax
		pi <- dim(xi)[2]
		length(xi) <- rmax * pi
		value[[i]] <- array(xi, c(rmax, pi), list(row.names, dn[[2]]))
	    }
    }
    for(i in 1:n) {
	xi <- unclass(allargs[[i]])
	if(!is.list(xi))
	    if(length(xi) != nvar)
		xi <- rep(xi, length = nvar)
	ri <- rows[[i]]
	pi <- perm[[i]]
	if(is.null(pi))
	    pi <- pseq
	for(j in 1:nvar) {
	    jj <- pi[j]
	    if(has.dim[jj])
		value[[jj]][ri,	 ] <- xi[[j]]
	    else value[[jj]][ri] <- xi[[j]]
	}
    }
    for(j in 1:nvar) {
	xj <- value[[j]]
	if(!has.dim[j] && !inherits(xj, "AsIs") &&
		is.character(xj))
	    value[[j]] <- factor(xj)
    }
    rlabs <- unlist(rlabs)
    while(any(xj <- duplicated(rlabs)))
	rlabs[xj] <- paste(rlabs[xj], 1:sum(xj), sep = "")
    if(is.null(cl)) {
	as.data.frame(value, row.names = rlabs)
    }
    else {
	class(value) <- cl
	## ensure that row names are ok.  Similar to row.names<-
	rlabs <- as.character(rlabs)
	if(any(duplicated(rlabs)))
	    rlabs <- make.names(rlabs, uniq = TRUE)
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
	if(!is.null(digits)) {
	    ## if 'x' has factors & numeric, as.matrix(x) will apply format(.)
	    ## to the numbers -- set options(.) for the following print(.):
	    op <- options(digits = digits)
	    on.exit(options(op))
	}
	## avoiding picking up e.g. format.AsIs
	print.matrix(format.data.frame(x), ..., quote = quote, right = right)
    }
    invisible(x)
}

as.matrix.data.frame <- function (x)
{
    dm <- dim(x)
    dn <- dimnames(x)
    if(any(dm == 0))
	return(array(NA, dim = dm, dimnames = dn))
    p <- dm[2]
    n <- dm[1]
    collabs <- as.list(dn[[2]])
    X <- x
    class(X) <- NULL
    non.numeric <- non.atomic <- FALSE
    for (j in 1:p) {
	xj <- X[[j]]
	if(length(dj <- dim(xj)) == 2 && dj[2] > 1) {
	    if(inherits(xj, "data.frame"))
		xj <- X[[j]] <- as.matrix(X[[j]])
	    dnj <- dimnames(xj)[[2]]
	    collabs[[j]] <- paste(collabs[[j]],
				  if(length(dnj) > 0) dnj else 1:dj[2],
				  sep = ".")
	}
	if(length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj))
	   || (!is.null(cl <- attr(xj, "class")) && # numeric classed objects to format:
	       any(cl == c("POSIXct", "POSIXlt"))))
	    non.numeric <- TRUE
	if(!is.atomic(xj))
	    non.atomic <- TRUE
    }
    if(non.atomic) {
	for (j in 1:p) {
	    xj <- X[[j]]
	    if(is.recursive(xj)) {
	    }
	    else X[[j]] <- as.list(as.vector(xj))
	}
    } else if(non.numeric) {
	for (j in 1:p) {
	    if (is.character(X[[j]]))
		next
	    xj <- X[[j]]
            miss<-is.na(xj)
	    xj <- if(length(levels(xj))) as.vector(xj) else format(xj)
            is.na(xj)<-miss
            X[[j]]<-xj
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
	class(r) <- class(x)
	row.names(r) <- row.names(x)
	return(r)
    }
    else {
	vnames <- names(x)
	if (is.null(vnames)) vnames <- seq(along=x)
	stop(paste("Non-numeric variable in dataframe:",vnames[!mode.ok]))
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
    FUN <- get(.Generic, envir = parent.frame(),mode="function")
    f <- if (unary)
	quote(FUN(left))
    else quote(FUN(left, right))
    lscalar <- rscalar <- FALSE
    if(lclass && rclass) {
	rn <- row.names(e1)
	cn <- names(e1)
	if(any(dim(e2) != dim(e1)))
	    stop(paste(.Generic, "only defined for equally-sized data frames"))
    } else if(lclass) {
	## e2 is not a data frame, but e1 is.
	rn <- row.names(e1)
	cn <- names(e1)
	rscalar <- length(e2) <= 1 # e2 might be null
	if(isList(e2)) {
	    if(rscalar) e2 <- e2[[1]]
	    else if(length(e2) != ncol(e1))
		stop(paste("list of length", length(e2), "not meaningful"))
	} else {
	    if(!rscalar)
		e2 <- split(rep(as.vector(e2), length = prod(dim(e1))),
			    rep(1:ncol(e1), rep(nrow(e1), ncol(e1))))
	}
    } else {
	## e1 is not a data frame, but e2 is.
	rn <- row.names(e2)
	cn <- names(e2)
	lscalar <- length(e1) <= 1
	if(isList(e1)) {
	    if(lscalar) e1 <- e1[[1]]
	    else if(length(e1) != ncol(e2))
		stop(paste("list of length", length(e1), "not meaningful"))
	} else {
	    if(!lscalar)
		e1 <- split(rep(as.vector(e1), length = prod(dim(e2))),
			    rep(1:ncol(e2), rep(nrow(e2), ncol(e2))))
	}
    }
    for(j in seq(along=cn)) {
	left <- if(!lscalar) e1[[j]] else e1
	right <-if(!rscalar) e2[[j]] else e2
	value[[j]] <- eval(f)
    }
    if(any(.Generic == c("+","-","*","/","%%","%/%"))) {
	names(value) <- cn
	data.frame(value, row.names=rn)
    }
    else matrix(unlist(value,recursive = FALSE, use.names=FALSE),
		nrow=length(rn), dimnames=list(rn,cn))
}

Summary.data.frame <- function(x, ...)
{
    x <- as.matrix(x)
    if(!is.numeric(x) && !is.complex(x))
	stop("only defined on a data frame with all numeric or complex variables")
    NextMethod(.Generic)
}

