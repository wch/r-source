### Useful Generics

"row.names<-" <- function(x, value) UseMethod("row.names<-")
"row.names"   <- function(x)  UseMethod("row.names")

### Dataframe specific code

row.names.default <- function(x) attr(x,"row.names")

"row.names<-.data.frame" <- function(x, value)
{
  if( !is.data.frame(x) )
    return(data.frame(x, row.names=value))
  else {
    old <- attr(x,"row.names")
    if(!is.null(old) && length(value) != length(old))
      stop("invalid row.names length")
    attr(x,"row.names") <- as.character(value)
  }
  x
}

"is.na.data.frame" <- function (x)
{
  y <- do.call("cbind", lapply(x, "is.na"))
  rownames(y) <- row.names(x)
  y
}

is.data.frame <- function(x) inherits(x, "data.frame")

I <- function(x) { structure(x, class = unique(c("AsIs", class(x)))) }

plot.data.frame <- function (x, ...)
{
  if(!is.data.frame(x))
    stop("plot.data.frame applied to non data frame")
  x <- data.matrix(x)
  if(ncol(x) == 1) {
    stripplot(x, ...)
  }
  else if(ncol(x) == 2) {
    plot(x, ...)
  }
  else {
    pairs(x, ...)
  }
}

t.data.frame <- function(x)
{
  x <- as.matrix(x)
  NextMethod("t")
}

dim.data.frame <- function(x) c(length(attr(x,"row.names")), length(x))


dimnames.data.frame <- function(x) list(attr(x,"row.names"), names(x))

"dimnames<-.data.frame" <- function(x, value)
{
  d <- dim(x)
  if(!is.list(value) || length(value) != 2
      || d[[1]] != length(value[[1]])
      || d[[2]] != length(value[[2]]))
    stop("invalid dimnames given for data frame")
  attr(x, "row.names") <- as.character(value[[1]])
  attr(x, "names") <- as.character(value[[2]])
  x
}


as.data.frame <- function(x, row.names = NULL, optional = FALSE)
    UseMethod("as.data.frame")

as.data.frame.default <- function(x, row.names = NULL, optional = FALSE)
{
  dcmethod <- paste("as.data.frame", data.class(x), sep=".")
  if(exists(dcmethod, mode="function"))
    (get(dcmethod, mode="function"))(x, row.names, optional)
  else stop(paste("can't coerce",data.class(x), "into a data.frame"))
}


###  Here are methods ensuring that the arguments to "data.frame"
###  are in a form suitable for combining into a data frame.

as.data.frame.data.frame <- function(x, row.names = NULL, optional = FALSE)
{
  cl <- class(x)
  i <- match("data.frame", cl)
  if(i > 1)
    class(x) <- cl[ - seq(length = i - 1)]
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
    if(length(row.names <- names(x)) == nrows &&
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

as.data.frame.ts <-
function(x, row.names=NULL, optional=F)
{
  if(is.matrix(x)) as.data.frame.matrix(x, row.names, optional)
  else as.data.frame.vector(x, row.names, optional)
}

as.data.frame.numeric <- .Alias(as.data.frame.vector)
as.data.frame.complex <- .Alias(as.data.frame.vector)
as.data.frame.integer <- .Alias(as.data.frame.vector)
as.data.frame.factor <- .Alias(as.data.frame.vector)
as.data.frame.ordered <- .Alias(as.data.frame.vector)

as.data.frame.character <- function(x, row.names = NULL, optional = FALSE)
	as.data.frame.vector(factor(x), row.names, optional)

as.data.frame.logical <- .Alias(as.data.frame.character)

as.data.frame.matrix <- function(x, row.names = NULL, optional = FALSE)
{
  d <- dim(x)
  nrows <- d[[1]]
  ncols <- d[[2]]
  dn <- dimnames(x)
  row.names <- dn[[1]]
  collabs <- dn[[2]]
  value <- vector("list", ncols)
  for(i in seq(length=ncols))
    value[[i]] <- x[,i]
  if(length(row.names)==nrows) {}
  else if(optional) row.names <- character(nrows)
  else row.names <- as.character(seq(length=nrows))
  if(length(collabs) == ncols) names(value) <- collabs
  else if(!optional) names(value) <- paste("V", seq(length=ncols), sep="")
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}

as.data.frame.model.matrix <- function(x, row.names = NULL, optional = FALSE)
{
  d <- dim(x)
  nrows <- d[[1]]
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
  else row.names <- as.character(seq(length=nrows))
  if(!optional) names(value) <- deparse(substitute(x))[[1]]
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}

as.data.frame.AsIs <- function(x, row.names = NULL, optional = FALSE)
{
  if(length(dim(x))==2) as.data.frame.model.matrix(x, row.names, optional)
  else as.data.frame.vector(x, row.names, optional)
}

###  This is the real "data.frame".
###  It does everything by calling the methods presented above.

data.frame <- function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE)
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
	stop(paste("mismatch of row names in elements of \"data.frame\", item",
		   i))
      }
    else function(current, new, i) {
      if(is.null(current) && !any(duplicated(new <- as.character(new))))
	new
      else current
    }
  object <- as.list(substitute(list(...)))[-1]
  x <- list(...)
  n <- length(x)
  if(n < 1)
    return(structure(list(), class = "data.frame"))
  vnames <- names(x)
  if(length(vnames) != n)
    vnames <- character(n)
  no.vn <- nchar(vnames) == 0
  value <- vnames <- as.list(vnames)
  nrows <- numeric(n)
  for(i in 1:n) {
    xi <- as.data.frame(x[[i]], optional=TRUE)
    rowsi <- attr(xi, "row.names")
    nnew <- length(xi)
    namesi <- names(xi)
    if(nnew>1) {
      if(length(namesi) == 0) namesi <- seq(length=nnew)
      if(no.vn[i]) vnames[[i]] <- namesi
      else vnames[[i]] <- paste(vnames[[i]], namesi, sep=".")
    }
    else if(length(namesi) > 0) vnames[[i]] <- namesi
    else if(no.vn[[i]]) vnames[[i]] <- deparse(object[[i]])
    nrows[[i]] <- length(rowsi)
    if(missing(row.names) && rowsi[[1]]!="")
      row.names <- data.row.names(row.names, rowsi, i)
    value[[i]] <- xi
  }
  nr <- max(nrows)
  for(i in seq(length=n)[nrows < nr]) {
    xi <- value[[i]]
    if(length(xi)==1 && nr%%nrows[[i]]==0 && is.vector(xi[[1]]))
      value[[i]] <- list(rep(xi[[1]], length=nr))
    else stop(paste("arguments imply differing number of rows:",
		    paste(unique(nrows), collapse = ", ")))
  }
  value <- unlist(value, recursive=FALSE, use.names=FALSE)
  vnames <- unlist(vnames)
  noname <- nchar(vnames) == 0
  if(any(noname))
    vnames[noname] <- paste("Var", 1:length(vnames), sep = ".")[noname]
  if(check.names)
    vnames <- make.names(vnames)
  names(value) <- vnames
  if(length(row.names) == 0)
    row.names <- 1:nr
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
    if(nargs() < 3) {
      if(missing(i))
	return(x)
      if(is.matrix(i))
	return(as.matrix(x)[i])
      return(structure(NextMethod("["), class = class(x),
		       row.names = row.names(x)))
    }

    ## preserve the attributes for later use ...

    rows <- attr(x, "row.names")
    cols <- names(x)
    cl <- class(x)
    class(x) <- attr(x, "row.names") <- NULL

    ## handle the column only subsetting ...

    if(missing(i)) {
      x <- x[j]
      cols <- names(x)
      if(is.null(cols) || any(nchar(cols) == 0))
	stop("undefined columns selected")
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
	if(length(dim(xj)) != 2)
	  x[[j]] <- xj[i]
	else x[[j]] <- xj[i, , drop = drop]
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
	if(nrow == 1) {
	  drop <- TRUE
	  names(x) <- cols
	  attr(x, "row.names") <- NULL
	}
      }
    }
    if(!drop) {
      names(x) <- cols
      if(any(duplicated(rows)))
	rows <- make.names(rows, unique = TRUE)
      attr(x, "row.names") <- rows
      class(x) <- cl
    }
    x
  }


"[[.data.frame"<-
  function(x, ...)
  {
    ## use in-line functions to refer to the 1st and 2nd ... arguments
    ## explicitly. Also will check for wrong number or empty args
    if(nargs() < 3)
      (function(x, i)
       if(is.matrix(i))
       as.matrix(x)[[i]]
       else unclass(x)[[i]])(x, ...)
    else (function(x, i, j)
	  x[[j]][[i]])(unclass(x), ...)
  }


"[<-.data.frame" <- function(x, i, j, value)
{
  if((nA <- nargs()) == 4) {
    has.i <- !missing(i)
    has.j <- !missing(j)
  }
  else if(nA == 3) {
    ## really ambiguous, but follow common use as if list
    if(is.matrix(i))
      stop("Matrix-subscripts not allowed in replacement")
    j <- i
    i <- NULL
    has.i <- FALSE
    has.j <- TRUE
  }
  else if(nA == 2) {
    value <- i
    i <- j <- NULL
    has.i <- has.j <- FALSE
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
	ii[new.rows] <- seq(from = nrows + 1, length =
			    nextra)
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
      x <- xpdrows.data.frame(x, nrows, nn, rows, new.rows)
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
		      paste("V", seq(from = nvars + 1, to = max(jseq)),
			    sep = ""))
	if(length(new.cols) - nvars != sum(jseq > nvars))
	  stop("new columns would leave holes after existing columns")
      }
    }
  }
  else jseq <- seq(along = x)
  n <- length(iseq)
  if(n == 0)
    n <- nrows
  p <- length(jseq)
  m <- length(value)
  value <- as.data.frame(value)
  dimv <- dim(value)
  nrowv <- dimv[[1]]
  if(nrowv < n) {
    if(n %% nrowv == 0) value <- value[rep(1:nrowv, length=n),]
    else stop(paste(nrowv, "rows in value to replace", n, "rows"))
  }
  else if(nrowv > n) warning(paste("replacement data has", nrowv,
				   "rows to replace", n, "rows"))
  vseq <- 1:n
  ncolv <- dimv[[2]]
  jvseq <- 1:p
  if(ncolv < p) jvseq <- rep(1:ncolv, length=p)
  else if(ncolv > p) warning(paste("provided", ncolv,
				   "variables to replace", p, "variables"))
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
    ## el(x,i) <- value is the preferred approach
    if(is.null(value)) {}
    else {
      if(!inherits(value, "data.frame"))
	value <- as.data.frame(value)
      if(length(value) != 1)
	stop(paste("trying to replace one column with", length(value)))
      if(length(row.names(value)) != nrows)
	stop(paste("replacement has", length(value),
		   "rows, data has", nrows))
      class(value) <- NULL
      value <- value[[1]]
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
    x <- xpdrows.data.frame(x, nrows, nn, rows, new.rows)
    rows <- attr(x, "row.names")
    nrows <- length(rows)
  }
  iseq <- seq(along = rows)[i]
  if(any(is.na(iseq)))
    stop("non-existent rows not allowed")
  if(is.character(j)) {
    jseq <- match(j, names(x))
    if(any(is.na(jseq)))
      stop(paste("replacing element in non-existent column:", j[is.na(jseq)]))
  }
  else if(is.logical(j) || min(j) < 0)
    jseq <- seq(along = x)[j]
  else {
    jseq <- j
    if(max(jseq) > nvars)
      stop(paste("replacing element in non-existent column:", jseq[jseq>nvars]))
  }
  if(length(iseq) > 1 || length(jseq) > 1)
    stop("only a single element should be replaced")
  x[[jseq]][[iseq]] <- value
  class(x) <- cl
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
      else if(nrow > 0 && all(ri == seq(length = ni)))
	seq(from = nrow + 1, length = ni)
      else ri
    }
  n <- nargs()
  if(n == 0)
    return(structure(list(), class = "data.frame", row.names = character()))
  all <- list(...)
  nms <- names(all)
  if(is.null(nms))
    nms <- character(length(all))
  cl <- NULL
  perm <- rows <- rlabs <- vector("list", n)
  nrow <- 0
  value <- clabs <- NULL
  all.levs <- list()
  for(i in 1:n) {
    ## check the arguments, develop row and column labels
    xi <- all[[i]]
    nmi <- nms[i]
    if(inherits(xi, "data.frame")) {
      if(is.null(cl))
	cl <- class(xi)
      ri <- row.names(xi)
      ni <- length(ri)
      if(is.null(clabs))
	clabs <- names(xi)
      else perm[[i]] <- pi <- match.names(clabs, names(xi))
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
	else perm[[i]] <- match.names(clabs, nmi)
      }
    }
    else if(length(xi) > 0) {
      rows[[i]] <- nrow <- nrow + 1
      rlabs[[i]] <- if(nchar(nmi) > 0) nmi else nrow
    }
  }
  nvar <- length(clabs)
  if(nvar == 0)
    nvar <- max(sapply(all, length))	# only vector args
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
    xi <- unclass(all[[i]])
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
    if(!has.dim[j] && (is.character(xj) || is.logical(xj)))
      value[[j]] <- factor(xj)
  }
  rlabs <- unlist(rlabs)
  while(any(xj <- duplicated(rlabs)))
    rlabs[xj] <- paste(rlabs[xj], seq(length = sum(xj)), sep = "")
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
      ## if 'x' has factors & numeric, as.matrix(x) will use
      ## format(.) on the numbers -- set options(.) for the following print(.):
      op <- options(digits = digits)
      on.exit(options(op))
    }
    print(as.matrix(x), ..., quote = quote, right = right)
  }
  invisible(x)
}

as.matrix.data.frame <- function (x)
{
  X <- x
  dm <- dim(X)
  p <- dm[2]
  n <- dm[1]
  dn <- dimnames(X)
  collabs <- as.list(dn[[2]])
  class(X) <- NULL
  non.numeric <- non.atomic <- FALSE
  for (j in 1:p) {
    xj <- X[[j]]
    if(length(dj <- dim(xj)) == 2 && dj[2] > 1) {
      if(inherits(xj, "data.frame"))
	xj <- X[[j]] <- as.matrix(X[[j]])
      dnj <- dimnames(xj)[[2]]
      collabs[[j]] <- paste(collabs[[j]],
			    if(length(dnj) > 0) dnj else seq(1:dj[2]),
			    sep = ".")
    }
    if(length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)))
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
      xj <- X[[j]]
      if(length(levels(xj)) > 0) {
	X[[j]] <- as.vector(xj)
      }
      else X[[j]] <- format(xj)
    }
  }
  X <- unlist(X, recursive = FALSE, use.names = FALSE)
  dim(X) <- c(n, length(X)/n)
  dimnames(X) <- list(dn[[1]], unlist(collabs, use.names = FALSE))
  ##NO! don't copy buggy S-plus!  either all matrices have class or none!!
  ##NO class(X) <- "matrix"
  X
}
