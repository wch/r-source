factor <- function (x, levels = sort(unique(x), na.last = TRUE),
		    labels=levels, exclude = NA, ordered = is.ordered(x))
{
    if (length(x) == 0)
	return(character(0))
    exclude <- as.vector(exclude, typeof(x))
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    names(f) <- names(x)
    attr(f, "levels") <- if (length(labels) == length(levels))
	as.character(labels)
    else if(length(labels) == 1)
	paste(labels, seq(along = levels), sep = "")
    else
	stop("invalid labels argument in \"factor\"")
    attr(f, "class") <- c(if(ordered)"ordered", "factor")
    f
}

is.factor <- function(x) inherits(x, "factor")
as.factor <- function (x) if (is.factor(x)) x else factor(x)

levels <- function(x) attr(x, "levels")
nlevels <- function(x) length(levels(x))

"levels<-" <-
function(x, value)
  UseMethod("levels<-")

"levels<-.default" <-
function(x, value)
{
  attr(x, "levels") <- value
  x
}

"levels<-.factor" <-
function (x, value)
{
  xlevs <- levels(x)
  if (is.list(value)) {
      nlevs <- rep(names(value), lapply(value, length))
      value <- unlist(value)
      m <- match(value, xlevs, nomatch=0)
      xlevs[m] <- nlevs
  }
  else {
    if (length(xlevs) > length(value))
      stop("number of levels differs")
    xlevs <- as.character(value)
  }
  factor(xlevs[x], levels=unique(xlevs))
}

codes <- function(x, ...) UseMethod("codes")

codes.factor <- function(x)
{
    ## This is the S-plus semantics.
    ## The deeper meaning? Search me...
    rank(levels(x))[x]
}

codes.ordered <- .Alias(as.integer)

"codes<-" <- function(x, value)
{
    if ( length(value) == 1 )
	value <- rep(value, length(x))
    else if ( length(x) != length(value) )
	stop("Length mismatch in \"codes<-\"")
    ## S-plus again...
    if ( !is.ordered(x) ) value <- order(levels(x))[value]
    attributes(value) <- attributes(x)
    value
}

as.vector.factor <- function(x, type="any")
{
    if(type== "any" || type== "character" || type== "logical" || type== "list")
	as.vector(levels(x)[x], type)
    else
	as.vector(unclass(x), type)
}


print.factor <- function (x, quote=FALSE, ...)
{
    if(length(x) <= 0)
	cat("factor(0)\n")
    else
	print(levels(x)[x], quote=quote, ...)
    cat("Levels: ", paste(levels(x), collapse=" "), "\n")
    invisible(x)
}


Math.factor <- function(x, ...) {
    stop(paste('"',.Generic,'"', " not meaningful for factors", sep=""))
}
Summary.factor <- function(x, ...) {
    stop(paste('"',.Generic,'"', " not meaningful for factors", sep=""))
}
Ops.factor <- function(e1, e2)
{
    ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
    if(!ok) {
	warning(paste('"',.Generic,'"', " not meaningful for factors", sep=""))
        return(rep(NA, max(length(e1),if(!missing(e2))length(e2))))
    }
    nas <- is.na(e1) | is.na(e2)
    if (nchar(.Method[1])) {
	l1 <- levels(e1)
	e1 <- l1[e1]
    }
    if (nchar(.Method[2])) {
	l2 <- levels(e2)
	e2 <- l2[e2]
    }
    if (all(nchar(.Method)) && (length(l1) != length(l2) ||
				!all(sort(l2) == sort(l1))))
	stop("Level sets of factors are different")
    value <- NextMethod(.Generic)
    value[nas] <- NA
    value
}

"[.factor" <- function(x, i, drop=FALSE)
{
    y <- NextMethod("[")
    class(y)<-class(x)
    attr(y,"contrasts")<-attr(x,"contrasts")
    attr(y,"levels")<-attr(x,"levels")
    if ( drop ) factor(y) else y
}

"[<-.factor" <- function(x, i, value)
{
    lx <- levels(x)
    cx <- class(x)
    nas <- is.na(x)
    if (is.factor(value))
	value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) && !is.na(value)))
	warning("invalid factor level, NAs generated")
    class(x) <- NULL
    x[i] <- m
    attr(x,"levels") <- lx
    class(x) <- cx
    x
}

## ordered factors ...

ordered <-
    function(x, levels = sort(unique(x), na.last = TRUE), labels = levels,
	     exclude = NA, ordered = TRUE)
{
    if (length(x) == 0)
        f <- numeric(0)
    else {
        exclude <- as.vector(exclude, typeof(x))
        levels <- levels[is.na(match(levels, exclude))]
        f <- match(as.character(x), levels)
        names(f) <- names(x)
        attr(f, "levels") <-
            if (length(labels) == length(levels))
                as.character(labels)
            else if (length(labels) == 1)
                paste(labels, seq(along = levels), sep = "")
            else
                stop("invalid labels argument in \"ordered\"")
    }
    class(f) <- c(if (ordered) "ordered", "factor")
    f
}

is.ordered <- function(x) inherits(x, "ordered")
as.ordered <- function(x) if (is.ordered(x)) x else ordered(x)

print.ordered <- function (x, quote=FALSE)
{
    if(length(x) <= 0)
	cat("ordered(0)\n")
    else
	print(levels(x)[x], quote=quote)
    cat("Levels: ",paste(levels(x), collapse=" < "), "\n")
    invisible(x)
}

Ops.ordered <- function(e1, e2)
{
    nas <- is.na(e1) | is.na(e2)
    if (nchar(.Method[1])) {
	l1 <- levels(e1)
	e1 <- l1[e1]
    }
    if (nchar(.Method[2])) {
	l2 <- levels(e2)
	e2 <- l2[e2]
    }
    if (all(nchar(.Method)) && (length(l1) != length(l2) ||
				!all(sort(l2) == sort(l1))))
	stop("Level sets of factors are different")
    value <- get(.Generic, mode="function")(e1,e2)
    value[nas] <- NA
    value
}
