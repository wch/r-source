factor <- function (x=character(), levels = sort(unique.default(x), 
                    na.last = TRUE), labels=levels, exclude = NA, 
                    ordered = is.ordered(x))
{
    exclude <- as.vector(exclude, typeof(x))
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    names(f) <- names(x)
    nl <- length(labels)
    attr(f, "levels") <-
	if (nl == length(levels))
	    as.character(labels)
	else if(nl == 1)
	    paste(labels, seq(along = levels), sep = "")
	else
	    stop(gettextf("invalid labels; length %d should be 1 or %d",
                          nl, length(levels)), domain = NA)
    class(f) <- c(if(ordered)"ordered", "factor")
    f
}

is.factor <- function(x) inherits(x, "factor")
as.factor <- function(x) if (is.factor(x)) x else factor(x)

## Help old S users:
category <- function(...) .Defunct()

levels <- function(x) attr(x, "levels")
nlevels <- function(x) length(levels(x))

"levels<-" <- function(x, value) UseMethod("levels<-")

"levels<-.default" <- function(x, value)
{
    attr(x, "levels") <- value
    x
}

"levels<-.factor" <- function(x, value)
{
    xlevs <- levels(x)
    if (is.list(value)) {
        nlevs <- rep.int(names(value), lapply(value, length))
        value <- unlist(value)
        m <- match(value, xlevs, nomatch=0)
        xlevs[m] <- nlevs[m > 0]
    } else {
        if (length(xlevs) > length(value))
            stop("number of levels differs")
        nlevs <- xlevs <- as.character(value)
        nlevs <- nlevs[!is.na(nlevs)]
    }
    ## take care here not to drop attributes, including class.
    ## factor(xlevs[x], levels = unique(nlevs))
    nlevs <- unique(nlevs)
    at <- attributes(x)
    at$levels <- nlevs
    y <- match(xlevs[x], nlevs)
    attributes(y) <- at
    y
}

as.vector.factor <- function(x, mode="any")
{
    if(mode== "any" || mode== "character" || mode== "logical" || mode== "list")
	as.vector(levels(x)[x], mode)
    else
	as.vector(unclass(x), mode)
}

as.character.factor <- function(x,...)
{
    cx <- levels(x)[x]
    if("NA" %in% levels(x)) cx[is.na(x)] <- "<NA>"
    cx
}

## for `factor' *and* `ordered' :
print.factor <- function (x, quote = FALSE, max.levels = NULL,
                          width = getOption("width"), ...)
{
    ord <- is.ordered(x)
    if (length(x) <= 0)
        cat(if(ord)"ordered" else "factor","(0)\n",sep="")
    else
        print(as.character(x), quote = quote, ...)
    maxl <- if(is.null(max.levels)) TRUE else max.levels
    if (maxl) {
        n <- length(lev <- encodeString(levels(x), quote=ifelse(quote, '"', '')))
        colsep <- if(ord) " < " else " "
        T0 <- "Levels: "
        if(is.logical(maxl))
            maxl <- { ## smart default
                width <- width - (nchar(T0, type="w") + 3 + 1 + 3)
                                        # 3='...', 3=#lev, 1=extra
                lenl <- cumsum(nchar(lev, type="w") + nchar(colsep, type="w"))
                if(n <= 1 || lenl[n] <= width) n
                else max(1, which(lenl > width)[1] - 1)
            }
        drop <- n > maxl
        cat(if(drop)paste(format(n),""), T0,
            paste(if(drop)c(lev[1:max(1,maxl-1)],"...",if(maxl > 1) lev[n])
                      else lev, collapse= colsep), "\n", sep="")
    }
    invisible(x)
}


Math.factor <- function(x, ...) {
    stop(.Generic, " not meaningful for factors")
}
Summary.factor <- function(x, ...) {
    stop(.Generic, " not meaningful for factors")
}
Ops.factor <- function(e1, e2)
{
    ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
    if(!ok) {
	warning(.Generic, " not meaningful for factors")
	return(rep.int(NA, max(length(e1), if(!missing(e2))length(e2))))
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
	stop("level sets of factors are different")
    value <- NextMethod(.Generic)
    value[nas] <- NA
    value
}

"[.factor" <- function(x, i, drop=FALSE)
{
    y <- NextMethod("[")
    attr(y,"contrasts")<-attr(x,"contrasts")
    ## NB factor has levels before class in attribute list (PR#6799)
    attr(y,"levels")<-attr(x,"levels")
    class(y) <- oldClass(x)
    if ( drop ) factor(y) else y
}

"[<-.factor" <- function(x, i, value)
{
    lx <- levels(x)
    cx <- oldClass(x)
#    nas <- is.na(x) # unused
    if (is.factor(value))
	value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value)))
	warning("invalid factor level, NAs generated")
    class(x) <- NULL
    if (missing(i))
	x[] <- m
    else
        x[i] <- m
    attr(x,"levels") <- lx
    class(x) <- cx
    x
}

## ordered factors ...

ordered <- function(x, ...) factor(x, ..., ordered=TRUE)

is.ordered <- function(x) inherits(x, "ordered")
as.ordered <- function(x) if(is.ordered(x)) x else ordered(x)

Ops.ordered <-
function (e1, e2)
{
    ok <- switch(.Generic,
		 "<" = , ">" = , "<=" = , ">=" = ,"=="=, "!=" =TRUE,
		 FALSE)
    if(!ok) {
	warning(sprintf("'%s' is not meaningful for ordered factors",
                        .Generic))
	return(rep.int(NA, max(length(e1), if(!missing(e2))length(e2))))
    }
    if (.Generic %in% c("==", "!="))
      return(NextMethod(.Generic))  ##not S-PLUS compatible, but saner
    nas <- is.na(e1) | is.na(e2)
    ord1 <- FALSE
    ord2 <- FALSE
    if (nchar(.Method[1])) {
	l1 <- levels(e1)
	ord1 <- TRUE
    }
    if (nchar(.Method[2])) {
	l2 <- levels(e2)
	ord2 <- TRUE
    }
    if (all(nchar(.Method)) && (length(l1) != length(l2) || !all(l2 == l1)))
	stop("level sets of factors are different")
    if (ord1 && ord2) {
	e1 <- as.integer(e1) # was codes, but same thing for ordered factor.
	e2 <- as.integer(e2)
    }
    else if (!ord1) {
	e1 <- match(e1, l2)
	e2 <- as.integer(e2)
    }
    else if (!ord2) {
	e2 <- match(e2, l1)
	e1 <- as.integer(e1)
    }
    value <- get(.Generic, mode = "function")(e1, e2)
    value[nas] <- NA
    value
}

"is.na<-.factor" <- function(x, value)
{
    lx <- levels(x)
    cx <- oldClass(x)
    class(x) <- NULL
    x[value] <- NA
    structure(x, levels = lx, class = cx)
}

"length<-.factor" <- function(x, value)
{
    cl <- class(x)
    levs <- levels(x)
    x <- NextMethod()
    structure(x, levels=levs, class=cl)
}
