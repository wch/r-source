"factor" <- function (x, levels = sort(unique(x), na.last = TRUE),
	labels=levels, exclude = NA, ordered = FALSE)
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


"is.factor" <- function(x) inherits(x, "factor")

levels <- function(x) attr(x, "levels")
nlevels <- function(x) length(levels(x))

"levels<-" <- function(x, value) {
  x <- as.factor(x)
  if (length(value) != nlevels(x)) 
    stop("Length mismatch in levels<-")
  value <- as.character(value)
  uvalue <- unique(value)
  factor(match(value, uvalue), labels = uvalue)[x]
}

codes <- function(x, ...) UseMethod("codes")

codes.factor <- function(x)
{
  ## This is the S-plus semantics.
  ## The deeper meaning? Search me...
  order(levels(x))[x]
}

"codes<-" <- function(x, value)
{
  if ( length(value) == 1 )
    value <- rep(value, length(x))
  else if ( length(x) != length(value) )
    stop("Length mismatch in \"codes<-\"")
  ## S-plus again...
  value<-rank(levels(x))[value]
  attributes(value)<-attributes(x)
  value
}

"as.factor" <- function (x) if (is.factor(x)) x else factor(x)

"as.vector.factor" <- function(x, type="any")
{
  if (type== "any" || type== "character" || type == "logical" || type == "list")
    as.vector(levels(x)[x], type)
  else
    as.vector(unclass(x), type)
}


"print.factor" <-
  function (x, quote=FALSE) {
    if(length(x) <= 0)
      cat("factor(0)\n")
    else
      print(levels(x)[x], quote=quote)
    cat("Levels: ",paste(levels(x), collapse=" "), "\n")
  }


"Math.factor" <- function(e1,e2)
	stop(paste('"',.Generic,'"', " not meaningful for factors", sep=""))

"Ops.factor" <- function(e1, e2)
{
  ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
  if (!ok) stop(paste('"',.Generic,'"', " not meaningful for factors", sep=""))
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

"[.factor" <- function(x, i)
{
  y <- NextMethod("[")
  class(y)<-"factor"
  attr(y,"levels")<-attr(x,"levels")
  y
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

ordered <- function (x, levels = sort(unique(x), na.last = TRUE),
	labels=levels, exclude = NA, ordered = TRUE)
{
  if (is.ordered(x)) return(x)
  if (is.factor(x)) {
    class(x) <- c("ordered", class(x))
    return(x)
  }
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
    stop("invalid labels argument in \"ordered\"")
  attr(f, "class") <- c(if(ordered)"ordered", "factor")
  f
}

"is.ordered" <- function(x) inherits(x, "ordered")

"as.ordered" <- function(x) if (is.ordered(x)) x else ordered(x)

"print.ordered" <-
  function (x, quote=FALSE) {
    if(length(x) <= 0)
      cat("ordered(0)\n")
    else
      print(levels(x)[x], quote=quote)
  cat("Levels: ",paste(levels(x), collapse=" < "), "\n")
}

"Ops.ordered" <- function(e1, e2)
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
