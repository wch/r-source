##-- all.equal (..)   emulate  some of  S's functionality
all.equal <- function(target, current, ...) UseMethod("all.equal")

##- all.equal.matrix <- .Alias(all.equal.default)
##- all.equal.array  <- .Alias(all.equal.default)
##-
##- all.equal.expression  <- .Alias(all.equal.language)

all.equal.default <- function(target, current, ...)
{
  if(is.language(target) || is.function(target))
    return(all.equal.language(target, current, ...))
  if(is.recursive(target))
    return(all.equal.list(target, current, ...))
  if(!is.character(msg <- attr.all.equal(target, current, ...))) {
    msg <- NULL
  } else {
    cont <- attr(msg, "continue")
    if(length(cont)==0 || !cont) return(msg)
  }
  new <- if(data.class(target) != data.class(current))
    paste("target is ", data.class(target), ", current is ",
	  data.class(current), sep = "") else
  switch(mode(target),
	 logical = ,
	 numeric   = all.equal.numeric(target, current, ...),
	 character = all.equal.character(target, current, ...),
	 complex   = all.equal.complex(target, current, ...),
	 NULL)
  if(is.character(new)) msg <- c(msg, new)
  if(is.null(msg)) TRUE else msg
}

all.equal.numeric <- function(target, current,
			      tolerance = .Machine$double.eps ^ .5, scale)
{
  lt <- length(target)
  lc <- length(current)
  if(lt != lc)
    return(paste("Numeric: lengths (", lt, ", ", lc, ") differ"), sep = "")
  else msg <- NULL
  target <- as.vector(target)
  current <- as.vector(current)
  out <- is.na(target)
  if(any(out != is.na(current)))
    return(paste("'is.NA' value mismatches:", sum(is.na(current)),
		 "in current,", sum(out), " in target"))
  out <- out | (target == current)
  if(all(out)) return(TRUE)
  target <- target[!out]
  current <- current[!out]
  xy <- mean(abs(target - current))
  what <-
    if(missing(scale)) {
      xn <- mean(abs(target))
      if(xn > tolerance) {
        xy <- xy/xn
        "relative"
      } else "absolute"
    } else {
      xy <- xy/scale
      "scaled"
    }
  if(is.na(xy) || xy > tolerance)
    paste("Mean", what, "difference:", format(xy)) else TRUE
}

all.equal.character <- function(target, current, ...)
{
  lt <- length(target)
  lc <- length(current)
  if(lt != lc) {
    msg <- paste("Lengths (", lt, ", ", lc,
		 ") differ (string compare on first ", ll <- min(lt, lc),
		 ")", sep = "")
    ll <- seq(length = ll)
    target <- target[ll]
    current <- current[ll]
  } else msg <- NULL
  ne <- target != current
  if(!any(ne) && is.null(msg)) TRUE
  else if(any(ne)) c(msg, paste(sum(ne), "string mismatches"))
  else msg
}

all.equal.complex <- function(target, current, tolerance = std.tolerance(), ...)
{
  lt <- length(target)
  lc <- length(current)
  if(lt != lc)
    return(paste("Complex: lengths (", lt, ", ", lc, ") differ", sep = ""))
  out <- is.na(target)
  if(any(out != is.na(current)))
    return(paste(sum(out != is.na(current)), "missing value mismatches"))
  out <- out | (target == current)
  if(all(out)) return(TRUE)
  if(any(out)) {
    target <- target[!out]
    current <- current[!out]
  }
  xy <- if((xn <- mean(Mod(target))) > tolerance)
    mean(Mod(target - current))/xn else mean(Mod(target - current))
  if(xy < tolerance) TRUE else paste("mean Mod difference:", format(xy))
}

all.equal.factor <- function(target, current, ...)
{
        if(!inherits(current, "factor"))
        	return("target is factor, but current is not")
        if(!is.character(msg <- attr.all.equal(target, current)))
          	msg <- NULL
        else {
                cont <- attr(msg, "continue")
                if(length(cont)==0 || !cont) return(msg)
        }
        class(target) <- class(current) <- NULL
        nax <- is.na(target)
        nay <- is.na(current)
        if(n <- sum(nax != nay))
          	msg <- c(msg, paste("NA mismatches:", n))
        else {
                target <- levels(target)[target[!nax]]
                current <- levels(current)[current[!nay]]
                if(is.character(n <- all.equal(target, current)))
                	msg <- c(msg, n)
        }
        if(is.null(msg)) TRUE else msg
}

all.equal.formula <- function(target, current, ...)
{
	if(length(target) != length(current))
		return(paste("target, current differ in having response: ",
			length(target) == 3, ", ", length(current) == 3))
	if(all(deparse(target) != deparse(current)))
		"formulas differ in contents"
	else TRUE
}

all.equal.language <- function(target, current, ...)
{
	mt <- mode(target)
	mc <- mode(current)
	if(mt == "expression" && mc == "expression")
		return(all.equal.list(target, current, ...))
	if(mt != mc)
		mmsg <- paste("Modes of target, current: ", mt, ", ", mc,
                              sep = "")
	else mmsg <- NULL
	ttxt <- paste(deparse(target), collapse = "\n")
	ctxt <- paste(deparse(current), collapse = "\n")
	msg <- c(mmsg,
                 if(ttxt != ctxt) {
                  if(pmatch(ttxt, ctxt, FALSE))
                  	"target a subset of current"
                  else if(pmatch(ctxt, ttxt, FALSE))
                  	"current a subset of target"
                  else 	"target, current don't match when deparsed"
          } else NULL)
	if(is.null(msg)) TRUE else msg
}

all.equal.list <- function(target, current, ...)
{
	if(!is.character(msg <- attr.all.equal(target, current, ...)))
		msg <- NULL
	nt <- names(target)
	nc <- names(current)
	iseq <-
          if(length(nt)>0 && length(nc)>0) {
                  if(any(not.in <- (c.in.t <- match(nc, nt, 0)) == 0))
                    msg <- c(msg, paste("Components not in target:",
                                        paste(nc[not.in], collapse = ", ")))
                  if(any(not.in <- match(nt, nc, 0) == 0))
                    msg <- c(msg, paste("Components not in current:",
                                        paste(nt[not.in], collapse = ", ")))
                  nt[c.in.t]
          } else if(length(target) == length(current)) {
                  seq(along = target)
          } else {
                  nc <- min(length(target), length(current))
                  msg <- c(msg, paste("Length mismatch: comparison on first",
                                      nc, "components"))
                  seq(length = nc)
          }
	for(i in iseq) {
		mi <- all.equal(target[[i]], current[[i]], ...)
		if(is.character(mi))
			msg <- c(msg, paste("Component ", i, ": ", mi, sep=""))
	}
	if(is.null(msg)) TRUE else msg
}


attr.all.equal <- function(target, current, ...)
{
  ##--- "all.equal(.)" for attributes ---
  ##---  Auxiliary in several all.equal(.) methods
  msg <- NULL
  if(mode(target) != mode(current))
    msg <- paste("Modes: ", mode(target), ", ", mode(current), sep = "")
  if(length(target) != length(current))
    msg <- c(msg, paste("Lengths: ", length(target), ", ",
			length(current), sep = ""))
  ax <- attributes(target)
  ay <- attributes(current)
  nx <- names(target)
  ny <- names(current)
  if((lx <- length(nx)) | (ly <- length(ny))) {
    ax$names <- NULL
    ay$names <- NULL
    if(lx && ly) {
      if(is.character(m <- all.equal.character(nx, ny)))
	msg <- c(msg, paste("Names:", m))
    } else if(lx)
	msg <- c(msg, "names for target but not for current")
    else msg <- "names for current but not for target"
  }
  if(length(ax) || length(ay)) {
    nx <- names(ax)
    ny <- names(ay)
    if(length(nx))      ax <- ax[order(nx)]
    if(length(ny))      ay <- ay[order(ny)]
    tt <- all.equal(ax, ay, ...)
    if(is.character(tt)) msg <- c(msg, paste("Attributes: <", tt, ">"))
  }
  if(is.null(msg)) TRUE else structure(msg, continue = TRUE)
}

