as.logical <- function(x,...) UseMethod("as.logical")
as.logical.default<-function(x) .Internal(as.vector(x,"logical"))

as.integer <- function(x,...) UseMethod("as.integer")
as.integer.default <- function(x) .Internal(as.vector(x,"integer"))

as.double <- function(x,...) UseMethod("as.double")
as.double.default <- function(x) .Internal(as.vector(x,"double"))
as.real <- .Alias(as.double)

as.complex <- function(x,...) UseMethod("as.complex")
as.complex.default <- function(x) .Internal(as.vector(x, "complex"))

as.single <- function(x,...) UseMethod("as.single")
as.single.default <- function(x) {
    structure(.Internal(as.vector(x,"double")), Csingle=TRUE)
}
as.character<- function(x,...) UseMethod("as.character")
as.character.default <- function(x) .Internal(as.vector(x,"character"))

as.expression <- function(x,...) UseMethod("as.expression")
as.expression.default <- function(x) .Internal(as.vector(x,"expression"))

as.list <- function(x,...) UseMethod("as.list")
as.list.default <- function (x)
{
    if (is.function(x))
	return(c(formals(x), body(x)))
    if (is.expression(x)) {
	n <- length(x)
	l <- vector("list", n)
	i <- 0
	for (sub in x) l[[i <- i + 1]] <- sub
	return(l)
    }
    .Internal(as.vector(x, "list"))
}
## FIXME:  Really the above  as.vector(x, "list")  should work for data.frames!
as.list.data.frame <- function(x) {
    x <- unclass(x)
    attr(x,"row.names") <- NULL
    x
}

##as.vector dispatches internally so no need for a generic
as.vector <- function(x, mode="any") .Internal(as.vector(x,mode))
as.matrix <- function(x) UseMethod("as.matrix")
as.matrix.default <- function(x) {
    if (is.matrix(x))
	x
    else
	array(x, c(length(x),1),
	      if(!is.null(names(x))) list(names(x), NULL) else NULL)
}
as.null <- function(x,...) UseMethod("as.null")
as.null.default <- function(x) NULL

as.function <- function(x,...) UseMethod("as.function")
as.function.default <- function (l, envir = sys.frame(sys.parent()))
if (is.function(l)) l else .Internal(as.function.default(l, envir))

as.array <- function(x)
{
    if(is.array(x))
	return(x)
    n <- names(x)
    dim(x) <- length(x)
    if(length(n)) dimnames(x) <- list(n)
    return(x)
}

as.symbol <- function(x) .Internal(as.vector(x, "symbol"))
as.name <- .Alias(as.symbol)
## would work too: as.name <- function(x) .Internal(as.vector(x, "name"))

## as.call <- function(x) stop("type call cannot be assigned")
as.numeric <- as.double
as.qr <- function(x) stop("you cannot be serious")
## as.ts <- function(x) if(is.ts(x)) x else ts(x) # in ts.R
as.formula <- function(object)
    if(inherits(object, "formula")) object else formula(object)
