as.logical <- function(x) .Internal(as.vector(x,"logical"))
as.integer <- function(x) .Internal(as.vector(x,"integer"))
as.real <- function(x) .Internal(as.vector(x,"real"))
as.complex <- function(x) .Internal(as.vector(x, "complex"))
as.double <- function(x) .Internal(as.vector(x,"real"))
as.single <- function(x) 
{
	warning("type single is not supported in R")
	.Internal(as.vector(x,"real"))
}
as.character <- function(x) .Internal(as.vector(x,"character"))
as.expression <- function(x) .Internal(as.vector(x,"expression"))
as.list <- function(x) .Internal(as.vector(x,"list"))
as.vector <- function(x, mode="any") .Internal(as.vector(x,mode))
as.matrix <- function(x)
{
	UseMethod("as.matrix")
}
as.matrix.default <- function(x)
{
	if( is.matrix(x) )
		x
	else
		array(x, c(length(x),1), if(!is.null(names(x))) list(names(x), NULL) else NULL)
}
as.matrix.data.frame <- function(x)
{
	y <- .Internal(as.matrix.data.frame(x))
	dimnames(y) <- dimnames(x)
	y
}
as.null <- function(x) NULL
as.function <- function(x) stop("mode function cannot be assigned")
as.array <- function(x)
{
	if( is.array(x) )
		return(x)
	dim(x) <-length(x)
	return(x)
}
as.name <- function(x) .Internal(as.name(x))
# as.call <- function(x) stop("type call cannot be assigned")
as.numeric <- as.double
as.qr <- function(x) stop("you cannot be serious")
as.ts <- function(x) if(is.ts(x)) x else ts(x)
as.formula <- function(object) 
	if(inherits(object, "formula")) object else formula(object)
