as.logical <- function(x) .Internal(as.vector(x,"logical"))
as.integer <- function(x) .Internal(as.vector(x,"integer"))
as.double <- function(x) .Internal(as.vector(x,"double"))
as.real <- .Alias(as.double)
as.complex <- function(x) .Internal(as.vector(x, "complex"))
as.single <- function(x) {
  warning("type single is not supported in R")
  .Internal(as.vector(x,"double"))
}
as.character <- function(x) .Internal(as.vector(x,"character"))
as.expression <- function(x) .Internal(as.vector(x,"expression"))
"as.list" <-
function (x) 
{
  if (is.function(x)) 
    return(c(formals(x), body(x)))
  if (is.expression(x)) {
    l <- vector("list")
    for (sub in x) l <- c(l, sub[[1]])
    return(l)
  }
  .Internal(as.vector(x, "list"))
}
as.vector <- function(x, mode="any") .Internal(as.vector(x,mode))
as.matrix <- function(x) UseMethod("as.matrix")
as.matrix.default <- function(x) {
  if (is.matrix(x))
    x
  else
    array(x, c(length(x),1), if(!is.null(names(x))) list(names(x), NULL) else NULL)
}
as.null <- function(x) NULL
as.function <- function(x,...) UseMethod("as.function")
"as.function.default" <-
function (l, envir = sys.frame(sys.parent())) 
{
  if (!is.list(l)) 
    stop("Can't coerce object to function")
  ln <- length(l)
  alist <- l[-ln]
  body <- l[[ln]]
  if (is.expression(body)) 
    body <- body[[1]]
  if (ln < 2) 
    e <- substitute(function() body)
  else {
    e <- substitute(function(x) body)
    e[[2]] <- alist
  }
  eval(e, envir)
}
as.array <- function(x)
{
	if( is.array(x) )
		return(x)
	dim(x) <-length(x)
	return(x)
}
as.name <- function(x) .Internal(as.vector(x, "name"))
# as.call <- function(x) stop("type call cannot be assigned")
as.numeric <- as.double
as.qr <- function(x) stop("you cannot be serious")
as.ts <- function(x) if(is.ts(x)) x else ts(x)
as.formula <- function(object)
	if(inherits(object, "formula")) object else formula(object)
