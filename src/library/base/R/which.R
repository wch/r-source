## should return  integer :
which <- function(x) {
	if(is.logical(x))
          if((n <- length(x))) (1:n)[x] else integer(0)
	else stop("argument to \"which\" is not logical")
}
