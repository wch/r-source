formals <- function(fun=sys.function(sys.parent())) {
	if(is.character(fun))
		fun <- get(fun, mode = "function")
	.Internal(formals(fun))
}
body <- function(fun=sys.function(sys.parent())) {
	if(is.character(fun))
		fun <- get(fun, mode = "function")
	.Internal(body(fun))
}
