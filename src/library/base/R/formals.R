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
alist <- function (...) as.list(sys.call())[-1]
"body<-" <- function (f, value, envir = sys.frame(sys.parent())) {
	value <- substitute(value)
	if (is.expression(value)) 
		value <- value[[1]]
	f <- as.function(c(formals(f), value), envir)
}
"formals<-" <- function (f, value, envir = sys.frame(sys.parent())) {
	value <- substitute(value)
	if (is.expression(value)) 
		value <- value[[1]]
	f <- as.function(c(value, body(f)), envir)
}
