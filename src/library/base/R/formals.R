formals <- function(fun=sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(formals(fun))
}
body <- function(fun=sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function")
    .Internal(body(fun))
}
alist <- function (...) as.list(sys.call())[-1]
"body<-" <- function (f, value, envir = parent.frame()) {
    if (is.expression(value))
	value <- value[[1]]
    f <- as.function(c(formals(f), value), envir)
}
"formals<-" <- function (f, value, envir = parent.frame()) {
    f <- as.function(c(value, body(f)), envir)
}
