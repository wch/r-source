formals <- function(fun = sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(formals(fun))
}

body <- function(fun = sys.function(sys.parent())) {
    if(is.character(fun))
	fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(body(fun))
}

alist <- function (...) as.list(sys.call())[-1]

"body<-" <- function (fun, envir = environment(fun), value) {
    if (is.expression(value)) value <- value[[1]]
    as.function(c(formals(fun), value), envir)
}

"formals<-" <- function (fun, envir = environment(fun), value)
    as.function(c(value, body(fun)), envir)

