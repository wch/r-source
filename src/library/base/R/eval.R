.GlobalEnv <- environment()
parent.frame <- function(n = 1) .Internal(parent.frame(n))

eval <-
    function(expr, envir = parent.frame(),
	     enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame() else baseenv())
    .Internal(eval(expr, envir,enclos))

eval.parent <- function(expr, n = 1){
    p <- parent.frame(n + 1)
    eval(expr , p)
}

evalq <-
    function (expr, envir, enclos)
    eval.parent(substitute(eval(quote(expr), envir, enclos)))

new.env <- function (hash=FALSE, parent=parent.frame())
    .Internal(new.env(hash, parent))

parent.env <- function(env)
    .Internal(parent.env(env))

"parent.env<-" <- function(env, value)
    .Internal("parent.env<-"(env, value))

local <-
    function (expr, envir = new.env())
    eval.parent(substitute(eval(quote(expr), envir)))

Recall <- function(...) .Internal(Recall(...))

with <- function(data, expr, ...) UseMethod("with")

with.default <- function(data, expr, ...)
    eval(substitute(expr), data, enclos=parent.frame())

force <- function(x) x
