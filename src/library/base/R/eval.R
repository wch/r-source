.GlobalEnv <- environment()
parent.frame <- function(n = 1) .Internal(parent.frame(n))

eval <-
    function(expr, envir = parent.frame(),
	     enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame())
    .Internal(eval(expr, envir,enclos))

eval.parent <- function(expr, n = 1){
    p <- parent.frame(n + 1)
    eval(expr , p)
}

evalq <-
    function (expr, envir, enclos)
    eval.parent(substitute(eval(quote(expr), envir, enclos)))

new.env <- function ()
  eval.parent(quote((function() environment())()))

local <-
    function (expr, envir = new.env())
    eval.parent(substitute(eval(quote(expr), envir)))

Recall <- function(...) .Internal(Recall(...))


