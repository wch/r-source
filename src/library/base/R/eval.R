.GlobalEnv <- environment()

eval <-
    function(expr, envir = sys.frame(sys.parent()),
	     enclos = if(is.list(envir) || is.pairlist(envir))
                       sys.frame(sys.parent()))
    .Internal(eval(expr, envir,enclos))

eval.with.vis <-
    function (expr, envir = sys.frame(sys.parent()),
              enclos = if (is.list(envir) || is.pairlist(envir))
              sys.frame(sys.parent()))
    .Internal(eval.with.vis(expr, envir, enclos))

quote <- function(x) substitute(x)

evalq <-
    function (expr, envir, enclos) 
    eval(substitute(eval(quote(expr), envir, enclos)), 
	 sys.frame(sys.parent()))

Recall <- function(...) .Internal(Recall(...))
