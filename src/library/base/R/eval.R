.GlobalEnv <- environment()

eval <-
function(expr, envir=sys.frame(sys.parent()))
.Internal(eval(expr, envir))

quote <- function(x) substitute(x)

Recall <- function(...) .Internal(Recall(...))
