delayedAssign <- function(x, value, eval.env=parent.frame(1), assign.env=parent.frame(1))
    .Internal(delayedAssign(x, substitute(value), eval.env, assign.env))
