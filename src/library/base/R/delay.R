delay <- function(x, env=.GlobalEnv)
    .Internal(delay(substitute(x), env))

delayedAssign <- function(x, value, eval.env=parent.frame(1), assign.env=parent.frame(1))
    .Internal(delayedAssign(x, substitute(value), eval.env, assign.env))
