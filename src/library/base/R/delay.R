delay <- function(x, env=.GlobalEnv)
    .Internal(delay(substitute(x), env))
