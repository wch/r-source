"fix" <-
    function (x, ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
        stop("fix requires a name")
    parent <- parent.frame()
    if (exists(subx, envir=parent, inherits = TRUE))
        x <- edit(get(subx, envir=parent), ...)
    else {
        x <- edit(function(){},...)
        environment(x) <- .GlobalEnv
    }
    assign(subx, x, env = .GlobalEnv)
}

