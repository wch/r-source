"fix" <-
    function (x, ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
        stop("fix requires a name")
    if (exists(subx, inherits = TRUE))
        x <- edit(get(subx), ...)
    else
        x <- edit(function(){},...)
    assign(subx, x, env = .GlobalEnv)
}

