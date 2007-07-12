dput <-
    function(x, file = "",
             control = c("keepNA", "keepInteger", "showAttributes"))
{
    if(is.character(file))
        if(nzchar(file)) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    opts <- .deparseOpts(control)
    .Internal(dput(x, file, opts))
}

dget <- function(file)
    eval(parse(file = file))
