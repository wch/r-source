dput <- function(x, file = "", fordisplay = TRUE)
{
    if(is.character(file))
        if(nchar(file) > 0) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    .Internal(dput(x, file, fordisplay))
}

dget <- function(file)
    eval(parse(file = file))
