dput <- function(x, file = "")
{
    if(is.character(file))
        if(nchar(file) > 0) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    .Internal(dput(x, file))
}

dget <- function(file)
    eval(parse(file = file))
