dput <- function(x, file = "", forDisplay = TRUE, useSource = FALSE)
{
    if(is.character(file))
        if(nchar(file) > 0) {
            file <- file(file, "wt")
            on.exit(close(file))
        } else file <- stdout()
    .Internal(dput(x, file, forDisplay, useSource))
}

dget <- function(file)
    eval(parse(file = file))
