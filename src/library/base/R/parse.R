## we don't use white; it's for compatibility

parse <- function(file = "", n = NULL, text = NULL, prompt = NULL,
                  white = FALSE)
{
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- file(file, "rt")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt))
}
