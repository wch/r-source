## we don't use white; it's for compatibility

parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  white = FALSE)
{
    if (!is.null(text) && length(as.character(text)) == 0)
        return(expression())
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt))
}
