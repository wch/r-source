parse <- function(file = "", n = NULL, text = NULL, prompt = "?",
                  white = FALSE)
{
    ## <NOTE>
    ## This used to have:
    ##   we don't use white; it's for compatibility.
    ## We now think it is a bad idea to have it.
    if(!missing(white))
        warning("argument 'white' is unimplemented and deprecated")
    ## </NOTE>
    if(!is.null(text) && length(as.character(text)) == 0)
        return(expression())
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt))
}
