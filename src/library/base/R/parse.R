parse <- function(file = "", n = NULL, text = NULL, prompt = "?", keep.src.refs = FALSE, srcfile = NULL)
{
    if(!is.null(text) && length(as.character(text)) == 0)
        return(expression())
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            if (keep.src.refs) srcfile <- srcfile(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile))
}
