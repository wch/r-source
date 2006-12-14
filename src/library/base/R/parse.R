parse <- function(file = "", n = NULL, text = NULL, prompt = "?", srcfile = NULL)
{
    if(!is.null(text)) {
    	if (length(as.character(text)) == 0)
	    return(expression())
	if (is.null(srcfile))    
	    srcfile <- srcfilecopy("<text>", text)
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            srcfile <- srcfile(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile))
}
