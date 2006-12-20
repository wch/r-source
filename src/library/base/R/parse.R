parse <- function(file = "", n = NULL, text = NULL, prompt = "?", srcfile = NULL)
{
    if(!is.null(text)) {
    	if (length(as.character(text)) == 0)
	    return(expression())
	if (missing(srcfile) && isTRUE(getOption("keep.source")))
	    srcfile <- srcfilecopy("<text>", text)
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            if (missing(srcfile) && isTRUE(getOption("keep.source")))
        	srcfile <- srcfile(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
    .Internal(parse(file, n, text, prompt, srcfile))
}
