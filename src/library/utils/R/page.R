page <- function(x, method = c("dput", "print"), ...)
{
    subx <- substitute(x)
    if( is.name(subx) )
	subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
	stop("page requires a name")
    method <- match.arg(method)
    parent <- parent.frame()
    if(exists(subx, envir = parent, inherits=TRUE)) {
        file <- tempfile("Rpage.")
        if(method == "dput")
            dput(get(subx, envir = parent, inherits=TRUE), file)
        else {
            sink(file)
            print(get(subx, envir = parent, inherits=TRUE))
            sink()
        }
	file.show(file, title = subx, delete.file = TRUE, ...)
    } else
	stop("no object named ", sQuote(subx), " to edit")
}
