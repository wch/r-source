page <- function(x)
{
    subx <- substitute(x)
    if( is.name(subx) )
	subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
	stop("page requires a name")
    if(exists(subx, inherits=TRUE)) {
        file <- tempfile("Rpage.")
        dput(get(subx, inherits=TRUE), file)
	file.show(file, title = subx, delete.file = TRUE)
    } else
	stop(paste("no object named \"", subx, "\" to edit",sep=""))
}
