attach <- function(what, pos=2, name=deparse(substitute(what))) 
.Internal(attach(what, pos, name))

detach <- function(name, pos=2)
{
	if(!missing(name)) {
		name <- substitute(name)
		if(!is.character(name))
			name <- deparse(name)
		pos <- match(name, search())
		if(is.na(pos))
			stop("invalid name")
	}
	.Internal(detach(pos))
}

objects <-
function (name, pos = -1, envir=pos.to.env(pos), all.names = FALSE, pattern) 
{
	if (!missing(name)) {
		name <- substitute(name)
		if (!is.character(name)) 
			name <- deparse(name)
		pos <- match(name, search())
		envir <- pos.to.env(pos)
	}
	all.names <- .Internal(ls(envir, all.names))
	if(!missing(pattern))
		grep(pattern, all.names, value = TRUE)
	else all.names
}

ls <- objects
