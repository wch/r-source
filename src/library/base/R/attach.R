attach <- function(what, pos=2, name=deparse(substitute(what)))
    .Internal(attach(what, pos, name))

detach <- function(name, pos=2)
{
    if(!missing(name)) {
	name <- substitute(name)# when a name..
	pos <-
	    if(is.numeric(name)) name
	    else match(if(!is.character(name))deparse(name) else name,
		       search())
	if(is.na(pos))
	    stop("invalid name")
    }
    .Internal(detach(pos))
}

objects <-
    function (name, pos = -1, envir=pos.to.env(pos), all.names = FALSE, pattern)
{
    if (!missing(name)) {
	if(!is.numeric(name) || name != (pos <- as.integer(name))) {
	    name <- substitute(name)
	    if (!is.character(name))
		name <- deparse(name)
	    pos <- match(name, search())
	}
	envir <- pos.to.env(pos)
    }
    all.names <- .Internal(ls(envir, all.names))
    if(!missing(pattern)) {
	if((ll <- length(grep("\\[", pattern))) > 0
	   && ll != (lr <- length(grep("\\]", pattern)))) {
	    ## fix forgotten "\\" for simple cases:
	    if(pattern == "[") {
		pattern <- "\\["
		warning("replaced regular expression pattern `[' by `\\\\['")
	    } else if(length(grep("[^\\\\]\\[<-",pattern)>0)) {
		pattern <- sub("\\[<-","\\\\\\[<-",pattern)
		warning("replaced `[<-' by `\\\\[<-' in regular expression pattern")
	    }
	}
	grep(pattern, all.names, value = TRUE)
    } else all.names
}

ls <- .Alias(objects)
