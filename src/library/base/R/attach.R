attach <- function(what, pos=2, name=deparse(substitute(what)))
{
    if (is.character(what) && (length(what)==1)){
        if (!file.exists(what))
            stop(paste("File", what, " not found.", sep=""))
        name<-paste("file:", what, sep="")
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir=as.environment(pos))
    }
    else
        value <- .Internal(attach(what, pos, name))
    if((length(objects(envir = value, all=TRUE)) > 0)
       && .isMethodsDispatchOn())
      cacheMetaData(value, TRUE)
    invisible(value)
}

detach <- function(name, pos=2)
{
    if(!missing(name)) {
        name <- substitute(name)# when a name..
	pos <-
	    if(is.numeric(name)) name
	    else match(if(!is.character(name)) deparse(name) else name,
		       search())
	if(is.na(pos))
	    stop("invalid name")
    }
    env <- as.environment(pos)
    if(exists(".Last.lib", where = pos, inherits=FALSE)) {
        .Last.lib <- get(".Last.lib", pos = pos, inherits=FALSE)
        if(is.function(.Last.lib)) {
            libpath <- attr(env, "path")
            if(!is.null(libpath)) try(.Last.lib(libpath))
        }
    }
    if(.isMethodsDispatchOn()) {
        if(pos != match("package:methods", search()))
            cacheMetaData(env, FALSE)
        else
            .isMethodsDispatchOn(FALSE)
    }
    .Internal(detach(pos))
}

ls <- objects <-
    function (name, pos = -1, envir = as.environment(pos), all.names = FALSE,
              pattern)
{
    if (!missing(name)) {
        nameValue <- try(name)
        if(identical(class(nameValue), "try-error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            pos <- name
        }
        else
            pos <- nameValue
    }
    all.names <- .Internal(ls(envir, all.names))
    if (!missing(pattern)) {
        if ((ll <- length(grep("\\[", pattern))) > 0 && ll !=
            (lr <- length(grep("\\]", pattern)))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern `[' by `\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern) > 0)) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced `[<-' by `\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}
