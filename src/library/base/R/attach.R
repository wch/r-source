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

detach <- function(name, pos=2, version)
{
    if(!missing(name)) {
        name <- substitute(name)# when a name..
	pos <-
	    if(is.numeric(name))
                name
	    else {
                if (!is.character(name))
                    name <- deparse(name)
                if (!missing(version))
                    name <- manglePackageName(name, version)
                match(name, search())
            }
	if(is.na(pos))
	    stop("invalid name")
    }
    env <- as.environment(pos)
    packageName <- search()[[pos]]
    if(exists(".Last.lib", mode = "function", where = pos, inherits=FALSE)) {
        .Last.lib <- get(".Last.lib",  mode = "function", pos = pos,
                         inherits=FALSE)
        libpath <- attr(env, "path")
        if(!is.null(libpath)) try(.Last.lib(libpath))
    }
    .Internal(detach(pos))
    ## check for detaching a  package required by another package (not by .GlobalEnv
    ## because detach() can't currently fix up the .required there)
    for(pkgs in search()[-1]) {
        if(!isNamespace(as.environment(pkgs)) &&
           exists(".required", pkgs, inherits = FALSE) &&
           packageName %in% paste("package:", get(".required", pkgs, inherits = FALSE),sep=""))
            warning(packageName, " is required by ", pkgs, " (still attached)")
    }
    if(.isMethodsDispatchOn()) {
        if("package:methods" %in% search())
            cacheMetaData(env, FALSE)
        else
            .isMethodsDispatchOn(FALSE)
    }
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
        if ((ll <- length(grep("[", pattern, fixed=TRUE))) > 0 &&
            ll != length(grep("]", pattern, fixed=TRUE))) {
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
