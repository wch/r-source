attach <- function(what, pos = 2, name = deparse(substitute(what)),
                   warn.conflicts = TRUE)
{
    checkConflicts <- function(env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".First.lib", ".Last.lib",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- objects(db.pos, all = TRUE)
        if(.isMethodsDispatchOn()) {
            these <- objects(db.pos, all = TRUE)
            these <- these[substr(these, 1, 6) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads", "CheckExEnv"), sp, 0))]
        for (i in ipos) {
            obj.same <- match(objects(i, all = TRUE), ob, nomatch = 0)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if(length(Classobjs)) same <- same[-Classobjs]
                ## report only objects which are both functions or
                ## both non-functions.
                is_fn1 <- sapply(same, function(x)
                                 exists(x, where = i, mode = "function",
                                        inherits = FALSE))
                is_fn2 <- sapply(same, function(x)
                                 exists(x, where = db.pos, mode = "function",
                                        inherits = FALSE))
                same <- same[is_fn1 == is_fn2]
                if(length(same)) {
                    cat("\n\tThe following object(s) are masked",
                        if (i < db.pos) "_by_" else "from", sp[i],
                        if (sum(sp == sp[i]) > 1) paste("( position", i, ")"),
                        ":\n\n\t", same, "\n\n")
                }
            }
        }
    }

    if(pos == 1) {
        warning("*** 'pos=1' is not possible; setting 'pos=2' for now.\n",
                "*** Note that 'pos=1' will give an error in the future")
        pos <- 2
    }
    if (is.character(what) && (length(what)==1)){
        if (!file.exists(what))
            stop(gettextf("file '%s' not found", what), domain = NA)
        name <- paste("file:", what, sep="")
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir=as.environment(pos))
    }
    else
        value <- .Internal(attach(what, pos, name))
    if(warn.conflicts &&
       !exists(".conflicts.OK", envir = value, inherits = FALSE)) {
        checkConflicts(value)
    }
    if((length(objects(envir = value, all=TRUE)) > 0)
       && .isMethodsDispatchOn())
      methods:::cacheMetaData(value, TRUE)
    invisible(value)
}

detach <- function(name, pos=2, version, unload=FALSE)
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
    libpath <- attr(env, "path")
    if(length(grep("^package:", packageName))) {
        pkgname <- sub("^package:", "", packageName)
        hook <- getHook(packageEvent(pkgname, "detach")) # might be list()
        for(fun in rev(hook)) try(fun(pkgname, libpath))
    }
    if(exists(".Last.lib", mode = "function", where = pos, inherits=FALSE)) {
        .Last.lib <- get(".Last.lib",  mode = "function", pos = pos,
                         inherits=FALSE)
        if(!is.null(libpath)) try(.Last.lib(libpath))
    }
    .Internal(detach(pos))
    ## note: here the code internally assumes the separator is "/" even
    ## on Windows.
    if(length(grep("^package:", packageName)))
        .Call("R_lazyLoadDBflush",
              paste(libpath, "/R/", pkgname, ".rdb", sep=""), PACKAGE="base")
    ## Check for detaching a  package required by another package (not
    ## by .GlobalEnv because detach() can't currently fix up the
    ## .required there)
    for(pkgs in search()[-1]) {
        if(!isNamespace(as.environment(pkgs)) &&
           exists(".required", pkgs, inherits = FALSE) &&
           packageName %in% paste("package:", get(".required", pkgs, inherits = FALSE),sep=""))
            warning(packageName, " is required by ", pkgs, " (still attached)")
    }
    if(unload)
      tryCatch(unloadNamespace(pkgname),
               error=function(e)
               warning(pkgname, " namespace cannot be unloaded\n",
                       conditionMessage(e), call. = FALSE))
    if(unload && .isMethodsDispatchOn() && !(pkgname %in% loadedNamespaces()))
        methods:::cacheMetaData(env, FALSE)
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
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern) > 0)) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}
