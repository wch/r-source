#  File src/library/base/R/attach.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

attach <- function(what, pos = 2L, name = deparse(substitute(what)),
                   warn.conflicts = TRUE)
{
    checkConflicts <- function(env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".Last.lib", ".onDetach",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics", ".requireCachedGenerics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- objects(db.pos, all.names = TRUE)
        if(.isMethodsDispatchOn()) {
            ## <FIXME>: this is wrong-headed: see library().
            these <- objects(db.pos, all.names = TRUE)
            these <- these[substr(these, 1L, 6L) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads", "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob, nomatch = 0L)
            if (any(obj.same > 0L)) {
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
                    objs <- strwrap(paste(same, collapse=", "),
                                    indent = 4L, exdent = 4L)
                    pkg <-
                        if (sum(sp == sp[i]) > 1L) {
                            sprintf("%s (position %d)", sp[i], i)
                        } else {
                            sp[i]
                        }
                    msg <- sprintf(ngettext(length(same),
                                            "The following object is masked %s %s:\n\n%s\n",
                                            "The following objects are masked %s %s:\n\n%s\n"),
                                   if (i < db.pos) "_by_" else "from",
                                   pkg, paste(objs, collapse="\n"))
                    cat(msg)
                }
            }
        }
    }

    if(pos == 1L) {
        warning("*** 'pos=1' is not possible; setting 'pos=2' for now.\n",
                "*** Note that 'pos=1' will give an error in the future")
        pos <- 2L
    }
    if (is.character(what) && (length(what) == 1L)){
        if (!file.exists(what))
            stop(gettextf("file '%s' not found", what), domain = NA)
        if(missing(name)) name <- paste0("file:", what)
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir = as.environment(pos))
    }
    else
        value <- .Internal(attach(what, pos, name))
    if(warn.conflicts &&
       !exists(".conflicts.OK", envir = value, inherits = FALSE)) {
        checkConflicts(value)
    }
    if( length(ls(envir = value, all.names = TRUE)) && .isMethodsDispatchOn() )
        methods:::cacheMetaData(value, TRUE)
    invisible(value)
}

detach <- function(name, pos = 2L, unload = FALSE, character.only = FALSE,
                   force = FALSE)
{
    if(!missing(name)) {
	if(!character.only) name <- substitute(name)
	pos <-
	    if(is.numeric(name)) name
	    else {
                if (!is.character(name)) name <- deparse(name)
                match(name, search())
            }
	if(is.na(pos)) stop("invalid 'name' argument")
    }

    packageName <- search()[[pos]]

    ## we need to treat packages differently from other objects, so get those
    ## out of the way now
    if (! grepl("^package:", packageName) )
        return(invisible(.Internal(detach(pos))))

    ## From here down we are detaching a package.
    pkgname <- sub("^package:", "", packageName)
    for(pkg in search()[-1L]) {
        if(grepl("^package:", pkg) &&
           exists(".Depends", pkg, inherits = FALSE) &&
           pkgname %in% get(".Depends", pkg, inherits = FALSE))
            if(force)
                warning(gettextf("package %s is required by %s, which may no longer work correctly",
                                 sQuote(pkgname), sQuote(sub("^package:", "", pkg))),
                     call. = FALSE, domain = NA)
            else
                stop(gettextf("package %s is required by %s so will not be detached",
                              sQuote(pkgname), sQuote(sub("^package:", "", pkg))),
                     call. = FALSE, domain = NA)
    }
    env <- as.environment(pos)
    libpath <- attr(env, "path")
    hook <- getHook(packageEvent(pkgname, "detach")) # might be a list
    for(fun in rev(hook)) try(fun(pkgname, libpath))
    ## some people, e.g. package g.data, have faked pakages without namespaces
    ns <- .getNamespace(pkgname)
    if(!is.null(ns) &&
       exists(".onDetach", mode = "function", where = ns, inherits = FALSE)) {
        .onDetach <- get(".onDetach",  mode = "function", pos = ns,
                         inherits = FALSE)
        if(!is.null(libpath)) {
            res <- tryCatch(.onDetach(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                                 ".onDetach", "detach", pkgname,
                                 deparse(conditionCall(res))[1L],
                                 conditionMessage(res)),
                        call. = FALSE, domain = NA)
            }
        }
    }
    else if(exists(".Last.lib", mode = "function", where = pos, inherits = FALSE)) {
        .Last.lib <- get(".Last.lib",  mode = "function", pos = pos,
                         inherits = FALSE)
        if(!is.null(libpath)) {
            res <- tryCatch(.Last.lib(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                                 ".Last.lib", "detach", pkgname,
                                 deparse(conditionCall(res))[1L],
                                 conditionMessage(res)),
                        call. = FALSE, domain = NA)
            }
        }
    }
    .Internal(detach(pos))

    if(pkgname %in% loadedNamespaces()) {
        ## the lazyload DB is flushed when the namespace is unloaded
        if(unload) {
            tryCatch(unloadNamespace(pkgname),
                     error = function(e)
                     warning(gettextf("%s namespace cannot be unloaded:\n  ",
                                      sQuote(pkgname)),
                             conditionMessage(e),
                             call. = FALSE, domain = NA))
        }
    } else {
        if(.isMethodsDispatchOn() && methods:::.hasS4MetaData(env))
            methods:::cacheMetaData(env, FALSE)
        .Internal(lazyLoadDBflush(paste0(libpath, "/R/", pkgname, ".rdb")))
    }
    invisible()
}

.detach <- function(pos) .Internal(detach(pos))

ls <- objects <-
    function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE,
              pattern)
{
    if (!missing(name)) {
        nameValue <- try(name, silent = TRUE)
        if(identical(class(nameValue), "try-error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            warning(gettextf("%s converted to character string", sQuote(name)),
                    domain = NA)
            pos <- name
        }
        else
            pos <- nameValue
    }
    all.names <- .Internal(ls(envir, all.names))
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed = TRUE))) &&
            ll != length(grep("]", pattern, fixed = TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}
