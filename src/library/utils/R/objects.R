## internal function used only in this file
findGeneric <- function(fname, envir)
{
    if(!exists(fname, mode = "function", envir = envir)) return("")
    f <- get(fname, mode = "function", envir = envir)
    if(.isMethodsDispatchOn() && methods::is(f, "genericFunction")) {
        ## maybe an S3 generic was turned into the S4 default
        fdeflt <- methods::finalDefaultMethod(methods::getMethodsForDispatch(fname, f))
        if(methods::is(fdeflt, "derivedDefaultMethod"))
            f <- fdeflt
        else
            warning(gettextf("'%s' is a formal generic function; S3 methods will not likely be found", fname), domain = NA)
    }
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1]))
            if (nchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3) isUME(e[[3]])
        else {
            if (nchar(res <- isUME(e[[3]]))) res
            else if (nchar(res <- isUME(e[[4]]))) res
            else ""
        }
    }
    isUME <- function(e) {
        if (is.call(e) && (is.name(e[[1]]) || is.character(e[[1]]))) {
            switch(as.character(e[[1]]),
                   UseMethod = as.character(e[[2]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    isUME(body(f))
}

methods <- function (generic.function, class)
{
    rbindSome <- function(df, nms, msg) {
        ## rbind.data.frame() -- dropping rows with duplicated names
        n2 <- length(nms)
        dnew <- data.frame(visible = rep.int(FALSE, n2),
                           from    = rep.int(msg,   n2),
                           row.names = nms)
        n <- nrow(df)
        if(n == 0) return(dnew)
        ## else
        keep <- !duplicated(c(rownames(df), rownames(dnew)))
        rbind(df  [keep[1:n] , ],
              dnew[keep[(n+1):(n+n2)] , ])
    }

    S3MethodsStopList <- tools:::.make_S3_methods_stop_list(NULL)
    knownGenerics <- c(names(.knownS3Generics),
                       tools:::.get_internal_S3_generics())

    an <- lapply(seq(along=(sp <- search())), ls)
    names(an) <- sp
    an <- unlist(an)
    an <- an[!duplicated(an)] # removed masked objects, *keep* names
    names(an) <- sub("[0-9]*$", "", names(an))
    info <- data.frame(visible = rep.int(TRUE, length(an)),
                       from = names(an),
                       row.names = an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
        if(!any(generic.function == knownGenerics)) {
            truegf <- findGeneric(generic.function, parent.frame())
            if(nchar(truegf) && truegf != generic.function) {
                warning(gettextf("generic function '%s' dispatches methods for generic '%s'",
                        generic.function, truegf), domain = NA)
                generic.function <- truegf
            }
        }
	name <- paste("^", generic.function, ".", sep = "")
        name <- gsub("([.[$+*])", "\\\\\\1",name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]
        ## check that these are all functions
        ## might be none at this point
        if(nrow(info)) {
            keep <- sapply(row.names(info),
                           function(nm) exists(nm, mode="function"))
            info <- info[keep, ]
        }

        ## also look for registered methods from namespaces
        ## we assume that only functions get registered.
        defenv <- if(!is.na(w <- .knownS3Generics[generic.function]))
            asNamespace(w)
        else {
            genfun <- get(generic.function, mode = "function",
                          envir = parent.frame())
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
        S3reg <- ls(get(".__S3MethodsTable__.", envir = defenv),
                    pattern = name)
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg =
                              paste("registered S3method for",
                                    generic.function))
        ## both all() and all.equal() are generic, so
        if(generic.function == "all")
            info <- info[-grep("^all\\.equal", row.names(info)), ]
    }
    else if (!missing(class)) {
	if (!is.character(class))
	    class <- paste(deparse(substitute(class)))
	name <- paste(".", class, "$", sep = "")
        name <- gsub("([.[])", "\\\\\\1", name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]

        if(nrow(info)) {
            ## check if we can find a generic matching the name
            possible.generics <- gsub(name, "", row.names(info))
            keep <- sapply(possible.generics, function(nm) {
                if(nm %in% knownGenerics) return(TRUE)
                where <- find(nm, mode = "function")
                if(!length(where)) return(FALSE)
                any(sapply(where, function(w)
                           nchar(findGeneric(nm, envir=as.environment(w))) > 0))
            })
            info <- info[keep, ]
        }

        ## also look for registered methods in loaded namespaces.
        ## These should only be registered in environments containing
        ## the corresponding generic, so we don't check again.
        ## Note that the generic will not necessarily be visible,
        ## as the package may not be loaded.
        S3reg <- unlist(lapply(loadedNamespaces(), function(i) ls(get(".__S3MethodsTable__.", envir = asNamespace(i)), pattern = name)))
        ## now methods like print.summary.aov will be picked up,
        ## so we do look for such mismatches.
        if(length(S3reg))
            S3reg <- S3reg[sapply(gsub(name, "", S3reg), exists)]
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg = "registered S3method")
    }
    else stop("must supply 'generic.function' or 'class'")

    info <- info[sort.list(row.names(info)), ]
    res <- row.names(info)
    class(res) <- "MethodsFunction"
    attr(res, "info") <- info
    res
}

print.MethodsFunction <- function(x, ...)
{
    visible <- attr(x, "info")[["visible"]]
    if(length(x)) {
        print(paste(x, ifelse(visible, "", "*"), sep=""), quote=FALSE, ...)
        if(any(!visible))
            cat("\n", "   ",
                "Non-visible functions are asterisked", "\n", sep="")
    } else cat("no methods were found\n")
    invisible(x)
}


getS3method <-  function(f, class, optional = FALSE)
{
    knownGenerics <- c(tools:::.get_internal_S3_generics(),
                       names(.knownS3Generics))
    if(!any(f == knownGenerics)) {
        truegf <- findGeneric(f, parent.frame())
        if(nchar(truegf)) f <- truegf
        else {
            if(optional) return(NULL)
            else stop(gettextf("no function '%s' could be found", f), domain = NA)
        }
    }
    method <- paste(f, class, sep=".")
    if(exists(method, mode = "function", envir = parent.frame()))
        return(get(method, mode = "function", envir = parent.frame()))
    ## also look for registered method in namespaces
    defenv <- if(!is.na(w <- .knownS3Generics[f])) asNamespace(w)
    else if(f %in% tools:::.get_internal_S3_generics()) .BaseNamespaceEnv
    else {
        genfun <- get(f, mode="function", envir = parent.frame())
        if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
            genfun <- methods::slot(genfun, "default")@methods$ANY
        if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
    }
    S3Table <- get(".__S3MethodsTable__.", envir = defenv)
    S3reg <- ls(S3Table)
    if(length(grep(gsub("([.[$])", "\\\\\\1", method), S3reg)))
        return(get(method, envir = S3Table))
    if(optional) NULL else stop(gettextf("S3 method '%s' not found", method),
                                domain = NA)
}

getFromNamespace <- function(x, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name")
        if(is.null(nm) || substring(nm, 1, 8) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9))
    } else ns <- asNamespace(ns)
    get(x, envir = ns, inherits = FALSE)
}

assignInNamespace <-
    function(x, value, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name")
        if(is.null(nm) || substring(nm, 1, 8) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9))
    } else ns <- asNamespace(ns)
    if(bindingIsLocked(x, ns)) {
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    } else {
        assign(x, value, envir = ns, inherits = FALSE)
    }
    if(!isBaseNamespace(ns)) {
        ## now look for possible copy as a registered S3 method
        S3 <- getNamespaceInfo(ns, "S3methods")
        if(!length(S3)) return(invisible(NULL))
        S3names <- S3[, 3]
        if(x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1], mode = "function", envir = parent.frame())
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            remappedName <- paste(S3[i, 1], S3[i, 2], sep = ".")
            if(exists(remappedName, envir = S3Table, inherits = FALSE))
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}

fixInNamespace <- function (x, ns, pos = -1, envir = as.environment(pos), ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
        stop("'fixInNamespace' requires a name")
    if(missing(ns)) {
        nm <- attr(envir, "name")
        if(is.null(nm) || substring(nm, 1, 8) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9))
    } else ns <- asNamespace(ns)
    x <- edit(get(subx, envir = ns, inherits = FALSE), ...)
    assignInNamespace(subx, x, ns)
}

getAnywhere <- function(x)
{
    x <- as.character(substitute(x))
    objs <- list(); where <- character(0); visible <- logical(0)
    ## first look on search path
    if(length(pos <- find(x, numeric=TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos=pos), x=x)
        where <- names(pos)
        visible <- rep.int(TRUE, length(pos))
    }
    ## next look for methods
    if(length(grep(".", x, fixed=TRUE))) {
        np <- length(parts <- strsplit(x, ".", fixed=TRUE)[[1]])
        for(i in 2:np) {
            gen <- paste(parts[1:(i-1)], collapse=".")
            cl <- paste(parts[i:np], collapse=".")
            if (gen == "" || cl == "") next
            if(!is.null(f <- getS3method(gen, cl, TRUE))) {
                ev <- topenv(environment(f), NULL)
                nmev <- if(isNamespace(ev)) getNamespaceName(ev) else NULL
                objs <- c(objs, f)
                msg <- paste("registered S3 method for", gen)
                if(!is.null(nmev))
                    msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    ## now look in namespaces, visible or not
    for(i in loadedNamespaces()) {
        ns <- asNamespace(i)
        if(exists(x, envir = ns, inherits = FALSE)) {
            f <- get(x, envir = ns, inherits = FALSE)
            objs <- c(objs, f)
            where <- c(where, paste("namespace", i, sep=":"))
            visible <- c(visible, FALSE)
        }
    }
    # now check for duplicates
    ln <- length(objs)
    dups <- rep.int(FALSE, ln)
    objs2 <- lapply(objs, function(x) {
        if(is.function(x)) environment(x) <- NULL
        x
    })
    if(ln > 1)
        for(i in 2:ln)
            for(j in 1:(i-1))
                if(identical(objs2[[i]], objs2[[j]])) {
                    dups[i] <- TRUE
                    break
                }
    res <- list(name=x, objs=objs, where=where, visible=visible, dups=dups)
    class(res) <- "getAnywhere"
    res
}

print.getAnywhere <- function(x, ...)
{
    n <- sum(!x$dups)
    if(n == 0) {
        cat("no object named", sQuote(x$name), "was found\n")
    } else if (n == 1) {
        cat("A single object matching", sQuote(x$name), "was found\n")
        cat("It was found in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("with value\n\n")
        print(x$objs[[1]])
    } else {
        cat(n, "differing objects matching", sQuote(x$name),
            "were found\n")
        cat("in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("Use [] to view one of them\n")
    }
    invisible(x)
}

"[.getAnywhere" <- function(x, i)
{
    if(!is.numeric(i)) stop("only numeric indices can be used")
    if(length(i) == 1) x$objs[[i]]
    else x$objs[i]
}
