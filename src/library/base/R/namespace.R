## give the base namespace a table for registered methods
".__S3MethodsTable__." <- new.env(hash = TRUE, parent = NULL)

getNamespace <- function(name) {
    ns <- .Internal(getRegisteredNamespace(as.name(name)))
    if (! is.null(ns)) ns
    else tryCatch(loadNamespace(name),
                  error = function(e) {
                    # This assignment is needed because 'name' contains
                    # version as second component when called from internal
                    # serialization code
                    name <- name[1]
                      if (name  %in% c("ctest","eda","modreg","mva","nls",
                                       "stepfun","ts")) {
                          warning(paste("package", sQuote(name),
                                        "has been merged into 'stats'"),
                                  call. = FALSE)
                          return(getNamespace("stats"))
                      }
                      else stop(e)
                  })
}

loadedNamespaces <- function()
    ls(env = .Internal(getNamespaceRegistry()), all = TRUE)

getNamespaceName <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) "base"
    else getNamespaceInfo(ns, "spec")["name"]
}

getNamespaceVersion <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns))
        c(version = paste(R.version$major, R.version$minor, sep="."))
    else getNamespaceInfo(ns, "spec")["version"]
}

getNamespaceExports <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) ls(NULL, all = TRUE)
    else ls(getNamespaceInfo(ns, "exports"), all = TRUE)
}

getNamespaceImports <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) NULL
    else getNamespaceInfo(ns, "imports")
}

getNamespaceUsers <- function(ns) {
    nsname <- getNamespaceName(asNamespace(ns))
    users <- character(0)
    for (n in loadedNamespaces()) {
        inames <- names(getNamespaceImports(n))
        if (match(nsname, inames, 0))
            users <- c(n, users)
    }
    users
}

getExportedValue <- function(ns, name) {
    getInternalExportName <- function(name, ns) {
        exports <- getNamespaceInfo(ns, "exports")
        if (! exists(name, env = exports, inherits = FALSE))
            stop(paste(sQuote(name), "is not an exported object from",
                       sQuote(paste("namespace",
                                    getNamespaceName(ns), sep=":"))),
                 call. = FALSE)
        get(name, env = exports, inherits = FALSE)
    }
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) get(name, env = ns, inherits=FALSE)
    else get(getInternalExportName(name, ns), env = ns)
}

"::" <- function(pkg, name){
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    getExportedValue(pkg, name)
}

":::" <- function(pkg, name){
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    get(name, env = asNamespace(pkg), inherits=FALSE)
}

attachNamespace <- function(ns, pos = 2, dataPath = NULL) {
    runHook <- function(hookname, env, ...) {
        if (exists(hookname, envir = env, inherits = FALSE)) {
            fun <- get(hookname, envir = env, inherits = FALSE)
            if (! is.null(try({ fun(...); NULL})))
                stop(paste(hookname, "failed in attachNamespace"),
                     call. = FALSE)
        }
    }
    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    nspath <- getNamespaceInfo(ns, "path")
    attname <- paste("package", nsname, sep=":")
    if (attname %in% search())
        stop("name space is already attached")
    env <- attach(NULL, pos = pos, name = attname)
    on.exit(detach(pos = pos))
    attr(env, "path") <- nspath
    exports <- getNamespaceExports(ns)
    importIntoEnv(env, exports, ns, exports)
    if(!is.null(dataPath)) {
        dbbase <- file.path(dataPath, "Rdata")
        if(file.exists(paste(dbbase, ".rdb", sep=""))) lazyLoad(dbbase, env)
    }
    runHook(".onAttach", ns, dirname(nspath), nsname)
    lockEnvironment(env, TRUE)
    on.exit()
    invisible(env)
}

loadNamespace <- function (package, lib.loc = NULL,
                           keep.source = getOption("keep.source.pkgs"),
                           partial = FALSE, declarativeOnly = FALSE) {
    # eventually allow version as second component; ignore for now.
    package <- as.character(package)[[1]]

    # check for cycles
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    loading <- dynGet("__NameSpacesLoading__", NULL)
    if (match(package, loading, 0))
        stop("cyclic name space dependencies are not supported")
    "__NameSpacesLoading__" <- c(package, loading)

    ns <- .Internal(getRegisteredNamespace(as.name(package)))
    if (! is.null(ns))
        ns
    else {
        runHook <- function(hookname, pkgname, env, ...) {
            if (exists(hookname, envir = env, inherits = FALSE)) {
                fun <- get(hookname, envir = env, inherits = FALSE)
                if (! is.null(try({ fun(...); NULL})))
                    stop(paste(hookname, "failed in loadNamespace",
                               "for", sQuote(pkgname)),
                         call. = FALSE)
            }
        }
        runUserHook <- function(pkgname, pkgpath) {
            hook <- getHook(packageEvent(pkgname, "onLoad")) # might be list()
            for(fun in hook) try(fun(pkgname, pkgpath))
        }
        makeNamespace <- function(name, version = NULL, lib = NULL) {
            impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
            env <- new.env(parent = impenv, hash = TRUE)
            name <- as.character(as.name(name))
            version <- as.character(version)
            info <- new.env(hash = TRUE, parent = NULL)
            assign(".__NAMESPACE__.", info, env = env)
            assign("spec", c(name=name,version=version), env = info)
            setNamespaceInfo(env, "exports", new.env(hash = TRUE, parent = NULL))
            setNamespaceInfo(env, "imports", list("base"=TRUE))
            setNamespaceInfo(env, "path", file.path(lib, name))
            setNamespaceInfo(env, "dynlibs", NULL)
            setNamespaceInfo(env, "S3methods", matrix(as.character(NA), 0, 3))
            assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = NULL),
                   envir = env)
            .Internal(registerNamespace(name, env))
            env
        }
        sealNamespace <- function(ns) {
            namespaceIsSealed <- function(ns)
               environmentIsLocked(ns)
            ns <- asNamespace(ns, base.OK = FALSE)
            if (namespaceIsSealed(ns))
                stop("namespace", sQuote(getNamespaceName(ns)),
                     " is already sealed in loadNamespace", call.=FALSE)
            lockEnvironment(ns, TRUE)
            lockEnvironment(parent.env(ns), TRUE)
        }
        addNamespaceDynLibs <- function(ns, newlibs) {
            dynlibs <- getNamespaceInfo(ns, "dynlibs")
            setNamespaceInfo(ns, "dynlibs", c(dynlibs, newlibs))
        }

        # find package and check it has a name space
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if (length(pkgpath) == 0)
            stop(paste("There is no package called", sQuote(package)))
        package.lib <- dirname(pkgpath)
        if (! packageHasNamespace(package, package.lib))
            stop(paste("package", sQuote(package),
                       "does not have a name space"))

        # create namespace; arrange to unregister on error
        nsInfoFilePath <- file.path(package.lib, package, "Meta", "nsInfo.rds")
        nsInfo <- if(file.exists(nsInfoFilePath)) .readRDS(nsInfoFilePath)
        else parseNamespaceFile(package, package.lib, mustExist = FALSE)
        version <- read.dcf(file.path(package.lib, package, "DESCRIPTION"),
                            fields = "Version")
        ns <- makeNamespace(package, version = version, lib = package.lib)
        on.exit(.Internal(unregisterNamespace(package)))

        # process imports
        for (i in nsInfo$imports) {
            if (is.character(i))
                namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()),
                                                  keep.source))
            else
                namespaceImportFrom(ns,
                                    loadNamespace(i[[1]],
                                                  c(lib.loc, .libPaths()),
                                                  keep.source), i[[2]])
        }
        for(imp in nsInfo$importClasses)
            namespaceImportClasses(ns, loadNamespace(imp[[1]],
                                                     c(lib.loc, .libPaths()),
                                                     keep.source), imp[[2]])
        for(imp in nsInfo$importMethods)
            namespaceImportMethods(ns, loadNamespace(imp[[1]],
                                                     c(lib.loc, .libPaths()),
                                                     keep.source), imp[[2]])


        # dynamic variable to allow/disable .Import and friends
        "__NamespaceDeclarativeOnly__" <- declarativeOnly

        # store info for loading name space for loadingNamespaceInfo to read
        "__LoadingNamespaceInfo__" <- list(libname = package.lib,
                                           pkgname = package)

        env <- asNamespace(ns)
        # save the package name in the environment
        assign(".packageName", package, envir = env)

        # load the code
        codename <- strsplit(package, "_", fixed=TRUE)[[1]][1]
        codeFile <- file.path(package.lib, package, "R", codename)
        if (file.exists(codeFile)) {
            res <- try(sys.source(codeFile, env, keep.source = keep.source))
            if(inherits(res, "try-error"))
                stop("Unable to load R code in package ", sQuote(package),
                     call. = FALSE)
        } else warning(paste("Package ", sQuote(package), "contains no R code"))

        ## partial loading stops at this point
        ## -- used in preparing for lazy-loading
        if (partial) return(ns)

        # lazy-load any sysdata
        dbbase <- file.path(package.lib, package, "R", "sysdata")
        if (file.exists(paste(dbbase, ".rdb", sep=""))) lazyLoad(dbbase, env)

        # register any S3 methods
        registerS3methods(nsInfo$S3methods, package, env)

        # load any dynamic libraries
        # We provide a way out for cross-building where we can't dynload
        if(!nchar(Sys.getenv("R_CROSS_BUILD"))) {
            dlls = list()
            for (lib in nsInfo$dynlibs) {
               dlls[[lib]]  = library.dynam(lib, package, package.lib)
            }
            setNamespaceInfo(env, "DLLs", dlls)
        }
        addNamespaceDynLibs(env, nsInfo$dynlibs)


        # run the load hook
        runHook(".onLoad", package, env, package.lib, package)

        # process exports, seal, and clear on.exit action
        exports <- nsInfo$exports

        for (p in nsInfo$exportPatterns)
            exports <- c(ls(env, pat = p, all = TRUE), exports)
        if(.isMethodsDispatchOn() &&
           !exists(".noGenerics", envir = ns, inherits = FALSE)) {
            ## process class definition objects
            expClasses <- nsInfo$exportClasses
            if(length(expClasses) > 0) {
                missingClasses <- !sapply(expClasses, methods:::isClass, where = ns)
                if(any(missingClasses))
                    stop("In", sQuote(package),
                         "classes for export not defined: ",
                         paste(expClasses[missingClasses], collapse = ", "))
                expClasses <- paste(methods:::classMetaName(""), expClasses, sep="")
            }
            ## process methods metadata explicitly exported or
            ## implied by exporting the generic function.
            allMethods <- unique(c(methods:::.getGenerics(ns),
                                   methods:::.getGenerics(parent.env(ns))))
            expMethods <- nsInfo$exportMethods
            if(length(allMethods) > 0) {
                expMethods  <- unique(c(expMethods,
                                       exports[!is.na(match(exports, allMethods))]))
                missingMethods <- !(expMethods %in% allMethods)
                if(any(missingMethods))
                    stop("In", sQuote(package),
                         "methods for export not found: ",
                         paste(expMethods[missingMethods], collapse = ", "))
                needMethods <- (exports %in% allMethods) & !(exports %in% expMethods)
                if(any(needMethods))
                    expMethods <- c(expMethods, exports[needMethods])
                for(i in seq(along=expMethods)) {
                    mi <- expMethods[[i]]
                    if(!(mi %in% exports) &&
                       exists(mi, envir = ns, mode = "function", inherits = FALSE))
                        exports <- c(exports, mi)
                    expMethods[[i]] <- methods:::mlistMetaName(mi, ns)
                }
            }
            else if(length(expMethods) > 0)
                stop("In", sQuote(package),
                     "methods specified for export, but none defined: ",
                     paste(expMethods, collapse=", "))
            exports <- c(exports, expClasses, expMethods)
        }
        namespaceExport(ns, exports)
        sealNamespace(ns)
        ## run user hooks here
        runUserHook(package, file.path(package.lib, package))
        on.exit()
        ns
    }
}

loadingNamespaceInfo <- function() {
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    dynGet("__LoadingNamespaceInfo__", stop("not loading a name space"))
}

saveNamespaceImage <- function (package, rdafile, lib.loc = NULL,
                                keep.source = getOption("keep.source.pkgs"),
                                compress = TRUE)
{
    if (! is.null(.Internal(getRegisteredNamespace(as.name(package)))))
        stop(paste("name space", sQuote(package), "is loaded"));
    ns <- loadNamespace(package, lib.loc, keep.source, TRUE, TRUE)
    vars <- ls(ns, all = TRUE)
    vars <- vars[vars != ".__NAMESPACE__."]
    save(list = vars, file = rdafile, envir = ns, compress = compress)
}

topenv <- function(envir = parent.frame(),
                   matchThisEnv = getOption("topLevelEnvironment")) {
    while (! is.null(envir)) {
        if (! is.null(attr(envir, "name")) ||
            identical(envir, matchThisEnv) ||
            identical(envir, .GlobalEnv) ||
            .Internal(isNamespaceEnv(envir)) ||
            exists(".packageName", envir = envir, inherits = FALSE))
            return(envir)
        else envir <- parent.env(envir)
    }
    return(.GlobalEnv)
}

unloadNamespace <- function(ns) {
    runHook <- function(hookname, env, ...) {
        if (exists(hookname, envir = env, inherits = FALSE)) {
            fun <- get(hookname, envir = env, inherits = FALSE)
            if (! is.null(try({ fun(...); NULL})))
                stop(hookname, " failed in unloadNamespace(", ns, ")",
                     call. = FALSE)
        }
    }
    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    pos <- match(paste("package", nsname, sep=":"), search())
    if (! is.na(pos)) detach(pos = pos)
    users <- getNamespaceUsers(ns)
    print(ns)
    if (length(users) != 0)
        stop(paste("name space", sQuote(getNamespaceName(ns)),
                   "still used by:", paste(sQuote(users), collapse = ", ")))
    nspath <- getNamespaceInfo(ns, "path")
    hook <- getHook(packageEvent(nsname, "onUnload")) # might be list()
    for(fun in rev(hook)) try(fun(nsname, nspath))
    try(runHook(".onUnload", ns, nspath))
    .Internal(unregisterNamespace(nsname))
    invisible()
}

.Import <- function(...) {
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    envir <- parent.frame()
    names <- as.character(substitute(list(...)))[-1]
    for (n in names)
        namespaceImportFrom(envir, n)
}

.ImportFrom <- function(name, ...) {
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    envir <- parent.frame()
    name <-  as.character(substitute(name))
    names <- as.character(substitute(list(...)))[-1]
    namespaceImportFrom(envir, name, names)
}

.Export <- function(...) {
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    ns <- topenv(parent.frame(), NULL)
    if (identical(ns, .BaseNamespaceEnv))
        warning("all objects in base name space are currently exported.")
    else if (! isNamespace(ns))
        stop("can only export from a name space")
    else {
        names <- as.character(substitute(list(...)))[-1]
        namespaceExport(ns, names)
    }
}

.S3method <- function(generic, class, method) {
    dynGet <- function(name, notFound = stop(paste(name, "not found"))) {
        n <- sys.nframe()
        while (n > 1) {
            n <- n - 1
            env <- sys.frame(n)
            if (exists(name, env = env, inherits = FALSE))
                return(get(name, env = env, inherits = FALSE))
        }
        notFound
    }
    if (dynGet("__NamespaceDeclarativeOnly__", FALSE))
        stop("imperative name space directives are disabled")
    generic <- as.character(substitute(generic))
    class <- as.character(substitute(class))
    if (missing(method)) method <- paste(generic, class, sep=".")
    registerS3method(generic, class, method, envir = parent.frame())
    invisible(NULL)
}

isNamespace <- function(ns) .Internal(isNamespaceEnv(ns))

isBaseNamespace <- function(ns) identical(ns, .BaseNamespaceEnv)

getNamespaceInfo <- function(ns, which) {
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- get(".__NAMESPACE__.", env = ns, inherits = FALSE)
    get(which, env = info, inherits = FALSE)
}

setNamespaceInfo <- function(ns, which, val) {
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- get(".__NAMESPACE__.", env = ns, inherits = FALSE)
    assign(which, val, env = info)
}

asNamespace <- function(ns, base.OK = TRUE) {
    if (is.character(ns) || is.name(ns))
        ns <- getNamespace(ns)
    if (! isNamespace(ns))
        stop("not a name space")
    else if (! base.OK && isBaseNamespace(ns))
        stop("operation not allowed on base name space")
    else ns
}

namespaceImport <- function(self, ...) {
    for (ns in list(...))
        namespaceImportFrom(self, asNamespace(ns))
}

namespaceImportFrom <- function(self, ns, vars) {
    addImports <- function(ns, from, what) {
        imp <- structure(list(what), names = getNamespaceName(from))
        imports <- getNamespaceImports(ns)
        setNamespaceInfo(ns, "imports", c(imports, imp))
    }
    namespaceIsSealed <- function(ns)
       environmentIsLocked(ns)
    makeImportExportNames <- function(spec) {
        old <- as.character(spec)
        new <- names(spec)
        if (is.null(new)) new <- old
        else new[new==""] <- old[new==""]
        names(old) <- new
        old
    }
    mergeImportMethods <- function(impenv, expenv, metaname) {
        expMethods <- get(metaname, envir = expenv)
        if(exists(metaname, envir = impenv, inherits = FALSE)) {
            impMethods <- get(metaname, envir = impenv)
            assign(metaname, methods:::mergeMethods(impMethods, expMethods), envir = impenv)
            TRUE
        }
        else
            FALSE
    }
    whichMethodMetaNames <- function(impvars) {
        if(!.isMethodsDispatchOn())
            return(numeric())
        mm <- ".__M__" # methods:::mlistMetaName() is slow
        seq(along = impvars)[substr(impvars, 1, nchar(mm)) == mm]
    }
    if (is.character(self))
        self <- getNamespace(self)
    ns <- asNamespace(ns)
    if (missing(vars)) impvars <- getNamespaceExports(ns)
    else impvars <- vars
    impvars <- makeImportExportNames(impvars)
    impnames <- names(impvars)
    if (any(duplicated(impnames))) {
        stop("duplicate import names ",
             paste(impnames[duplicated(impnames)], collapse=", "))
    }
    if (isNamespace(self) && isBaseNamespace(self)) {
        impenv <- self
        msg <- "replacing local value with import:"
        register <- FALSE
    }
    else if (isNamespace(self)) {
        if (namespaceIsSealed(self))
            stop("cannot import into a sealed namespace")
        impenv <- parent.env(self)
        msg <- "replacing previous import:"
        register <- TRUE
    }
    else if (is.environment(self)) {
        impenv <- self
        msg <- "replacing local value with import:"
        register <- FALSE
    }
    else stop("invalid import target")
    which <- whichMethodMetaNames(impvars)
    if(length(which)) {
        ## If methods are already in impenv, merge and don't import
        delete <- integer()
        for(i in which)
            if(mergeImportMethods(impenv, ns, impvars[[i]]))
                delete <- c(delete, i)
        if(length(delete)>0) {
            impvars <- impvars[-delete]
            impnames <- impnames[-delete]
        }
    }
    for (n in impnames)
        if (exists(n, env = impenv, inherits = FALSE))
            warning(paste(msg, n))
    importIntoEnv(impenv, impnames, ns, impvars)
    if (register) {
        if (missing(vars)) addImports(self, ns, TRUE)
        else addImports(self, ns, impvars)
    }
}

namespaceImportClasses <- function(self, ns, vars) {
    for(i in seq(along = vars))
        vars[[i]] <- methods:::classMetaName(vars[[i]])
    namespaceImportFrom(self, asNamespace(ns), vars)
}

namespaceImportMethods <- function(self, ns, vars) {
    allVars <- character()
    allMlists <- methods:::.getGenerics(ns)
    if(any(is.na(match(vars, allMlists))))
        stop("Requested methods objects not found in environment/package \"",
                methods:::getPackageName(ns), "\": ",
                paste(vars[is.na(match(vars, allMlists))], collapse = ", "))
    for(i in seq(along = allMlists)) {
        ## import methods list objects if asked for
        ## or if the corresponding generic was imported
        g <- allMlists[[i]]
        if(exists(g, envir=self, inherits = FALSE) # already imported
           || g %in% vars) # requested explicitly
            allVars <- c(allVars, methods:::mlistMetaName(g, ns))
        if(g %in% vars && !exists(g, envir=self, inherits = FALSE) &&
           exists(g, envir=ns, inherits = FALSE) &&
           methods:::is(get(g, envir = ns), "genericFunction"))
            allVars <- c(allVars, g)
    }
    namespaceImportFrom(self, asNamespace(ns), allVars)
}

importIntoEnv <- function(impenv, impnames, expenv, expnames) {
    exports <- getNamespaceInfo(expenv, "exports")
    ex <- .Internal(ls(exports, TRUE))
    if(!all(expnames %in% ex)) {
        miss <- expnames[! expnames %in% ex]
        stop("object(s) ", paste(sQuote(miss), collapse=", "),
             " are not exported by ",
             sQuote(paste("namespace", getNamespaceName(expenv), sep=":"))
             )
    }
    expnames <- unlist(lapply(expnames, get, env = exports, inherits = FALSE))
    if (is.null(impnames)) impnames <- character(0)
    if (is.null(expnames)) expnames <- character(0)
    .Internal(importIntoEnv(impenv, impnames, expenv, expnames))
}

namespaceExport <- function(ns, vars) {
    namespaceIsSealed <- function(ns)
       environmentIsLocked(ns)
    if (namespaceIsSealed(ns))
        stop("cannot add to exports of a sealed namespace")
    ns <- asNamespace(ns, base.OK = FALSE)
    if (length(vars) > 0) {
        addExports <- function(ns, new) {
            exports <- getNamespaceInfo(ns, "exports")
            expnames <- names(new)
            intnames <- new
            objs <- .Internal(ls(exports, TRUE))
            ex <- expnames %in% objs
            if(any(ex))
                warning(paste("previous export(s)",
                              paste(sQuote(info[notex, 3]), collapse=", "),
                              "are being replaced"), call. = FALSE)
            for (i in seq(along = new))
                assign(expnames[i], intnames[i], env = exports)
        }
        makeImportExportNames <- function(spec) {
            old <- as.character(spec)
            new <- names(spec)
            if (is.null(new)) new <- old
            else new[new==""] <- old[new==""]
            names(old) <- new
            old
        }
        new <- makeImportExportNames(unique(vars))
        ## calling exists each time is too slow, so do two phases
        undef <- new[! new %in% .Internal(ls(ns, TRUE))]
        undef <- undef[! sapply(undef, exists, env = ns)]
        if (length(undef) != 0) {
            undef <- do.call("paste", as.list(c(undef, sep=", ")))
            stop(paste("undefined exports:", undef))
        }
        if(.isMethodsDispatchOn()) .mergeExportMethods(new, ns)
        addExports(ns, new)
    }
}

.mergeExportMethods <- function(new, ns) {
#    if(!.isMethodsDispatchOn()) return(FALSE)
    mm <- methods:::mlistMetaName()
    newMethods <- new[substr(new, 1, nchar(mm)) == mm]
    nsimports <- parent.env(ns)
    for(what in newMethods) {
        if(exists(what, envir = nsimports, inherits = FALSE)) {
            m1 <- get(what, envir = nsimports)
            m2 <- get(what, envir = ns)
            assign(what, envir = ns, methods:::mergeMethods(m1, m2))
        }
    }
}

## NB this needs a decorated name, foo_ver, if appropriate
packageHasNamespace <- function(package, package.lib) {
    namespaceFilePath <- function(package, package.lib)
        file.path(package.lib, package, "NAMESPACE")
    file.exists(namespaceFilePath(package, package.lib))
}

parseNamespaceFile <- function(package, package.lib, mustExist = TRUE) {
    namespaceFilePath <- function(package, package.lib)
        file.path(package.lib, package, "NAMESPACE")
    nsFile <- namespaceFilePath(package, package.lib)
    if (file.exists(nsFile))
        directives <- parse(nsFile)
    else if (mustExist)
        stop(paste("package", sQuote(package), "has no NAMESPACE file"))
    else directives <- NULL
    exports <- character(0)
    exportPatterns <- character(0)
    exportClasses <- character(0)
    exportMethods <- character(0)
    imports <- list()
    importMethods <- list()
    importClasses <- list()
    dynlibs <- character(0)
    S3methods <- matrix(as.character(NA), 500, 3)
    nS3 <- 0
    parseDirective <- function(e) {
        switch(as.character(e[[1]]),
               "if" = if (eval(e[[2]], .GlobalEnv))
                          parseDirective(e[[3]])
                      else if (length(e) == 4)
                          parseDirective(e[[4]]),
               "{" =  for (ee in as.list(e[-1])) parseDirective(ee),
               export = {
                   exp <- e[-1]
                   exp <- structure(as.character(exp), names=names(exp))
                   exports <<- c(exports, exp)
               },
               exportPattern = {
                   pat <- as.character(e[-1])
                   exportPatterns <<- c(pat, exportPatterns)
               },
               exportClass = , exportClasses = {
                   exportClasses <<- c(as.character(e[-1]), exportClasses)
               },
               exportMethods = {
                   exportMethods <<- c(as.character(e[-1]), exportMethods)
               },
               import = imports <<- c(imports,as.list(as.character(e[-1]))),
               importFrom = {
                   imp <- e[-1]
                   ivars <- imp[-1]
                   inames <- names(ivars)
                   imp <- list(as.character(imp[1]),
                               structure(as.character(ivars), names=inames))
                   imports <<- c(imports, list(imp))
               },
               importClassFrom = , importClassesFrom = {
                   imp <- as.character(e[-1])
                   pkg <- imp[[1]]
                   impClasses <- imp[-1]
                   imp <- list(as.character(pkg), as.character(impClasses))
                   importClasses <<- c(importClasses, list(imp))
               },
               importMethodsFrom = {
                   imp <- as.character(e[-1])
                   pkg <- imp[[1]]
                   impMethods <- imp[-1]
                   imp <- list(as.character(pkg), as.character(impMethods))
                   importMethods <<- c(importMethods, list(imp))
               },
               useDynLib = {
                   dyl <- e[-1]
                   dynlibs <<- c(dynlibs, as.character(dyl))
               },
               S3method = {
                   spec <- e[-1]
                   if (length(spec) != 2 && length(spec) != 3)
                       stop(paste("bad S3method directive:", deparse(e)),
                            call. = FALSE)
                   nS3 <<- nS3 + 1;
                   if(nS3 > 500)
                       stop("too many S3method directives", call. = FALSE)
                   S3methods[nS3, 1:length(spec)] <<- as.character(spec)
               },
               stop(paste("unknown namespace directive:", deparse(e)),
                    call. = FALSE)
               )
    }
    for (e in directives)
        parseDirective(e)
    list(imports=imports, exports=exports, exportPatterns = exportPatterns,
         importClasses=importClasses, importMethods=importMethods,
         exportClasses=exportClasses, exportMethods=exportMethods,
         dynlibs=dynlibs, S3methods = S3methods[seq(len=nS3), ,drop=FALSE])
}

registerS3method <- function(genname, class, method, envir = parent.frame()) {
    addNamespaceS3method <- function(ns, generic, class, method) {
        regs <- getNamespaceInfo(ns, "S3methods")
        regs <- cbind(regs, c(generic, class, method))
        setNamespaceInfo(ns, "S3methods", regs)
    }
    groupGenerics <- c("Math", "Ops",  "Summary", "Complex")
    defenv <- if(genname %in% groupGenerics) .BaseNamespaceEnv
    else {
        genfun <- get(genname, envir = envir)
        if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
            genfun <- methods::finalDefaultMethod(methods::getMethods(genname))@.Data
        if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
    }
    if (! exists(".__S3MethodsTable__.", envir = defenv, inherits = FALSE))
        assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = NULL),
               envir = defenv)
    table <- get(".__S3MethodsTable__.", envir = defenv, inherits = FALSE)
    if (is.character(method)) {
        wrap <- function(method, home) {
            method <- method            # force evaluation
            home <- home                # force evaluation
            delay(get(method, env = home), env = environment())
        }
        if(!exists(method, env = envir)) {
            warning(paste("S3 method",
                          sQuote(method),
                          "was declared in NAMESPACE but not found"),
                    call. = FALSE)
        } else {
            assign(paste(genname, class, sep = "."), wrap(method, envir),
                   envir = table)
        }
    }
    else if (is.function(method))
        assign(paste(genname, class, sep = "."), method, envir = table)
    else stop("bad method")
    if (isNamespace(envir) && ! identical(envir, .BaseNamespaceEnv))
        addNamespaceS3method(envir, genname, class, method)
}

# export <- function(expr, where = topenv(parent.frame()),
#                    exclusions = c("last.dump", "last.warning", ".Last.value",
#                        ".Random.seed", ".packageName", ".noGenerics", ".required")) {
#     ns <- as.environment(where)
#     if(isNamespace(ns)) {
#         expEnv <- new.env(hash = TRUE, parent =ns)
#         ## copy .packageName (will also make this qualify as topenv()
#         ## for class & method assignment
#         assign(".packageName", get(".packageName", envir = ns), envir = expEnv)
#         eval(substitute(expr), expEnv)
#         ## objects assigned will be exported.
#         allObjects  <- objects(expEnv, all=TRUE)
#         newExports <- allObjects[!(allObjects %in% exclusions)]
#         ## Merge any methods lists with existing versions in ns == parent.env(expEnv)
#         .mergeExportMethods(newExports, expEnv)
#         ## copy the objects
#         for(what in allObjects)
#             assign(what, get(what, envir = expEnv), envir = ns)
#         ## and update the exports information
#         exports <- getNamespaceInfo(ns, "exports")
#         for(what in newExports)
#             assign(what, what, envir = exports)
#     }
#     else
#         eval(substitute(expr), ns)
# }

registerS3methods <- function(info, package, env)
{
    wrap <- function(method, home) {
        method <- method            # force evaluation
        home <- home                # force evaluation
        delay(get(method, env = home), env = environment())
    }
    .registerS3method <- function(genname, class, method, nm, envir)
    {
        ## S3 generics should either be imported explicitly or be in
        ## the base namespace, so we start the search at the imports
        ## environment, parent.env(envir), which is followed by the
        ## base namespace.  (We have already looked in the namespace.)
        ## However, in case they have not been imported, we first
        ## look up where some commonly used generics are (including the
        ## group generics).
        defenv <- if(!is.na(w <- .knownS3Generics[genname])) asNamespace(w)
        else {
            if(!exists(genname, envir = parent.env(envir)))
                stop("object ", sQuote(genname),
                     " not found whilst loading namespace ",
                     sQuote(package), call. = FALSE)
            genfun <- get(genname, envir = parent.env(envir))
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction")) {
                genfun <- methods::slot(genfun, "default")@methods$ANY
                warning("found an S4 version of ", sQuote(genname),
                        " so it has not been imported correctly", call.=FALSE)
            }
            if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
        if (! exists(".__S3MethodsTable__.", envir = defenv, inherits = FALSE))
            assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = NULL),
                   envir = defenv)
        table <- get(".__S3MethodsTable__.", envir = defenv, inherits = FALSE)
        assign(nm, wrap(method, envir), envir = table)
    }

    n <- NROW(info)
    if(n == 0) return()
    methname <- paste(info[,1], info[,2], sep=".")
    z <- is.na(info[,3])
    info[z,3] <- methname[z]
    Info <- cbind(info, methname)
    loc <- .Internal(ls(env, TRUE))
    notex <- !(info[,3] %in% loc)
    if(any(notex))
        warning(paste("S3 method(s)",
                      paste(sQuote(info[notex, 3]), collapse=", "),
                      "were declared in NAMESPACE but not found"),
                call. = FALSE)
    Info <- Info[!notex, , drop = FALSE]

    ## do local generics first -- this could be load-ed if pre-computed.
    localGeneric <- Info[,1] %in% loc
    lin <- Info[localGeneric, , drop = FALSE]
    S3MethodsTable <- get(".__S3MethodsTable__.", envir = env,
                          inherits = FALSE)
    for(i in seq(len=nrow(lin)))
        assign(lin[i,4], get(lin[i,3], envir=env), envir = S3MethodsTable)

    ## now the rest
    fin <- Info[!localGeneric, , drop = FALSE]
    for(i in seq(len=nrow(fin)))
        .registerS3method(fin[i, 1], fin[i, 2], fin[i, 3], fin[i, 4], env)

    setNamespaceInfo(env, "S3methods",
                     rbind(info, getNamespaceInfo(env, "S3methods")))
}
