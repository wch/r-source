#  File src/library/base/R/namespace.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
#  https://www.R-project.org/Licenses/

## give the base namespace a table for registered methods
`.__S3MethodsTable__.` <- new.env(hash = TRUE, parent = baseenv())

## NOTA BENE:
##  1) This code should work also when methods is not yet loaded
##  2) We use  ':::' instead of '::' inside the code below, for efficiency only

getNamespace <- function(name) {
    ns <- .Internal(getRegisteredNamespace(name))
    if (! is.null(ns)) ns
    else tryCatch(loadNamespace(name), error = function(e) stop(e))
}

.getNamespace <- function(name) .Internal(getRegisteredNamespace(name))

..getNamespace <- function(name, where) {
    ns <- .Internal(getRegisteredNamespace(name))
    if (!is.null(ns)) ns
    else tryCatch(loadNamespace(name),
                  error = function(e) {
                      warning(gettextf("namespace %s is not available and has been replaced\nby .GlobalEnv when processing object %s",
                                       sQuote(name)[1L], sQuote(where)),
                              domain = NA, call. = FALSE, immediate. = TRUE)
                      .GlobalEnv
                  })
}

loadedNamespaces <- function() names(.Internal(getNamespaceRegistry()))

isNamespaceLoaded <- function(name) .Internal(isRegisteredNamespace(name))

getNamespaceName <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) "base"
    else .getNamespaceInfo(ns, "spec")["name"]
}

getNamespaceVersion <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns))
        c(version = paste(R.version$major, R.version$minor, sep = "."))
    else .getNamespaceInfo(ns, "spec")["version"]
}

getNamespaceExports <- function(ns) {
    ns <- asNamespace(ns)
    names(if(isBaseNamespace(ns)) .BaseNamespaceEnv
          else .getNamespaceInfo(ns, "exports"))
}

getNamespaceImports <- function(ns) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns)) NULL
    else .getNamespaceInfo(ns, "imports")
}

getNamespaceUsers <- function(ns) {
    nsname <- getNamespaceName(asNamespace(ns))
    users <- character()
    for (n in loadedNamespaces()) {
        inames <- names(getNamespaceImports(n))
        if (match(nsname, inames, 0L))
            users <- c(n, users)
    }
    users
}

getExportedValue <- function(ns, name) {
    ns <- asNamespace(ns)
    if (isBaseNamespace(ns))
	get(name, envir = ns, inherits = FALSE) # incl. error
    else {
	if (!is.null(oNam <- .getNamespaceInfo(ns, "exports")[[name]])) {
	    get0(oNam, envir = ns)
	} else { ##  <pkg> :: <dataset>  for lazydata :
	    ld <- .getNamespaceInfo(ns, "lazydata")
	    if (!is.null(obj <- ld[[name]]))
		obj
	    else { ## if there's a lazydata object with value NULL:
		if(exists(name, envir = ld, inherits = FALSE))
		    NULL
		else
		    stop(gettextf("'%s' is not an exported object from 'namespace:%s'",
				  name, getNamespaceName(ns)),
			 call. = FALSE, domain = NA)
	    }
	}
    }
}


`::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    getExportedValue(pkg, name)
}

## NOTE: Both "::" and ":::" must signal an error for non existing objects

`:::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    get(name, envir = asNamespace(pkg), inherits = FALSE)
}


attachNamespace <- function(ns, pos = 2L, depends = NULL)
{
    ## only used to run .onAttach
    runHook <- function(hookname, env, libname, pkgname) {
        if (!is.null(fun <- env[[hookname]])) {
            res <- tryCatch(fun(libname, pkgname), error = identity)
            if (inherits(res, "error")) {
                stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                              hookname, "attachNamespace", nsname,
                              deparse(conditionCall(res))[1L],
                              conditionMessage(res)),
                     call. = FALSE, domain = NA)
            }
        }
##         else if (exists(".First.lib", envir = env, inherits = FALSE) &&
##                  nsname == Sys.getenv("R_INSTALL_PKG"))
##             warning(sprintf("ignoring .First.lib() for package %s",
##                             sQuote(nsname)), domain = NA, call. = FALSE)
    }
    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach")) # might be list()
        for(fun in hook) try(fun(pkgname, pkgpath))
    }

    ns <- asNamespace(ns, base.OK = FALSE)
    nsname <- getNamespaceName(ns)
    nspath <- .getNamespaceInfo(ns, "path")
    attname <- paste("package", nsname, sep = ":")
    if (attname %in% search())
        stop("namespace is already attached")
    env <- attach(NULL, pos = pos, name = attname)
    ## we do not want to run e.g. .onDetach here
    on.exit(.Internal(detach(pos)))
    attr(env, "path") <- nspath
    exports <- getNamespaceExports(ns)
    importIntoEnv(env, exports, ns, exports)
    ## always exists, might be empty
    dimpenv <- .getNamespaceInfo(ns, "lazydata")
    dnames <- names(dimpenv)
    .Internal(importIntoEnv(env, dnames, dimpenv, dnames))
    if(length(depends) > 0L) env$.Depends <- depends
    Sys.setenv("_R_NS_LOAD_" = nsname)
    on.exit(Sys.unsetenv("_R_NS_LOAD_"), add = TRUE)
    runHook(".onAttach", ns, dirname(nspath), nsname)
    lockEnvironment(env, TRUE)
    runUserHook(nsname, nspath)
    on.exit()
    Sys.unsetenv("_R_NS_LOAD_")
    invisible(env)
}

## *inside* another function, useful to check for cycles
dynGet <- function(x, ifnotfound = stop(gettextf("%s not found",
			     sQuote(x)), domain = NA),
		   minframe = 1L, inherits = FALSE)
{
    n <- sys.nframe()
    myObj <- structure(list(.b = as.raw(7)), foo = 47L)# "very improbable" object
    while (n > minframe) {
	n <- n - 1L
	env <- sys.frame(n)
	r <- get0(x, envir = env, inherits=inherits, ifnotfound = myObj)
	if(!identical(r, myObj))
	    return(r)
    }
    ifnotfound
}

loadNamespace <- function (package, lib.loc = NULL,
                           keep.source = getOption("keep.source.pkgs"),
                           partial = FALSE, versionCheck = NULL)
{
    libpath <- attr(package, "LibPath")
    package <- as.character(package)[[1L]]

    loading <- dynGet("__NameSpacesLoading__", NULL)
    if (match(package, loading, 0L))
        stop("cyclic namespace dependency detected when loading ",
             sQuote(package), ", already loading ",
             paste(sQuote(loading), collapse = ", "),
             domain = NA)
    "__NameSpacesLoading__" <- c(package, loading)

    ns <- .Internal(getRegisteredNamespace(package))
    if (! is.null(ns)) {
        if(!is.null(zop <- versionCheck[["op"]]) &&
           !is.null(zversion <- versionCheck[["version"]])) {
            current <- getNamespaceVersion(ns)
            if(!do.call(zop, list(as.numeric_version(current), zversion)))
                stop(gettextf("namespace %s %s is already loaded, but %s %s is required",
                              sQuote(package), current, zop, zversion),
                     domain = NA)
        }
        ns
    } else {
        ## only used here for .onLoad
        runHook <- function(hookname, env, libname, pkgname) {
	    if (!is.null(fun <- env[[hookname]])) {
                res <- tryCatch(fun(libname, pkgname), error = identity)
                if (inherits(res, "error")) {
                    stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                                  hookname, "loadNamespace", pkgname,
                                  deparse(conditionCall(res))[1L],
                                  conditionMessage(res)),
                         call. = FALSE, domain = NA)
                }
            }
        }
        runUserHook <- function(pkgname, pkgpath) {
            hooks <- getHook(packageEvent(pkgname, "onLoad")) # might be list()
            for(fun in hooks) try(fun(pkgname, pkgpath))
        }
        makeNamespace <- function(name, version = NULL, lib = NULL) {
            impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
            attr(impenv, "name") <- paste("imports", name, sep = ":")
            env <- new.env(parent = impenv, hash = TRUE)
            name <- as.character(as.name(name))
            version <- as.character(version)
            info <- new.env(hash = TRUE, parent = baseenv())
            env$.__NAMESPACE__. <- info
            info$spec <- c(name = name, version = version)
            setNamespaceInfo(env, "exports", new.env(hash = TRUE, parent = baseenv()))
            dimpenv <- new.env(parent = baseenv(), hash = TRUE)
            attr(dimpenv, "name") <- paste("lazydata", name, sep = ":")
            setNamespaceInfo(env, "lazydata", dimpenv)
            setNamespaceInfo(env, "imports", list("base" = TRUE))
            ## this should be an absolute path
            setNamespaceInfo(env, "path",
                             normalizePath(file.path(lib, name), "/", TRUE))
            setNamespaceInfo(env, "dynlibs", NULL)
            setNamespaceInfo(env, "S3methods", matrix(NA_character_, 0L, 3L))
            env$.__S3MethodsTable__. <-
                new.env(hash = TRUE, parent = baseenv())
            .Internal(registerNamespace(name, env))
            env
        }
        sealNamespace <- function(ns) {
            namespaceIsSealed <- function(ns)
               environmentIsLocked(ns)
            ns <- asNamespace(ns, base.OK = FALSE)
            if (namespaceIsSealed(ns))
                stop(gettextf("namespace %s is already sealed in 'loadNamespace'",
                              sQuote(getNamespaceName(ns))),
                     call. = FALSE, domain = NA)
            lockEnvironment(ns, TRUE)
            lockEnvironment(parent.env(ns), TRUE)
        }
        addNamespaceDynLibs <- function(ns, newlibs) {
            dynlibs <- .getNamespaceInfo(ns, "dynlibs")
            setNamespaceInfo(ns, "dynlibs", c(dynlibs, newlibs))
        }

        bindTranslations <- function(pkgname, pkgpath)
        {
            ## standard packages are treated differently
            std <- c("compiler", "foreign", "grDevices", "graphics", "grid",
                     "methods", "parallel", "splines", "stats", "stats4",
                     "tcltk", "tools", "utils")
            popath <- if (pkgname %in% std) .popath else file.path(pkgpath, "po")
            if(!file.exists(popath)) return()
            bindtextdomain(pkgname, popath)
            bindtextdomain(paste("R", pkgname, sep = "-"), popath)
        }

        assignNativeRoutines <- function(dll, lib, env, nativeRoutines) {
            if(length(nativeRoutines) == 0L) return(NULL)

            if(nativeRoutines$useRegistration) {
                ## Use the registration information to register ALL the symbols
                fixes <- nativeRoutines$registrationFixes
                routines <- getDLLRegisteredRoutines.DLLInfo(dll, addNames = FALSE)
                lapply(routines,
                       function(type) {
                           lapply(type,
                                  function(sym) {
                                      varName <- paste0(fixes[1L], sym$name, fixes[2L])
                                      if(exists(varName, envir = env, inherits = FALSE))
                                          warning(gettextf("failed to assign RegisteredNativeSymbol for %s to %s since %s is already defined in the %s namespace",
                                                           sym$name, varName, varName, sQuote(package)),
                                                  domain = NA, call. = FALSE)
                                      else
                                          env[[varName]] <- sym
                                  })
                       })

            }

            symNames <- nativeRoutines$symbolNames
            if(length(symNames) == 0L) return(NULL)

            symbols <- getNativeSymbolInfo(symNames, dll, unlist = FALSE,
                                           withRegistrationInfo = TRUE)
            lapply(seq_along(symNames),
                   function(i) {
                       ## could vectorize this outside of the loop
                       ## and assign to different variable to
                       ## maintain the original names.
                       varName <- names(symNames)[i]
                       origVarName <- symNames[i]
                       if(exists(varName, envir = env, inherits = FALSE))
                           if(origVarName != varName)
                               warning(gettextf("failed to assign NativeSymbolInfo for %s to %s since %s is already defined in the %s namespace",
                                                origVarName, varName, varName, sQuote(package)),
                                       domain = NA, call. = FALSE)
                           else
                               warning(gettextf("failed to assign NativeSymbolInfo for %s since %s is already defined in the %s namespace",
                                                origVarName, varName, sQuote(package)),
                                       domain = NA, call. = FALSE)
                       else
                           assign(varName, symbols[[origVarName]], envir = env)

                   })
            symbols
        }

        ## find package and check it has a namespace
        pkgpath <- find.package(package, c(libpath, lib.loc), quiet = TRUE)
        if (length(pkgpath) == 0L)
            stop(gettextf("there is no package called %s", sQuote(package)),
                 domain = NA)
        bindTranslations(package, pkgpath)
        package.lib <- dirname(pkgpath)
        package <- basename(pkgpath) # need the versioned name
        if (! packageHasNamespace(package, package.lib)) {
            hasNoNamespaceError <-
                function (package, package.lib, call = NULL) {
                class <- c("hasNoNamespaceError", "error", "condition")
                msg <- gettextf("package %s does not have a namespace",
                                sQuote(package))
                structure(list(message = msg, package = package,
                               package.lib = package.lib, call = call),
                          class = class)
            }
            stop(hasNoNamespaceError(package, package.lib))
        }

        ## create namespace; arrange to unregister on error
        ## Can we rely on the existence of R-ng 'nsInfo.rds' and
        ## 'package.rds'?
        ## No, not during builds of standard packages
        ## stats4 depends on methods, but exports do not matter
        ## whilst it is being built
        nsInfoFilePath <- file.path(pkgpath, "Meta", "nsInfo.rds")
        nsInfo <- if(file.exists(nsInfoFilePath)) readRDS(nsInfoFilePath)
        else parseNamespaceFile(package, package.lib, mustExist = FALSE)

        pkgInfoFP <- file.path(pkgpath, "Meta", "package.rds")
        if(file.exists(pkgInfoFP)) {
            pkgInfo <- readRDS(pkgInfoFP)
            version <- pkgInfo$DESCRIPTION["Version"]
            vI <- pkgInfo$Imports
            if(is.null(built <- pkgInfo$Built))
                stop(gettextf("package %s has not been installed properly\n",
                              sQuote(basename(pkgpath))),
                     call. = FALSE, domain = NA)
            R_version_built_under <- as.numeric_version(built$R)
            if(R_version_built_under < "3.0.0")
                stop(gettextf("package %s was built before R 3.0.0: please re-install it",
                             sQuote(basename(pkgpath))),
                     call. = FALSE, domain = NA)
            ## we need to ensure that S4 dispatch is on now if the package
            ## will require it, or the exports will be incomplete.
            dependsMethods <- "methods" %in% names(pkgInfo$Depends)
            if(dependsMethods) loadNamespace("methods")
            if(!is.null(zop <- versionCheck[["op"]]) &&
               !is.null(zversion <- versionCheck[["version"]]) &&
               !do.call(zop, list(as.numeric_version(version), zversion)))
                stop(gettextf("namespace %s %s is being loaded, but %s %s is required",
                              sQuote(package), version, zop, zversion),
                     domain = NA)
        }
        ns <- makeNamespace(package, version = version, lib = package.lib)
        on.exit(.Internal(unregisterNamespace(package)))

        ## process imports
        for (i in nsInfo$imports) {
            if (is.character(i))
                namespaceImport(ns,
                                loadNamespace(i, c(lib.loc, .libPaths()),
                                              versionCheck = vI[[i]]),
                                from = package)
            else if (!is.null(i$except))
                namespaceImport(ns,
                                loadNamespace(j <- i[[1L]],
                                              c(lib.loc, .libPaths()),
                                              versionCheck = vI[[j]]),
                                from = package,
                                except = i$except)
            else
                namespaceImportFrom(ns,
                                    loadNamespace(j <- i[[1L]],
                                                  c(lib.loc, .libPaths()),
                                                  versionCheck = vI[[j]]),
                                    i[[2L]], from = package)
        }
        for(imp in nsInfo$importClasses)
            namespaceImportClasses(ns, loadNamespace(j <- imp[[1L]],
                                                     c(lib.loc, .libPaths()),
                                                     versionCheck = vI[[j]]),
                                   imp[[2L]], from = package)
        for(imp in nsInfo$importMethods)
            namespaceImportMethods(ns, loadNamespace(j <- imp[[1L]],
                                                     c(lib.loc, .libPaths()),
                                                     versionCheck = vI[[j]]),
                                   imp[[2L]], from = package)

        ## store info for loading namespace for loadingNamespaceInfo to read
        "__LoadingNamespaceInfo__" <- list(libname = package.lib,
                                           pkgname = package)

        env <- asNamespace(ns)
        ## save the package name in the environment
        env$.packageName <- package

        ## load the code
        codename <- strsplit(package, "_", fixed = TRUE)[[1L]][1L]
        codeFile <- file.path(pkgpath, "R", codename)
        if (file.exists(codeFile)) {
	    # The code file has been converted to the native encoding
	    save.enc <- options(encoding = "native.enc")
            res <- try(sys.source(codeFile, env, keep.source = keep.source))
	    options(save.enc)
            if(inherits(res, "try-error"))
                stop(gettextf("unable to load R code in package %s",
                              sQuote(package)), call. = FALSE, domain = NA)
        }
        # a package without R code currently is required to have a namespace
        # else warning(gettextf("package %s contains no R code",
        #                        sQuote(package)), call. = FALSE, domain = NA)

        ## partial loading stops at this point
        ## -- used in preparing for lazy-loading
        if (partial) return(ns)

        ## lazy-load any sysdata
        dbbase <- file.path(pkgpath, "R", "sysdata")
        if (file.exists(paste0(dbbase, ".rdb"))) lazyLoad(dbbase, env)

        ## load any lazydata into a separate environment
        dbbase <- file.path(pkgpath, "data", "Rdata")
        if(file.exists(paste0(dbbase, ".rdb")))
            lazyLoad(dbbase, .getNamespaceInfo(env, "lazydata"))

        ## register any S3 methods
        registerS3methods(nsInfo$S3methods, package, env)

        ## load any dynamic libraries
        dlls <- list()
        dynLibs <- nsInfo$dynlibs
        for (i in seq_along(dynLibs)) {
            lib <- dynLibs[i]
            dlls[[lib]]  <- library.dynam(lib, package, package.lib)
            assignNativeRoutines(dlls[[lib]], lib, env,
                                 nsInfo$nativeRoutines[[lib]])

            ## If the DLL has a name as in useDynLib(alias = foo),
            ## then assign DLL reference to alias.  Check if
            ## names() is NULL to handle case that the nsInfo.rds
            ## file was created before the names were added to the
            ## dynlibs vector.
            if(!is.null(names(nsInfo$dynlibs))
               && nzchar(names(nsInfo$dynlibs)[i]))
                env[[names(nsInfo$dynlibs)[i]]] <- dlls[[lib]]
            setNamespaceInfo(env, "DLLs", dlls)
        }
        addNamespaceDynLibs(env, nsInfo$dynlibs)


        ## used in e.g. utils::assignInNamespace
        Sys.setenv("_R_NS_LOAD_" = package)
        on.exit(Sys.unsetenv("_R_NS_LOAD_"), add = TRUE)
        ## run the load hook
        runHook(".onLoad", env, package.lib, package)

        ## process exports, seal, and clear on.exit action
        exports <- nsInfo$exports

        for (p in nsInfo$exportPatterns)
            exports <- c(ls(env, pattern = p, all.names = TRUE), exports)
        ##
        if(.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns) &&
           !identical(package, "methods") ) {
            ## cache generics, classes in this namespace (but not methods itself,
            ## which pre-cached at install time
            methods::cacheMetaData(ns, TRUE, ns)
            ## This also ran .doLoadActions
            ## load actions may have added objects matching patterns
            for (p in nsInfo$exportPatterns) {
                expp <- ls(ns, pattern = p, all.names = TRUE)
                newEx <- !(expp %in% exports)
                if(any(newEx))
                    exports <- c(expp[newEx], exports)
            }
            ## process class definition objects
            expClasses <- nsInfo$exportClasses
            ##we take any pattern, but check to see if the matches are classes
            pClasses <- character()
            aClasses <- methods::getClasses(ns)
            classPatterns <- nsInfo$exportClassPatterns
            ## defaults to exportPatterns
            if(!length(classPatterns))
                classPatterns <- nsInfo$exportPatterns
            for (p in classPatterns) {
                pClasses <- c(aClasses[grep(p, aClasses)], pClasses)
            }
            pClasses <- unique(pClasses)
            if( length(pClasses) ) {
                good <- vapply(pClasses, methods::isClass, NA, where = ns)
                if( !any(good) && length(nsInfo$exportClassPatterns))
                    warning(gettextf("'exportClassPattern' specified in 'NAMESPACE' but no matching classes in package %s", sQuote(package)),
                            call. = FALSE, domain = NA)
                expClasses <- c(expClasses, pClasses[good])
            }
            if(length(expClasses)) {
                missingClasses <-
                    !vapply(expClasses, methods::isClass, NA, where = ns)
                if(any(missingClasses))
                    stop(gettextf("in package %s classes %s were specified for export but not defined",
                                  sQuote(package),
                                  paste(expClasses[missingClasses],
                                        collapse = ", ")),
                         domain = NA)
                expClasses <- paste0(methods::classMetaName(""), expClasses)
            }
            ## process methods metadata explicitly exported or
            ## implied by exporting the generic function.
            allGenerics <- unique(c(methods:::.getGenerics(ns),
                                   methods:::.getGenerics(parent.env(ns))))
            expMethods <- nsInfo$exportMethods
            ## check for generic functions corresponding to exported methods
            addGenerics <- expMethods[is.na(match(expMethods, exports))]
            if(length(addGenerics)) {
                nowhere <- vapply(addGenerics, function(what) !exists(what, mode = "function", envir = ns),
                                  NA, USE.NAMES=FALSE)
                if(any(nowhere)) {
                    warning(gettextf("no function found corresponding to methods exports from %s for: %s",
                                     sQuote(package),
                                     paste(sQuote(sort(unique(addGenerics[nowhere]))), collapse = ", ")),
                         domain = NA, call. = FALSE)
                    addGenerics <- addGenerics[!nowhere]
                }
                if(length(addGenerics)) {
                    ## skip primitives
                    addGenerics <- addGenerics[vapply(addGenerics, function(what)
                        !is.primitive(get(what, mode = "function", envir = ns)), NA)]
                    ## the rest must be generic functions, implicit or local
                    ## or have been cached via a DEPENDS package
		    ok <- vapply(addGenerics, methods:::.findsGeneric, 1L, ns)
                    if(!all(ok)) {
                        bad <- sort(unique(addGenerics[!ok]))
                        msg <-
                            ngettext(length(bad),
                                     "Function found when exporting methods from the namespace %s which is not S4 generic: %s",
                                     "Functions found when exporting methods from the namespace %s which are not S4 generic: %s")
                        stop(sprintf(msg, sQuote(package),
                                     paste(sQuote(bad), collapse = ", ")),
                             domain = NA, call. = FALSE)
                    }
                    else if(any(ok > 1L))  #from the cache, don't add
                        addGenerics <- addGenerics[ok < 2L]
                }
### <note> Uncomment following to report any local generic functions
### that should have been exported explicitly.  But would be reported
### whenever the package is loaded, which is not when it is relevant.
### </note>
                ## local <- sapply(addGenerics, function(what) identical(as.character(get(what, envir = ns)@package), package))
                ## if(any(local))
                ##     message(gettextf("export(%s) from package %s generated by exportMethods()",
                ##        paste(addGenerics[local], collapse = ", ")),
                ##             domain = NA)
                exports <- c(exports, addGenerics)
            }
            expTables <- character()
            if(length(allGenerics)) {
                expMethods <-
                    unique(c(expMethods,
                             exports[!is.na(match(exports, allGenerics))]))
                missingMethods <- !(expMethods %in% allGenerics)
                if(any(missingMethods))
                    stop(gettextf("in %s methods for export not found: %s",
                                  sQuote(package),
                                  paste(expMethods[missingMethods],
                                        collapse = ", ")),
                         domain = NA)
                tPrefix <- methods:::.TableMetaPrefix()
                allMethodTables <-
                    unique(c(methods:::.getGenerics(ns, tPrefix),
                             methods:::.getGenerics(parent.env(ns), tPrefix)))
                needMethods <-
                    (exports %in% allGenerics) & !(exports %in% expMethods)
                if(any(needMethods))
                    expMethods <- c(expMethods, exports[needMethods])
                ## Primitives must have their methods exported as long
                ## as a global table is used in the C code to dispatch them:
                ## The following keeps the exported files consistent with
                ## the internal table.
                pm <- allGenerics[!(allGenerics %in% expMethods)]
                if(length(pm)) {
                    prim <- vapply(pm, function(pmi) {
                                       f <- methods::getFunction(pmi, FALSE,
                                                                 FALSE, ns)
                                       is.primitive(f)
                                   }, logical(1L))
                    expMethods <- c(expMethods, pm[prim])
                }
                for(i in seq_along(expMethods)) {
                    mi <- expMethods[[i]]
                    if(!(mi %in% exports) &&
                       exists(mi, envir = ns, mode = "function",
                              inherits = FALSE))
                        exports <- c(exports, mi)
                    pattern <- paste0(tPrefix, mi, ":")
                    ii <- grep(pattern, allMethodTables, fixed = TRUE)
                    if(length(ii)) {
			if(length(ii) > 1L) {
			    warning(gettextf("multiple methods tables found for %s",
				    sQuote(mi)), call. = FALSE, domain = NA)
			    ii <- ii[1L]
			}
                        expTables[[i]] <- allMethodTables[ii]
                     }
                    else { ## but not possible?
                      warning(gettextf("failed to find metadata object for %s",
                                       sQuote(mi)), call. = FALSE, domain = NA)
                    }
                }
            }
            else if(length(expMethods))
                stop(gettextf("in package %s methods %s were specified for export but not defined",
                              sQuote(package),
                              paste(expMethods, collapse = ", ")),
                     domain = NA)
            exports <- unique(c(exports, expClasses,  expTables))
        }
        ## certain things should never be exported.
        if (length(exports)) {
            stoplist <- c(".__NAMESPACE__.", ".__S3MethodsTable__.",
                          ".packageName", ".First.lib", ".onLoad",
                          ".onAttach", ".conflicts.OK", ".noGenerics")
            exports <- exports[! exports %in% stoplist]
        }
        namespaceExport(ns, exports)
        sealNamespace(ns)
        runUserHook(package, pkgpath)
        on.exit()
        Sys.unsetenv("_R_NS_LOAD_")
        ns
    }
}

## A version which returns TRUE/FALSE
requireNamespace <- function (package, ..., quietly = FALSE)
{
    package <- as.character(package)[[1L]] # like loadNamespace
    ns <- .Internal(getRegisteredNamespace(package))
    res <- TRUE
    if (is.null(ns)) {
        if(!quietly)
            packageStartupMessage(gettextf("Loading required namespace: %s",
                                           package), domain = NA)
        value <- tryCatch(loadNamespace(package, ...), error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ",
                    sQuote(msg), "\n", file = stderr(), sep = "")
                .Internal(printDeferredWarnings())
            }
            res <- FALSE
        }
    }
    invisible(res)
}

loadingNamespaceInfo <- function() {
    dynGet("__LoadingNamespaceInfo__", stop("not loading a namespace"))
}

topenv <- function(envir = parent.frame(),
                   matchThisEnv = getOption("topLevelEnvironment")) {
    .Internal(topenv(envir, matchThisEnv))
}

unloadNamespace <- function(ns)
{
    ## check, so we do not load & unload:
    if ((is.character(ns) && any(ns == loadedNamespaces())) ||
        (is.environment(ns) && any(getNamespaceName(ns) == loadedNamespaces()))) {
	## only used to run .onUnload
	runHook <- function(hookname, env, ...) {
	    if (!is.null(fun <- env[[hookname]])) {
		res <- tryCatch(fun(...), error=identity)
		if (inherits(res, "error")) {
		    warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
				     hookname, "unloadNamespace", nsname,
				     deparse(conditionCall(res))[1L],
				     conditionMessage(res)),
			    call. = FALSE, domain = NA)
		}
	    }
	}
	ns <- asNamespace(ns, base.OK = FALSE)
	nsname <- getNamespaceName(ns)
	pos <- match(paste("package", nsname, sep = ":"), search())
	if (! is.na(pos)) detach(pos = pos)
	users <- getNamespaceUsers(ns)
	if (length(users))
	    stop(gettextf("namespace %s is imported by %s so cannot be unloaded",
			  sQuote(getNamespaceName(ns)),
			  paste(sQuote(users), collapse = ", ")),
		 domain = NA)
	nspath <- .getNamespaceInfo(ns, "path")
	hook <- getHook(packageEvent(nsname, "onUnload")) # might be list()
	for(fun in rev(hook)) try(fun(nsname, nspath))
	runHook(".onUnload", ns, nspath)
	.Internal(unregisterNamespace(nsname))
	if(.isMethodsDispatchOn() && methods:::.hasS4MetaData(ns))
	    methods::cacheMetaData(ns, FALSE, ns)
	.Internal(lazyLoadDBflush(paste0(nspath, "/R/", nsname, ".rdb")))
    }
    invisible()
}

isNamespace <- function(ns) .Internal(isNamespaceEnv(ns))

isBaseNamespace <- function(ns) identical(ns, .BaseNamespaceEnv)

getNamespaceInfo <- function(ns, which) {
    ns <- asNamespace(ns, base.OK = FALSE)
    get(which, envir = ns[[".__NAMESPACE__."]])
}

.getNamespaceInfo <- function(ns, which) {
    ns[[".__NAMESPACE__."]][[which]]
}

setNamespaceInfo <- function(ns, which, val) {
    ns <- asNamespace(ns, base.OK = FALSE)
    info <- ns[[".__NAMESPACE__."]]
    info[[which]] <- val
}

asNamespace <- function(ns, base.OK = TRUE) {
    if (is.character(ns) || is.name(ns))
        ns <- getNamespace(ns)
    if (! isNamespace(ns))
        stop("not a namespace")
    else if (! base.OK && isBaseNamespace(ns))
        stop("operation not allowed on base namespace")
    else ns
}

namespaceImport <- function(self, ..., from = NULL, except = character(0L))
    for (ns in list(...))
        namespaceImportFrom(self, asNamespace(ns), from = from,
                            except = except)

namespaceImportFrom <- function(self, ns, vars, generics, packages,
                                from = "non-package environment",
                                except = character(0L))
{
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
        else {
            change <- !nzchar(new)
            new[change] <- old[change]
        }
        names(old) <- new
        old
    }
    whichMethodMetaNames <- function(impvars) {
        if(!.isMethodsDispatchOn())
            return(numeric())
	seq_along(impvars)[startsWith(impvars, ".__T__")]
    }
    genericPackage <- function(f) {
        if(methods::is(f, "genericFunction")) f@package
        else if(is.primitive(f)) "base"
        else "<unknown>"
    }
    if (is.character(self))
        self <- getNamespace(self)
    ns <- asNamespace(ns)
    nsname <- getNamespaceName(ns)
    impvars <- if (missing(vars)) {
        ## certain things should never be imported:
        ## but most of these are never exported (exception: .Last.lib)
        stoplist <- c(".__NAMESPACE__.", ".__S3MethodsTable__.",
                      ".packageName", ".First.lib", ".Last.lib",
                      ".onLoad", ".onAttach", ".onDetach",
                      ".conflicts.OK", ".noGenerics")
        vars <- getNamespaceExports(ns)
        vars <- vars[! vars %in% stoplist]
    } else vars
    impvars <- impvars[! impvars %in% except]
    impvars <- makeImportExportNames(impvars)
    impnames <- names(impvars)
    if (anyDuplicated(impnames)) {
        stop(gettextf("duplicate import names %s",
                      paste(sQuote(impnames[duplicated(impnames)]),
                            collapse = ", ")), domain = NA)
    }
    if (isNamespace(self)) {
        if(isBaseNamespace(self)) {
            impenv <- self
            msg <- gettext("replacing local value with import %s when loading %s")
            register <- FALSE
        }
        else {
            if (namespaceIsSealed(self))
                stop("cannot import into a sealed namespace")
            impenv <- parent.env(self)
            msg <- gettext("replacing previous import by %s when loading %s")
            register <- TRUE
        }
    }
    else if (is.environment(self)) {
        impenv <- self
        msg <- gettext("replacing local value with import %s when loading %s")
        register <- FALSE
    }
    else stop("invalid import target")
    which <- whichMethodMetaNames(impvars)
    if(length(which)) {
	## If methods are already in impenv, merge and don't import
	delete <- integer()
	for(i in which) {
	    methodsTable <- .mergeImportMethods(impenv, ns, impvars[[i]])
	    if(is.null(methodsTable))
	    {} ## first encounter, just import it
	    else { ##
		delete <- c(delete, i)
		if(!missing(generics)) {
		    genName <- generics[[i]]
                    ## if(i > length(generics) || !nzchar(genName))
                    ##   {warning("got invalid index for importing ",mlname); next}
		    fdef <- methods::getGeneric(genName,
                                                where = impenv,
                                                package = packages[[i]])
		    if(is.null(fdef))
			warning(gettextf("found methods to import for function %s but not the generic itself",
					 sQuote(genName)),
                                call. = FALSE, domain = NA)
		    else
			methods:::.updateMethodsInTable(fdef, ns, TRUE)
		}
	    }
	}
	if(length(delete)) {
	    impvars <- impvars[-delete]
	    impnames <- impnames[-delete]
	}
    }
    for (n in impnames)
	if (!is.null(genImp <- impenv[[n]])) {
	    if (.isMethodsDispatchOn() && methods::isGeneric(n, ns)) {
		## warn only if generic overwrites a function which
		## it was not derived from
		genNs <- genericPackage(get(n, envir = ns))
                if(identical(genNs, genericPackage(genImp))) next # same generic
		genImpenv <- environmentName(environment(genImp))
                ## May call environment() on a non-function--an undocumented
                ## "feature" of environment() is that it returns a special
                ## attribute for non-functions, usually NULL
		if (!identical(genNs, genImpenv) ||
                    methods::isGeneric(n, impenv)) {}
                else next
	    }
            if (identical(genImp, get(n, ns))) next
            if (isNamespace(self) && !isBaseNamespace(self)) {
                ## Now try to figure out where we imported from
                ## The 'imports' list is named by where-from
                ## and is in order of adding.
                current <- getNamespaceInfo(self, "imports")
                poss <- lapply(rev(current), "[", n)
                poss <- poss[!sapply(poss, is.na)]
                if(length(poss) >= 1L) {
                    msg <- gettext("replacing previous import %s by %s when loading %s")
                    prev <- names(poss)[1L]
                    warning(sprintf(msg,
                                    sQuote(paste(prev, n, sep = "::")),
                                    sQuote(paste(nsname, n, sep = "::")),
                                    sQuote(from)),
                            call. = FALSE, domain = NA)
                } else
                    warning(sprintf(msg, sQuote(paste(nsname, n, sep = "::")),
                                    sQuote(from)),
                            call. = FALSE, domain = NA)
            } else {
                ## this is always called from another function,
                ## so reporting call is unhelpful
                warning(sprintf(msg, sQuote(paste(nsname, n, sep = "::")),
                                sQuote(from)),
                        call. = FALSE, domain = NA)
            }
	}
    importIntoEnv(impenv, impnames, ns, impvars)
    if (register)
        addImports(self, ns, if (missing(vars)) TRUE else impvars)
}

namespaceImportClasses <- function(self, ns, vars, from = NULL)
{
    for(i in seq_along(vars))
        vars[[i]] <- methods::classMetaName(vars[[i]])
    namespaceImportFrom(self, asNamespace(ns), vars, from = from)
}

namespaceImportMethods <- function(self, ns, vars, from = NULL)
{
    allVars <- character()
    generics <- character()
    packages <- character()
    allFuns <- methods:::.getGenerics(ns) # all the methods tables in ns
    allPackages <- attr(allFuns, "package")
    pkg <- methods::getPackageName(ns)
    if(!all(vars %in% allFuns)) {
        message(gettextf("No methods found in \"%s\" for requests: %s",
                         pkg, paste(vars[is.na(match(vars, allFuns))], collapse = ", ")),
                domain = NA)
        vars <- vars[vars %in% allFuns]
    }
    if(any(is.na(match(vars, allFuns))))
        stop(gettextf("requested methods not found in environment/package %s: %s",
                      sQuote(pkg),
                      paste(vars[is.na(match(vars, allFuns))],
                            collapse = ", ")), call. = FALSE, domain = NA)
    for(i in seq_along(allFuns)) {
        ## import methods tables if asked for
        ## or if the corresponding generic was imported
        g <- allFuns[[i]]
        p <- allPackages[[i]]
        if(exists(g, envir = self, inherits = FALSE) # already imported
           || g %in% vars) { # requested explicitly
            tbl <- methods:::.TableMetaName(g, p)
            if(is.null(.mergeImportMethods(self, ns, tbl))) { # a new methods table
               allVars <- c(allVars, tbl) # import it;else, was merged
               generics <- c(generics, g)
               packages <- c(packages, p)
            }
        }
        if(g %in% vars && !exists(g, envir = self, inherits = FALSE)) {
	    if(!is.null(f <- get0(g, envir = ns)) && methods::is(f, "genericFunction")) {
                allVars <- c(allVars, g)
                generics <- c(generics, g)
                packages <- c(packages, p)
            } else if (g %in% c("as.vector", "is.unsorted", "unlist")) {
                ## implicit generics
            } else { # should be primitive
                fun <- methods::getFunction(g, mustFind = FALSE, where = self)
                if(is.primitive(fun) || methods::is(fun, "genericFunction")) {}
                else
                    warning(gettextf("No generic function %s found corresponding to requested imported methods from package %s when loading %s (malformed exports?)",
                                 sQuote(g), sQuote(pkg), sQuote(from)),
                        domain = NA, call. = FALSE)
            }
        }
    }
    namespaceImportFrom(self, asNamespace(ns), allVars, generics, packages,
                        from = from)
}

importIntoEnv <- function(impenv, impnames, expenv, expnames) {
    exports <- getNamespaceInfo(expenv, "exports")
    ex <- names(exports)
    if(!all(eie <- expnames %in% ex)) {
        miss <- expnames[!eie]
        ## if called (indirectly) for namespaceImportClasses
        ## these are all classes
        if(all(startsWith(miss, ".__C__"))) {
            miss <- sub("^\\.__C__", "", miss)
            stop(sprintf(ngettext(length(miss),
                                  "class %s is not exported by 'namespace:%s'",
                                  "classes %s are not exported by 'namespace:%s'"),
                         paste(paste0('"', miss, '"'), collapse = ", "),
                         getNamespaceName(expenv)),
                 call. = FALSE, domain = NA)
        } else {
            stop(sprintf(ngettext(length(miss),
                                  "object %s is not exported by 'namespace:%s'",
                                  "objects %s are not exported by 'namespace:%s'"),
                         paste(sQuote(miss), collapse = ", "),
                         getNamespaceName(expenv)),
                 call. = FALSE, domain = NA)
        }
    }
    expnames <- unlist(mget(expnames, envir = exports, inherits = FALSE), recursive=FALSE)
    if (is.null(impnames)) impnames <- character()
    if (is.null(expnames)) expnames <- character()
    .Internal(importIntoEnv(impenv, impnames, expenv, expnames))
}

namespaceExport <- function(ns, vars) {
    namespaceIsSealed <- function(ns)
       environmentIsLocked(ns)
    if (namespaceIsSealed(ns))
        stop("cannot add to exports of a sealed namespace")
    ns <- asNamespace(ns, base.OK = FALSE)
    if (length(vars)) {
        addExports <- function(ns, new) {
            exports <- .getNamespaceInfo(ns, "exports")
            expnames <- names(new)
            objs <- names(exports)
            ex <- expnames %in% objs
            if(any(ex))
                warning(sprintf(ngettext(sum(ex),
                                         "previous export '%s' is being replaced",
                                         "previous exports '%s' are being replaced"),
                                paste(sQuote(expnames[ex]), collapse = ", ")),
                        call. = FALSE, domain = NA)
            list2env(as.list(new), exports)
        }
        makeImportExportNames <- function(spec) {
            old <- as.character(spec)
            new <- names(spec)
            if (is.null(new)) new <- old
            else {
                change <- !nzchar(new)
                new[change] <- old[change]
            }
            names(old) <- new
            old
        }
        new <- makeImportExportNames(unique(vars))
        ## calling exists each time is too slow, so do two phases
        undef <- new[! new %in% names(ns)]
        undef <- undef[! vapply(undef, exists, NA, envir = ns)]
        if (length(undef)) {
            undef <- do.call("paste", as.list(c(undef, sep = ", ")))
            stop(gettextf("undefined exports: %s", undef), domain = NA)
        }
        if(.isMethodsDispatchOn()) .mergeExportMethods(new, ns)
        addExports(ns, new)
    }
}

.mergeExportMethods <- function(new, ns)
{
    ## avoid bootstrapping issues when using methods:::methodsPackageMetaName("M","")
    ## instead of  ".__M__" :
    newMethods <- new[startsWith(new, ".__M__")]
    nsimports <- parent.env(ns)
    for(what in newMethods) {
	if(!is.null(m1 <- nsimports[[what]])) {
            m2 <- get(what, envir = ns)
            ns[[what]] <- methods::mergeMethods(m1, m2)
        }
    }
}

packageHasNamespace <- function(package, package.lib)
    file.exists(file.path(package.lib, package, "NAMESPACE"))

parseNamespaceFile <- function(package, package.lib, mustExist = TRUE)
{
    namespaceFilePath <- function(package, package.lib)
        file.path(package.lib, package, "NAMESPACE")

    ## These two functions are essentially local to the parsing of
    ## the namespace file and don't need to be made available to
    ## users.  These manipulate the data from useDynLib() directives
    ## for the same DLL to determine how to map the symbols to R
    ## variables.

    nativeRoutineMap <-
        ## Creates a new NativeRoutineMap.
        function(useRegistration, symbolNames, fixes) {
            proto <- list(useRegistration = FALSE,
                          symbolNames = character())
            class(proto) <- "NativeRoutineMap"

            mergeNativeRoutineMaps(proto, useRegistration, symbolNames, fixes)
        }

    mergeNativeRoutineMaps <-
        ## Merges new settings into a NativeRoutineMap
        function(map, useRegistration, symbolNames, fixes) {
            if(!useRegistration)
                names(symbolNames) <-
                    paste0(fixes[1L],  names(symbolNames), fixes[2L])
            else
                map$registrationFixes <- fixes
            map$useRegistration <- map$useRegistration || useRegistration
            map$symbolNames <- c(map$symbolNames, symbolNames)
            map
        }

    nsFile <- namespaceFilePath(package, package.lib)
    descfile <- file.path(package.lib, package, "DESCRIPTION")
    enc <- if (file.exists(descfile)) {
        read.dcf(file = descfile, "Encoding")[1L]
    } else NA_character_
    if (file.exists(nsFile))
        directives <- if (!is.na(enc) &&
                          ! Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
	    con <- file(nsFile, encoding=enc)
            on.exit(close(con))
	    parse(con, keep.source = FALSE, srcfile = NULL)
        } else parse(nsFile, keep.source = FALSE, srcfile = NULL)
    else if (mustExist)
        stop(gettextf("package %s has no 'NAMESPACE' file", sQuote(package)),
             domain = NA)
    else directives <- NULL
    exports <- character()
    exportPatterns <- character()
    exportClasses <- character()
    exportClassPatterns <- character()
    exportMethods <- character()
    imports <- list()
    importMethods <- list()
    importClasses <- list()
    dynlibs <- character()
    nS3methods <- 1000L
    S3methods <- matrix(NA_character_, nS3methods, 3L)
    nativeRoutines <- list()
    nS3 <- 0L
    parseDirective <- function(e) {
        ## trying to get more helpful error message:
	asChar <- function(cc) {
	    r <- as.character(cc)
	    if(any(r == ""))
		stop(gettextf("empty name in directive '%s' in 'NAMESPACE' file",
			      as.character(e[[1L]])),
		     domain = NA)
	    r
	}
        evalToChar <- function(cc) {
            vars <- all.vars(cc)
            names(vars) <- vars
            as.character(eval(eval(call("substitute", cc, as.list(vars))),
                              .GlobalEnv))
        }
        switch(as.character(e[[1L]]),
               "if" = if (eval(e[[2L]], .GlobalEnv))
               parseDirective(e[[3L]])
               else if (length(e) == 4L)
               parseDirective(e[[4L]]),
               "{" =  for (ee in as.list(e[-1L])) parseDirective(ee),
               "=" =,
               "<-" = {
                   parseDirective(e[[3L]])
                   if(as.character(e[[3L]][[1L]]) == "useDynLib")
                       names(dynlibs)[length(dynlibs)] <<- asChar(e[[2L]])
               },
               export = {
                   exp <- e[-1L]
                   exp <- structure(asChar(exp), names = names(exp))
                   exports <<- c(exports, exp)
               },
               exportPattern = {
                   pat <- asChar(e[-1L])
                   exportPatterns <<- c(pat, exportPatterns)
               },
               exportClassPattern = {
                   pat <- asChar(e[-1L])
                   exportClassPatterns <<- c(pat, exportClassPatterns)
               },
               exportClass = , exportClasses = {
                   exportClasses <<- c(asChar(e[-1L]), exportClasses)
               },
               exportMethods = {
                   exportMethods <<- c(asChar(e[-1L]), exportMethods)
               },
               import = {
                   except <- e$except
                   e$except <- NULL
                   pkgs <- as.list(asChar(e[-1L]))
                   if (!is.null(except)) {
                       pkgs <- lapply(pkgs, list, except=evalToChar(except))
                   }
                   imports <<- c(imports, pkgs)
               },
               importFrom = {
                   imp <- e[-1L]
                   ivars <- imp[-1L]
                   inames <- names(ivars)
                   imp <- list(asChar(imp[1L]),
                               structure(asChar(ivars), names = inames))
                   imports <<- c(imports, list(imp))
               },
               importClassFrom = , importClassesFrom = {
                   imp <- asChar(e[-1L])
                   pkg <- imp[[1L]]
                   impClasses <- imp[-1L]
                   imp <- list(asChar(pkg), asChar(impClasses))
                   importClasses <<- c(importClasses, list(imp))
               },
               importMethodsFrom = {
                   imp <- asChar(e[-1L])
                   pkg <- imp[[1L]]
                   impMethods <- imp[-1L]
                   imp <- list(asChar(pkg), asChar(impMethods))
                   importMethods <<- c(importMethods, list(imp))
               },
               useDynLib = {

                   ## This attempts to process as much of the
                   ## information as possible when NAMESPACE is parsed
                   ## rather than when it is loaded and creates
                   ## NativeRoutineMap objects to handle the mapping
                   ## of symbols to R variable names.

                   ## The name is the second element after useDynLib
                   dyl <- as.character(e[2L])
                   ## We ensure uniqueness at the end.
                   dynlibs <<-
                       structure(c(dynlibs, dyl),
                                 names = c(names(dynlibs),
                                 ifelse(!is.null(names(e)) &&
                                        nzchar(names(e)[2L]), names(e)[2L], "" )))
                   if (length(e) > 2L) {
                       ## Author has specified some mappings for the symbols

                       symNames <- as.character(e[-c(1L, 2L)])
                       names(symNames) <- names(e[-c(1, 2)])

                       ## If there are no names, then use the names of
                       ## the symbols themselves.
                       if (length(names(symNames)) == 0L)
                           names(symNames) = symNames
                       else if (any(w <- names(symNames) == "")) {
                           names(symNames)[w] = symNames[w]
                       }

                       ## For each DLL, we build up a list the (R
                       ## variable name, symbol name) mappings. We do
                       ## this in a NativeRoutineMap object and we
                       ## merge potentially multiple useDynLib()
                       ## directives for the same DLL into a single
                       ## map.  Then we have separate NativeRoutineMap
                       ## for each different DLL.  E.g. if we have
                       ## useDynLib(foo, a, b, c) and useDynLib(bar,
                       ## a, x, y) we would maintain and resolve them
                       ## separately.

                       dup <- duplicated(names(symNames))
                       if (any(dup))
                           warning(gettextf("duplicate symbol names %s in useDynLib(\"%s\")",
                                            paste(sQuote(names(symNames)[dup]),
                                                  collapse = ", "), dyl),
                                   domain = NA, call. = FALSE)

                       symNames <- symNames[!dup]

                       ## Deal with any prefix/suffix pair.
                       fixes <- c("", "")
                       idx <- match(".fixes", names(symNames))
                       if(!is.na(idx)) {
                           ## Take .fixes and treat it as a call,
                           ## e.g. c("pre", "post") or a regular name
                           ## as the prefix.
                           if(nzchar(symNames[idx])) {
                               e <- parse(text = symNames[idx],
                                          keep.source = FALSE,
                                          srcfile = NULL)[[1L]]
                               if(is.call(e))
                                   val <- eval(e, .GlobalEnv)
                               else
                                   val <- as.character(e)
                               if(length(val))
                                   fixes[seq_along(val)] <- val
                           }
                           symNames <- symNames[-idx]
                       }

                       ## Deal with a .registration entry. It must be
                       ## .registration = value and value will be coerced
                       ## to a logical.
                       useRegistration <- FALSE
                       idx <- match(".registration", names(symNames))
                       if(!is.na(idx)) {
                           useRegistration <- as.logical(symNames[idx])
                           symNames <- symNames[-idx]
                       }

                       ## Now merge into the NativeRoutineMap.
                       nativeRoutines[[ dyl ]] <<-
                           if(dyl %in% names(nativeRoutines))
                               mergeNativeRoutineMaps(nativeRoutines[[ dyl ]],
                                                      useRegistration,
                                                      symNames, fixes)
                           else
                               nativeRoutineMap(useRegistration, symNames,
                                                fixes)
                   }
               },
               S3method = {
                   spec <- e[-1L]
                   if (length(spec) != 2L && length(spec) != 3L)
                       stop(gettextf("bad 'S3method' directive: %s",
                                     deparse(e)),
                            call. = FALSE, domain = NA)
                   nS3 <<- nS3 + 1L
                   if(nS3 > nS3methods) {
                       old <- S3methods
                       nold <- nS3methods
                       nS3methods <<- nS3methods * 2L
                       new <- matrix(NA_character_, nS3methods, 3L)
                       ind <- seq_len(nold)
                       for (i in 1:3) new[ind, i] <- old[ind, i]
                       S3methods <<- new
                       rm(old, new)
                   }
                   S3methods[nS3, seq_along(spec)] <<- asChar(spec)
               },
               stop(gettextf("unknown namespace directive: %s", deparse(e, nlines=1L)),
                    call. = FALSE, domain = NA)
               )
    }
    for (e in directives)
        parseDirective(e)

    ## need to preserve the names on dynlibs, so unique() is not appropriate.
    dynlibs <- dynlibs[!duplicated(dynlibs)]
    list(imports = imports, exports = exports,
         exportPatterns = unique(exportPatterns),
         importClasses = importClasses, importMethods = importMethods,
         exportClasses = unique(exportClasses),
         exportMethods = unique(exportMethods),
         exportClassPatterns = unique(exportClassPatterns),
         dynlibs = dynlibs, nativeRoutines = nativeRoutines,
         S3methods = unique(S3methods[seq_len(nS3), , drop = FALSE]) )
} ## end{parseNamespaceFile}

## unused
registerS3method <- function(genname, class, method, envir = parent.frame()) {
    addNamespaceS3method <- function(ns, generic, class, method) {
	regs <- rbind(.getNamespaceInfo(ns, "S3methods"),
		      c(generic, class, method))
        setNamespaceInfo(ns, "S3methods", regs)
    }
    groupGenerics <- c("Math", "Ops",  "Summary", "Complex")
    defenv <- if(genname %in% groupGenerics) .BaseNamespaceEnv
    else {
        genfun <- get(genname, envir = envir)
        if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
            genfun <- methods::finalDefaultMethod(genfun@default)
        if (typeof(genfun) == "closure") environment(genfun)
	else .BaseNamespaceEnv
    }
    if (is.null(table <- defenv[[".__S3MethodsTable__."]])) {
	table <- new.env(hash = TRUE, parent = baseenv())
	defenv[[".__S3MethodsTable__."]] <- table
    }

    if (is.character(method)) {
        assignWrapped <- function(x, method, home, envir) {
            method <- method            # force evaluation
            home <- home                # force evaluation
            delayedAssign(x, get(method, envir = home), assign.env = envir)
        }
        if(!exists(method, envir = envir)) {
            ## need to avoid conflict with message at l.1298
            warning(gettextf("S3 method %s was declared but not found",
                             sQuote(method)), call. = FALSE)
        } else {
	    assignWrapped(paste(genname, class, sep = "."), method, home = envir,
	    	    envir = table)
        }
    }
    else if (is.function(method))
        assign(paste(genname, class, sep = "."), method, envir = table)
    else stop("bad method")
    if (isNamespace(envir) && ! identical(envir, .BaseNamespaceEnv))
        addNamespaceS3method(envir, genname, class, method)
}


registerS3methods <- function(info, package, env)
{
    n <- NROW(info)
    if(n == 0L) return()

    assignWrapped <- function(x, method, home, envir) {
	method <- method            # force evaluation
	home <- home                # force evaluation
	delayedAssign(x, get(method, envir = home), assign.env = envir)
    }
    overwrite <- matrix(NA_character_, 0, 2)
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
	    if(is.null(genfun <- get0(genname, envir = parent.env(envir))))
		stop(gettextf("object '%s' not found whilst loading namespace '%s'",
			      genname, package), call. = FALSE, domain = NA)
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
		genfun <- genfun@default  # nearly always, the S3 generic
            if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
	if (is.null(table <- defenv[[".__S3MethodsTable__."]])) {
	    table <- new.env(hash = TRUE, parent = baseenv())
	    defenv[[".__S3MethodsTable__."]] <- table
	}
        if(!is.null(e <- table[[nm]])) {
            current <- environmentName(environment(e))
            overwrite <<- rbind(overwrite, c(as.vector(nm), current))
        }
	assignWrapped(nm, method, home = envir, envir = table)
    }

    methname <- paste(info[,1], info[,2], sep = ".")
    z <- is.na(info[,3])
    info[z,3] <- methname[z]
    Info <- cbind(info, methname)
    loc <- names(env)
    notex <- !(info[,3] %in% loc)
    if(any(notex))
        warning(sprintf(ngettext(sum(notex),
                                 "S3 method %s was declared in NAMESPACE but not found",
                                 "S3 methods %s were declared in NAMESPACE but not found"),
                        paste(sQuote(info[notex, 3]), collapse = ", ")),
                call. = FALSE, domain = NA)
    Info <- Info[!notex, , drop = FALSE]

    ## Do local generics first (this could be load-ed if pre-computed).
    ## However, the local generic could be an S4 takeover of a non-local
    ## (or local) S3 generic.  We can't just pass S4 generics on to
    ## .registerS3method as that only looks non-locally (for speed).
    l2 <- localGeneric <- Info[,1] %in% loc
    if(.isMethodsDispatchOn())
        for(i in which(localGeneric)) {
            genfun <- get(Info[i, 1], envir = env)
            if(methods::is(genfun, "genericFunction")) {
                localGeneric[i] <- FALSE
                registerS3method(Info[i, 1], Info[i, 2], Info[i, 3], env)
            }
        }
    if(any(localGeneric)) {
        lin <- Info[localGeneric, , drop = FALSE]
        S3MethodsTable <- env[[".__S3MethodsTable__."]]
        ## we needed to move this to C for speed.
        ## for(i in seq_len(nrow(lin)))
        ##    assign(lin[i,4], get(lin[i,3], envir = env),
        ##           envir = S3MethodsTable)
        .Internal(importIntoEnv(S3MethodsTable, lin[,4], env, lin[,3]))
    }

    ## now the rest
    fin <- Info[!l2, , drop = FALSE]
    for(i in seq_len(nrow(fin)))
        .registerS3method(fin[i, 1], fin[i, 2], fin[i, 3], fin[i, 4], env)
    if(package != "MASS" && ## MASS is providing methods for stubs in stats
       nrow(overwrite) &&
       Sys.getenv("_R_LOAD_CHECK_OVERWRITE_S3_METHODS_")
          %in% c(package, "all")) {
        std <- as.vector(unlist(tools:::.get_standard_package_names()))
        overwrite <- overwrite[overwrite[, 2L] %in% std, , drop = FALSE]
       if(nr <- nrow(overwrite)) {
           msg <- ngettext(nr,
                           "Registered S3 method from a standard package overwritten by '%s':",
                           "Registered S3 methods from standard package(s) overwritten by '%s':",
                           domain = NA)
           message(sprintf(msg, package))
           colnames(overwrite) <- c("method", "from")
           print(as.data.frame(overwrite), row.names = FALSE, right = FALSE)
       }
    }

    setNamespaceInfo(env, "S3methods",
                     rbind(info, getNamespaceInfo(env, "S3methods")))
}

.mergeImportMethods <- function(impenv, expenv, metaname)
{
    impMethods <- impenv[[metaname]]
    if(!is.null(impMethods))
	impenv[[metaname]] <-
	    methods:::.mergeMethodsTable2(impMethods,
					  newtable = expenv[[metaname]], # known to exist by caller
					  expenv, metaname)
    impMethods # possibly NULL
}
