..First.lib  <-
  ## Initialize the methods library:  the session table of method
  ## definitions.
  ##
  ## run the initial computations for the methods package, if this
  ## wasn't done at INSTALL time:
  ##  - define the basic classes (vector, the informal classes that
  ##  extend vector)
  ##  - define the classes needed to represent methods
  function(libname, pkgname, where)
{
    library.dynam(pkgname, pkgname)
    if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- as.environment(where)
    }
    initMethodDispatch(where)
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        cat("initializing class and method definitions now ...")
        on.exit(assign(".saveImage", NA, envir = where))
        assign(".SealedClasses", character(), envir = where)
        .InitClassDefinition(where)
        assign("possibleExtends", .possibleExtends, envir = where)
        .InitBasicClasses(where)
        .initClassSupport(where)
        .InitMethodsListClass(where)
        .setCoerceGeneric(where)
        ## now install the non-dummy versions of some functions
        assign("makeGeneric", .makeGeneric, envir = where)
        assign("newClassRepresentation", .newClassRepresentation, envir = where)
        assign(".mergeClassDefSlots", ..mergeClassDefSlots, envir = where)
        .makeBasicFuns(where)
        rm(.makeGeneric, .newClassRepresentation, .possibleExtends, ..mergeClassDefSlots,
           envir = where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".isPrototype", ..isPrototype, envir = where)
        .InitClassUnion(where)
        ## now seal the classes defined in the package
        for(cl in get(".SealedClasses", where))
            sealClass(cl, where)
        assign(".requirePackage", ..requirePackage, envir = where)
        ## TO DO: .InitSubsetMethods(where)
        assign(".saveImage", TRUE, envir = where)
        on.exit()
        cat("done\n")
    }
    else {
        if(!identical(saved, TRUE))
            stop("Looks like the methods library was not installed correctly; check the make results!\n")
        ## assign the environment of a function from the methods package--
        ## the namespace of the methods package, if it has one, or the global environment
        assign(".methodsNamespace", environment(get("setGeneric", where)), where)
        ## cache metadata for all environments in search path.  The assumption is that
        ## this has not been done, since cacheMetaData is in this package.  library, attach,
        ## and detach functions look for cacheMetaData and call it if it's found.
        sch <- rev(search())[-(1:2)]  # skip base and autoloads
        sch <- sch[! sch %in% paste("package", c("utils", "graphics", "stats"),
                                    sep=":")]
        for(i in sch) {
            nev <- ev <- as.environment(i)
#            try(nev <- asNamespace(getPackageName(ev)), silent = TRUE)
            ns <- .Internal(getRegisteredNamespace(as.name(getPackageName(ev))))
            if(!is.null(ns)) nev <- asNamespace(ns)
            if(!exists(".noGenerics", where = nev, inherits = FALSE) &&
               !identical(getPackageName(ev), "methods"))
                cacheMetaData(ev, TRUE, searchWhere = .GlobalEnv)

        }
    }
}

.First.lib <- ..First.lib  ## will be overwritten on loading

.onLoad <- function(libname, pkgName) {
    env <- environment(sys.function())
    doSave <- identical(get(".saveImage", envir = env), FALSE)
    ..First.lib(libname, pkgName, env)
    if(doSave) {
        rdafile <- file.path(.Library, "methods", "R", "all.rda")
        vars <- objects(env, all=TRUE)
        vars <- vars[vars != ".__NAMESPACE__."]
        save(list = vars, file = rdafile, envir = env, compress = TRUE)
    }
}

.onUnload <- function(libpath) {
    .isMethodsDispatchOn(FALSE)
    library.dynam.unload("methods", libpath)
}


.onAttach <- function(libname, pkgName) {
#    ..First.lib(libname, pkgName)
    env <- environment(sys.function())
    ## unlock some bindings that must be modifiable to set methods
    unlockBinding(".BasicFunsList", env)
}

.Last.lib <- function(libpath) {
    methods:::.onUnload(libpath)
}

### The following code is only executed when dumping
### with no namespace for "methods"
local({
    env <- topenv()
    rdafile <- file.path(.Library, "methods", "R", "all.rda")
    libname <- pkgname <- .packageName
    assign(".saveImage", FALSE, env)
    if(identical(env, .GlobalEnv)) {
        .First.lib(libname, pkgname, env)
        save(list = objects(env, all=TRUE), file = rdafile,
                   compress = TRUE, envir = env)
    }
    else {
        message("Saving namespace image ...")
    }
}
)
