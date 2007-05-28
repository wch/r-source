..First.lib  <-
  ## Initialize the methods package:  the session table of method
  ## definitions.
  ##
  ## run the initial computations for the methods package, if this
  ## wasn't done at INSTALL time:
  ##  - define the basic classes (vector, the informal classes that
  ##  extend vector)
  ##  - define the classes needed to represent methods
  function(libname, pkgname, where)
{
    if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(gettextf("not a package name: \"%s\"", pkgname), domain = NA)
            return()
        }
        where <- as.environment(where)
    }
    initMethodDispatch(where)
    ## temporary empty reference to the package's own namespace
    assign(".methodsNamespace", new.env(), envir = where)
    useTables <-  !nzchar(Sys.getenv("R_NO_METHODS_TABLES"))
    .UsingMethodsTables(useTables) ## turn it on (or off)
    .Call("R_set_method_dispatch", useTables, PACKAGE = "methods")
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        cat("initializing class and method definitions ...")
        on.exit(assign(".saveImage", NA, envir = where))
        ## set up default prototype (uses .Call so has be at load time)
        assign(".defaultPrototype",
                .Call("Rf_allocS4Object",PACKAGE="methods"),
               envir = where)
        assign(".SealedClasses", character(), envir = where)
        .InitClassDefinition(where)
        assign("possibleExtends", .possibleExtends, envir = where)
        .InitBasicClasses(where)
        .initClassSupport(where)
        .InitMethodsListClass(where)
        .setCoerceGeneric(where)
        ## now install the non-dummy versions of some functions
        assign(".classEnv", ..classEnv, envir = where)
        assign("makeGeneric", .makeGeneric, envir = where)
        assign("newClassRepresentation", .newClassRepresentation, envir = where)
        assign(".mergeClassDefSlots", ..mergeClassDefSlots, envir = where)
        .makeBasicFuns(where)
        rm(.makeGeneric, .newClassRepresentation, .possibleExtends,
           ..mergeClassDefSlots, envir = where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".isPrototype", ..isPrototype, envir = where)
        .InitClassUnion(where)
        .InitS3Classes(where)
        ## now seal the classes defined in the package
        for(cl in get(".SealedClasses", where))
            sealClass(cl, where)
        assign(".requirePackage", ..requirePackage, envir = where)
        assign(".addToMetaTable", ..addToMetaTable, envir = where)
        .makeGenericTables(where)
        ## TO DO: .InitSubsetMethods(where)
        assign(".saveImage", TRUE, envir = where)
        on.exit()
        cat("done\n")
    }
    else {
        if(!isTRUE(saved))
            stop("maybe the methods package was not installed correctly; check the make results!",
                 domain = NA)

        ## assign the environment of a function from the methods
        ## package-- the namespace of the methods package, if it has
        ## one, or the global environment

        assign(".methodsNamespace",
               environment(get("setGeneric", where)), where)
        ## cache metadata for all environments in search path.  The
        ## assumption is that this has not been done, since
        ## cacheMetaData is in this package.  library, attach, and
        ## detach functions look for cacheMetaData and call it if it's
        ## found.

        sch <- rev(search())[-(1:2)]  # skip base and autoloads
        sch <- sch[! sch %in% paste("package",
                                    c("datasets", "grDevices", "graphics",
                                      "stats", "utils"),
                                    sep=":")]
        for(i in sch) {
            nev <- ev <- as.environment(i)
            ns <- .Internal(getRegisteredNamespace(as.name(getPackageName(ev))))
            if(!is.null(ns)) nev <- asNamespace(ns)
            if(identical(getPackageName(ev), "methods"))
               next
            if(!exists(".noGenerics", where = nev, inherits = FALSE))
                cacheMetaData(ev, TRUE, searchWhere = .GlobalEnv)
        }
    }
}

.onLoad <- function(libname, pkgName) {
    env <- environment(sys.function())
    doSave <- identical(get(".saveImage", envir = env), FALSE)
    ..First.lib(libname, pkgName, env)
    if(doSave) {
        dbbase <- file.path(libname, pkgName, "R", pkgName)
        ns <- asNamespace(pkgName)
        tools:::makeLazyLoadDB(ns, dbbase)
    }
    if(Sys.getenv("R_S4_BIND") == "active")
        methods:::bind_activation(TRUE)
}

.onUnload <- function(libpath) {
    cat("unloading 'methods' package ...\n")# see when this is called
    .isMethodsDispatchOn(FALSE)
    methods:::bind_activation(FALSE)
    library.dynam.unload("methods", libpath)
}


.onAttach <- function(libname, pkgName) {
    env <- environment(sys.function())
    ## unlock some bindings that must be modifiable to set methods
    unlockBinding(".BasicFunsList", env)
    ## following  has to be on attach , not on load, but why???
    if(.UsingMethodsTables()) {
      cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
      result <- try(cacheMetaData(.GlobalEnv, TRUE))
      ## still attach  methods package if global env has bad objets
      if(is(result, "try-error"))
        warning("apparently bad method or class metadata in saved environment; move the file or remove the class/method")
    }
}

.Last.lib <- function(libpath) {
    methods:::.onUnload(libpath)
}
## redefining it here, invalidates the one above:
## Why don't we unload "methods" on detach() ?
.Last.lib <- function(libpath) .isMethodsDispatchOn(FALSE)

.saveImage <- FALSE
## cat("Saving namespace image ...\n")
