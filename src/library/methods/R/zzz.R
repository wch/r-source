.First.lib  <-
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
    initMethodDispatch()
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        cat("initializing class and method definitions now ...")
        on.exit(assign(".saveImage", NA, envir = where))
        assign(".SealedClasses", character(), envir = where)
        .InitClassDefinition(where)
        .InitBasicClasses(where)
        .initClassSupport(where)
        .InitMethodsListClass(where)
        .setCoerceGeneric(where)
        ## now install the non-dummy versions of some functions
        assign("makeGeneric", .makeGeneric, envir = where)
        assign("newClassRepresentation", .newClassRepresentation, envir = where)
        assign(".mergeClassDefSlots", ..mergeClassDefSlots, envir = where)
        assign(".requirePackage", ..requirePackage, envir = where)
        .makeBasicFunsList(where)
        rm(.makeGeneric, .newClassRepresentation, envir = where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".isPrototype", ..isPrototype, envir = where)
        .InitClassUnion(where)
        ## now seal the classes defined in the package
        for(cl in get(".SealedClasses", where))
            sealClass(cl, where)
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
        for(i in rev(seq(along = search()))) {
            ev <- as.environment(i)
            if(!exists(".noGenerics", where = ev, inherits = FALSE) &&
               !identical(getPackageName(ev), "methods"))
                cacheMetaData(ev, TRUE)
        }
    }
}

### The following code is only executed when dumping
assign(".saveImage", FALSE, .GlobalEnv)
.First.lib("methods", "methods", .GlobalEnv)
save.image(file = file.path(.Library, "methods", "R", "all.rda"),
           compress = TRUE, safe = FALSE)
