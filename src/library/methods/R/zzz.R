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
    ## initialize the environment used as the session table to store methods definitions
    table <- new.env(hash=TRUE)
    assign("__MethodMetaData", table, envir = where)
    .Call("R_initialize_methods_metadata", table, PACKAGE = "methods")
    initMethodDispatch()
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        cat("initializing class and method definitions now ...")
        on.exit(assign(".saveImage", NA, envir = where))
        .InitClassDefinition(where)
        .InitBasicClasses(where)
        .initClassSupport(where)
        .InitMethodsListClass(where)
        .setCoerceGeneric(where)
        ## now install the non-dummy versions of some functions
        assign("makeGeneric", .makeGeneric, envir = where)
        assign("newClassRepresentation", .newClassRepresentation, envir = where)
        assign(".mergeClassDefSlots", ..mergeClassDefSlots, envir = where)
        .makeBasicFunsList(where)
        rm(.makeGeneric, .newClassRepresentation, envir = where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".isPrototype", ..isPrototype, envir = where)
        ## TO DO: .InitSubsetMethods(where)
        assign(".saveImage", TRUE, envir = where)
        on.exit()
        cat("done\n")
    }
    else {
        if(!identical(saved, TRUE))
            stop("Looks like the methods library was not installed correctly; check the make results!\n")
        classRepClass <- getClassDef("classRepresentation", where)
        if(is.null(classRepClass))
            stop("The methods library was not initialized correctly: couldn't find the definition of the \"classRepresentation\" class")
        ## cache the definition of classRepresentation to boot the class system
        assignClassDef("classRepresentation", classRepClass, 0)
    }
    ## cache metadata for all environments in search path.  The assumption is that
    ## this has not been done, since cacheMetaData is in this package.  library, attach,
    ## and detach functions look for cacheMetaData and call it if it's found.
    for(i in rev(seq(along = search())))
      if(!exists(".noGenerics", where = i, inherits = FALSE))
        cacheMetaData(i, TRUE)
}

### The following code is only executed when dumping
assign(".saveImage", FALSE, .GlobalEnv)
.First.lib("methods", "methods", .GlobalEnv)
save.image(file = file.path(.Library, "methods", "R", "all.rda"),
           compress = TRUE, safe = FALSE)
