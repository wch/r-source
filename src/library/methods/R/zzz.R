.conflicts.OK <- TRUE

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
    .C("R_initMethodDispatch", PACKAGE = "methods")# C-level initialization
    saved <- (if(exists(".saveImage", envir = where, inherits = FALSE))
              get(".saveImage", envir = where)
              else
              NA)
    if(identical(saved, FALSE)) {
        cat("initializing class and method definitions now ...")
        on.exit(assign(".saveImage", NA, envir = where))
        .InitBasicClasses(where)
        .InitMethodsListClass(where)
        .makeBasicFunsList(where)
        .setCoerceGeneric(where)
        .InitMethodDefinitions(where)
        .InitShowMethods(where)
        assign(".saveImage", TRUE, envir = where)
        on.exit()
        cat("done\n")
    }
    else if(!identical(saved, TRUE))
        stop("Looks like the methods library was not installed correctly; check the make results!\n")
    ## cache metadata for all environments in search path.  The assumption is that
    ## this has not been done, since cacheMetaData is in this package.  library, attach,
    ## and detach functions look for cacheMetaData and call it if it's found.
    for(i in rev(seq(along = search())))
      cacheMetaData(as.environment(i), TRUE)
    if (! identical(environment(log), .GlobalEnv)) { # check if namespace used
        # if base is using a name space then any replacements for functions
        # in base defined in methods need to be installed directly into base
        # or they will not be visible to base code.
        for (n in ls(env = where))
            if (exists(n, env = NULL))
                assign(n, get(n, env = where), env = NULL)
    }
}

### The following code is only executed when dumping
assign(".saveImage", FALSE, .GlobalEnv)
.First.lib("methods", "methods", .GlobalEnv)
save.image(file = file.path(.Library, "methods", "R", "all.rda"),
           compress = TRUE, safe = FALSE)
