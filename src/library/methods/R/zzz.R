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
    .C("R_initMethodDispatch", PACKAGE = "methods")# C-level initialization
    if(missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- pos.to.env(where)
    }
    ## assign a pointer to the environment in the environment!
    ## (Fortunately, environments are references, not true objects)
    ## Note:  this is only used currently for turning method search on and off,
    ## and that use should disappear when standardGeneric is implemented in C
    assign(".methodsEnv", where, envir=where)
    assign(".MethodsDispatchOn", TRUE, envir = where)
    ## initialize the environment used as the session table to store methods definitions
    table <- new.env()
    assign("__MethodMetaData", table, envir = where)
    .Call("R_initialize_methods_metadata", table, PACKAGE = "methods")
    if(!get(".saveImage", envir = where)) {
        cat("Initializing class and method definitions now\n")
        .InitBasicClasses(where)
        .InitMethodsListClass(where)
        .makeBasicFunsList(where)
        assign(".saveImage", TRUE, envir = where)
    }
}

### The following code is only executed when dumping

.saveImage <- FALSE # not already saved
dyn.load(file.path(.Library, "methods", "libs",
                   paste("methods", .Platform$dynlib.ext, sep="")))
.First.lib("methods", "methods", .GlobalEnv)
save.image(file=file.path(.Library, "methods", "R", "all.rda"))
