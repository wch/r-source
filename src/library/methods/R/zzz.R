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
    if(!get(".saveImage", envir = where)) {
        cat("initializing class and method definitions now\n")
        .InitBasicClasses(where)
        .InitMethodsListClass(where)
        .makeBasicFunsList(where)
        .setCoerceGeneric(where)
        setGeneric("show", where = where)
        assign(".saveImage", TRUE, envir = where)
    }
    ## cache metadata for all environments in search path.  The assumption is that
    ## this has not been done, since cacheMetaData is in this package.  library, attach,
    ## and detach functions look for cacheMetaData and call it if it's found.
    for(i in rev(seq(along = search())))
      cacheMetaData(as.environment(i), TRUE)
    # make assignments for new class and class<- functions diretly into base
    # if base is defined in a name space
    if (! identical(environment(log), .GlobalEnv)) { # check if namespace used
        assign("class", get("class"), env = NULL)
        assign("class<-", get("class<-"), env = NULL)
    }
}

### The following code is only executed when dumping
assign(".saveImage", FALSE, .GlobalEnv)
.First.lib("methods", "methods", .GlobalEnv)
save.image(file = file.path(.Library, "methods", "R", "all.rda"),
           compress = TRUE, safe = FALSE)
