
setGeneric <-
  ## Define `name' to be a generic  function, for which methods will be defined.
  ##
  ## If there is already a non-generic function of this name, it will be used
  ## to define the generic unless `def' is supplied, and the current function will
  ## become the default method for the generic.
  ##
  ## If `def' is supplied, this defines the generic function.  The default method for
  ## a new generic will usually be an existing non-generic.  See the .Rd page
  ##
    function(name, def, group = NULL, valueClass = NULL, where = 1)
{
    useAsDefault <- existsFunction(name) && missing(def)
    if(useAsDefault) {
        fdef <- getFunction(name)
    }
    else {
        if(missing(def))
            stop(paste("No existing function \"", name, "\", arguments must be supplied"))
        fdef <- def
    }
    if(!isGeneric(name, fdef = fdef))
        ## create a generic; if the original fdef was NOT a generic, it becomes the default
        ## Otherwise, there will be no default method.
        fdef <- makeGeneric(name, fdef, FALSE, useAsDefault, group=group, valueClass=valueClass)
    ev <- environment(fdef)
    if(!missing(def) && !identical(def, fdef)) {
        ## use the supplied body and the generated environment
        body(fdef) <- body(def)
        environment(fdef) <- ev
    }
    assign(name, fdef, where)
    ## The S-Plus definition claims to return an object of class "Generic", which we don't
    ## have, but in fact just returns name.  The R  function returns the
    ## definition of the generic.
    fdef
}

##
## make a generic function object corresponding to the given function name.
##
## Simulates the getOrMakeGeneric S4 function but using the environment mechanism
## rather than metadata as in S.

isGeneric <-
  ## Is there a function named `f', and if so, is it a generic?
  ##
  ## If the `fdef' argument is supplied, take this as the definition of the
  ## generic, and test whether it is really a generic, with `f' as the name of
  ## the generic.  (This argument is not available in S-Plus.)
  function(f, where = -1, fdef, getName = FALSE)
{
    ## the fdef argument is not found in S4 but should be ;-)
    if(missing(fdef)) {
        if(missing(where))
            fdef <- getFunction(f, mustFind = FALSE)
        else
          fdef <- getFunction(f, where=where, mustFind = FALSE)
    if(is.null(fdef))
      return(FALSE)
    }
    env <- environment(fdef)
    if(!exists(".Generic", envir=env))
        return(FALSE)
    gen <- get(".Generic", envir=env)
    if(getName)
        return(gen)
    else if(gen == f)
        return(TRUE)
    else {
        warning(paste("Function \"", f, "\" appears to be a Generic function, but with generic name \"", gen, "\""))
        return(FALSE)
    }
}

removeGeneric <-
  ## Remove the generic function of this name.
  ##
  ## If `where' supplied, just remove the version on this element of the search list;
  ## otherwise, removes the first version encountered.
    function(name, where = -1)
{
    if(identical(where, -1)) {
        where <- findFunction(name)
        if(length(where) > 0)
            where <- el(where, 1)
    }
    else {
        if(!existsFunction(name, where=where))
            where <- character()
    }
    if(length(where) > 0)
        rm(list = name, pos = where)
    else
        warning(paste("Function \"", name, "\" not found for removal", sep=""))
}

evalSelectedMethod <-
    function(f, ev, fname)
    .Call("R_eval_selected_method", f, ev, fname, PACKAGE = "methods")



getMethods <-
## The list of methods for the specified generic.  If the function is not
## a generic function, returns NULL.
## The `f' argument can be either the character string name of the generic
## or the object itself.
##
## The `where' argument optionally says where to look for the function, if
## `f' is given as the name.
  ## Methods objects are kept during the session in a special environment.  Inside
  ## this environment, individual methods are added and updated as they are found (for
  ## example, a method that is inherited is stored, again, under the actual signature).
  ## These updates speed up the method search; however, they are not stored with
  ## the original generic function, since they might change in future sessions.
  function(f, where = -1)
{
    if(is.function(f))
        fdef <- f
    else {
        if(identical(where, -1))
            return(getMethodsForDispatch(f))
        else
            fdef <- getFunction(f, where=where)
    }
    ev <- environment(fdef)
    if(exists(".Methods", ev)) {
        get(".Methods", ev )
    }
    else
        NULL
}

getMethodsForDispatch <-
  function(f)
{
    ## look in the internal environment
    fdef <- getFromMethodMetaData(f)
    if(is.null(fdef)) {
        fdef <- mergeGenericFunctions(f)
        assignToMethodMetaData(f, fdef)
    }
    get(".Methods", envir = environment(fdef))
}

setMethod <-
## Define a method for the specified combination of generic function and signature.
## The method is stored in the methods meta-data of the specified database.
  ##
  ## Note that assigning methods anywhere but the global environment (`where==1') will
  ## not have a permanent effect beyond the current R session.
  function(f, signature = character(), definition, where = 1, valueClass)
{
    whereString <- if(is.environment(where)) deparse(where) else where
    doAssign <- !isGeneric(f, where = where)
    ## automatic creation of a new generic is allowed in two cases:  there is a
    ## generic (somewhere else) or there is a non-generic (to be made into the
    ## default method).  In either case, arguments
    ## must match definition.
    if(doAssign) {
        fdef <- getGeneric(f, mustFind=FALSE)
        if(is.null(fdef)) {
            fdef <- getFunction(f, mustFind=FALSE)
            if(is.null(fdef))
                stop(paste("No existing definition for function \"",f,"\"", sep=""))
            else
                warning(paste("Creating a new generic function for \"", f, "\" on element ",
                              whereString, " of the search path", sep=""))
        }
        else
            message("Creating a new version of the generic function for \"", f,
                    "\" on element ", whereString, " of the search path")
        fdef <- makeGeneric(f, fdef)
    }
    else
        fdef <- getFunction(f, where = where)
    ## get the methods unconditionally.  Unlike getMethods(), this
    ## code requires the environment to contain a Methods list
    ev <- environment(fdef)
    names <- formalArgs(fdef)
    snames <- names(signature)
    if(length(snames)>0)
        signature <- matchSignature(names, signature, definition)
    ## check the formal arguments
    if(identical(typeof(definition), "closure") && !identical(names, formalArgs(definition))) {
        warning("Method and generic have different arguments: a revised version of the method will be generated")
        definition <- conformMethodArgs(definition, fdef, ev)
    }
    allMethods <- get(".Methods",  ev )
    allMethods <- insertMethod(allMethods, signature, names, definition)
    assign(".Methods", allMethods, ev)
    ## cancel the session copy of the generic if there is one.
    if(!is.null(getFromMethodMetaData(f)))
        removeFromMethodMetaData(f)
    ## assign the generic function to the database:  the assign waits until
    ## the successful end of the function to avoid premature method search
    if(doAssign)
        assign(f, fdef, where)
    f
}

getMethod <-
  ## Return the function that is defined as the method for this generic function and signature
  ## (classes to be matched to the arguments of the generic function).
  function(f, signature = character(), where = -1, optional = FALSE)
{
    mlist <- getMethods(f, where)
    if(length(signature) == 0)
        return(finalDefaultMethod(mlist, f))
    while(is(mlist, "MethodsList")) {
        Class <- signature[[1]]
        if(Class == "")
            Class <- "ANY"
        methods <- slot(mlist, "methods")
        mlist <- elNamed(methods, Class)# may be function, MethodsList or NULL
        signature <- signature[-1]
        if(length(signature) == 0)
            break
    }
    if(is.function(mlist) && length(signature) == 0)
        ## the only successful outcome
        mlist
    else if(optional)
        mlist                           ## may be NULL or a MethodsList object
    else
        stop(paste("No method defined for function \"", f,
                   "\" for this signature ", sep = ""))
}

dumpMethod <-
  ## Dump the method for this generic function and signature.
  ## The resulting source file will recreate the method.
  function(f, signature=character(), file = defaultDumpName(f, signature), where = -1, def = getMethod(f, signature, where=where, optional = TRUE)) {
  if(!is.function(def))
    def <- getMethod(f, character(), where=where, optional = TRUE)
  if(file != "")
    sink(file)
  cat("setMethod(\"", f, "\", ", deparse(signature), ",\n", sep="")
  dput(def)
  cat(")\n", sep="")
  if(file != "")
    sink()
  file
}


selectMethod <-
  ## Returns the method (a function) that R would use to evaluate a call to this generic,
  ## with arguments corresponding to the specified signature.
  ##
  ## f = the name of the generic function
  ## signature = the signature of classes to match to the arguments of f.  The vector of strings
  ##       for the classes can be named or not.  If named, the names must match formal
  ##       argument names of f.  If not named, the signature is assumed to apply to the
  ##       arguments of f in order.
  ## optional = If TRUE, and no explicit selection results, return result anyway. else error
  ## mlist = Optional MethodsList object to use in the search.  Usually omitted, but
  ##         required for a recursive call from within selectMethod.
    function(f, signature = character(), optional = FALSE,
             inherited = TRUE, mlist)
{
    if(missing(mlist)) {
        mlist <- getMethods(f)
        if(is.null(mlist)) {
            if(optional)
                return(mlist)
            else
                stop(paste("\"", f, "\" has no methods defined", sep=""))
        }                               # else, go on with selection
    }
    methods <- slot(mlist, "methods")
    defaultMethod <- elNamed(methods, "ANY")
    sigNames <- names(signature)
    if(is.null(sigNames)) {
        ## convert to a named signature for recursive processing
        sigNames <- formalArgs(f)
        length(sigNames) <- length(signature)
        if(length(signature))
            names(signature) <- sigNames
    }
    argName <- as.character(slot(mlist, "argument"))
    which <- match(argName, sigNames)
    if(is.na(which))
        thisClass <- "ANY"
    else {
        thisClass <- el(signature, which)
        signature <- signature[-which]
    }
    exact <- TRUE
    selection <- elNamed(methods, thisClass)
    if(is.null(selection)) {
        if(inherited) {
            selection <- elNamed(slot(mlist, "allMethods"), thisClass)
            if(is.null(selection)) {
                ## do the inheritance calculations
                allSelections <- matchArgClass(thisClass, names(methods), methods)
                for(cl in names(allSelections)) {
                  method <- elNamed(allSelections, cl)
                  if(is(method, "MethodsList"))
                    ## try completing the selection
                    method <- Recall(mlist = method, signature = signature[-1],
                                     optional = TRUE)
                  if(is(method, "function")) {
                    thisClass <- cl
                    selection <- method
                    break
                  }
                  ## else, try the next possibility
                }
                ## on exiting the loop, selection will still be NULL unless
                ## one of the Recall's successfully found a single method
                
              }
          }
      }
    if(is(selection, "function"))
        selection
    else if(is(selection, "MethodsList")) {
        if(length(signature) == 0) {
            if(optional)
                selection
            else
                stop("No unique method corresponding to this signature")
        }
        else
            Recall(f, signature, mustFind, inherited, selection)
    }
    else {
        if(optional)
            selection
        else
            stop(paste("Unable to match argument \"", argName, "\" to methods", sep=""))
    }
}

hasMethod <-
  ## returns `TRUE' if `f' is the name of a generic function with an (explicit) method for
  ## this signature.
  function(f, signature = character())
{
    if(isGeneric(f))
        is(selectMethodsList(f, signature, FALSE, FALSE), "function")
    else
        FALSE
}

dumpMethods <-
  ## Dump all the methods for this generic.
  ##
  ## If `signature' is supplied only the methods matching this initial signature
  ## are dumped.  (This feature is not found in S-Plus:  don't use it if you want
  ## compatibility.)
  function(f, file = "", signature = character(), methods, where = -1 )
{
    if(missing(methods))
        methods <-  getMethods(f, where = where)
    if(file != "")
        sink(file)
    on.exit(if(file!="")
            sink())
    for(what in names(methods)) {
        el <- methods[[what]]
        if(is.function(el))
            dumpMethod(f, c(signature, what), file = "", def = el)
        else
            dumpMethods(f, "", c(signature, what), el, where)
    }
    el <- attr(methods, ".Default")
    if(is.function(el))
        dumpMethod(f, signature, file = "", def = el)
    else if(!is.null(el))
        dumpMethods(f, "", c(signature, ""), el, where)
}

signature <-
  ## A named list of classes to be matched to arguments of a generic function.
  ## It is recommended to supply signatures to `setMethod' via a call to `signature',
  ## to make clear which arguments are being used to select this method.
  ## It works, however, just to give a vector of character strings, which will
  ## be associated with the formal arguments of the function, in order.  The advantage
  ## of using `signature' is to provide a check on which arguments you meant, as well
  ## as clearer documentation in your method specification.  In addition, `signature'
  ## checks that each of the elements is a single character string.
  function(...)
{
    value <- list(...)
    for(i in seq(along=value)) {
        sigi <- el(value, i)
        if(!is.character(sigi) || length(sigi) != 1)
            stop(paste("Bad class specified for element",i,"(should be a single character string)"))
    }
    value
}

showMethods <-
  ## Show all the methods for the specified function.
  ##
  ## If `where' is supplied, the definition from that database will be used; otherwise,
  ## the current definition is used (which will include inherited methods that have arisen so
  ## far in the session).
  ##
  ## The argument `classes' is included for consistency with S-Plus, but currently ignored.
  ## Similarly, omitting `f' is not currently supported.  Someday, ...
  ##
  ## The output style is different from S-Plus in that it does not show the database
  ## from which the definition comes, but can optionally include the method definitions,
  ## if `includeDefs == TRUE'.
  function(f, where, classes, includeDefs = FALSE, inherited = TRUE)
{
    if(!missing(classes))
        warning("argument \"classes\" is not (yet) supported")
    if(missing(f))
        stop("argument \"f\" must (currently) be supplied")
    if(missing(where))
        mlist <- getMethods(f)
    else
        mlist <- getMethods(f, where)
    showMlist(mlist, includeDefs, inherited)
}

removeMethods <-
  ## removes all the methods defined for this generic function.  Returns `TRUE' if
  ## `f' was a generic function, `FALSE' (silently) otherwise.
  ##
  ## If there is a default method, the function will be re-assigned as
  ## a simple function with this definition; otherwise, it will be removed.  The
  ## assignment or removal can be controlled by optional argument `where', which
  ## defaults to the first element of the search list having a function called `f'.
  function(f, where)
{
    if(missing(where)) {
        where <- findFunction(f)
        if(length(where) == 0)
            return(FALSE)
        where <- el(where, 1)
    }
    else if(!exists(f, where))
        return(FALSE)
    if(!isGeneric(f))
        return(FALSE)
    default <- getMethod(f, optional=TRUE)
    if(is.function(default)) {
        cat("Restoring default function definition of", f, "\n")
        assign(f, default, where)
    }
    else {
        cat("No default method for ", f, "; removing generic function\n")
        rm(list=f, pos=where)
    }
    return(TRUE)
}

resetGeneric <-
  ## reset the currently defined methods for this generic by un-caching any inherited
  ## methods.  You MUST call this function when you change relevant inheritance information during a
  ## session, to guarantee that the new information is used if this generic has already been
  ## called.
  function(f)
{
    fdef <- getFromMethodMetaData(f)
    if(is.null(fdef))
        return(FALSE)
    methods <- get(".Methods", envir = environment(fdef), inherit=FALSE)
    slot(methods, "allMethods") <- slot(methods, "methods")
    assign(".Methods", envir = environment(fdef), methods)
    return(TRUE)
}
