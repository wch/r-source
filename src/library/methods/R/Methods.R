
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
    function(name, def = NULL, group = list(), valueClass = character(), where = 1,
             package = NULL, signature = NULL,
             useAsDefault = existsFunction(name, generic = FALSE),
             genericFunction = NULL)
{
    if(nargs() == 1 && isGeneric(name)) {
        message("Function \"", name, "\" is already a generic; no change")
        return(name)
    }
    if(exists(name, "package:base") &&
         typeof(get(name, "package:base")) != "closure") {
        msg <- paste("\"",name, "\" is a primitive function; its generic definition is built in and automatically included.", sep="")
        if(nargs() == 1)
            warning(msg)
        else
            stop(msg)
        return(name)
    }
    stdGenericBody <- substitute(standardGeneric(NAME), list(NAME = name))
    if(is.null(def)) {
        ## get the current function which may already be a generic
        fdef <- getFunction(name, mustFind = FALSE)
        if(is.null(fdef))
            stop("Must supply a function skeleton, explicitly or via an existing function")
        else if(is.primitive(fdef)) ## get the pre-defined version
            fdef <- getGeneric(name)
        else
            body(fdef) <- stdGenericBody
        if(is.null(package))
            ## infer the package name; takes the first to be consistent with
            ## ignoring conflicts in finding fdef above.
            package <- functionPackageName(name)[[1]]
    }
    else {
        fdef <- def
        if(is.null(genericFunction) && .NonstandardGenericTest(body(fdef), name, stdGenericBody))
            genericFunction <- new("nonstandardGenericFunction")
        if(is.null(package))
            package <- getPackageName(where)
    }
    if(!is(fdef, "genericFunction")) {
        if(is.function(useAsDefault))
            fdeflt <- useAsDefault
        else if(useAsDefault)
            fdeflt <- getFunction(name, generic = FALSE, mustFind = FALSE)
        else
            fdeflt <- NULL
        fdef <- makeGeneric(name, fdef, fdeflt, group=group, valueClass=valueClass,
                            package = package, signature = signature,
                            genericFunction = genericFunction)
    }
    methods <- fdef@default # methods list: empty or containing the default
    ## there are two assignment steps.  First, assign the methods metadata
    assignMethodsMetaData(name, methods, fdef, where)
    ## Second, the generic version of the function.
    assign(name, fdef, where)
    name
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
  function(f, where = -1, fdef = NULL, getName = FALSE)
{
    ## the fdef argument is not found in S4 but should be ;-)
    if(is.null(fdef))
          fdef <- getFunction(f, where=where, mustFind = FALSE)
    if(is.null(fdef))
      return(FALSE)
    ## check primitives. These are never stored as explicit generic functions.
    ## The definition of isGeneric for them is that methods metadata exists,
    ## either on this database or anywhere (where == -1)
    if(!identical(typeof(fdef), "closure")) {
      if(is.null(getMethodsMetaData(f, where)))
        return(FALSE)
      else fdef <- getGeneric(f)
    }
    if(!is(fdef, "genericFunction"))
        return(FALSE)
    gen <- fdef@generic
    if(getName)
        return(gen)
    else if(missing(f) || identical(gen, f))
        return(TRUE)
    else {
        warning(paste("Function \"", f, "\" appears to be a Generic function, but with generic name \"", gen, "\""))
        return(FALSE)
    }
}

removeGeneric <-
  ## Remove the generic function of this name.
  ##
  ## If `where' supplied,  remove the version on this element of the search list;
  ## otherwise, removes the first version encountered.
    function(f, where = -1)
{
    found <- is(f, "genericFunction")
    if(!found) {
        if(identical(where, -1)) {
            fdef <- getGeneric(f)
            if(is(fdef, "genericFunction")) {
                where <- find(fdef@generic)
                if(length(where)>1)
                    for(db in where)
                        if(is(get(f, db), "genericFunction")) {
                            where <- db
                            break
                        }
            }
        }
        else  fdef <- get(f, where)
    }
    if(is(fdef, "genericFunction")) {
        found <- TRUE
        removeMethodsObject(fdef, where)
        rm(list = fdef@generic, pos = where)
    }
    if(!found) {
        if(!is.character(f))
            f <- deparse(f)
        warning(paste("Generic function \"", f, "\" not found for removal", sep=""))
    }
    return(found)
}


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
    if(identical(where, -1))
        getMethodsForDispatch(f)
    else
        getMethodsMetaData(f, where)
}

getMethodsForDispatch <-
  function(f, fdef = getGeneric(f))
{
    ev <- environment(fdef)
    if(exists(".Methods", envir = ev)) {
        methods <- get(".Methods", envir = ev)
        if(is.null(methods))
            getAllMethods(fdef@generic, fdef)
        else
            methods
    }
    else
        NULL
}

cacheMethod <-
  ## cache the given definition in the method metadata for f
  ## Support function:  DON'T USE DIRECTLY (does no checking)
  function(f, sig, def, args = names(sig), fdef = getGeneric(f)) {
    ev <- environment(fdef)
    methods <- get(".Methods", envir = ev)
    if(is.null(methods))
        methods <- getAllMethods(f, fdef)
    methods <- insertMethod(methods, sig, args, def, TRUE)
    assign(".Methods", methods, envir = ev)
    methods
  }
  

setMethod <-
## Define a method for the specified combination of generic function and signature.
## The method is stored in the methods meta-data of the specified database.
  ##
  ## Note that assigning methods anywhere but the global environment (`where==1') will
  ## not have a permanent effect beyond the current R session.
  function(f, signature = character(), definition, where = 1, valueClass = NULL)
{
    whereString <- if(is.environment(where)) deparse(where) else where
    ## Methods are stored in metadata in database where.  A generic function will be
    ## assigned if there is no current generic, and the function is NOT a primitive.
    ## Primitives are dispatched from the main C code, and an explicit generic NEVER
    ## is assigned for them.

    ## slight subtlety:  calling getGeneric vs calling isGeneric
    ## For primitive functions, getGeneric returns the (hidden) generic function,
    ## even if no methods have been defined.  An explicit generic MUST NOT be
    ## for these functions, dispatch is done inside the evaluator.
    fdef <- getGeneric(f)
    deflt <- getFunction(f, generic = FALSE, mustFind = FALSE)
    hasMethods <- !is.null(fdef)
    if(!hasMethods)
      fdef <- deflt
    if(is.null(fdef))
      stop(paste("No existing definition for function \"",f,"\"", sep=""))
    if(!hasMethods) {
      message("Creating a new generic function for \"", f, "\" in package ",
                    getPackageName(where))
      setGeneric(f, where = where)
      fdef <- getGeneric(f)
    }
    signature <- matchSignature(signature, fdef)
    allMethods <- .getOrMakeMethodsList(f, where, fdef)
    switch(typeof(definition),
           closure = {
               fnames <- formalArgs(fdef)
               mnames <- formalArgs(definition)
               if(!identical(mnames, fnames)) {
                   ## omitted classes in method => "missing"
                   fullSig <- conformMethod(signature, mnames, fnames, f)
                   if(!identical(fullSig, signature)) {
                       formals(definition, envir = environment(definition)) <- formals(fdef)
                       signature <- fullSig
                   }
                   ## extra classes in method => use "..." to rematch
                   definition <- rematchDefinition(definition, fdef, mnames, fnames)
               }
           },
           builtin = , special = {
             ## the only primitive methods allowed are those equivalent
             ## to the default, for generics that were primitives before
             ## and will be dispatched by C code.
             if(!identical(definition, deflt))
                stop("Primitive functions cannot be methods; they must be enclosed in a regular function")
           },
           "NULL" = {}, # Will remove the method, if any, currently in this signature
           stop("Invalid method definition: not a function"))
    allMethods <- insertMethod(allMethods, signature, fnames[1:length(signature)],
                               asMethodDefinition(definition, signature))
    ## assign the methods (also updates the session info)
    assignMethodsMetaData(f, allMethods, fdef, where, deflt)
    f
}

removeMethod <- function(f, signature = character(), where) {
    if(missing(where)) {
        where <- findMethod(f, signature)
        if(length(where) == 0) {
            warning("No method found for function ",f, " and signature ",
                    paste(signature, collapse =", "))
            return(FALSE)
        }
        else if(length(where) > 1) {
            warning("Method found in multiple packages: ", paste(where, collapse = ", "),
                    " (The first one will be removed)")
            where <- where[[1]]
        }
    }
    setMethod(f, signature, NULL, where = where)
    TRUE
}

findMethod <- function(f, signature, where = search()) {
    found <- logical(length(where))
    metaName <- mlistMetaName(f)
    for(i in seq(along = where)) {
        found[i] <- exists(metaName, where[[i]]) &&
            is(getMethod(f, signature, where = where[[i]], optional = TRUE),
               "function")
    }
    where[found]
}

getMethod <-
  ## Return the function that is defined as the method for this generic function and signature
  ## (classes to be matched to the arguments of the generic function).
  function(f, signature = character(), where = -1, optional = FALSE)
{
    mlist <- getMethods(f, where)
    if(length(signature) == 0)
        mlist <- finalDefaultMethod(mlist)
    else while(is(mlist, "MethodsList")) {
        Class <- signature[[1]]
        if(Class == "")
            Class <- "ANY"
        methods <- slot(mlist, "methods")
        mlist <- elNamed(methods, Class)# may be function, MethodsList or NULL
        signature <- signature[-1]
        if(length(signature) == 0)
            break
    }
    if(is(mlist, "function") && length(signature) == 0)
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
  ## env = an environment, in which the class corresponding to each argument
  ##       is assigned with the argument's name.
  ## optional = If TRUE, and no explicit selection results, return result anyway. else error
  ## mlist = Optional MethodsList object to use in the search.
    function(f, signature, optional = FALSE,
             useInherited = TRUE, mlist = getMethods(fdef), fdef = getGeneric(f))
{
    evalArgs <- is.environment(signature)
    if(evalArgs)
        env <- signature
    else if(length(names(signature)) == length(signature))
        env <- sigToEnv(signature)
    else if(is.character(signature)) {
        argNames <-  formalArgs(fdef)
        length(argNames) <- length(signature)
        argNames <- argNames[is.na(match(argNames, "..."))]
        names(signature) <- argNames
        env <- sigToEnv(signature)
    }
    else
        stop("signature must be a vector of classes or an environment")
    if(is.null(mlist)) {
        if(optional)
            return(mlist)
        else
            stop(paste("\"", f, "\" has no methods defined", sep=""))
    }
    selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
    if(is.null(selection) && !identical(useInherited, FALSE)) {
      ## do the inheritance computations to update the methods list, try again.
      ##
      ## assign the updated information to the method environment
      fEnv <- environment(fdef)
      if(exists(".SelectMethodOn", fEnv, inherits = FALSE))
          stop(paste("Apparent loop in selectMethod for function \"",
                     f, "\":  try resetGeneric(\"", f,
                     "\"), or else a bug in method selection", sep=""))
      assign(".SelectMethodOn", TRUE, fEnv)
      on.exit(rm(.SelectMethodOn, envir = fEnv))
      mlist <- MethodsListSelect(f, env, mlist, NULL, evalArgs = evalArgs, useInherited = useInherited)
      if(is(mlist, "MethodsList"))
          selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
    }
    if(is(selection, "function"))
        selection
    else if(is(selection, "MethodsList")) {
      if(optional)
        selection
      else
        stop("No unique method corresponding to this signature")
    }
    else {
        if(optional)
            selection
        else
            stop("Unable to match signature to methods")
    }
}

hasMethod <-
  ## returns `TRUE' if `f' is the name of a generic function with an (explicit or inherited) method for
  ## this signature.
  function(f, signature = character())
{
    if(isGeneric(f))
        !is.null(selectMethod(f, signature, optional = TRUE))
    else
        FALSE
}

existsMethod <-
  ## returns `TRUE' if `f' is the name of a generic function with an (explicit) method for
  ## this signature.
  function(f, signature = character(), where = -1)
{
    if(isGeneric(f))
        !is.null(getMethod(f, signature, where = where, optional = TRUE))
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
    ##
    ## The output style is different from S-Plus in that it does not show the database
    ## from which the definition comes, but can optionally include the method definitions,
    ## if `includeDefs == TRUE'.
    function(f = character(), where = -1, classes = NULL, includeDefs = FALSE,
             inherited = TRUE, printTo = stdout())
{
    if(identical(printTo, FALSE)) {
        tmp <- tempfile()
        con <- file(tmp, "w")
    }
    else con <- printTo
    if(length(f)==0) {
        if(missing(where)) {
            f <- getGenerics()
            where = -1
        }
        else
            f <- getGenerics(where)
    }
    if(length(f) == 0)
        cat(file = con, "No applicable functions")
    else if(length(f) > 1) {
        value <- character()
        for(ff in f) {
            if(missing(where))
                mlist <- getMethods(ff)
            else
                mlist <- getMethods(ff, where)
            if(length(mlist@methods) == 0)
                next
            value <- c(value, Recall(ff, where, classes, includeDefs, inherited, printTo))
        }
        if(length(value) > 0)
            return(value)
        else
            return()
    }
    else {
        cat(file= con, "\nFunction \"", f, "\":\n", sep="")
        if(!isGeneric(f))
            cat(file = con, "<not a generic function>\n")
        else {
            if(missing(where))
                mlist <- getMethods(f)
            else
                mlist <-  getMethods(f, where)
            if(is.null(mlist))
                cat(file = con, "<no applicable methods>\n")
            else
                showMlist(mlist, includeDefs, inherited, classes, printTo = con)
        }
    }
    if(identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
    }
}

removeMethodsObject <-
    function(f, where = findUnique(mlistMetaName(f), ,paste("methods object for function",f)))
{
  what <- mlistMetaName(f)
  if(length(where) == 0 ||!exists(what, where))
      return(FALSE)
  resetGeneric(f)
  for(db in where)
      rm(list = what, pos = db)
  return(TRUE)
}
    

removeMethods <-
  ## removes all the methods defined for this generic function.  Returns `TRUE' if
  ## `f' was a generic function, `FALSE' (silently) otherwise.
  ##
  ## If there is a default method, the function will be re-assigned as
  ## a simple function with this definition; otherwise, it will be removed.  The
  ## assignment or removal can be controlled by optional argument `where', which
  ## defaults to the first element of the search list having a function called `f'.
  function(f, where = find(mlistMetaName(f)))
{
    ## NOTE:  The following is more delicate than one would like, all because of
    ## methods for primitive functions.  For those, no actual generic function exists,
    ## but isGeneric(f) is TRUE if there are methods.  We have to get the default from
    ## the methods object BEFORE calling removeMethodsObject, in case there are no more
    ## methods left afterwards. AND we can't necessarily use the same default "where"
    ## location for methods object and generic, for the case of primitive functions.
    ## And missing(where) only works in R BEFORE the default is calculated.  Hence
    ## the peculiar order of computations and the explicit use of missing(where).
    if(isGeneric(f))
        default <- getMethod(f, optional=TRUE)
    else if(missing(where))
        return(FALSE)
    value <- removeMethodsObject(f, where)
    ## call below is done to clear primitive methods if there
    ## are none for this generic on other databases.
    cacheGenericsMetaData(f, FALSE, where)
    for(db in where) {
        if(isGeneric(f, db)) {
            if(is.function(default)) {
                if(is(default, "MethodDefinition"))
                    default <- as(default, "function") # strict, removes slots
                cat("Restoring default function definition of", f, "\n")
                assign(f, default, db)
            }
            break
        }
    }
    value
}

resetGeneric <- function(f, fdef) {
  ## reset the currently defined methods for this generic by un-caching any inherited
  ## methods.  You MUST call this function when you change relevant inheritance information during a
  ## session, to guarantee that the new information is used if this generic has already been
  ## called.
    if(missing(fdef)) {
        if(is(f, "genericFunction"))
        {fdef <- f; f <- fdef@generic}
        else
            fdef <- getGeneric(f)
    }
    if(is(fdef, "genericFunction"))
        assign(".Methods", NULL, envir = environment(fdef))
    else
        warning("Can't reset \"", f, "\", the definition is not a generic function object")
}

setReplaceMethod <-
  function(f, ...)
  setMethod(paste(f, "<-", sep=""), ...)

setGroupGeneric <-
    ## create a group generic function for this name.
    function(name, def = NULL, group = list(), valueClass = character(),
             knownMembers = list(), package = getPackageName(where), where = 1)
{
    if(is.null(def)) {
        def <- getFunction(name)
        if(isGroup(name, fdef = def)) {
            ## a special R-only mechanism to turn on method dispatch
            ## for the members of groups of primitives
            members <- def@groupMembers
            if(length(members)>0) 
                for(what in members)
                    if(is.character(what) && is.primitive(getFunction(what, mustFind=FALSE)))
                        setGeneric(what)
            if(nargs() == 1) {
                message("Function \"", name, "\" is already a group generic; no change")
                return(name)
            }
        }
    }
    ## By definition, the body must generate an error.
    body(def, envir = environment(def)) <- substitute(stop(MSG), list(MSG =
                                            paste("Function \"", name,
                                                  "\" is a group generic; don't call it directly",
                                                  sep ="")))
    if(is.character(knownMembers))
        knownMembers <- as.list(knownMembers) # ? or try to find them?
    setGeneric(name, def, group = group, valueClass = valueClass,
               package = package,
               genericFunction =
                 new("groupGenericFunction", def, groupMembers = knownMembers),
               where = where)
    name
}

isGroup <-
  function(f, where = -1, fdef = getGeneric(f))
  {
      if(!identical(where, -1)) {
          if(!isGeneric(f, where = where, fdef = fdef))
              return(FALSE)
          if(missing(fdef))
              fdef <- get(f, where)
      }
    is(fdef, "groupGenericFunction")
  }

callGeneric <- function(...)
{
    frame <- sys.parent()
    fdef <- sys.function(frame)
    if(is.primitive(fdef)) {
        if(nargs() == 0)
            stop("callGeneric with a primitive needs explict arguments (no formal args defined)")
        else {
            fname <- sys.call(frame)[[1]]
            call <- substitute(fname(...))
        }
    }
    else {
        env <- environment(fdef)
        if(!exists(".Generic", env, inherits = FALSE))
            stop("callGeneric must be called from a generic function or method")
        f <- get(".Generic", env, inherits = FALSE)
        fname <- as.name(f)
        if(nargs() == 0) {
            call <- sys.call(frame)
            call <- match.call(fdef, call)
            anames <- names(call)
            matched <- !is.na(match(anames, names(formals(fdef))))
            for(i in seq(along = anames))
                if(matched[[i]])
                    call[[i]] <- as.name(anames[[i]])
        }
        else {
            call <- substitute(fname(...))
        }
    }
    eval(call, sys.frame(sys.parent()))
}

initMethodDispatch <- function()
    .C("R_initMethodDispatch", PACKAGE = "methods")# C-level initialization
