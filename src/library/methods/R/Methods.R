
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
    function(name, def = NULL, group = list(), valueClass = character(), where = topenv(parent.frame()),
             package = NULL, signature = NULL,
             useAsDefault = NULL,
             genericFunction = NULL)
{
    if(exists(name, "package:base") &&
       typeof(get(name, "package:base")) != "closure") {
        getGeneric(name) # will fail if this can't have methods
        msg <- gettextf("'%s' is a primitive function;  methods can be defined, but the generic function is implicit, and cannot be changed.", name)
        if(nargs() == 1)
            warning(msg, domain = NA)
        else
            stop(msg, domain = NA)
        return(name)
    }
    stdGenericBody <- substitute(standardGeneric(NAME), list(NAME = name))
    ## get the current function which may already be a generic
    fdef <- getFunction(name, mustFind = FALSE, where = where)
    if(is.null(fdef) && !isNamespace(where))
        fdef <- getFunction(name, mustFind = FALSE)
    if(is.null(fdef) && is.function(useAsDefault))
        fdef <- useAsDefault
    ## Use the previous function definition to get the default
    ## and to set the package if not supplied.
    if(is(fdef, "genericFunction")) {
        prevDefault <- finalDefaultMethod(getMethods(fdef))
        if(is.null(package))
            package <- fdef@package
    }
    else if(is.function(fdef)) {
        prevDefault <- fdef
        if(is.null(package))
            package <- getPackageName(environment(fdef))
    }
    else
        prevDefault <- NULL
    if(is.primitive(fdef)) ## get the pre-defined version
        fdef <- getGeneric(name, where = where)
    else if(is.function(fdef))
        body(fdef, envir = as.environment(where)) <- stdGenericBody
    if(!is.null(def)) {
        if(is.primitive(def) || !is(def, "function"))
            stop(gettextf("if the `def' argument is supplied, it must be a function that calls standardGeneric(\"%s\") to dispatch methods", name), domain = NA)
        fdef <- def
        if(is.null(genericFunction) && .NonstandardGenericTest(body(fdef), name, stdGenericBody))
            genericFunction <- new("nonstandardGenericFunction") # force this class for fdef
    }
    if(is.null(package) || nchar(package) == 0)
        ## either no previous def'n or failed to find its package name
        package <- getPackageName(where)
    if(is.null(fdef))
        stop("must supply a function skeleton, explicitly or via an existing function")
    if(!is(fdef, "genericFunction")) {
        if(is.function(useAsDefault))
            fdeflt <- useAsDefault
        else if(identical(useAsDefault, FALSE))
            fdeflt <- NULL
        else
            fdeflt <- prevDefault
        if(is.function(fdeflt))
            fdeflt <- .derivedDefaultMethod(fdeflt)
        fdef <- makeGeneric(name, fdef, fdeflt, group=group, valueClass=valueClass,
                            package = package, signature = signature,
                            genericFunction = genericFunction)
    }
    assign(name, fdef, where)
    if(length(fdef@group)> 0 && !is.null(getGeneric(fdef@group[[1]], where = where)))
        methods <- getAllMethods(name, fdef, where)
    else
        methods <- fdef@default # empty or containing the default
    assignMethodsMetaData(name, methods, fdef, where)
    name
}

##
## make a generic function object corresponding to the given function name.
##

isGeneric <-
  ## Is there a function named `f', and if so, is it a generic?
  ##
  ## If the `fdef' argument is supplied, take this as the definition of the
  ## generic, and test whether it is really a generic, with `f' as the name of
  ## the generic.  (This argument is not available in S-Plus.)
  function(f, where = topenv(parent.frame()), fdef = NULL, getName = FALSE)
{
    if(is.null(fdef))
        fdef <- getFunction(f, where=where, mustFind = FALSE)
    if(is.null(fdef))
      return(FALSE)
    ## check primitives. These are never stored as explicit generic functions.
    ## The definition of isGeneric for them is that methods metadata exists,
    ## either on this database or anywhere (where == -1)
    if(!identical(typeof(fdef), "closure"))
      return(exists(mlistMetaName(f, "base"))) # all primitives are on package base
    if(!is(fdef, "genericFunction"))
        return(FALSE)
    gen <- fdef@generic
    if(getName)
        return(gen)
    else if(missing(f) || .identC(gen, f))
        return(TRUE)
    else {
        warning(gettextf("function '%s' appears to be a generic function, but with generic name '%s'", f, gen), domain = NA)
        return(FALSE)
    }
}

removeGeneric <-
  ## Remove the generic function of this name, specifically the first version
  ## encountered from environment where
  ##
    function(f, where = topenv(parent.frame()))
{
    ev <- fdef <- NULL
    allEv <- findFunction(f, where = where)
    for(maybeEv in allEv) {
        fdef <- get(f, maybeEv)
        if(is(fdef, "genericFunction")) {
            ev <- maybeEv
            break
        }
    }
    found <- is(fdef, "genericFunction")
    if(found) {
        removeMethodsObject(f, where)
        rm(list = fdef@generic, pos = where)
    }
    else {
        if(!is.character(f))
            f <- deparse(f)
        warning(gettextf("generic function '%s' not found for removal", f),
                domain = NA)
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
  function(f, where = topenv(parent.frame()))
{
    if(is.character(f))
        fdef <- getGeneric(f, where = where)
    else if(is.function(f))
        fdef <- f
    else
        stop(gettextf("invalid argument 'f', expected function or its name, got an object of class \"%s\"", class(f)), domain = NA)
    if(is.null(fdef))
        NULL
    else
        getMethodsForDispatch(f, fdef)

}

getMethodsForDispatch <-
  function(f, fdef)
{
    if(is(fdef, "genericMethods"))
        fdef@.Methods
    else {
        ev <- environment(fdef)
        if(exists(".Methods", envir = ev, inherits = FALSE))
            get(".Methods", envir = ev)
        else
            NULL
    }
}

## some functions used in MethodsListSelect, that must be safe against recursive
## method selection.  TODO:  wouldn't need this if methods package had a name space

.existsBasic <- get("exists", "package:base")
.getBasic <- get("get", "package:base")
.evBasic <- get("environment", "package:base")
.assignBasic <- get("assign", "package:base")
.setIfBase <- function(f, fdef, mlist) {
    if(is.null(f))
        FALSE
    else {
        found <- .existsBasic(f, "package:base")
        if(found) {
            ## force (default) computation of mlist in MethodsListSelect
            .assignBasic(".Methods", envir = .evBasic(fdef), .getBasic(f, "package:base"))
        }
        found
    }
}

##NB used internally in MethodsListSelect.  Must NOT use the standard version
## to prevent recursion
.getMethodsForDispatch <- function(f, fdef) {
    ev <- .evBasic(fdef)
    if(.existsBasic(".Methods", envir = ev)) {
        .getBasic(".Methods", envir = ev)
    }
    else
        NULL
}

.setMethodsForDispatch <- function(f, fdef, mlist) {
    ev <- environment(fdef)
    if(!is(fdef, "genericFunction") ||
       !exists(".Methods", envir = ev, inherits = FALSE))
        stop(gettextf("internal error: did not get a valid generic function object for function '%s'", f), domain = NA)
    assign(".Methods", envir = ev, mlist)
}

cacheMethod <-
  ## cache the given definition in the method metadata for f
  ## Support function:  DON'T USE DIRECTLY (does no checking)
  function(f, sig, def, args = names(sig), fdef) {
    ev <- environment(fdef)
    methods <- get(".Methods", envir = ev)
    methods <- insertMethod(methods, sig, args, def, TRUE)
    assign(".Methods", methods, envir = ev)
    deflt <- finalDefaultMethod(methods)
    if(is.primitive(deflt))
        setPrimitiveMethods(f, deflt, "set", fdef, methods)
    methods
  }


setMethod <-
## Define a method for the specified combination of generic function and signature.
## The method is stored in the methods meta-data of the specified database.
  ##
  ## Note that assigning methods anywhere but the global environment (`where==1') will
  ## not have a permanent effect beyond the current R session.
  function(f, signature = character(), definition, where = topenv(parent.frame()), valueClass = NULL,
           sealed = FALSE)
{
    ## Methods are stored in metadata in database where.  A generic function will be
    ## assigned if there is no current generic, and the function is NOT a primitive.
    ## Primitives are dispatched from the main C code, and an explicit generic NEVER
    ## is assigned for them.
    if(is.function(f) && is(f, "genericFunction")) {
        ## (two-part test to deal with bootstrapping of methods package)
        fdef <- f
        f <- fdef@generic
        gwhere <- .genEnv(f)
    }
    ## slight subtlety:  calling getGeneric vs calling isGeneric
    ## For primitive functions, getGeneric returns the (hidden) generic function,
    ## even if no methods have been defined.  An explicit generic MUST NOT be
    ## for these functions, dispatch is done inside the evaluator.
    else {
        where <- as.environment(where)
        gwhere <- .genEnv(f, where)
        fdef <- getGeneric(f, where = if(is.null(gwhere)) where else gwhere)
    }
    if(.lockedForMethods(fdef, where))
        stop(gettextf("the environment '%s' is locked; cannot assign methods for function '%s'", getPackageName(where), f), domain = NA)
    hasMethods <- !is.null(fdef)
    deflt <- getFunction(f, generic = FALSE, mustFind = FALSE, where = where)
    ## where to insert the methods in generic
    if(is.null(gwhere)) {
        allWhere <- findFunction(f, where = where)
        generics <-logical(length(allWhere))
        if(length(allWhere)>0) { # put methods into existing generic
            for(i in seq(along = allWhere)) {
                fi <- get(f, allWhere[[i]])
                geni <- is(fi, "genericFunction")
                generics[[i]] <- geni
                if(!geni && is.null(deflt))
                    deflt <- fi
            }
        }
        if(any(generics)) {
            ## try to add method to the existing generic, but if the corresponding
            ## environment is sealed, must create a new generic in where
            gwhere <- as.environment(allWhere[generics][[1]])
            if(.lockedForMethods(fdef, gwhere)) {
                if(identical(as.environment(where), gwhere))
                    stop(gettextf("the 'where' environment (%s) is a locked namespace; cannot assign methods there",
                                  getPackageName(where)), domain = NA)
                msg <-
                    gettextf("copying the generic function '%s' to environment '%s', because the previous version was in a sealed namespace (%s)",
                             f,
                             getPackageName(where),
                             getPackageName(gwhere))
                message(strwrap(msg), domain = NA)
                assign(f, fdef, where)
                gwhere <- where
            }
        }
    }
    if(!hasMethods)
      fdef <- deflt
    if(is.null(fdef))
      stop(gettextf("no existing definition for function '%s'", f),
           domain = NA)
    if(!hasMethods) {
        message(gettextf("Creating a new generic function for '%s' in '%s'",
                         f, getPackageName(where)),
                domain = NA)
        ## create using the visible non-generic as a pattern and default method
        setGeneric(f, where = where)
        fdef <- getGeneric(f, where = where)
    }
    else if(identical(gwhere, NA)) {
        ## better be a primitive since getGeneric returned a generic, but none was found
        if(is.null(elNamed(.BasicFunsList, f)))
            stop(gettextf("apparent internal error: a generic function was found for '%s', but no corresponding object was found searching from '%s'",
                          f, getPackageName(where)), domain = NA)
    }
    if(isSealedMethod(f, signature, fdef))
        stop(gettextf("the method for function '%s' and signature %s is sealed and cannot be re-defined", f, .signatureString(fdef, signature)), domain = NA)
    signature <- matchSignature(signature, fdef, where)
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
                   definition <- rematchDefinition(definition, fdef, mnames, fnames, signature)
               }
               definition <- matchDefaults(definition, fdef) # use generic's defaults if none in method
           },
           builtin = , special = {
             ## the only primitive methods allowed are those equivalent
             ## to the default, for generics that were primitives before
             ## and will be dispatched by C code.
             if(!identical(definition, deflt))
                stop("primitive functions cannot be methods; they must be enclosed in a regular function")
           },
           "NULL" = {

           },
           stop(gettextf("invalid method definition: expected a function, got an object of class \"%s\"", class(definition)), domain = NA)
           )
    margs  <- (fdef@signature)[1:length(signature)]
    definition <- asMethodDefinition(definition, signature, sealed)
    whereMethods <- .getOrMakeMethodsList(f, where, fdef)
    whereMethods <- insertMethod(whereMethods, signature, margs, definition)
    allMethods <- getMethods(fdef)
    allMethods <- insertMethod(allMethods, signature, margs, definition)
    resetGeneric(f, fdef, allMethods, gwhere, deflt) # Note: gwhere not used by resetGeneric
    ## assigns the methodslist object
    ## and deals with flags for primitives & for updating group members
    if(!is.null(where))
        assignMethodsMetaData(f, whereMethods, fdef, where, deflt)
    f
}

removeMethod <- function(f, signature = character(), where = topenv(parent.frame())) {
    fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function '%s' found", f), domain = NA)
        return(FALSE)
    }
    if(is.null(getMethod(fdef, signature, optional=TRUE))) {
        warning(gettextf("no method found for function '%s' and signature %s",
                         fdef@generic,
                         paste(dQuote(signature), collapse =", ")),
                domain = NA)
        return(FALSE)
    }
    setMethod(f, signature, NULL, where = where)
    TRUE
}

findMethod <- function(f, signature, where = topenv(parent.frame())) {
    fM <- mlistMetaName(f)
    where <- .findAll(fM, where)
    found <- logical(length(where))
    for(i in seq(along = where)) {
        wherei <- where[[i]]
        mi <- get(fM, wherei, inherits=FALSE)
        mi <- getMethod(f, signature, where = wherei, optional = TRUE, mlist = mi)
        found[i] <- is(mi, "function")
        if(found[i] && is.environment(wherei))
            simple <- FALSE
    }
    value <- where[found]
    ## to conform to the API, try to return a numeric or character vector
    ## if possible
    what <- sapply(value, class)
    if(identical(what, "numeric") || identical(what, "character"))
        unlist(value)
    else
        value
}

getMethod <-
  ## Return the function that is defined as the method for this generic function and signature
  ## (classes to be matched to the arguments of the generic function).
  function(f, signature = character(), where = topenv(parent.frame()), optional = FALSE,
           mlist)
{
    fdef <- getGeneric(f, !optional, where = where)
    if(missing(mlist)) {
        if(missing(where))
            mlist <- getMethods(f)
        else
            mlist <- getMethods(f, where)
    }
    if(!is(fdef, "genericFunction")) # must be the optional case, else an error in getGeneric
        return(NULL)
    i <- 1
    argNames <- fdef@signature
    signature <- matchSignature(signature, fdef)
    Classes <- signature # a copy just for possible error message
    while(length(signature) > 0 && is(mlist, "MethodsList")) {
        if(!identical(argNames[[i]], as.character(mlist@argument)))
            stop(gettextf("apparent inconsistency in the methods for function '%s'; argument '%s' in the signature corresponds to '%s' in the methods list object",
                          .genericName(f), argNames[[i]], as.character(mlist@argument)), domain = NA)
        Class <- signature[[1]]
        signature <- signature[-1]
        methods <- slot(mlist, "methods")
        mlist <- elNamed(methods, Class)# may be function, MethodsList or NULL
        i <- i + 1
    }
    if(length(signature) == 0) {
        ## process the implicit remaining "ANY" elements
        if(is(mlist, "MethodsList"))
            mlist <- finalDefaultMethod(mlist)
        if(is(mlist, "function"))
            return(mlist) # the only successful outcome
    }
    if(optional)
        mlist                           ## may be NULL or a MethodsList object
    else {
        ## for friendliness, look for (but don't return!) an S3 method
        if(length(Classes) == 1 && exists(paste(.genericName(f), Classes, sep="."), where))
            stop(gettextf("no S4 method for function '%s' and signature %s; consider getS3method() if you wanted the S3 method",
                          .genericName(f), Classes), domain = NA)
        if(length(Classes) > 0) {
            length(argNames) <- length(Classes)
            Classes <- paste(argNames," = \"", unlist(Classes),
                             "\"", sep = "", collapse = ", ")
        }
        else
            Classes <- "\"ANY\""
        stop(gettextf("no method defined for function '%s' and signature %s",
                      .genericName(f), Classes), domain = NA)
    }
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
             useInherited = TRUE,
             mlist = (if(is.null(fdef)) NULL else getMethodsForDispatch(f, fdef)),
             fdef = getGeneric(f, !optional))
{
    evalArgs <- is.environment(signature)
    if(evalArgs)
        env <- signature
    else if(length(names(signature)) == length(signature))
        env <- sigToEnv(signature, fdef)
    else if(is.character(signature)) {
        argNames <-  formalArgs(fdef)
        length(argNames) <- length(signature)
        argNames <- argNames[is.na(match(argNames, "..."))]
        names(signature) <- argNames
        env <- sigToEnv(signature, fdef)
    }
    else
        stop("signature must be a vector of classes or an environment")
    if(is.null(mlist)) {
        if(optional)
            return(mlist)
        else
            stop(gettextf("'%s' has no methods defined", f), domain = NA)
    }
    selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
    if(is.null(selection) && !identical(useInherited, FALSE)) {
      ## do the inheritance computations to update the methods list, try again.
      ##
      ## assign the updated information to the method environment
      fEnv <- environment(fdef)
      if(exists(".SelectMethodOn", fEnv, inherits = FALSE))
          ##<FIXME> This should have been eliminated now
          ## we shouldn't be doing method selection on a function used in method selection!
          ## Having name spaces for methods will prevent this happening -- until then
          ## force a return of the original default method
          return(finalDefaultMethod(mlist, f))
      assign(".SelectMethodOn", TRUE, fEnv)
      on.exit(rm(.SelectMethodOn, envir = fEnv))
      ##</FIXME>
      mlist <- MethodsListSelect(f, env, mlist, NULL, evalArgs = evalArgs,
                                 useInherited = useInherited, resetAllowed = FALSE)
      if(is(mlist, "MethodsList"))
          selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
    }
    if(is(selection, "function"))
        selection
    else if(is(selection, "MethodsList")) {
      if(optional)
        selection
      else
        stop("no unique method corresponding to this signature")
    }
    else {
        if(optional)
            selection
        else
            stop("unable to match signature to methods")
    }
}

hasMethod <-
  ## returns `TRUE' if `f' is the name of a generic function with an (explicit or inherited) method for
  ## this signature.
  function(f, signature = character(), where = .genEnv(f, topenv(parent.frame())))
{
    fdef <- getGeneric(f, where = where)
    if(is.null(fdef))
        FALSE
    else
        !is.null(selectMethod(f, signature, optional = TRUE, fdef = fdef))
}

existsMethod <-
  ## returns `TRUE' if `f' is the name of a generic function with an (explicit) method for
  ## this signature.
  function(f, signature = character(), where = topenv(parent.frame()))
{
    if(isGeneric(f, where))
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
  function(f, file = "", signature = character(), methods, where = topenv(parent.frame()) )
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
            stop(gettextf("bad class specified for element %d (should be a single character string)", i), domain = NA)
    }
    value
}

showMethods <-
    ## Show all the methods for the specified function.
    ##
    ## If `where' is supplied, the definition from that database will
    ## be used; otherwise, the current definition is used (which will
    ## include inherited methods that have arisen so far in the
    ## session).
    ##
    ## The output style is different from S-Plus in that it does not
    ## show the database from which the definition comes, but can
    ## optionally include the method definitions, if `includeDefs == TRUE'.
    ##
    function(f = character(), where = topenv(parent.frame()), classes = NULL,
             includeDefs = FALSE, inherited = TRUE, printTo = stdout())
{
    if(identical(printTo, FALSE)) {
        tmp <- tempfile()
        on.exit(unlink(tmp))
        con <- file(tmp, "w")
    }
    else con <- printTo
    if(is(f, "function"))
        f <- as.character(substitute(f))
    if(!is(f, "character"))
        stop(gettextf("first argument should be the name(s) of generic functions (got object of class \"%s\)", class(f)), domain = NA)
    if(length(f)==0) {
        f <- if(missing(where)) getGenerics() else getGenerics(where)
    }
    if(length(f) == 0)
        cat(file = con, "No applicable functions")
    else if(length(f) > 1) {
        value <- character()
        for(ff in f) { ## recall for each
            mlist <- getMethods(ff, where)
            if(length(mlist@methods) == 0)
                next
            value <- c(value,
                       Recall(ff, where, classes, includeDefs, inherited, printTo))
        }
        if(length(value) > 0)
            return(value)
        else
            return()
    }
    else { ## f of length 1
        cat(file= con, "\nFunction \"", f, "\":\n", sep="")
        if(!isGeneric(f, where))
            cat(file = con, "<not a generic function>\n")
        else {
            mlist <-  getMethods(f, where)
            if(is.null(mlist))
                cat(file = con, "<no applicable methods>\n")
            else
                showMlist(mlist, includeDefs, inherited, classes, printTo = con)
        }
    }
    if(identical(printTo, FALSE)) {
        close(con)
        readLines(tmp)
    }
}

removeMethodsObject <-
    function(f, where = topenv(parent.frame()))
{
  what <- mlistMetaName(f)
  if(!exists(what, where))
      return(FALSE)
  where <- as.environment(where)
  if(environmentIsLocked(where)) {
      warning(gettextf("the environment/package '%s' is locked; cannot remove methods data for '%s'",
                       getPackageName(where), f), domain = NA)
      return(FALSE)
  }
  rm(list = what, pos = where)
  TRUE
}


removeMethods <-
  ## removes all the methods defined for this generic function.  Returns `TRUE' if
  ## `f' was a generic function, `FALSE' (silently) otherwise.
  ##
  ## If there is a default method, the function will be re-assigned as
  ## a simple function with this definition; otherwise, it will be removed.  The
  ## assignment or removal can be controlled by optional argument `where', which
  ## defaults to the first element of the search list having a function called `f'.
  function(f, where = topenv(parent.frame()), all = TRUE)
{
    ## NOTE:  The following is more delicate than one would like, all because of
    ## methods for primitive functions.  For those, no actual generic function exists,
    ## but isGeneric(f) is TRUE if there are methods.  We have to get the default from
    ## the methods object BEFORE calling removeMethodsObject, in case there are no more
    ## methods left afterwards. AND we can't necessarily use the same default "where"
    ## location for methods object and generic, for the case of primitive functions.
    ## And missing(where) only works in R BEFORE the default is calculated.  Hence
    ## the peculiar order of computations and the explicit use of missing(where).
    fdef <- getGeneric(f, where = where)
    if(!is(fdef, "genericFunction")) {
        warning(gettextf("'%s' is not a generic function in '%s'; methods not removed",
                f, getPackageName(where)), domain = NA)
        return(FALSE)
    }
    mlist <- getMethods(fdef)
    default <- finalDefaultMethod(mlist)
    fMetaName <- mlistMetaName(f)
    allWhere <- .findAll(fMetaName, where)
    if(!all)
        allWhere <- allWhere[1]
    value <- rep(TRUE, length(allWhere))
    for(i in seq(along=allWhere)) {
        db <- allWhere[[i]]
        obj <- get(fMetaName, db)
        ## remove non-empty methods list objects
        if(is(obj, "MethodsList") && length(obj@methods)>0)
            value[[i]] <- removeMethodsObject(f, where = db)
    }
    ## cacheGenericsMetaData is called to clear primitive methods if there
    ## are none for this generic on other databases.
    cacheGenericsMetaData(f, fdef, FALSE, where)
    allWhere <- allWhere[value] # process functions only where methods successfully removed
    for(i in seq(along=allWhere)) {
        db <- as.environment(allWhere[[i]])
        if(isGeneric(f, db)) { # note use of isGeneric to work for primitives
            if(environmentIsLocked(db)) {
                warning(gettextf("cannot restore previous version of '%s' in locked environment/package '%s'",
                        f, getPackageName(db)), domain = NA)
                value[[i]] <- FALSE
            }
            ## restore the original function if one was used as default
            if(is(default, "derivedDefaultMethod")) {
                default <- as(default, "function") # strict, removes slots
                rm(list=f, pos = db)
                if(!existsFunction(f, FALSE, db)) {
                    message(gettextf("restoring default function definition of \'%s'",
                                     f), domain = NA)
                    assign(f, default, db)
                }
                ## else the generic is removed, nongeneric will be found elsewhere
            }
            ## else, leave the generic in place
            else {
                mlist@methods <- mlist@allMethods <- list()
                resetGeneric(f, fdef, mlist, db, default)
                assignMethodsMetaData(f, mlist, fdef, db, default)
            }
            break
        }
    }
    any(value)
}


resetGeneric <- function(f, fdef = getGeneric(f, where = where), mlist = getMethods(fdef), where = topenv(parent.frame()), deflt = finalDefaultMethod(mlist)) {
    if(!is(fdef, "genericFunction")) {
        if(missing(mlist)) {
            warning(gettextf("cannot reset '%s', the definition is not a generic function object", f), domain = NA)
            return(NULL)
        }
        else
            stop(gettextf("error in updating generic function '%s'; the function definition is not a generic function (class \"%s\")", f, class(fdef)),
                 domain = NA)
    }
    ## uncache
    mlist@allMethods <- mlist@methods
    .genericAssign(f, fdef, mlist, where, deflt)
    f
}

setReplaceMethod <-
  function(f, ..., where = topenv(parent.frame()))
  setMethod(paste(f, "<-", sep=""), ..., where = where)

setGroupGeneric <-
    ## create a group generic function for this name.
    function(name, def = NULL, group = list(), valueClass = character(),
             knownMembers = list(), package = getPackageName(where), where = topenv(parent.frame()))
{
    if(is.null(def)) {
        def <- getFunction(name, where = where)
        if(isGroup(name, fdef = def)) {
            if(nargs() == 1) {
                message(gettextf("function \"%s\" is already a group generic; no change",
                                 name),
                        domain = NA)
                return(name)
            }
        }
    }
    ## By definition, the body must generate an error.
    body(def, envir = environment(def)) <- substitute(
              stop(MSG, domain = NA),
              list(MSG = gettextf("function '%s' is a group generic; do not call it directly", name)))
    if(is.character(knownMembers))
        knownMembers <- as.list(knownMembers) # ? or try to find them?
    setGeneric(name, def, group = group, valueClass = valueClass,
               package = package, useAsDefault = FALSE,
               genericFunction =
                 new("groupGenericFunction", def, groupMembers = knownMembers),
               where = where)
    name
}

isGroup <-
  function(f, where = topenv(parent.frame()), fdef = getGeneric(f, where = where))
  {
    is(fdef, "groupGenericFunction")
  }

callGeneric <- function(...)
{
    frame <- sys.parent()

    # the two lines below this comment do what the previous version of
    # did in the expression fdef <- sys.function(frame)
    fname <- sys.call(frame)[[1]]
    fdef <- get(as.character(fname), env = parent.frame())

    if(is.primitive(fdef)) {
        if(nargs() == 0)
            stop("'callGeneric' with a primitive needs explicit arguments (no formal args defined)")
        else {
            fname <- sys.call(frame)[[1]]
            call <- substitute(fname(...))
        }
    }
    else {
        env <- environment(fdef)
        if(!exists(".Generic", env, inherits = FALSE))
            stop("'callGeneric' must be called from a generic function or method")
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

initMethodDispatch <- function(where = topenv(parent.frame()))
    .Call("R_initMethodDispatch", as.environment(where),
          PACKAGE = "methods")# C-level initialization

isSealedMethod <- function(f, signature, fdef = getGeneric(f, FALSE, where = where), where = topenv(parent.frame())) {
    fNonGen <- getFunction(f, FALSE, FALSE, where = where)
    if(!is.primitive(fNonGen)) {
        mdef <- getMethod(f, signature, optional = TRUE)
        return(is(mdef, "SealedMethodDefinition"))
    }
    ## else, a primitive
    if(is(fdef, "genericFunction"))
        signature <- matchSignature(signature, fdef)
    if(length(signature)==0)
        TRUE # default method for primitive
    else {
        sealed <- !is.na(match(signature[[1]], .BasicClasses))
        if(sealed &&
           (!is.na(match("Ops", c(f, getGroup(f, TRUE))))
            || !is.na(match(f, c("%*%", "crossprod")))))
            ## Ops methods are only sealed if both args are basic classes
            sealed <- sealed && (length(signature) > 1) &&
                      !is.na(match(signature[[2]], .BasicClasses))
        sealed
    }
}

.lockedForMethods <- function(fdef, env) {
    ## the env argument is NULL if setMethod is only going to assign into the
    ## table of the generic function, and not to assign methods list object
    if(is.null(env) || !environmentIsLocked(env))
        return(FALSE) #? can binding be locked and envir. not?
    name <- fdef@generic
    package <- fdef@package
    objs <- c(name, mlistMetaName(name, package))
    for(obj in objs) {
        hasIt <- exists(obj, env, inherits = FALSE)
        ## the method object may be bound, or a new one may be needed
        ## in which case the env. better not be locked
        if((!hasIt || bindingIsLocked(obj, env)))
            return(TRUE)
    }
    FALSE
}
