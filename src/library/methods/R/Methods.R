#  File src/library/methods/R/Methods.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


setGeneric <-
  ## Define `name' to be a generic  function, for which methods will be defined.
  ##
  ## If there is already a non-generic function of this name, it will be used
  ## to define the generic unless `def' is supplied, and the current
  ## function will become the default method for the generic.
  ##
  ## If `def' is supplied, this defines the generic function.  The
  ## default method for a new generic will usually be an existing
  ## non-generic.  See the .Rd page
  ##
    function(name, def = NULL, group = list(), valueClass = character(),
             where = topenv(parent.frame()),
             package = NULL, signature = NULL,
             useAsDefault = NULL, genericFunction = NULL)
{
    name <- switch(name, "as.double" =, "as.real" = "as.numeric", name)
    if(exists(name, "package:base") &&
       is.primitive(get(name, "package:base"))) { # primitives

        fdef <- getGeneric(name) # will fail if this can't have methods
        if(nargs() <= 1) {
            ## generics for primitives are global, so can & must always be cached
            .cacheGeneric(name, fdef)
            return(name)
        }
        ## you can only conflict with a primitive if you supply
        ## useAsDefault to signal you really mean a different function
        if(!is.function(useAsDefault) && !identical(useAsDefault, FALSE)) {
            msg <- gettextf("\"%s\" is a primitive function;  methods can be defined, but the generic function is implicit, and cannot be changed.", name)
            stop(msg, domain = NA)
        }
    }
    simpleCall <- nargs() < 2 || (nargs() == 2 & !missing(where))
    stdGenericBody <- substitute(standardGeneric(NAME), list(NAME = name))
    ## get the current function which may already be a generic
    fdef <- getFunction(name, mustFind = FALSE, where = where)
    ## if not a generic, try for a saved implicit version
    if(!is(fdef, "genericFunction") && .restoreImplicitGeneric(name, fdef, where, simpleCall))
        return(name)
    if(is.null(fdef) && !isNamespace(where))
        fdef <- getFunction(name, mustFind = FALSE)
    if(is.null(fdef) && is.function(useAsDefault))
        fdef <- useAsDefault
    ## Use the previous function definition to get the default
    ## and to set the package if not supplied.
    doUncache <- FALSE
    if(is.object(fdef) && is(fdef, "genericFunction")) {
        doUncache <- TRUE
        oldDef <- fdef
        prevDefault <- finalDefaultMethod(fdef@default)
        if(is.null(package))
            package <- fdef@package
    }
    else if(is.function(fdef)) {
        prevDefault <- fdef
        if(is.primitive(fdef)) package <- "base"
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
        if(is.primitive(def) || !is.function(def))
            stop(gettextf("if the `def' argument is supplied, it must be a function that calls standardGeneric(\"%s\") to dispatch methods", name), domain = NA)
        fdef <- def
        if(is.null(genericFunction) && .NonstandardGenericTest(body(fdef), name, stdGenericBody))
            genericFunction <- new("nonstandardGenericFunction") # force this class for fdef
    }
    thisPackage <- getPackageName(where)
    if(is.null(package) || !nzchar(package))
        ## either no previous def'n or failed to find its package name
        package <- thisPackage
    if(is.null(fdef))
        stop("must supply a function skeleton, explicitly or via an existing function")
    if(!(is.object(fdef) && is(fdef, "genericFunction"))) {
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
    if(!identical(package, thisPackage)) {
        ## setting a generic for a function in another package.
        ## In this case, the generic definition must agree with the implicit
        ## generic for the given function and package
        implicit <- implicitGeneric(name, where)
        if(is.null(implicit))
          {} # New function, go ahead
        else if(.identicalGeneric(fdef, implicit)) {
        }  # go ahead (FIXME: Would like  fdef <- implicit, but there are subtle diff's)
        else if(is.function(implicit)) {
            message(gettextf(
              "New generic for \"%s\" does not agree with implicit generic from package \"%s\"; a new generic will be assigned with package \"%s\"",
                             name, package, thisPackage),
                    domain = NA)
            fdef@package <- attr(fdef@generic, "package") <- thisPackage
        }
        else { # generic prohibited
            warning(gettextf(
              "No generic version of  \"%s\" on package \"%s\" is allowed; a new generic will be assigned with package \"%s\"",
                             name, package, thisPackage),
                    domain = NA)
            fdef@package <- attr(fdef@generic, "package") <- thisPackage
        }
    }
    if(doUncache)
      .uncacheGeneric(name, oldDef)
    groups = fdef@group
    for(group in groups) { # add as member of group generic(s) if not there
        gdef <- getGeneric(group)
        if(is(gdef, "groupGenericFunction") &&
           is.na(match(fdef@generic, as.character(gdef@groupMembers)))) {
            gwhere <- .genEnv(group, where)
            gdef@groupMembers <- c(gdef@groupMembers, list(fdef@generic))
            assign(group, gdef, gwhere)
        }
    }
    assign(name, fdef, where)
    .cacheGeneric(name, fdef)
    methods <- fdef@default # empty or containing the default
    assignMethodsMetaData(name, methods, fdef, where) # MethodsList, eventually will go away
    .assignMethodsTableMetaData(name, fdef, where)
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
    if(is.character(f) && f %in% c("as.double", "as.real")) f <- "as.numeric"
    if(is.null(fdef) && missing(where))
        fdef <- .getGenericFromCache(f, where)
    if(is.null(fdef))
        fdef <- getFunction(f, where=where, mustFind = FALSE)
    if(is.null(fdef))
      return(FALSE)
    ## check primitives. These are never found as explicit generic functions.
    if(is.primitive(fdef)) {
        ## the definition of isGeneric() for a primitive is that methods are defined
        ## (other than the default primitive)
        gen <- genericForPrimitive(f)
        return(is.function(gen) && length(objects(.getMethodsTable(gen), all=TRUE)) > 1)
    }
    if(!is(fdef, "genericFunction"))
        return(FALSE)
    gen <- fdef@generic # the name with package attribute
    if(missing(f) || .identC(gen, f)) {
         if(getName)
           gen
         else 
           TRUE
     }
    else {
        warning(gettextf("function \"%s\" appears to be a generic function, but with generic name \"%s\"", f, gen), domain = NA)
        FALSE
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
         .removeMethodsMetaTable(fdef, where)
         oldMetaName <- methodsPackageMetaName("M",fdef@generic, fdef@package)
         if(exists(oldMetaName, where, inherits = FALSE))
           rm(list = oldMetaName, pos = where)
        .uncacheGeneric(f, fdef)
        rm(list = fdef@generic, pos = where)
    }
    else {
        if(!is.character(f))
            f <- deparse(f)
        warning(gettextf("generic function \"%s\" not found for removal", f),
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
    ## This function returns a MethodsList object, no longer used for method dispatch
    ## A better structure for most purposes is the linear methods list returned by findMethods()
    ## There are no plans currently to make getMethods defunct, but it will be less
    ## efficient than findMethods()  both for creating the object and using it.

  ##  The function getMethods continues to
  ## return a methods list object, but now this is the metadata from where,
  ## or is converted from the internal table if where is missing.

    function(f, where = topenv(parent.frame()))
{
    nowhere <- missing(where) 
    if(is.character(f))
        fdef <- getGeneric(f, where = where)
    else if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else
        stop(gettextf("invalid argument \"f\", expected a function or its name, got an object of class \"%s\"",
                      class(f)), domain = NA)
    if(!is.null(fdef)) {
        if(nowhere)
          value <- .makeMlistFromTable(fdef)
        else
          value <- getMethodsMetaData(f, where = where)
        if(is.null(value))              # return empty methods list
            new("MethodsList", argument = fdef@default@argument)
        else
            value
    } # else NULL
}

getMethodsForDispatch <- function(fdef)
{
    .getMethodsTable(fdef, environment(fdef))
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
.getMethodsForDispatch <- function(fdef) {
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
        stop(gettextf("internal error: did not get a valid generic function object for function \"%s\"", f), domain = NA)
    assign(".Methods", envir = ev, mlist)
}

cacheMethod <-
  ## cache the given definition in the method metadata for f
  ## Support function:  DON'T USE DIRECTLY (does no checking)
  function(f, sig, def, args = names(sig), fdef, inherited = FALSE) {
      ev <- environment(fdef)

      .cacheMethodInTable(fdef, sig, def,
			  .getMethodsTable(fdef, ev, inherited = inherited))
      ## if this is not an inherited method, update the inherited table as well
      ## TODO:	in this case, should uncache inherited methods, though the callin
      ##  function will normally have done this.
      if(!inherited)
	  .cacheMethodInTable(fdef, sig, def,
			      .getMethodsTable(fdef, ev, inherited = TRUE))
  }

.removeCachedMethod <- function(f, sig, fdef = getGeneric(f))
    cacheMethod(f, sig, NULL, names(sig), fdef)


setMethod <-
## Define a method for the specified combination of generic function and signature.
## The method is stored in the methods meta-data of the specified database.
  ##
  ## Note that assigning methods anywhere but the global environment (`where==1') will
  ## not have a permanent effect beyond the current R session.
  function(f, signature = character(), definition, where = topenv(parent.frame()), valueClass = NULL,
           sealed = FALSE)
{

    ischar <- try(is.character(f), silent = TRUE)
    funcName = if (inherits(ischar, "try-error")) deparse(substitute(f)) else f
    if(is(funcName, "standardGeneric") || is(funcName, "MethodDefinition"))
       funcName = funcName@generic

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
    else if(is.function(f)) {
          if(is.primitive(f)) {
            f <- .primname(f)
            fdef <- genericForPrimitive(f)
            gwhere <- .genEnv(f)
         }
          else
            stop("A function for argument \"f\" must be a generic function")
    }
    ## slight subtlety:  calling getGeneric vs calling isGeneric
    ## For primitive functions, getGeneric returns the (hidden) generic function,
    ## even if no methods have been defined.  An explicit generic MUST NOT be
    ## for these functions, dispatch is done inside the evaluator.
    else {
        where <- as.environment(where)
        gwhere <- .genEnv(f, where)
        f <- switch(f, "as.double" =, "as.real" = "as.numeric", f)
        fdef <- getGeneric(f, where = if(identical(gwhere, baseenv())) where else gwhere)
    }
    if(.lockedForMethods(fdef, where))
        stop(gettextf("the environment \"%s\" is locked; cannot assign methods for function \"%s\"",
                      getPackageName(where), f), domain = NA)
    hasMethods <- !is.null(fdef)
    deflt <- getFunction(f, generic = FALSE, mustFind = FALSE, where = where)
    ## where to insert the methods in generic
    if(identical(gwhere, baseenv())) {
        allWhere <- findFunction(f, where = where)
        generics <-logical(length(allWhere))
        if(length(allWhere)>0) { # put methods into existing generic
            for(i in seq_along(allWhere)) {
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
                    gettextf("copying the generic function \"%s\" to environment \"%s\", because the previous version was in a sealed namespace (%s)",
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
      stop(gettextf("no existing definition for function \"%s\"", f),
           domain = NA)
    if(!hasMethods) {
        message(gettextf("Creating a new generic function for \"%s\" in \"%s\"",
                         f, getPackageName(where)),
                domain = NA)
        ## create using the visible non-generic as a pattern and default method
        setGeneric(f, where = where)
        fdef <- getGeneric(f, where = where)
    }
    else if(identical(gwhere, NA)) {
        ## better be a primitive since getGeneric returned a generic, but none was found
        if(is.null(elNamed(.BasicFunsList, f)))
            stop(gettextf("apparent internal error: a generic function was found for \"%s\", but no corresponding object was found searching from \"%s\"",
                          f, getPackageName(where)), domain = NA)
        if(!isGeneric(f))
          setGeneric(f) #  turn on this generic and cache it.
    }
    if(isSealedMethod(f, signature, fdef, where=where))
        stop(gettextf("the method for function \"%s\" and signature %s is sealed and cannot be re-defined", f, .signatureString(fdef, signature)), domain = NA)
    signature <- matchSignature(signature, fdef, where)
    switch(typeof(definition),
           closure = {
               fnames <- formalArgs(fdef)
               mnames <- formalArgs(definition)
               ## fix up arg name for single-argument generics
               ## useful for e.g. '!'
               if(!identical(mnames, fnames) &&
                  length(fnames) == length(mnames) && length(mnames) == 1) {
                   warning(gettextf("argument in method definition changed from (%s) to (%s)",
                                    mnames, fnames), domain = NA, call. = FALSE)
                   formals(definition) <- formals(fdef)
                   ll <- list(as.name(formalArgs(fdef))); names(ll) <- mnames
                   body(definition) <- substituteDirect(body(definition), ll)
                   mnames <- fnames
               }
               if(!identical(mnames, fnames)) {
                   ## omitted classes in method => "missing"
                   fullSig <- conformMethod(signature, mnames, fnames, f, fdef)
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
    fenv <- environment(fdef)
    ## check length against active sig. length, reset if necessary in .addToMetaTable
    nSig <- .getGenericSigLength(fdef, fenv, TRUE)
    signature <- .matchSigLength(signature, fdef, fenv, TRUE)
    margs  <- (fdef@signature)[1:length(signature)]
    definition <- asMethodDefinition(definition, signature, sealed, funcName)
    if(is(definition, "MethodDefinition"))
      definition@generic <- fdef@generic
    whereMethods <- .getOrMakeMethodsList(f, where, fdef)
    whereMethods <- insertMethod(whereMethods, signature, margs, definition)
    allMethods <- getMethodsForDispatch(fdef)
        ## cache in both direct and inherited tables
      .cacheMethodInTable(fdef, signature, definition, allMethods) #direct
      .cacheMethodInTable(fdef, signature, definition) # inherited, by default
      if(!identical(where, baseenv()))
         .addToMetaTable(fdef, signature, definition,where, nSig)
    resetGeneric(f, fdef, allMethods, gwhere, deflt) # Note: gwhere not used by resetGeneric
    ## assigns the methodslist object
    ## and deals with flags for primitives & for updating group members
    if(!identical(where, baseenv()))
        assignMethodsMetaData(f, whereMethods, fdef, where, deflt)
    f
}

removeMethod <- function(f, signature = character(), where = topenv(parent.frame())) {
    if(is.function(f)) {
      if(is(f, "genericFunction"))
         { fdef <- f; f <- f@generic}
      else if(is.primitive(f))
        { f <- .primname(f); fdef <- genericForPrimitive(f)}
      else
        stop("Function supplied as argument \"f\" must be a generic")
    }
    else
      fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function \"'%s\" found", f), domain = NA)
        return(FALSE)
    }
    if(is.null(getMethod(fdef, signature, optional=TRUE))) {
        warning(gettextf("no method found for function \"%s\" and signature %s",
                         fdef@generic,
                         paste(dQuote(signature), collapse =", ")),
                domain = NA)
        return(FALSE)
    }
    setMethod(f, signature, NULL, where = where)
    TRUE
}

## an extension to removeMethod that resets inherited methods as well
.undefineMethod <- function(f, signature = character(), where = topenv(parent.frame())) {
    fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function \"%s\" found", f), domain = NA)
        return(FALSE)
    }
    if(!is.null(getMethod(fdef, signature, optional=TRUE)))
      setMethod(f, signature, NULL, where = where)
  }

findMethod <- function(f, signature, where = topenv(parent.frame())) {
    fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function \"%s\" found", f), domain = NA)
        return(character())
    }
    fM <- .TableMetaName(fdef@generic, fdef@package)
    where <- .findAll(fM, where)
    found <- logical(length(where))
    for(i in seq_along(where)) {
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
           mlist, fdef = getGeneric(f, !optional, where = where))
{
    if(!is(fdef, "genericFunction")) # must be the optional case, else an error in getGeneric
        return(NULL)
    if(missing(mlist))
            mlist <- getMethodsForDispatch(fdef)
    if(is.environment(mlist)) {
        value <- .findMethodInTable(signature, mlist, fdef)
        if(is.null(value) && !optional)
          stop(gettextf('No method found for function "%s" and signature %s',
                        f, paste(signature, collapse = ", ")))
        return(value)
    }
    ## the rest of the code will be executed only if a methods list object is supplied
    ## as an argument.  Should be deprecated from 2.8.0
    i <- 1
    argNames <- fdef@signature
    signature <- matchSignature(signature, fdef)
    Classes <- signature # a copy just for possible error message
    while(length(signature) > 0 && is(mlist, "MethodsList")) {
        if(!identical(argNames[[i]], as.character(mlist@argument)))
            stop(gettextf("apparent inconsistency in the methods for function \"%s\"; argument \"%s\" in the signature corresponds to \"%s\" in the methods list object",
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
            stop(gettextf("no S4 method for function \"%s\" and signature %s; consider getS3method() if you wanted the S3 method",
                          .genericName(f), Classes), domain = NA)
        if(length(Classes) > 0) {
            length(argNames) <- length(Classes)
            Classes <- paste(argNames," = \"", unlist(Classes),
                             "\"", sep = "", collapse = ", ")
        }
        else
            Classes <- "\"ANY\""
        stop(gettextf("no method defined for function \"%s\" and signature %s",
                      .genericName(f), Classes), domain = NA)
    }
}

dumpMethod <-
  ## Dump the method for this generic function and signature.
  ## The resulting source file will recreate the method.
  function(f, signature=character(), file = defaultDumpName(f, signature),
           where = -1,
           def = getMethod(f, signature, where=where, optional = TRUE))
{
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
	     mlist = if(!is.null(fdef)) getMethodsForDispatch(fdef),
             fdef = getGeneric(f, !optional),
             verbose = FALSE)
{
    if(is.environment(mlist))  {# using methods tables
        fenv <- environment(fdef)
        nsig <- .getGenericSigLength(fdef, fenv, FALSE)
        if(verbose)
            cat("* mlist environment with", length(mlist),"potential methods\n")
        if(length(signature) < nsig)
            signature[(length(signature)+1):nsig] <- "ANY"
        if(missing(useInherited))
	    useInherited <- is.na(match(signature, "ANY"))# -> vector
        method <- .findMethodInTable(signature, mlist, fdef)
	if(is.null(method)) {
	    methods <-
		if(any(useInherited)) {
		    allmethods <- .getMethodsTable(fdef, fenv, FALSE, TRUE)
		    ## look in the supplied (usually standard) table, cache w. inherited
		    .findInheritedMethods(signature, fdef,
					  mtable = allmethods, table = mlist,
					  useInherited = useInherited,
					  verbose = verbose)
		    ##MM: TODO? allow 'excluded' to be passed
		}
		else list() # just look in the direct table

	    if(length(methods) > 0)
		return(methods[[1]])
	    else if(optional)
		return(NULL)
	    else stop(gettextf("No method found for signature %s",
			       paste(signature, collapse=", ")))
	}
	else
	  return(method)
    }
    else if(is.null(mlist)) {
	if(optional)
	    return(mlist)
	else
	    stop(gettextf('"%s" has no methods defined', f), domain = NA)
    }

    ## ELSE  {mlist not an environment nor NULL }

    evalArgs <- is.environment(signature)
    env <-
	if(evalArgs)
	    signature
	else if(length(names(signature)) == length(signature))
	    sigToEnv(signature, fdef)
	else if(is.character(signature)) {
	    argNames <-	 formalArgs(fdef)
	    length(argNames) <- length(signature)
	    argNames <- argNames[is.na(match(argNames, "..."))]
	    names(signature) <- argNames
	    sigToEnv(signature, fdef)
	}
	else
	    stop("signature must be a vector of classes or an environment")

    selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
    if(verbose)
	cat("* mlist non-environment ... => 'env' of length", length(env),
	    if(is.null(selection)) "; selection = NULL -- further search", "\n")
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
      if(verbose) cat("* new mlist with", length(mlist), "potential methods\n")
      if(is(mlist, "MethodsList"))
          selection <- .Call("R_selectMethod", f, env, mlist, evalArgs, PACKAGE = "methods")
      ## else: selection remains NULL
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
    fdef <- getGeneric(f, FALSE, where = where)
    if(is.null(fdef))
      FALSE
    else  {
        if(missing(where))
          method <- getMethod(f, signature, fdef = fdef, optional = TRUE)
        else
          method <- getMethod(f, signature, where = where, optional = TRUE)
        !is.null(method)
    }
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
        methods <-  getMethods(f, where = where) # TODO: change this to findMethods()
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
    names <- names(value)
    for(i in seq_along(value)) {
        sigi <- el(value, i)
        if(!is.character(sigi) || length(sigi) != 1)
            stop(gettextf("bad class specified for element %d (should be a single character string)", i), domain = NA)
    }
      value <- as.character(value)
      names(value) <- names
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
             includeDefs = FALSE, inherited = !includeDefs,
             showEmpty, printTo = stdout(), fdef = getGeneric(f, where = where))
{
    if(missing(showEmpty))
	showEmpty <- !missing(f)
    con <- printTo
    ## must resolve showEmpty in line; using an equivalent default
    ## fails because R resets the "missing()" result for f later on (grumble)
    if(is(f, "function"))
        f <- as.character(substitute(f))
    if(!is(f, "character"))
        stop(gettextf("first argument should be the name(s) of generic functions (got object of class \"%s\")", class(f)), domain = NA)
    if(length(f)==0) {
        f <- if(missing(where)) getGenerics() else getGenerics(where)
    }
    if(length(f) == 0)
	cat(file = con, "No applicable functions\n")
    else if(length(f) > 1) {
	for(ff in f) { ## recall for each
            ffdef <- getGeneric(ff, where = where)
            if(missing(where)) {
                if(isGeneric(ff))
                  Recall(ff, classes=classes,
		   includeDefs=includeDefs, inherited=inherited,
		   showEmpty=showEmpty, printTo=printTo, fdef = ffdef)
            }
            else if(isGeneric(ff, where)) {
                Recall(ff, where=where, classes=classes,
		   includeDefs=includeDefs, inherited=inherited,
		   showEmpty=showEmpty, printTo=printTo, fdef = ffdef)
            }
	}
    }
    else { ## f of length 1 --- the "workhorse" :
        out <- paste("\nFunction \"", f, "\":\n", sep="")
        isGen  <- if(missing(fdef)) {
            (if(missing(where)) isGeneric(f) else isGeneric(f, where)) }
                     else is(fdef, "genericFunction")
        if(!isGen)
            cat(file = con, out, "<not a generic function>\n")
        else
            ## maybe no output for showEmpty=FALSE
            .showMethodsTable(fdef, includeDefs, inherited,
				  classes = classes, showEmpty = showEmpty,
				  printTo = con)
    }
    invisible(printTo)
}

## this should be made obsolete:  asserted that it is no longer called in this pacakage
removeMethodsObject <-
    function(f, where = topenv(parent.frame()))
{
    fdef <- getGeneric(f, where=where)
    if(!is(fdef, "genericFunction")) {
      warning(gettextf(
    "No generic function found for \"%s\"; no action taken in removeMethodsObject", f))
      return(FALSE)
  }
  what <- methodsPackageMetaName("M", f, fdef@package)
  if(!exists(what, where, inherits = FALSE))
      return(FALSE)
  where <- as.environment(where)
  if(environmentIsLocked(where)) {
      warning(gettextf("the environment/package \"%s\" is locked; cannot remove methods data for \"%s\"",
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
  function(f, where = topenv(parent.frame()), all = missing(where))
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
        warning(gettextf("\"%s\" is not a generic function in \"%s\"; methods not removed",
                f, getPackageName(where)), domain = NA)
        return(FALSE)
    }

    methods <- getMethodsForDispatch(fdef) 
    default <- getMethod(fdef, "ANY", optional = TRUE)
    fMetaName <- .TableMetaName(fdef@generic, fdef@package)
    oldMetaName <- methodsPackageMetaName("M",fdef@generic, fdef@package)
    allWhere <- .findAll(fMetaName, where)
    if(!all)
        allWhere <- allWhere[1]
    value <- rep(TRUE, length(allWhere))
    ## cacheGenericsMetaData is called to clear primitive methods if there
    ## are none for this generic on other databases.
    cacheGenericsMetaData(f, fdef, FALSE, where)
    .uncacheGeneric(f, fdef) # in case it gets removed or re-assigned
    doGeneric <- TRUE # modify the function
    for(i in seq_along(allWhere)) {
        db <- as.environment(allWhere[[i]])
        if(environmentIsLocked(db)) {
                warning(gettextf("cannot remove methods for  \"%s\" in locked environment/package \"%s\"",
                        f, getPackageName(db)), domain = NA)
                value[[i]] <- FALSE
                next
            }
            if(exists(fMetaName, db, inherits = FALSE)) {
                ## delete these methods from the generic
                theseMethods <- get(fMetaName, db)
                .mergeMethodsTable(fdef, methods, theseMethods, FALSE)
                rm(list = fMetaName, pos = db)
                if(exists(oldMetaName, db, inherits = FALSE))
                  rm(list = oldMetaName, pos = db)
            }
    }
    all <- all && base::all(value) # leave methods on any locked packages
    # now find and reset the generic function
    for(i in seq_along(allWhere)) {
        db <- as.environment(allWhere[[i]])    
        if(doGeneric && isGeneric(f, db)) { 
            ## restore the original function if one was used as default
            if(all && is(default, "derivedDefaultMethod")) {
                default <- as(default, "function") # strict, removes slots
                rm(list=f, pos = db)
                if(!existsFunction(f, FALSE, db)) {
                    message(gettextf("restoring default function definition of \"%s\"",
                                     f), domain = NA)
                    assign(f, default, db)
                }
                ## else the generic is removed, nongeneric will be found elsewhere
            }
            ## else, leave the generic in place, with methods removed
            ## and inherited methods reset
            else {
                resetGeneric(f, fdef, where = db, deflt = default)
            }
            doGeneric <- FALSE
        }
    }
    any(value)
}


resetGeneric <- function(f, fdef = getGeneric(f, where = where), mlist = getMethodsForDispatch(fdef), where = topenv(parent.frame()), deflt = finalDefaultMethod(mlist)) {
    if(!is(fdef, "genericFunction")) {
            stop(gettextf("error in updating generic function \"%s\"; the function definition is not a generic function (class \"%s\")", f, class(fdef)),
                 domain = NA)
        }
    ## reset inherited methods
    .updateMethodsInTable(fdef, attach = "reset")
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
              list(MSG = gettextf("function \"%s\" is a group generic; do not call it directly", name)))
    if(is.character(knownMembers))
        knownMembers <- as.list(knownMembers) # ? or try to find them?
    setGeneric(name, def, group = group, valueClass = valueClass,
               package = package, useAsDefault = FALSE,
               genericFunction =
                 new("groupGenericFunction", def, groupMembers = knownMembers),
               where = where)
    .MakeImplicitGroupMembers(name, knownMembers, where)
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
    envir <- parent.frame()
    call <- sys.call(frame)

    ## localArgs == is the evaluation in a method that adds special arguments
    ## to the generic.  If so, look back for the call to generic.  Also expand  "..."
    localArgs <- FALSE
    ## the  lines below this comment do what the previous version
    ## did in the expression fdef <- sys.function(frame)
    if(exists(".Generic", envir = envir, inherits = FALSE))
	fname <- get(".Generic", envir = envir)
    else { # in a local method (special arguments), or	an error
        localArgs <- identical(as.character(call[[1]]), ".local")
	if(localArgs)
	    call <- sys.call(sys.parent(2))
	fname <- as.character(call[[1]])
    }
    fdef <- get(fname, envir = envir)

    if(is.primitive(fdef)) {
        if(nargs() == 0)
            stop("'callGeneric' with a primitive needs explicit arguments (no formal args defined)")
        else {
            fname <- as.name(fname)
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
            call[[1]] <- as.name(fname) # in case called from .local
        ## exapnd the ... if this is  a locally modified argument list.
        ## This is a somewhat ambiguous case and may not do what the
        ## user expects.  Not clear there is a single solution.  Should we warn?
            call <- match.call(fdef, call, expand.dots = localArgs)
            anames <- names(call)
            matched <- !is.na(match(anames, names(formals(fdef))))
            for(i in seq_along(anames))
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
    ## look for the generic to see if it is a primitive
    fGen <- getFunction(f, TRUE, FALSE, where = where)
    if(!is.primitive(fGen)) {
        mdef <- getMethod(f, signature, optional = TRUE, where = where, fdef = fGen)
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
    if(!is(fdef, "genericFunction"))
      return(TRUE)
    name <- fdef@generic
    package <- fdef@package
    objs <- c(name, .TableMetaName(name, package))
    for(obj in objs) {
        hasIt <- exists(obj, env, inherits = FALSE)
        ## the method object may be bound, or a new one may be needed
        ## in which case the env. better not be locked
        if((!hasIt || bindingIsLocked(obj, env)))
            return(TRUE)
    }
    FALSE
}

implicitGeneric <- function(...) NULL

## real version, installed after methods package initialized

.implicitGeneric <- function(name, where = topenv(parent.frame()),
                             generic = getGeneric(name, where = where))
### Add the named function to the table of implicit generics in environment where.
###
### If there is a generic function of this name, it is saved to the
### table.  This is the reccomended approach and is required if you
### want the saved generic to include any non-default methods.
###
### Otherwise, a generic is temporarily created by a call to
### setGeneric(), and then saved.
###
### Unless restore is FALSE,  the generic will be removed by a call
### to removeMethods(), which will leave the nongeneric (== the
### default method) as the function.  It's currently an error if there
### is no such default, because the nongeneric was supposed to be in
### this same package/environment.  The case keepGeneric=TRUE is used
### by the methods package to establish implicit generics for
### functions in the base package, without owning a version of these.
  {
      createGeneric <- (missing(generic) || !is(generic, "genericFunction")) && !isGeneric(name, where)
      if(createGeneric) {
          fdefault <- getFunction(name, where = where, mustFind = FALSE)
          if(is.null(fdefault))
            return(NULL)  # no implicit generic
          fdefault <- .derivedDefaultMethod(fdefault)
          if(is.primitive(fdefault))
            package <- "base"
          else
            package <- getPackageName(environment(fdefault))
          ## look for a group
          if(identical(package,"base"))
            group <- .getImplicitGroup(name, .methodsNamespace)
          else
            group <- .getImplicitGroup(name, environment(fdefault))
          if(missing(generic)) {
            generic <- .getImplicitGeneric(name, where, package)
            if(is.null(generic)) {
                generic <- makeGeneric(name, fdefault = fdefault, package = package,
                                     group = group)
            }
            else #return value, don't save it again
              return(generic)
          }
          else
            generic <- makeGeneric(name, generic, fdefault, package = package,
                                   group = group)

      }
      .saveToImplicitGenerics(name, generic, where)
      generic
  }

setGenericImplicit <- function(name, where = topenv(parent.frame()), restore = TRUE) {
    if(!isGeneric(name, where)) {
        warning(gettextf("\"%s\" is not currently a generic:  define it first to create a non-default implicit form",
                         name), domain = NA)
        return(FALSE)
    }
    generic <- getGeneric(name, where)
    if(restore)
        removeMethods(name, where)
    else
        removeGeneric(name, where)
    .saveToImplicitGenerics(name, generic, where)
}

prohibitGeneric <- function(name, where = topenv(parent.frame()))
### store a definition in the implicit generic table that explicitly prohibits
### a function from being made generic
  {
      .saveToImplicitGenerics(name, FALSE, where)
  }

registerImplicitGenerics <- function(what = .ImplicitGenericsTable(where), where = topenv(parent.frame()))
  {
      if(!is.environment(what))
        stop(gettextf("Must provide an environment table; got class \"%s\"",
             class(what)), domain = NA)
      objs <- objects(what, all=TRUE)
      for(f in objs)
        .cacheImplicitGeneric(f, get(f, envir = what))
  }


### the metadata name for the implicit generic table
.ImplicitGenericsMetaName <- ".__IG__table" # methodsPackageMetaName("IG", "table")

.ImplicitGenericsTable <- function(where)
  {
### internal utility to add a function to the implicit generic table
      if(!exists(.ImplicitGenericsMetaName, where, inherits = FALSE))
        assign(.ImplicitGenericsMetaName, new.env(TRUE), where)
      get(.ImplicitGenericsMetaName, where)
  }

.saveToImplicitGenerics <- function(name, def, where)
  .cacheGenericTable(name, def, .ImplicitGenericsTable(where))

.getImplicitGeneric <- function(name, where, pkg = "")
  {
      value <- .getImplicitGenericFromCache(name, where, pkg)
      if(is.null(value) && exists(.ImplicitGenericsMetaName, where, inherits = FALSE)) {
          tbl <-  get(.ImplicitGenericsMetaName, where)
          value <- .getGenericFromCacheTable(name, where, pkg, tbl)
      }
      value
  }

.restoreImplicitGeneric <- function(name, fdef, where, OK)
  {
      ## look for an implicit function table in the function's namespace or package
      fwhere <- environment(fdef)
      if(!isNamespace(fwhere)) {
          fwhere <- findFunction(name, where = where)
          if(length(fwhere) > 0)
            fwhere <- fwhere[[1]]
          else
            return(FALSE)
      }
      genericFunction <- .getImplicitGeneric(name, fwhere)
      if(is.null(genericFunction)) {
          if(OK)  #try to create the default implicit generic
            genericFunction <- implicitGeneric(name, where)
          if(is.null(genericFunction)) # usually, a new definition
            return(FALSE)
      }
      if(identical(genericFunction, FALSE))
        stop(gettextf("Function \"%s\" may not be used as a generic (implicitly prohibited)", name), domain = NA)
      if(!OK)
        warning(gettextf("Implicit generic exists for \"%s\", but will be overriden by explicit call to setGeneric()", name), domain = NA)
      else {
          ## compare this to the end of setGeneric().  It does not check group membership
          ## or assign a methods list object.  Keep an eye on the differences.
          assign(name, genericFunction, where)
          .cacheGeneric(name, genericFunction)
          .assignMethodsTableMetaData(name, genericFunction, where)
      }
      OK
  }

.identicalGeneric <- function(f1, f2) {
    ## environments will be different
    f2d <- f2@.Data; f1d <- f1@.Data
    if(!(is.function(f2d) && is.function(f1d)))
      return(FALSE)
    environment(f2d) <- environment(f1d)
    (identical(class(f1), class(f2)) &&
     identical(f1d, f2d) &&
     identical(f1@signature, f2@signature) &&
     identical(f1@group, f2@group) &&
     identical(f1@valueClass, f2@valueClass) &&
     identical(f1@generic, f2@generic)
     )
}

.ImplicitGroupMetaName <- ".__IGM__table"
.MakeImplicitGroupMembers <- function(group, members, where) {
    if(!exists(.ImplicitGroupMetaName, where, inherits = FALSE))
      assign(.ImplicitGroupMetaName, new.env(TRUE), where)
    tbl <- get(.ImplicitGroupMetaName, where)
    for(what in members)
      assign(what, as.list(group), envir = tbl)
}

.getImplicitGroup <- function(name, where) {
    if(exists(.ImplicitGroupMetaName, where, inherits = FALSE)) {
        tbl <- get(.ImplicitGroupMetaName, where)
        if(exists(name, envir = tbl, inherits = FALSE))
            return(get(name, envir = tbl))
    }
    list()
}

findMethods <- function(f, where, classes = character(), inherited = FALSE) {
    if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else if(is.character(f) && length(f) == 1) {
        fdef <- getGeneric(f, where)
    }
    else if(!is(f, "function"))
        stop(gettextf("argument \"f\" must be a generic function or a single character string; got an object of class \"%s\"", class(f)), domain = NA)
    else {
        fdef <- f
        f <- deparse(substitute(f))
    }
    if(!is(fdef, "genericFunction")) {
        warning("non-generic function given to findMethods()")
        return(list())
    }
    if(missing(where))
      table <- get(if(inherited) ".AllMTable" else ".MTable", envir = environment(fdef))
    else {
        if(!identical(inherited, FALSE))
          stop("Only FALSE meaningful for inherited, when where is supplied (got ", inherited, "\"")
        where <- as.environment(where)
        what <- .TableMetaName(f, fdef@package)
        if(exists(what, envir = where, inherits = FALSE))
          table <- get(what, envir = where)
        else
          return(list())
    }
    objNames <- objects(table, all.names = TRUE)
    if(length(classes)>0) {
        classesPattern <- paste("#", classes, "#", sep="", collapse = "|")
        which <- grep(classesPattern, paste("#",objNames,"#", sep=""))
        objNames <- objNames[which]
    }
    if(length(objNames) > 0) {
        value <- lapply(objNames, function(x)get(x, envir = table))
        names(value) <- objNames
        value
    }
    else
      list()
}

findMethodSignatures <- function(..., target = TRUE, methods = findMethods(...)) {
    ## unless target is FALSE, the signatures are just the names from the table
    ## unscrambled.
    if(length(methods) < 1)
      return(methods)
    ## find the argument names
    prims <- sapply(methods, is.primitive)
    if(!all(prims)) {
        what <- lapply(methods[!prims], function(x)  names(x@target))
        lens <- sapply(what, length)
        if(length(unique(lens)) == 1) # asserted to be true for legit. method tables
          what <- what[[1]]
        else
          what <- what[lens == max(lens)][[1]]
    }
    else
      what <- names(formals(args(methods[[1]]))) # uses the hack that args() returns a function if its argument is a primitve!
    if(target) 
      sigs <- strsplit(names(methods), "#", fixed = TRUE)
    else {
        anySig <- rep("ANY", length(what))
        sigs <- lapply(methods, function(x) if(is.primitive(x)) anySig else as.character(x@defined))
    }
    lens <- unique(sapply(sigs, length))
    if(length(lens) > 1)
      sigs
    else
      t(matrix(unlist(sigs), nrow = lens, dimnames = list(what, NULL)))
}

hasMethods <- function(f, where,package) {
    fdef <- NULL
    nowhere <- missing(where) # because R resets this if where is assigned
    if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else if(!(is.character(f) && length(f) == 1))
        stop(gettextf("argument \"f\" must be a generic function or a single character string; got an object of class \"%s\"", class(f)), domain = NA)
    if(missing(package)) {
        package <- packageSlot(f)
        if(is.null(package)) {
            if(missing(where))
              where <- .GlobalEnv
            fdef <- getFunction(f, where = where)
            if(is(fdef, "genericFunction"))
              package <- fdef@package
            else if(is.primitive(fdef))
              package <- "base"
            else if( length(ff <- findFunction(f, where = where)) == 1)
                package <- getPackageName(ff[[1]])
            else
              stop(gettextf("f does not correspond to  a generic function and package not specified"), domain = NA)
        }
    }  
    what <- .TableMetaName(f, package)
    testEv <- function(ev)
      exists(what, envir = ev, inherits = FALSE) && (length(objects(get(what, envir = ev), all.names = TRUE)) > 0)
    if(nowhere) {
        for(i in seq(along = search())) {
            if(testEv(as.environment(i)))
              return(TRUE)
        }
        return(FALSE)
    }
    else
      testEv(as.environment(where))
}
