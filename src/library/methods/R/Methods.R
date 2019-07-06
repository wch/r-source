#  File src/library/methods/R/Methods.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

## copy here to avoid importing from stats and hence loading stats
## namespace when methods if loaded
setNames <- stats::setNames


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
             useAsDefault = NULL, genericFunction = NULL,
             simpleInheritanceOnly = NULL)
{
    if(is.character(.isSingleName(name)))
        stop(gettextf("invalid argument 'name': %s",
                      .isSingleName(name)), domain = NA)
    if(exists(name, "package:base") && inBasicFuns(name)) {

        name <- switch(name, "as.double" = "as.numeric", name)
        fdef <- getGeneric(name) # will fail if this can't have methods
        compatibleSignature <- nargs() == 2L && !missing(signature) &&
            identical(signature, fdef@signature)
        if(nargs() <= 1 || compatibleSignature) {
            ## generics for primitives are global, so can & must always be cached
            .cacheGeneric(name, fdef)
            return(name)
        }
        ## you can only conflict with a primitive if you supply
        ## useAsDefault to signal you really mean a different function
        if(!is.function(useAsDefault) && !isFALSE(useAsDefault)) {
            msg <- gettextf("%s dispatches internally;  methods can be defined, but the generic function is implicit, and cannot be changed.", sQuote(name))
            stop(msg, domain = NA)
        }
    }
    simpleCall <- { nargs() < 2 ||
		    all(missing(def), missing(group), missing(valueClass),
			missing(package), missing(signature), missing(useAsDefault),
			missing(genericFunction), missing(simpleInheritanceOnly)) }
    stdGenericBody <- substitute(standardGeneric(NAME), list(NAME = name))
    ## get the current function which may already be a generic
    fdef <-
	if(is.null(package))
	    getFunction(name, mustFind = FALSE, where = where)
	else {
	    ev <- .NamespaceOrPackage(package)
	    if(simpleCall)
		implicitGeneric(name, ev) # generic or NULL
	    else
		getFunction(name, mustFind = FALSE, where = ev)
	}
    if(simpleCall) {
        if(is(fdef, "genericFunction"))
          return(.GenericAssign(name, fdef, where))
    }
    if(is.null(fdef)) {
        if(isNamespace(where))
            fdef <- .getFromStandardPackages(name)
        else
            fdef <- getFunction(name, mustFind = FALSE)
    }
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
            stop(gettextf("if the 'def' argument is supplied, it must be a function that calls standardGeneric(\"%s\") or is the default",
                          name), domain = NA)
        nonstandardCase <- .NonstandardGenericTest(body(def), name, stdGenericBody)
        if(is.na(nonstandardCase)) {
            if(is.null(useAsDefault)) {# take this as the default
                useAsDefault <- def
            }
            body(def, envir = as.environment(where)) <- stdGenericBody
            nonstandardCase <- FALSE
        }
        fdef <- def
        if(is.null(genericFunction) && nonstandardCase)
            genericFunction <- new("nonstandardGenericFunction") # force this class for fdef
    }
    thisPackage <- getPackageName(where)
    if(is.null(package) || !nzchar(package))
        ## either no previous def'n or failed to find its package name
        package <- thisPackage
    if(is.null(fdef))
        stop(gettextf("must supply a function skeleton for %s, explicitly or via an existing function", sQuote(name)), domain = NA)
    ensureGeneric.fdef <- function(sig = signature) {
        if(!(is.object(fdef) && is(fdef, "genericFunction"))) {
            fdeflt <-
                if(is.function(useAsDefault)) useAsDefault
                else if(isFALSE(useAsDefault)) NULL
                else if(is.function(prevDefault) &&
                        !identical(formalArgs(prevDefault), formalArgs(fdef)) &&
                        !is.primitive(prevDefault))
                    NULL
                else prevDefault
            if(is.function(fdeflt))
                fdeflt <- .derivedDefaultMethod(fdeflt)
            fdef <<-
                makeGeneric(name, fdef, fdeflt, group=group, valueClass=valueClass,
                            package = package, signature = sig,
                            genericFunction = genericFunction,
                            simpleInheritanceOnly = simpleInheritanceOnly)
        }
    }
    if(identical(package, thisPackage)) {
        ensureGeneric.fdef()
    } else {
        ## setting a generic for a function in another package.
        ## In this case, the generic definition must agree with the implicit
        ## generic for the given function and package
        implicit <- implicitGeneric(name, .NamespaceOrPackage(package))
        if(is.null(implicit)) { # New function, go ahead
            ensureGeneric.fdef()
        }
        else {
	    ## possibly take the signature from the *implicit* generic:
	    ensureGeneric.fdef(if(is.null(signature) && is.null(def))
			       implicit@signature else signature)
	    cmp <- .identicalGeneric(fdef, implicit,
				     allow.extra.dots =
				     !nzchar(Sys.getenv("R_SETGENERIC_PICKY_DOTS")))
            if(isTRUE(cmp)) {
                fdef <- implicit
            }  # go ahead silently
            else if(is.function(implicit)) {
                thisPName <- if(identical(thisPackage, ".GlobalEnv"))
                    "the global environment" else paste("package", sQuote(thisPackage))
                ## choose the implicit unless an explicit def was given
                if(is.null(def) && is.null(signature)) {
                    message(gettextf(
                       "Creating a generic function for %s from %s in %s\n    (from the saved implicit definition)",
                                     sQuote(name), sQuote(package),
                                     thisPName), domain = NA)
                    fdef <- implicit
                }
                else {
                    message(gettextf(
                         "Creating a new generic function for %s in %s",
                                     sQuote(name), thisPName),
                         domain = NA)
                    fdef@package <- packageSlot(fdef@generic) <- packageSlot(environment(fdef)$.Generic) <- thisPackage
                }
            }
            else { # generic prohibited
                warning(gettextf(
			"no generic version of %s on package %s is allowed;\n   a new generic will be assigned for %s",
                                 sQuote(name), sQuote(package),
                                 thisPName),
                        domain = NA)
                fdef@package <- packageSlot(fdef@generic) <- packageSlot(environment(fdef)$.Generic) <- thisPackage
            }
        }
    }
    if(identical(fdef@signature, "..."))
	fdef <- .dotsGeneric(fdef)
    if(doUncache)
	.uncacheGeneric(name, oldDef)
    groups <- fdef@group
    for(group in groups) { # add as member of group generic(s) if not there
        gdef <- getGeneric(group)
        if(is(gdef, "groupGenericFunction") &&
           is.na(match(fdef@generic, as.character(gdef@groupMembers)))) {
            gwhere <- .genEnv(group, where)
            gdef@groupMembers <- c(gdef@groupMembers, list(fdef@generic))
            assign(group, gdef, gwhere)
        }
    }
    .GenericAssign(name, fdef, where)
}

.GenericAssign <- function(name, fdef, where) {
    assign(name, fdef, where)
    .cacheGeneric(name, fdef)
    methods <- fdef@default # empty or containing the default
    assignMethodsMetaData(name, methods, fdef, where)
    .assignMethodsTableMetaData(name, fdef, where)
    name
}

## Mimic the search for a function in the standard search() list for packages
## with namespace, to be consistent with the evaluator's search for objects
### Deprecate? Seems like we should search the imports, not the search path
.standardPackageNamespaces <- new.env()
.standardPackages <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods")
.getFromStandardPackages <- function(name) {
    namespaces <- as.list(.standardPackageNamespaces, all.names=TRUE)
    if(length(namespaces) == 0L) { # initialize the table of namespaces
        namespaces <- lapply(.standardPackages, function(pkg) {
            tryCatch(loadNamespace(pkg),
                     error = function(e) new.env())
        })
        names(namespaces) <- .standardPackages
        list2env(namespaces, .standardPackageNamespaces)
    } else {
        for(ns in namespaces) {
            obj <- ns[[name]]
            if(is.function(obj))
              return(obj)
        }
    }
    return(NULL)
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
    if(is.null(fdef) && missing(where)) {
         fdef <- .getGenericFromCache(f, where)
        ## a successful search will usually end here w/o other tests
         if(!is.null(fdef))
           return(if(getName) fdef@generic else TRUE)
     }
    if(is.null(fdef))
        fdef <- getFunction(f, where=where, mustFind = FALSE)
    if(is.null(fdef))
      return(FALSE)
    ## check primitives. These are never found as explicit generic functions.
    if(isBaseFun(fdef)) {
        if(is.character(f) && f %in% "as.double") f <- "as.numeric"
        ## the definition of isGeneric() for a base function is that methods are defined
        ## (other than the default primitive)
        gen <- genericForBasic(f, mustFind = FALSE)
        return(is.function(gen) && length(names(.getMethodsTable(gen))) > 1L)
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
        warning(gettextf("function %s appears to be a generic function, but with generic name %s",
                         sQuote(f), sQuote(gen)),
                domain = NA)
        FALSE
    }
}

removeGeneric <-
  ## Remove the generic function of this name, specifically the first version
  ## encountered from environment where
  ##
    function(f, where = topenv(parent.frame()))
{
    fdef <- NULL
    allEv <- findFunction(f, where = where)
    for(maybeEv in allEv) {
        fdef <- get(f, maybeEv)
        if(is(fdef, "genericFunction"))
            break
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
        warning(gettextf("generic function %s not found for removal",
                         sQuote(f)),
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
  ## or is converted from the internal table if where is missing
  ## or Mlists are dummies.

    function(f, where = topenv(parent.frame()), table = FALSE)
{
    if(!table)
      .MlistDefunct("getMethods", "findMethods")
    nowhere <- missing(where)
    fdef <- getGeneric(f, where = where)
    f <- fdef@generic
    if(!is.null(fdef)) {
        if(table)
          return(getMethodsForDispatch(fdef, TRUE))
    } ## else NULL
}

getMethodsForDispatch <- function(fdef, inherited = FALSE)
{
    .getMethodsTable(fdef, environment(fdef), inherited = inherited)
}

## Some functions used in MethodsListSelect, that must be safe against recursive
## method selection.

.setIfBase <- function(f, fdef, mlist) {
    if(is.null(f))
        FALSE
    else {
        found <- base::exists(f, "package:base")
	if(found) {
	    ## force (default) computation of mlist in MethodsListSelect
	    base::assign(".Methods", envir = base::environment(fdef),
			 base::get(f, "package:base"))
	}
        found
    }
}

## Must NOT use the standard version to prevent recursion  {still true ?}
.getMethodsForDispatch <- function(fdef) {
    ev <- base::environment(fdef)
    if(base::exists(".Methods", envir = ev))
        base::get(".Methods", envir = ev)
    ## else NULL
}

.setMethodsForDispatch <- function(f, fdef, mlist) {
    ev <- environment(fdef)
    if(!is(fdef, "genericFunction") ||
       !exists(".Methods", envir = ev, inherits = FALSE))
        stop(sprintf("internal error: did not get a valid generic function object for function %s",
                      sQuote(f)),
             domain = NA)
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
    function(f, signature = character(), definition,
	     where = topenv(parent.frame()), valueClass = NULL,
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
    else if(is.function(f)) {
        if(is.primitive(f)) {
            f <- .primname(f)
            fdef <- genericForBasic(f)
            gwhere <- .genEnv(f)
        }
        else
            stop("a function for argument 'f' must be a generic function")
    }
    ## slight subtlety:  calling getGeneric vs calling isGeneric
    ## For primitive functions, getGeneric returns the (hidden) generic function,
    ## even if no methods have been defined.  An explicit generic MUST NOT be
    ## for these functions, dispatch is done inside the evaluator.
    else {
        where <- as.environment(where)
        gwhere <- .genEnv(f, where)
        f <- switch(f, "as.double" = "as.numeric", f)
        fdef <- getGeneric(f, where = if(identical(gwhere, baseenv())) where else gwhere)
    }
    if(.lockedForMethods(fdef, where))
        stop(gettextf("the environment %s is locked; cannot assign methods for function %s",
                      sQuote(getPackageName(where)),
                      sQuote(f)),
             domain = NA)
    hasMethods <- !is.null(fdef)
    deflt <- getFunction(f, generic = FALSE, mustFind = FALSE, where = where)
    ## where to insert the methods in generic
    if(identical(gwhere, baseenv())) {
        allWhere <- findFunction(f, where = where)
        generics <- logical(length(allWhere))
        if(length(allWhere)) { # put methods into existing generic
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
            gwhere <- as.environment(allWhere[generics][[1L]])
            if(.lockedForMethods(fdef, gwhere)) {
                if(identical(as.environment(where), gwhere))
                    stop(gettextf("the 'where' environment (%s) is a locked namespace; cannot assign methods there",
                                  getPackageName(where)), domain = NA)
                msg <-
                    gettextf("Copying the generic function %s to environment %s, because the previous version was in a sealed namespace (%s)",
                             sQuote(f),
                             sQuote(getPackageName(where)),
                             sQuote(getPackageName(gwhere)))
                message(strwrap(msg), domain = NA)
                assign(f, fdef, where)
                gwhere <- where
            }
        }
    }
    if(!hasMethods)
        fdef <- deflt
    if(is.null(fdef))
        stop(gettextf("no existing definition for function %s",
                      sQuote(f)),
             domain = NA)
    if(!hasMethods) {
        ## create using the visible non-generic as a pattern and default method
        setGeneric(f, where = where)
        doMessage <- !isS3Generic(fdef)
        fdef <- getGeneric(f, where = where)
        if(doMessage) {
            thisPackage <- getPackageName(where)
            thisPName <- if(identical(thisPackage, ".GlobalEnv"))
                             "the global environment" else paste("package", sQuote(thisPackage))
            if(identical(as.character(fdef@package), thisPackage))
                message(gettextf("Creating a generic function from function %s in %s",
                                 sQuote(f), thisPName), domain = NA)
            else
                message(gettextf("Creating a generic function for %s from package %s in %s",
                                 sQuote(f), sQuote(fdef@package), thisPName),
                        domain = NA)
        }
    }
    else if(identical(gwhere, NA)) {
        ## better be a primitive since getGeneric returned a generic, but none was found
	if(is.null(.BasicFunsList[[f]]))
            stop(sprintf("apparent internal error: a generic function was found for \"%s\", but no corresponding object was found searching from \"%s\"",
                          f, getPackageName(where)), domain = NA)
        if(!isGeneric(f))
            setGeneric(f) # turn on this generic and cache it.
    }
    if(isSealedMethod(f, signature, fdef, where=where))
        stop(gettextf("the method for function %s and signature %s is sealed and cannot be re-defined",
                      sQuote(f),
                      .signatureString(fdef, signature)),
             domain = NA)
    signature <- matchSignature(signature, fdef, where)
    createMethod <- FALSE # TRUE for "closure" only
    switch(typeof(definition),
	   "closure" = {
	       fnames <- formalArgs(fdef)
	       mnames <- formalArgs(definition)
	       if(!identical(mnames, fnames)) {
		   ## fix up arg name for single-argument generics
		   ## useful for e.g. '!'
		   if(length(fnames) == length(mnames) && length(mnames) == 1L) {
		       warning(gettextf("For function %s, signature %s: argument in method definition changed from (%s) to (%s)",
					sQuote(f),
                                        sQuote(signature),
                                        mnames,
                                        fnames),
                               domain = NA, call. = FALSE)
		       formals(definition) <- formals(fdef)
		       ll <- list(as.name(formalArgs(fdef))); names(ll) <- mnames
		       body(definition) <- substituteDirect(body(definition), ll)
		       mnames <- fnames
		   }
		   else {
		       ## omitted arguments (classes) in method => "missing";  NB: 'definition' unused:
		       fullSig <- conformMethod(signature, mnames, fnames, f, fdef, definition)
		       if(!identical(fullSig, signature)) {
			   formals(definition, envir = environment(definition)) <- formals(fdef)
			   signature <- fullSig
		       }
		       ## extra arguments (classes) in method => use "..." to rematch
		       definition <- rematchDefinition(definition, fdef, mnames, fnames, signature)
		   }
	       }
	       definition <- matchDefaults(definition, fdef) # use generic's defaults if none in method
               createMethod <- TRUE
	   },
	   "builtin" = , "special" = {
	       ## the only primitive methods allowed are those equivalent
	       ## to the default, for generics that were primitives before
	       ## and will be dispatched by C code.
	       if(!identical(definition, deflt))
		   stop("primitive functions cannot be methods; they must be enclosed in a regular function")
	   },
	   "NULL" = {

	   },
           stop(gettextf("invalid method definition: expected a function, got an object of class %s",
			 dQuote(class(definition))), domain = NA)
	   )
    fenv <- environment(fdef)
    ## check length against active sig. length, reset if necessary in .addToMetaTable
    nSig <- .getGenericSigLength(fdef, fenv, TRUE)
    signature <- .matchSigLength(signature, fdef, fenv, TRUE)
    margs <- (fdef@signature)[seq_along(signature)]
    if(createMethod) {
        definition <- asMethodDefinition(definition, signature, sealed, fdef)
        definition@generic <- fdef@generic
    }
    is.not.base <- !identical(where, baseenv())
    whereMethods <-
	## do.mlist <- is.not.base && (!.noMlists() || all(signature == "ANY"))
	if(is.not.base && !.noMlists()) # do.mlist
	    insertMethod(getMethodsMetaData(f, where),
			 signature, margs, definition) ## else NULL
    mtable <- getMethodsForDispatch(fdef)
    if(cacheOnAssign(where)) { # will be FALSE for sourceEnvironment's
        ## cache in both direct and inherited tables
        .cacheMethodInTable(fdef, signature, definition, mtable) #direct
        .cacheMethodInTable(fdef, signature, definition) # inherited, by default
        if(is.not.base)
            .addToMetaTable(fdef, signature, definition, where, nSig)
        resetGeneric(f, fdef, mtable, gwhere, deflt) # Note: gwhere not used by resetGeneric
    }
    ## assigns the methodslist object
    ## and deals with flags for primitives & for updating group members
    assignMethodsMetaData(f, whereMethods, fdef, where)
    invisible(f)
}

removeMethod <- function(f, signature = character(), where = topenv(parent.frame())) {
    if(is.function(f)) {
      if(is(f, "genericFunction"))
         { fdef <- f; f <- f@generic}
      else if(is.primitive(f))
        { f <- .primname(f); fdef <- genericForBasic(f, mustFind=FALSE)}
      else
        stop("function supplied as argument 'f' must be a generic")
    }
    else
      fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function %s found", sQuote(f)),
                domain = NA)
        return(FALSE)
    }
    if(is.null(getMethod(fdef, signature, optional=TRUE))) {
        warning(gettextf("no method found for function %s and signature %s",
                         sQuote(fdef@generic),
                         paste(.dQ(signature), collapse =", ")),
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
        warning(gettextf("no generic function %s found",
                         sQuote(f)),
                domain = NA)
        return(FALSE)
    }
    if(!is.null(getMethod(fdef, signature, optional=TRUE)))
      setMethod(f, signature, NULL, where = where)
  }

findMethod <- function(f, signature, where = topenv(parent.frame())) {
    if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else
	fdef <- getGeneric(f, where = where)
    if(is.null(fdef)) {
        warning(gettextf("no generic function %s found",
                         sQuote(f)),
                domain = NA)
        return(character())
    }
    fM <- .TableMetaName(fdef@generic, fdef@package)
    where <- .findAll(fM, where)
    found <- logical(length(where))
    for(i in seq_along(where)) {
        wherei <- where[[i]]
        table <- get(fM, wherei, inherits=FALSE)
        ## because we are using the table from the package, we must
        ## search for both the unexpanded & expanded signature, which
        ## .findMethodInTable does not do.
        mi <- .findMethodForFdef(signature, table, fdef)
        found[i] <- !is.null(mi)
    }
    value <- where[found]
    ## to conform to the API, try to return a numeric or character vector
    ## if possible
    what <- vapply(value, class, "", USE.NAMES=FALSE)
    if(identical(what, "numeric") || identical(what, "character"))
        unlist(value)
    else
        value
}

getMethod <-
  ## Return the function that is defined as the method for this generic function and signature
  ## (classes to be matched to the arguments of the generic function).
  function(f, signature = character(), where = topenv(parent.frame()), optional = FALSE,
           mlist, fdef )
{
    if(!missing(where)) {
        env <- .NamespaceOrEnvironment(where)
        if(is.null(env))
          stop(gettextf("no environment or package corresponding to argument where=%s",
               deparse(where)), domain = NA)
        where <- env
    }
    if(missing(fdef)) {
        if(missing(where))
          fdef <- getGeneric(f, FALSE)
        else {
            fdef <- getGeneric(f, FALSE, where = where)
            if(is.null(fdef))
              fdef <- getGeneric(f, FALSE)
        }
    }
    if(!is(fdef, "genericFunction")) {
	if(optional)
	    return(NULL)
	## else
	if(!is.character(f)) f <- deparse(substitute(f))
	stop(gettextf("no generic function found for '%s'", f), domain = NA)
    }
    if(missing(mlist))
	mlist <-
	    if(missing(where))
		getMethodsForDispatch(fdef)
	    else
		.getMethodsTableMetaData(fdef, where, optional)
    if(is.environment(mlist)) {
	signature <- matchSignature(signature, fdef)
	value <- .findMethodInTable(signature, mlist, fdef)
	if(is.null(value) && !optional) {
	    if(!is.character(f)) f <- deparse(substitute(f))
	    stop(gettextf("no method found for function '%s' and signature %s",
			  f, paste(signature, collapse = ", ")))
	}
        return(value)
    }
    else if(is.null(mlist)) return(mlist)

    ## the rest of the code will be executed only if a methods list object is supplied
    ## as an argument.  Should be deleted from 2.8.0 --> Error from 3.2.0
    stop("defunct methods list search", domain = NA)
}

dumpMethod <-
  ## Dump the method for this generic function and signature.
  ## The resulting source file will recreate the method.
  function(f, signature=character(), file = defaultDumpName(f, signature),
           where = topenv(parent.frame()),
           def = getMethod(f, signature, where=where, optional = TRUE))
{
    if(!is.function(def))
        def <- getMethod(f, character(), where=where, optional = TRUE)

    ## sink() handling as general as possible -- unbelievably unpretty coding:
    closeit <- TRUE ; isSTDOUT <- FALSE
    if (is.character(file)) {
        if(!(isSTDOUT <- file == "")) ## stdout() -- no sink() needed
            file <- file(file, "w")
    }
    else if (inherits(file, "connection")) {
	if (!isOpen(file)) open(file, "w") else closeit <- FALSE
    } else stop("'file' must be a character string or a connection")
    if(!isSTDOUT){ sink(file); on.exit({sink(); if(closeit) close(file)}) }

    cat("setMethod(\"", f, "\", ", deparse(signature), ",\n", sep="")
    dput(def@.Data)
    cat(")\n", sep="")
    if(!isSTDOUT) { on.exit(); sink(); if(closeit) close(file) }
    invisible(file)
}

dumpMethods <- function(f, file = "", signature = NULL, methods= findMethods(f, where = where),
                        where = topenv(parent.frame()) )
{
    ## The signature argument was used in recursive calls to dumpMethods()
    ## using the old MethodsList objects.  It is not meaningful with
    ## the current listOfMethods class
    if(length(signature) > 0)
        warning("argument 'signature' is not meaningful with the current implementation and is ignored \n(extract a subset of the methods list instead)")

    ## sink() handling as general as possible -- unbelievably unpretty coding:
    closeit <- TRUE ; isSTDOUT <- FALSE
    if (is.character(file)) {
        if(!(isSTDOUT <- file == "")) ## stdout() -- no sink() needed
            file <- file(file, "w")
    }
    else if (inherits(file, "connection")) {
	if (!isOpen(file)) open(file, "w") else closeit <- FALSE
    } else stop("'file' must be a character string or a connection")
    if(!isSTDOUT){ sink(file); on.exit({sink(); if(closeit) close(file)}) }
    sigs <- methods@signatures
    for(i in seq_along(methods))
        dumpMethod(f, sigs[[i]], file = "", def = methods[[i]])
}


selectMethod <-
    ## Returns the method (a function) that R would use to evaluate a call to
    ## generic 'f' with arguments corresponding to the specified signature.
    function(f, signature, optional = FALSE, useInherited = TRUE,
	     mlist = if(!is.null(fdef)) getMethodsForDispatch(fdef),
	     fdef = getGeneric(f, !optional), verbose = FALSE, doCache = FALSE)
{
    if(is.environment(mlist))  {# using methods tables
        fenv <- environment(fdef)
        nsig <- .getGenericSigLength(fdef, fenv, FALSE)
        if(verbose)
            cat("* mlist environment with", length(mlist),"potential methods\n")
        if(length(signature) < nsig)
            signature[(length(signature)+1):nsig] <- "ANY"
        if(identical(fdef@signature, "...")) {
            method <- .selectDotsMethod(signature, mlist,
                 if(useInherited) getMethodsForDispatch(fdef, inherited = TRUE))
            if(is.null(method) && !optional)
              stop(gettextf("no method for %s matches class %s",
                            sQuote("..."), dQuote(signature)),
                   domain = NA)
            return(method)
        }
        method <- .findMethodInTable(signature, mlist, fdef)
	if(is.null(method)) {
	    if(missing(useInherited))
		useInherited <- (is.na(match(signature, "ANY")) & # -> vector
				 if(identical(fdef, coerce))# careful !
				 c(TRUE,FALSE) else TRUE)
	    if(verbose) cat("  no direct match found to signature (",
			    paste(signature, collapse=", "),")\n", sep="")
	    methods <-
		if(any(useInherited)) {
		    allmethods <- .getMethodsTable(fdef, fenv, check=FALSE,
                                                   inherited=TRUE)
		    ## look in the supplied (usually standard) table
		    .findInheritedMethods(signature, fdef,
					  mtable = allmethods, table = mlist,
					  useInherited = useInherited,
                                          verbose = verbose, doCache = doCache)
		    ##MM: TODO? allow 'excluded' to be passed
		}
		## else list() : just look in the direct table

	    if(length(methods))
		return(methods[[1L]])
	    else if(optional)
		return(NULL)
	    else stop(gettextf("no method found for signature %s",
			       paste(signature, collapse=", ")))
	}
	else
	  return(method)
    }
    else if(is.null(mlist)) {
	if(optional)
	    return(mlist)
	else
	    stop(gettextf("%s has no methods defined",
                          sQuote(f)),
                 domain = NA)
    }
    else ## mlist not an environment nor NULL :
	stop("selectMethod(): mlist is not an environment or NULL :\n",
	     "** should no longer happen!", domain = NA)
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
        if(missing(where))
          method <- getMethod(f, signature,  optional = TRUE)
        else
          method <- getMethod(f, signature, where = where, optional = TRUE)
        !is.null(method)
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
        sigi <- value[[i]]
        if(!is.character(sigi) || length(sigi) != 1L)
            stop(gettextf(
		"bad class specified for element %d (should be a single character string)",
		i), domain = NA)

    }
    setNames(as.character(value), names)
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
    if(isFALSE(printTo))
        con <- textConnection(NULL, "w")
    else
        con <- printTo
    ## must resolve showEmpty in line; using an equivalent default
    ## fails because R resets the "missing()" result for f later on (grumble)
    if(is.function(f)) {
        fdef <- f ## note that this causes missing(fdef) to be FALSE below
        if(missing(where))
            where <- environment(f)
        f <- deparse(substitute(f))
        if(length(f) > 1L) f <- paste(f, collapse = "; ")
    }
    if(!is(f, "character"))
        stop(gettextf("first argument should be the names of one of more generic functions (got object of class %s)",
                      dQuote(class(f))), domain = NA)
    if(length(f) ==  0L) { ## usually, the default character()
        f <- if(missing(where)) getGenerics() else getGenerics(where)
    }
    if(length(f) == 0L)
	cat(file = con, "no applicable functions\n")
    else if(length(f) > 1L) {
	for(ff in f) { ## recall for each
            ffdef <- getGeneric(ff, where = where)
            if(missing(where)) {
                if(isGeneric(ff))
		    Recall(ff, classes=classes,
			   includeDefs=includeDefs, inherited=inherited,
			   showEmpty=showEmpty, printTo=con, fdef = ffdef)
            }
            else if(isGeneric(ff, where)) {
                Recall(ff, where=where, classes=classes,
                       includeDefs=includeDefs, inherited=inherited,
                       showEmpty=showEmpty, printTo=con, fdef = ffdef)
            }
	}
    }
    else { ## f of length 1 --- the "workhorse" :
        out <- paste0("\nFunction \"", f, "\":\n")
        if(!is(fdef, "genericFunction"))
            cat(file = con, out, "<not an S4 generic function>\n")
        else
            ## maybe no output for showEmpty=FALSE
            .showMethodsTable(fdef, includeDefs, inherited,
                              classes = classes, showEmpty = showEmpty,
                              printTo = con)
    }
    if(isFALSE(printTo)) {
        txtOut <- textConnectionValue(con)
        close(con)
        txtOut
    }
    else
        invisible(printTo)
}

.methods_info <-
    ## (not exported) simplify construction of standard data.frame
    ## return value from .S4methodsFor*
    function(generic=character(), signature=character(),
             visible=rep(TRUE, length(signature)), from=character())
{
    if (length(signature))
        signature <- paste0(generic, ",", signature, "-method")
    keep <- !duplicated(signature)
    data.frame(visible=visible[keep], from=from[keep],
               generic=generic[keep], isS4=rep(TRUE, sum(keep)),
               row.names=signature[keep], stringsAsFactors=FALSE)
}

.S4methodsForClass <-
    ## (not exported) discover methods for specific class;
    ## generic.function ignored
    function(generic.function, class)
{
    def <- tryCatch(getClass(class), error=function(...) NULL)
    if (is.null(def))
        return(.methods_info())

    classes <- c(class, names(getClass(class)@contains))
    generics <- as.vector(getGenerics(where=search()))
    nms <- setNames(generics, generics)

    packages <- lapply(nms, function(generic) {
	table <- environment(getGeneric(generic))[[".MTable"]]
	lapply(table, function(m) environmentName(environment(m)))
    })
    methods <- lapply(nms, function(generic) {
	table <- environment(getGeneric(generic))[[".MTable"]]
	lapply(table, function(m) {
            if (is(m, "MethodDefinition") && any(m@defined %in% classes))
                setNames(as.vector(m@defined), names(m@defined))
            ## else NULL
        })
    })

    geom <- lapply(methods, function(method) {
        !vapply(method, is.null, logical(1))
    })
    filter <- function(elt, geom) elt[geom]
    packages <- Map(filter, packages, geom)
    methods  <- Map(filter, methods,  geom)
    non0 <- lengths(methods) != 0L
    packages <- packages[non0]
    methods  <-  methods[non0]

    ## only derived methods
    geom <- lapply(methods, function(method, classes) {
        sig <- simplify2array(method)
        if (!is.matrix(sig))
            sig <- matrix(sig, ncol=length(method))
        idx <- apply(sig, 2, match, classes, 0)
        if (!is.matrix(idx))
            idx <- matrix(idx, ncol=ncol(sig))
        keep <- colSums(idx != 0) != 0
        sidx <- idx[,keep, drop=FALSE]

        ## 'nearest' method
        shift <- c(0, cumprod(pmax(1, apply(sidx, 1, max)))[-nrow(sidx)])
        score <- colSums(sidx + shift)
        sig0 <- sig <- sig[,keep, drop=FALSE]
        sig0[sidx != 0] <- "*"
        sig0 <- apply(sig0, 2, paste, collapse="#")
        split(score, sig0) <-
            lapply(split(score, sig0), function(elt) elt == min(elt))
        score == 1
    }, classes)

    packages <- Map(filter, packages, geom)
    methods  <- Map(filter, methods,  geom)

    generic <- rep(names(methods), lengths(methods))
    signature <- unlist(lapply(methods, function(method) {
        vapply(method, paste0, character(1L), collapse=",")
    }), use.names=FALSE)
    package <- unlist(packages, use.names=FALSE)

    .methods_info(generic=generic, signature=signature, from=package)
}

.S4methodsForGeneric <-
    ## (not exported) discover methods for specific generic; class
    ## ignored.
    function(generic.function, class)
{
    if (is.null(getGeneric(generic.function)))
        return(.methods_info())

    mtable <- ".MTable"
    generic <- generic.function
    table <- get(mtable, environment(getGeneric(generic)))
    packages <- sapply(names(table), function(nm, table) {
        environmentName(environment(table[[nm]]))
    }, table)

    methods <- names(table)
    signatures <- lapply(methods, function(method, classes) {
        m <- table[[method]]
        if (is(m, "MethodDefinition"))
            setNames(as.vector(m@defined), names(m@defined))
        else
            NULL
    })

    geom <- vapply(signatures, Negate(is.null), logical(1))
    packages <- packages[geom]
    methods <- methods[geom]
    signatures <- sapply(signatures[geom], function(elt) {
        paste0(as.vector(elt), collapse=",")
    })

    .methods_info(generic=rep(generic.function, length(packages)), from=packages,
                  signature=signatures)
}

.S4methods <-
    ## discover methods by generic or class, primarily for interactive
    ## display via utils::methods()
    function(generic.function, class)
{
    info <- if (!missing(generic.function))
        .S4methodsForGeneric(generic.function, class)
    else if (!missing(class))
        .S4methodsForClass(generic.function, class)
    else
        stop("must supply 'generic.function' or 'class'")
    structure(rownames(info), info=info, byclass=missing(generic.function),
              class="MethodsFunction")
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
        warning(gettextf("%s is not an S4 generic function in %s; methods not removed",
                         sQuote(f),
                         sQuote(getPackageName(where))),
                domain = NA)
        return(FALSE)
    }

    methods <- getMethodsForDispatch(fdef)
    default <- getMethod(fdef, "ANY", optional = TRUE)
    fMetaName <- .TableMetaName(fdef@generic, fdef@package)
    oldMetaName <- methodsPackageMetaName("M",fdef@generic, fdef@package)
    allWhere <- .findAll(fMetaName, where)
    if(!all)
        allWhere <- allWhere[1L]
    value <- rep(TRUE, length(allWhere))
    ## cacheGenericsMetaData is called to clear primitive methods if there
    ## are none for this generic on other databases.
    cacheGenericsMetaData(f, fdef, FALSE, where)
    .uncacheGeneric(f, fdef) # in case it gets removed or re-assigned
    doGeneric <- TRUE # modify the function
    for(i in seq_along(allWhere)) {
        db <- as.environment(allWhere[[i]])
        if(environmentIsLocked(db)) {
                warning(gettextf("cannot remove methods for %s in locked environment/package %s",
                                 sQuote(f), sQuote(getPackageName(db))),
                        domain = NA)
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
                    message(gettextf("Restoring default function definition of %s",
                                     sQuote(f)),
                            domain = NA)
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


resetGeneric <- function(f, fdef = getGeneric(f, where = where),
			 mlist = getMethodsForDispatch(fdef),
			 where = topenv(parent.frame()),
			 deflt = finalDefaultMethod(mlist))
{
    if(!is(fdef, "genericFunction")) {
            stop(gettextf("error in updating S4 generic function %s; the function definition is not an S4 generic function (class %s)", sQuote(f), dQuote(class(fdef))),
                 domain = NA)
        }
    ## reset inherited methods
    .updateMethodsInTable(fdef, attach = "reset")
    f
}

setReplaceMethod <-
  function(f, ..., where = topenv(parent.frame()))
  setMethod(paste0(f, "<-"), ..., where = where)

setGroupGeneric <-
    ## create a group generic function for this name.
    function(name, def = NULL, group = list(), valueClass = character(),
             knownMembers = list(), package = getPackageName(where), where = topenv(parent.frame()))
{
    if(is.null(def)) {
        def <- getFunction(name, where = where)
        if(isGroup(name, fdef = def)) {
            if(nargs() == 1) {
                message(gettextf("Function %s is already a group generic; no change",
                                 sQuote(name)),
                        domain = NA)
                return(name)
            }
        }
    }
    ## By definition, the body must generate an error.
    body(def, envir = environment(def)) <- substitute(
              stop(MSG, domain = NA),
              list(MSG =
                   gettextf("Function %s is a group generic; do not call it directly",
                            sQuote(name))))
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

getGenericFromCall <- function(call, methodEnv) {
    generic <- methodEnv$.Generic
    if(is.null(generic)) {
        fdef <- if (is.name(call[[1L]]))
            getGeneric(as.character(call[[1L]]), mustFind=TRUE, where=methodEnv)
        else call[[1L]]
        generic <- environment(fdef)$.Generic
    }
    generic
}

fromNextMethod <- function(call) {
  identical(call[[1L]], quote(.nextMethod))
}

callGeneric <- function(...) {
    call <- sys.call(sys.parent(1L))
    .local <- identical(call[[1L]], quote(.local))
    methodCtxInd <- 1L + if (.local) 1L else 0L
    callerCtxInd <- methodCtxInd + 1L
    methodCall <- sys.call(sys.parent(methodCtxInd))
    if (fromNextMethod(methodCall)) {
        methodCtxInd <- methodCtxInd + 1L
    }
    methodFrame <- parent.frame(methodCtxInd)
    genericName <- getGenericFromCall(methodCall, methodFrame)
    if (is.null(genericName)) {
        stop("callGeneric() must be called from within a method body")
    }
    if (nargs() == 0L) {
        callerFrame <- sys.frame(sys.parent(callerCtxInd))
        methodDef <- sys.function(sys.parent(1L))
        call <- match.call(methodDef,
                           methodCall,
                           expand.dots=FALSE,
                           envir=callerFrame)
        call[-1L] <- lapply(names(call[-1L]), as.name)
    } else {
        call <- sys.call()
    }
    call[[1L]] <- as.name(genericName)
    eval(call, parent.frame())
}

## This uses 'where' to record the methods namespace: default may not be that
initMethodDispatch <- function(where = topenv(parent.frame()))
    .Call(C_R_initMethodDispatch, as.environment(where))# C-level initialization

### dummy version for booting
isSealedMethod <- function(f, signature, fdef = getGeneric(f, FALSE, where = where),
			   where = topenv(parent.frame())) FALSE

### real version
.isSealedMethod <- function(f, signature, fdef = getGeneric(f, FALSE, where = where),
			   where = topenv(parent.frame()))
{
    ## look for the generic to see if it is a primitive
    fGen <- getFunction(f, TRUE, FALSE, where = where)
    if(!is.primitive(fGen)) {
        mdef <- getMethod(f, signature, optional = TRUE, where = where, fdef = fGen)
        return(is(mdef, "SealedMethodDefinition"))
    }
    ## else, a primitive
    if(is(fdef, "genericFunction"))
        signature <- matchSignature(signature, fdef)
    if(length(signature) == 0L)
        TRUE # default method for primitive
    else if(f %in% .subsetFuns)
        ## primitive dispatch requires some argument to be an S4 object.
        ## This does not quite guarantee an S4 object; e.g., a class union might have only basic types in it.
        !any(is.na(match(signature, .BasicClasses)))
    else {
        sealed <- !is.na(match(signature[[1L]], .BasicClasses))
        if(sealed &&
           (!is.na(match("Ops", c(f, getGroup(f, TRUE))))
            || !is.na(match(f, c("%*%", "crossprod")))))
            ## Ops methods are only sealed if both args are basic classes
            sealed <- sealed && (length(signature) > 1L) &&
                      !is.na(match(signature[[2L]], .BasicClasses))
        sealed
    }
}

.subsetFuns <- c("[", "[[","[<-","[[<-")

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
  {
      if(!nzchar(name))
        stop(gettextf('expected a non-empty character string for argument name'), domain = NA)
      if(!missing(generic) && is(generic, "genericFunction") && !.identC(name, generic@generic))
        stop(gettextf('generic function supplied was not created for %s',
                      sQuote(name)),
             domain = NA)
      createGeneric <- (missing(generic) || !is(generic, "genericFunction")) && !isGeneric(name, where)
      if(createGeneric) {
          fdefault <- getFunction(name, where = where, mustFind = FALSE)
          if(is.null(fdefault))
            return(NULL)  # no implicit generic
          env <- environment(fdefault) # the environment for an implicit generic table
          fdefault <- .derivedDefaultMethod(fdefault)
          if(isBaseFun(fdefault)) {
              value <- genericForBasic(name)
              if (is.function(value)) {
                  if(!missing(generic) && !identical(value, generic))
                      stop(gettextf("%s is a primitive function; its generic form cannot be redefined",
                                    sQuote(name)),
                           domain = NA)
                  generic <- value
                  fdefault <- generic@default
              }
              package <- "base"
          }
          else
              package <- getPackageName(env)
          ## look for a group
          group <-
              .getImplicitGroup(name,
                                if(identical(package,"base"))
                                .methodsNamespace else environment(fdefault))
          if(missing(generic)) {
            generic <- .getImplicitGeneric(name, env, package)
            if(is.null(generic))  { # make a new one
                generic <- makeGeneric(name, fdefault = fdefault, package = package,
                                       group = group)
                .cacheImplicitGeneric(name, generic)
            }
          }
          else {
            generic <- makeGeneric(name, generic, fdefault, package = package,
                                   group = group)
            .cacheImplicitGeneric(name, generic)
        }
      }
      generic
  }

setGenericImplicit <- function(name, where = topenv(parent.frame()), restore = TRUE) {
    if(!isGeneric(name, where)) {
        warning(gettextf("%s is not currently a generic:  define it first to create a non-default implicit form",
                         sQuote(name)),
                domain = NA)
        return(FALSE)
    }
    generic <- getGeneric(name, where = where)
    if(restore)
        removeMethods(name, where, TRUE)
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

registerImplicitGenerics <- function(what = .ImplicitGenericsTable(where),
                                     where = topenv(parent.frame()))
{
    if(!is.environment(what))
        stop(gettextf("must provide an environment table; got class %s",
                      dQuote(class(what))), domain = NA)
    objs <- as.list(what, all.names = TRUE)
    mapply(.cacheImplicitGeneric, names(objs), objs)
    NULL
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
    if(is.null(value) && !is.null(tbl <- where[[.ImplicitGenericsMetaName]]))
       .getGenericFromCacheTable(name, where, pkg, tbl)
    else
        value
}

## only called from setGeneric, f1 = supplied, f2 = implicit
.identicalGeneric <- function(f1, f2, allow.extra.dots = FALSE)
{
    gpString <- function(gp) {
	if(length(gp))
	    paste(as.character(gp), collapse = ", ")
	else
	    "<none>"
    }
    if(isFALSE(f2))
	return(gettext("original function is prohibited as a generic function"))
    if(!(is.function(f2) && is.function(f1)))
	return(gettext("not both functions!"))
    ## environments will be different
    if(!identical(class(f1), class(f2)))
	return(sprintf("classes: %s, %s",
                       .dQ(class(f1)), .dQ(class(f2))))
    if(!isS4(f1)) return(gettextf("argument %s is not S4",
                                  deparse(substitute(f1))))
    if(!isS4(f2)) return(gettextf("argument %s is not S4",
                                  deparse(substitute(f2))))
    f1d <- f1@.Data
    f2d <- f2@.Data
    ## xtra... <- FALSE
    if(!identical(formals(f1d), formals(f2d))) {
	a1 <- names(formals(f1d)); a2 <- names(formals(f2d))
	if(identical(a1, a2))
	    return(gettext("formal arguments differ (in default values?)"))
	else if(identical(c(a1, "..."), a2) && allow.extra.dots)
            ## silently accept an extra "..."
            { } ## xtra... <- TRUE
	    ## and continue
	else
	    return(gettextf("formal arguments differ: (%s), (%s)",
			    paste(a1, collapse = ", "),
			    paste(a2, collapse = ", ")))
    }
    if(!identical(f1@valueClass, f2@valueClass))
	return(gettextf("value classes differ: %s, %s",
                        .dQ(gpString(f1@valueClass)),
                        .dQ(gpString(f2@valueClass))))
    if(!identical(body(utils::removeSource(f1d)),
                  body(utils::removeSource(f2d))))
	return("function body differs")
    if(!identical(f1@signature, f2@signature))
	return(gettextf("signatures differ:  (%s), (%s)",
                        paste(f1@signature, collapse = ", "),
                        paste(f2@signature, collapse = ", ")))
    if(!identical(f1@package, f2@package))
	return(gettextf("package slots  differ: %s, %s",
                        .dQ(gpString(f1@package)),
                        .dQ(gpString(f2@package))))
    if(!identical(f1@group, f2@group)) {
	return(gettextf("groups differ: %s, %s",
                        .dQ(gpString(f1@group)),
                        .dQ(gpString(f2@group))))
    }
    if(!identical(as.character(f1@generic), as.character(f2@generic)))
	return(gettextf("generic names differ: %s, %s",
                        .dQ(f1@generic), .dQ(f2@generic)))
    TRUE
}

.ImplicitGroupMetaName <- ".__IGM__table"
.MakeImplicitGroupMembers <- function(group, members, where) {
    if(!exists(.ImplicitGroupMetaName, where, inherits = FALSE))
        assign(.ImplicitGroupMetaName, new.env(TRUE), where)
    tbl <- get(.ImplicitGroupMetaName, where)
    for(what in members)
        assign(what, as.list(group), envir = tbl)
    NULL
}

.getImplicitGroup <- function(name, where) {
    if(!is.null(tbl <- where[[.ImplicitGroupMetaName]])) {
	if(!is.null(r <- tbl[[name]]))
	    return(r)
    }
    list()
}

findMethods <- function(f, where, classes = character(), inherited = FALSE, package = "") {
    if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else if(.isSingleString(f)) {
        if(missing(where))
            fdef <- getGeneric(f, package = package)
        else { # the generic may not be in the where= environment
            ##  but we prefer that version if it is
            fdef <- getGeneric(f, where = where, package = package)
            if(is.null(fdef))
                fdef <- getGeneric(f, package = package)
        }
    }
    else if(!is.function(f))
        stop(gettextf("argument %s must be a generic function or a single character string; got an object of class %s",
                      sQuote("f"), dQuote(class(f))),
             domain = NA)
    else {
        fdef <- f
        f <- deparse(substitute(f))
    }
    if(!is(fdef, "genericFunction")) {
        warning(gettextf("non-generic function '%s' given to findMethods()", f),
                domain = NA)
        return(list())
    }
    object <- new("listOfMethods", arguments = fdef@signature,
                  generic = fdef) # empty list of methods
    if(missing(where))
      table <- get(if(inherited) ".AllMTable" else ".MTable", envir = environment(fdef))
    else {
        if(!isFALSE(inherited))
          stop(gettextf("only FALSE is meaningful for 'inherited', when 'where' is supplied (got %s)", inherited), domain = NA)
        where <- as.environment(where)
        what <- .TableMetaName(f, fdef@package)
        if(is.null(table <- where[[what]]))
          return(object)
    }
    objNames <- sort(names(table))
    if(length(classes)) {
        classesPattern <- paste0("#", classes, "#", collapse = "|")
        which <- grep(classesPattern, paste0("#",objNames,"#"))
        objNames <- objNames[which]
    }
    object@.Data <- mget(objNames, table)
    object@names <- objNames
    object@signatures <- strsplit(objNames, "#", fixed = TRUE)
    object
}

findMethodSignatures <- function(..., target = TRUE, methods = findMethods(...))
{
    what <- methods@arguments
    if(target)
      sigs <- methods@signatures
    else {
        anySig <- rep("ANY", length(what))
        ## something of a kludge for the case of some primitive
        ## default methods to get a vector of "ANY" of right length
        for(m in methods)
          if(!is.primitive(m)) {
              length(anySig) <- length(m@defined)
              break
          }
        sigs <- lapply(methods, function(x)
                       if(is.primitive(x)) anySig else as.character(x@defined))
    }
    lens <- unique(vapply(sigs, length, 1, USE.NAMES=FALSE))
    if(length(lens) == 0)
        return(matrix(character(), 0, length(methods@arguments)))
    if(length(lens) > 1L) {
        lens <- max(lens)
        anys <- rep("ANY", lens)
        sigs <- lapply(sigs, function(x) {
            if(length(x) < lens) {
              anys[seq_along(x)] <- x
              anys
          } else x
        })
    }
    length(what) <- lens # if not all possible arguments used
    t(matrix(unlist(sigs), nrow = lens, dimnames = list(what, NULL)))
}

hasMethods <- function(f, where, package = "")
{
    fdef <- NULL
    nowhere <- missing(where) # because R resets this if where is assigned
    if(is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
        if(missing(package))
            package <- fdef@package
    }
    else if(!.isSingleString(f))
        stop(gettextf("argument 'f' must be a generic function or %s",
                      .notSingleString(f)), domain = NA)
    else if(missing(package)) {
        package <- packageSlot(f) # maybe a string with package slot
	if(is.null(package)) {
            if(missing(where))
                fdef <- getGeneric(f)
            else { # the generic may not be in this package, but prefer it if so
                fdef <- getGeneric(f, where = where)
                if(is.null(fdef))
                    fdef <- getGeneric(f)
            }
            if(is(fdef, "genericFunction"))
                package <- fdef@package
	    else
		stop(gettextf("'%s' is not a known generic function {and 'package' not specified}",
			      f),
		     domain = NA)
	}
    }
    what <- .TableMetaName(f, package)
    testEv <- function(ev)
      exists(what, envir = ev, inherits = FALSE) &&
        length(names(get(what, envir = ev))) > 0L
    if(nowhere) {
        for(i in seq_along(search())) {
            if(testEv(as.environment(i)))
              return(TRUE)
        }
        return(FALSE)
    }
    else
      testEv(as.environment(where))
}
## returns TRUE if the argument is a non-empty character vector of length 1
## otherwise, returns a diagnostic character string reporting the non-conformance
.isSingleName <- function(x) {
    if(!is.character(x))
      return(paste0('required to be a character vector, got an object of class "', class(x)[[1L]], '"'))
    if(length(x) != 1)
      return(paste0("required to be a character vector of length 1, got length ",length(x)))
    if(is.na(x) || !nzchar(x))
      return(paste0('required a non-empty string, got "',x, '"'))
    TRUE
}
