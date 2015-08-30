#  File src/library/methods/R/RMethodUtils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

## The real version of makeGeneric, to be installed after there are some
## generic functions to boot the definition (in particular, coerce and coerce<-)

.makeGeneric <-
## Makes a generic function object corresponding to the given function name.
## and definition.
  function(f, fdef,
           fdefault = fdef,
           group = list(),
           valueClass = character(),
           package = getPackageName(environment(fdef)),
           signature = NULL,
           genericFunction = NULL,
           simpleInheritanceOnly = NULL)
{
    checkTrace <- function(fun, what, f) {
        if(is(fun, "traceable")) {
            warning(gettextf("the function being used as %s in making a generic function for %s is currently traced; the function used will have tracing removed",
                             what,
                             sQuote(f)),
                    domain = NA)
            .untracedFunction(fun)
        }
        else
            fun
    }
    if(missing(fdef)) {
        if(missing(fdefault))
            stop(gettextf("must supply either a generic function or a function as default for %s",
                          sQuote(f)),
                 domain = NA)
        else if(is.primitive(fdefault)) {
            return(genericForPrimitive(f))
        }
        fdef <- fdefault
        body(fdef) <- substitute(standardGeneric(NAME), list(NAME = f))
        environment(fdef) <- asNamespace(package)
    }
    ## give the function a new environment, to cache methods later
    ev <- new.env()
    parent.env(ev) <- environment(fdef)
    environment(fdef) <- ev
    packageSlot(f) <- package
    assign(".Generic", f, envir = ev)
    fdef <- checkTrace(fdef)
    if(length(valueClass))
        fdef <- .ValidateValueClass(fdef, f, valueClass)
    group <- .asGroupArgument(group)
    if(is.null(genericFunction))
        value <- new("standardGeneric")
    else if(is(genericFunction, "genericFunction"))
        value <- genericFunction
    else
        stop(gettextf("the %s argument must be NULL or a generic function object; got an object of class %s",
                      sQuote("genericFunction"),
                      dQuote(class(genericFunction))),
             domain = NA)
    value@.Data <- fdef
    value@generic <- f
    value@group <- group
    value@valueClass <- valueClass
    value@package <- package
    args <- formalArgs(fdef)
    if(is.null(signature))
        signature <- args
    else if(any(is.na(match(signature, args))))
        stop(sprintf(ngettext(sum(is.na(match(signature, args))),
                              "non-argument found in the signature: %s",
                              "non-arguments found in the signature: %s"),
                     paste(signature[is.na(match(signature, args))], collapse = ", ")),
             domain = NA)
    dots <- match("...", signature)
    if(!is.na(dots)) { # remove "..." unless it is the only element of the signature
        if(length(signature) > 1L)
            signature <- signature[-dots]
    }
    if(length(signature) == 0L)
        stop("no suitable arguments to dispatch methods in this function")
    attr(signature, "simpleOnly") <- simpleInheritanceOnly # usually NULL
    value@signature <- signature
##    name <- signature[[1L]]
    if(is.null(fdefault))
        {} # pre 2.11.0: methods <- MethodsList(name)
    else {
        fdefault <- checkTrace(fdefault)
        if(!identical(formalArgs(fdefault), formalArgs(fdef)) &&
           !is.primitive(fdefault))
            stop(sprintf(ngettext(length(fdef),
	"the formal argument of the generic function for %s (%s) differs from that of the non-generic to be used as the default (%s)",
	"the formal arguments of the generic function for %s (%s) differ from those of the non-generic to be used as the default (%s)"),
			 f,
			 paste(formalArgs(fdef), collapse = ", "),
			 paste(formalArgs(fdefault), collapse = ", ")),
                 domain = NA)
        fdefault <- asMethodDefinition(fdefault, fdef = value)
        if(is(fdefault, "MethodDefinition"))
            fdefault@generic <- value@generic
        ## pre 2.11.0 methods <- MethodsList(name, fdefault)
    }
    value@default <- fdefault # pre 2.11.0 methods
    assign(".Methods", fdefault, envir = ev) ## ? why
    .setupMethodsTables(value, TRUE)
    value@skeleton <- generic.skeleton(f, fdef, fdefault)
    value
}

## stripped down version of asS4 in base (asS4 can't be used until the methods
## namespace is available -- no longer true)
.asS4 <- function (object)
    asS4(object, TRUE, 0L)

.notS4 <- function (object)
    asS4(object, FALSE, 0L)


## the bootstrap version: "#----" brackets lines that replace parts of the real version
makeGeneric <-
      function(f, fdef,
           fdefault = getFunction(f, generic = FALSE, mustFind = FALSE),
           group = list(), valueClass = character(), package, signature = NULL,
           genericFunction = NULL, simpleInheritanceOnly = NULL)
{
    ## give the function a new environment, to cache methods later
    ev <- new.env()
    parent.env(ev) <- environment(fdef)
    environment(fdef) <- ev
    packageSlot(f) <- package
    assign(".Generic", f, envir = ev)
    if(length(valueClass))
        fdef <- .ValidateValueClass(fdef, f, valueClass)
    group <- .asGroupArgument(group)
###--------
    value <- .asS4(fdef)
    if(is.null(genericFunction))
        class(value) <- .classNameFromMethods("standardGeneric")
    else
        class(value) <- class(genericFunction)
    slot(value, "generic", FALSE) <- f
    slot(value, "group", FALSE) <- group
    slot(value, "valueClass", FALSE) <- valueClass
    slot(value, "package", FALSE) <- package
###--------
    args <- formalArgs(fdef)
    if(is.null(signature))
        signature <- args
    else if(any(is.na(match(signature, args))))
        stop(sprintf(ngettext(sum(is.na(match(signature, args))),
                              "non-argument found in the signature: %s",
                              "non-arguments found in the signature: %s"),
                     paste(signature[is.na(match(signature, args))], collapse = ", ")),
             domain = NA)
    attr(signature, "simpleOnly") <- simpleInheritanceOnly # usually NULL
    dots <- match("...", signature)
    if(!is.na(dots)) ## ... is not currently supported in method signatures
        signature <- signature[-dots]
    if(length(signature) == 0L)
        stop("no suitable arguments to dispatch methods in this function")
###--------
    slot(value, "signature", FALSE) <- signature
###--------
    name <- signature[[1L]]
    if(is.null(fdefault))
      {}
    else
        fdefault <- asMethodDefinition(fdefault, fdef = value)
        if(is(fdefault, "MethodDefinition"))
            fdefault@generic <- value@generic
        ## pre 2.11.0 methods <- MethodsList(name, fdefault)
###--------
    assign(".Methods", fdefault, envir = ev)
    slot(value, "default", FALSE) <- fdefault
    slot(value, "skeleton", FALSE) <- generic.skeleton(f, fdef, fdefault)
###--------
    value
}

### FIXME: Not used by methods, but exposed through namespace. Deprecate?
makeStandardGeneric <-
  ## a utility function that makes a valid function calling
  ## standardGeneric for name f Works (more or less) even if the
  ## actual definition, fdef, is not a proper function, that is, it is
  ## a primitive or internal
  function(f, fdef)
{
    fgen <- fdef
    body(fgen) <- substitute(standardGeneric(FNAME), list(FNAME=f))
    ## detect R specials and builtins:  these don't provide an argument list
    if(typeof(fdef) != "closure") {
        ## Look in a list of pre-defined functions (and also of
        ## functions for which methods are prohibited)
        fgen <- genericForPrimitive(f)
        message(gettextf("making a generic for special function %s",
                         sQuote(f)),
                domain = NA)
        setPrimitiveMethods(f, fdef, "reset", fgen, NULL)
        ## Note that the body of the function comes from the list.  In
        ## a few cases ("$"), this body is not just a call to
        ## standardGeneric
    }
    fgen
}

generic.skeleton <- function(name, fdef, fdefault)
{
    anames <- formalArgs(fdef)
    skeleton <- lapply(as.list(c(name, anames)), as.name)
    ## any arguments after "..." have to be named
    dots <- match("...", anames)
    if(!is.na(dots) && dots < length(anames)) {
        anames[1L:dots] <- ""
        names(skeleton) <- c("", anames)
    }
    if(is.null(fdefault)) {
        fdefault <- fdef
        body(fdefault) <- substitute(stop(MESSAGE, domain = NA), list(MESSAGE=
                                                   gettextf("invalid call in method dispatch to '%s' (no default method)", name)))
        environment(fdefault) <- baseenv()
    }
    skeleton[[1L]] <- fdefault
    as.call(skeleton)
}


defaultDumpName <-
  ## the default name to be used for dumping a method.
  function(generic, signature)
{
    if(missing(signature))
        paste(generic, "R", sep=".", collapse =".")
    else
        paste(generic, paste(signature, collapse ="."), "R", sep=".")
}


mergeMethods <-
    ## merge the methods in the second MethodsList object into the first,
    ## and return the merged result.
    function(m1, m2, genericLabel = character())
{
    .MlistDeprecated("mergeMethods()")
    if(length(genericLabel) && is(m2, "MethodsList"))
        m2 <- .GenericInPrimitiveMethods(m2, genericLabel)
    if(is.null(m1) || is(m1, "EmptyMethodsList"))
        return(m2)
    tmp <- listFromMlist(m2)
    sigs <- el(tmp, 1)
    methods <- el(tmp, 2)
    for(i in seq_along(sigs)) {
        sigi <- el(sigs, i)
        if(.noMlists() && !identical(unique(sigi), "ANY"))
          next
        args <- names(sigi)
        m1 <- insertMethod(m1, as.character(sigi), args, el(methods, i), FALSE)
    }
    m1
}

doPrimitiveMethod <-
  ## do a primitive call to builtin function 'name' the definition and call
  ## provided, and carried out in the environment 'ev'.
  ##
  ## A call to 'doPrimitiveMethod' is used when the actual method is a .Primitive.
  ##  (because primitives don't behave correctly as ordinary functions,
  ## not having either formal arguments nor a function body).
  function(name, def, call = sys.call(sys.parent()), ev = sys.frame(sys.parent(2)))
{
    cat("called doPrimitiveMethod\n\n")
    ## Store a local version of function 'name' back where the current version was
    ## called.  Restore the previous state there on exit, either removing or re-assigning.
    if(!is.null(prev <- ev[[name]])) {
        on.exit(assign(name, prev, envir = ev))
    }
    else
        on.exit(rm(list=name, envir=ev))
    assign(name, def, envir = ev)
    eval(call, ev)
}

.renderSignature <- function(f, signature)
{
    nm <- names(signature)
    nm[nzchar(nm)] <- paste0(nm[nzchar(nm)], "=")
    msig <- paste0(nm, '"', as.vector(signature), '"')
    msig <- paste(msig, collapse = ",")
    gettextf("in method for %s with signature %s: ", sQuote(f), sQuote(msig))
}

conformMethod <- function(signature, mnames, fnames,
			  f = "<unspecified>", fdef, method)
{
    sig0 <- signature
    fsig <- fdef@signature
    if(is.na(match("...", mnames)) && !is.na(match("...", fnames)))
        fnames <- fnames[-match("...", fnames)]
    imf <- match(fnames, mnames)
    omitted <- is.na(imf)
    if(is.unsorted(imf[!omitted]))
	stop(.renderSignature(f, signature),
             "formal arguments in method and generic do not appear in the same order",
             call. = FALSE)
    if(!any(omitted)) ## i.e. mnames contains all fnames
        return(signature)
    sigNames <- names(signature)
    omittedSig <- sigNames %in% fnames[omitted] #  names in signature & generic but not in method defn
### FIXME:  the test below is too broad, with all.names().  Would be nice to have a test
### for something like assigning to one of the omitted arguments.
    ##     missingFnames <- fnames[omitted]
    ##     foundNames <- missingFnames %in% all.names(body(method), unique = TRUE)
    ##     if(any(foundNames))
    ##         warning(gettextf("%s function arguments omitted from method arguments, (%s), were found in method definition",
    ##                       label, paste(missingFnames[foundNames], collapse = ", ")),
    ##              domain = NA)
    if(!any(omittedSig))
      return(signature)
    if(any(is.na(match(signature[omittedSig], c("ANY", "missing"))))) {
        bad <- omittedSig & is.na(match(signature[omittedSig], c("ANY", "missing")))
        bad2 <- paste0(fnames[bad], " = \"", signature[bad], "\"", collapse = ", ")
        stop(.renderSignature(f, sig0),
             gettextf("formal arguments (%s) omitted in the method definition cannot be in the signature", bad2),
             call. = TRUE, domain = NA)
    }
    else if(!all(signature[omittedSig] == "missing")) {
        omittedSig <- omittedSig && (signature[omittedSig] != "missing")
        .message("Note: ", .renderSignature(f, sig0),
                 gettextf("expanding the signature to include omitted arguments in definition: %s",
                          paste(sigNames[omittedSig], "= \"missing\"",collapse = ", ")))
        omittedSig <- seq_along(omittedSig)[omittedSig] # logical index will extend signature!
        signature[omittedSig] <- "missing"
    }
    ## remove trailing "ANY"'s
    n <- length(signature)
    while(.identC(signature[[n]], "ANY"))
        n <- n - 1L
    length(signature) <- n
    length(fsig) <- n
    setNames(signature, fsig)
}

rematchDefinition <- function(definition, generic, mnames, fnames, signature)
{
    added <- any(is.na(match(mnames, fnames)))
    keepsDots <- !is.na(match("...", mnames))
    if(!added && keepsDots) {
        ## the formal args of the method must be identical to generic
        formals(definition, envir = environment(definition)) <- formals(generic)
        return(definition)
    }
    dotsPos <- match("...", fnames)
    if(added && is.na(dotsPos))
        stop(gettextf("methods can add arguments to the generic %s only if '...' is an argument to the generic", sQuote(generic@generic)),
             call. = TRUE)
    ## pass down all the names in common between method & generic,
    ## plus "..."  even if the method doesn't have it.  But NOT any
    ## arguments having class "missing" implicitly (see conformMethod),
    ## i.e., are not among 'mnames':
    useNames <- !is.na(imf <- match(fnames, mnames)) | fnames == "..."
    newCall <- lapply(c(".local", fnames[useNames]), as.name)

    ## Should not be needed, if conformMethod() has already been called:
    if(is.unsorted(imf[!is.na(imf)]))
	stop(.renderSignature(generic@generic, signature),
             "formal arguments in method and generic do not appear in the same order",
             call. = FALSE)

    ## leave newCall as a list while checking the trailing args
    if(keepsDots && dotsPos < length(fnames)) {
	## Trailing arguments are required to match.  This is a little
	## stronger than necessary, but this is a dicey case, because
	## the argument-matching may not be consistent otherwise (in
	## the generic, such arguments have to be supplied by name).
	## The important special case is replacement methods, where
	## value is the last argument.

	ntrail <- length(fnames) - dotsPos
	trailingArgs <- fnames[seq.int(to = length(fnames), length.out = ntrail)]
	if(!identical(	mnames[seq.int(to = length(mnames), length.out = ntrail)],
		      trailingArgs))
	    stop(gettextf("arguments (%s) after '...' in the generic must appear in the method, in the same place at the end of the argument list",
			  paste(trailingArgs, collapse=", ")),
                 call. = TRUE, domain = NA)
	newCallNames <- character(length(newCall))
	newCallNames[seq.int(to = length(newCallNames), length.out = ntrail)] <-
	    trailingArgs
	names(newCall) <- newCallNames
    }
    newCall <- as.call(newCall)
    newBody <- substitute({.local <- DEF; NEWCALL},
			  list(DEF = definition, NEWCALL = newCall))
    generic <- .copyMethodDefaults(generic, definition)
    body(generic, envir = environment(definition)) <- newBody
    generic
}

unRematchDefinition <- function(definition)
{
    ## undo the effects of rematchDefiniition, if it was used.
    ## Has the obvious disadvantage of depending on the implementation.
    ## If we considered the rematching part of the API, a cleaner solution
    ## would be to include the "as given to setMethod" definition as a slot
    bdy <- body(definition)
    if(.identC(class(bdy),"{") && length(bdy) > 1L) {
        bdy <- bdy[[2L]]
        if(.identC(class(bdy), "<-") &&
           identical(bdy[[2L]], as.name(".local")))
            definition <- bdy[[3L]]
    }
    definition
}

getGeneric <-
  ## return the definition of the function named f as a generic.
  ##
  ## If there is no definition, throws an error or returns
  ## NULL according to the value of mustFind.
  function(f, mustFind = FALSE, where, package = "")
{
    if(is.function(f)) {
        if(is(f, "genericFunction"))
            return(f)
        else if(is.primitive(f))
            return(genericForPrimitive(.primname(f), mustFind=mustFind))
        else
            stop("argument 'f' must be a string, generic function, or primitive: got an ordinary function")
    }
    value <- if(missing(where))
		  .getGeneric(f,      , package)
	     else .getGeneric(f, where, package)
    if(is.null(value) && !is.null(baseDef <- baseenv()[[f]])) {
        ## check for primitives
        if(is.primitive(baseDef)) {
            value <- genericForPrimitive(f)
            if(!is.function(value) && mustFind)
                stop(gettextf("methods cannot be defined for the primitive function %s",
                              sQuote(f)), domain = NA)
            if(is(value, "genericFunction"))
                value <- .cacheGeneric(f, value)
        }
    }
    if(is.function(value))
        value
    else {
        if(nzchar(package) && is.na(match(package, c("methods", "base")))) {
            value <- tryCatch({
                ## load package namespace or error
                ev <- getNamespace(package)
                .getGeneric(f, ev, package)
            }, error = function(e) NULL)
        }
        if(is.function(value))
            value
        else if(mustFind)
            ## the C code will have thrown an error if f is not a single string
            stop(gettextf("no generic function found for %s", sQuote(f)),
                 domain = NA)
        else
            NULL
    }
}

## low-level version
.getGeneric <- function(f, where = .GlobalEnv, # default only for C search
                        package = "")
{
    ## do not search the cache if getGeneric() was called with explicit where=
    value <- if(missing(where))
        .getGenericFromCache(f, where,  package) ## else NULL
    if(is.null(value)) {
        if(is.character(f) && f %in% "as.double") f <- "as.numeric"
        if(is.character(f) && !nzchar(f)) {
            message("Empty function name in .getGeneric")
            dput(sys.calls())
        }
        value <- .Call(C_R_getGeneric, f, FALSE, as.environment(where), package)
        ## cache public generics (usually these will have been cached already
        ## and we get to this code for non-exported generics)
        if(!is.null(value) && !is.null(vv <- get0(f, .GlobalEnv)) &&
           identical(vv, value))
            .cacheGeneric(f, value)
    }
    ##     if(is.null(value) && nzchar(package) && !identical(package, "base")) {
    ##         env <- .requirePackage(package, FALSE)
    ##         if(is.environment(env))
    ##           value <- .Call("R_getGeneric", f, FALSE, env, package,
    ##                      PACKAGE = "methods")
    ##     }
    value
}

## cache and retrieve generic functions.  If the same generic name
## appears for multiple packages, a named list of the generics is cached.
.genericTable <- new.env(TRUE, baseenv())

.implicitTable <- new.env(TRUE, baseenv())

.cacheGeneric <- function(name, def)
  .cacheGenericTable(name, def, .genericTable)

.cacheImplicitGeneric <- function(name, def)
   .cacheGenericTable(name, def, .implicitTable)

.cacheGenericTable <- function(name, def, table)
{
    fdef <- def
    if(!is.null(prev <- table[[name]])) {
        newpkg <- def@package
        if(is.function(prev)) {
            if(identical(prev, def))
                return(fdef)
            ## the following makes the cached version != package
            ##  fdef <- def <- .makeGenericForCache(def)
            pkg <- prev@package
            if(identical(pkg, newpkg)) { # redefinition
                assign(name, def, envir = table)
                return(fdef)
            }
            prev <- list(prev)          # start a per-package list
            names(prev) <- pkg
        }
        i <- match(newpkg, names(prev))
        if(is.na(i))
            prev[[newpkg]] <- def # or, .makeGenericForCache(def) as above
        else if(identical(def, prev[[i]]))
            return(fdef)
        else
            prev[[i]] <- def  # or, .makeGenericForCache(def) as above
        def <- prev
    }

    .getMethodsTable(fdef)              # force initialization
    assign(name, def, envir = table)
    fdef
}

.uncacheGeneric <- function(name, def)
  .uncacheGenericTable(name, def, .genericTable)

.uncacheImplicitGeneric <- function(name, def)
  .uncacheGenericTable(name, def, .implicitTable)

.uncacheGenericTable <- function(name, def, table)
{
    if(exists(name, envir = table, inherits = FALSE)) {
        newpkg <- def@package
        prev <- get(name, envir = table)
        if(is.function(prev))  # we might worry if  prev not identical
            return(remove(list = name, envir = table))
        i <- match(newpkg, names(prev))
        if(!is.na(i))
            prev[[i]] <- NULL
        else           # we might warn about unchaching more than once
            return()
        if(length(prev) == 0L)
            return(remove(list = name, envir = table))
        else if(length(prev) == 1L)
            prev <- prev[[1L]]
        assign(name, prev, envir  = table)
    }
}

.getGenericFromCache <- function(name, where,  pkg = "")
   .getGenericFromCacheTable(name,where, pkg, .genericTable)

.getImplicitGenericFromCache <- function(name, where,  pkg = "")
   .getGenericFromCacheTable(name,where, pkg, .implicitTable)

.getGenericFromCacheTable <- function(name, where, pkg = "", table)
{
    if(exists(name, envir = table, inherits = FALSE)) {
        value <- get(name, envir = table)
        if(is.list(value)) {        # multiple generics with this name
            ## force a check of package name, even if argument is ""
            if(!nzchar(pkg)) {
                if(is.character(where))
                    pkg <- where
                else {
                    pkg <- attr(name, "package")
                    if(is.null(pkg))
                        pkg <- getPackageName(where, FALSE)
                    if(identical(pkg, ".GlobalEnv"))
                        pkg <- ""
                }
            }
            pkgs <- names(value)
            i <- match(pkg, pkgs, 0L)
            if(i > 0L)
                return(value[[i]])
            i <- match("methods", pkgs, 0L)
            if(i > 0L)
                return(value[[i]])
            i <- match("base", pkgs, 0L)
            if(i > 0L)
                return(value[[i]])
            else
                return(NULL)
        }
        else if(nzchar(pkg) && !identical(pkg, value@package))
            NULL
        else
            value
    }
    else
        NULL
}

.genericOrImplicit <- function(name, pkg, env)
{
    fdef <- .getGenericFromCache(name, env, pkg)
    if(is.null(fdef)) {
	penv <- tryCatch(getNamespace(pkg), error = function(e)e)
	if(!isNamespace(penv))	{      # no namespace--should be rare!
	    pname <- paste0("package:", pkg)
	    penv <- if(pname %in% search()) as.environment(pname) else env
	}
        fdef <- getFunction(name, TRUE, FALSE, penv)
        if(!is(fdef, "genericFunction")) {
            if(is.primitive(fdef))
                fdef <- genericForPrimitive(name, penv)
            else
                fdef <- implicitGeneric(name, penv)
        }
    }
    fdef
}


## copy the environments in the generic function so later merging into
## the cached generic will not modify the generic in the package.
## NOT CURRENTLY USED: see comments in .getGeneric()
.makeGenericForCache <- function(fdef)
{
    value <- fdef
    ev <- environment(fdef)
    objs <- lapply(as.list(ev, all.names=TRUE), function(obj) {
        if(is.environment(obj))
            obj <- .copyEnv(obj)
        obj
    })
    environment(value) <- list2env(objs, hash=TRUE, parent=parent.env(ev))
    value
}

.copyEnv <- function(env)
{
    list2env(as.list(env, all.names=TRUE), hash=TRUE, parent=parent.env(env))
}

getGroup <-
  ## return the groups to which this generic belongs.  If 'recursive=TRUE', also all the
  ## group(s) of these groups.
  function(fdef, recursive = FALSE, where = topenv(parent.frame()))
{
    if(is.character(fdef))
        fdef <- getGeneric(fdef, where = where)
    if(is(fdef, "genericFunction"))
        group <- fdef@group
    else
        group <- list()
    if(recursive && length(group)) {
        allGroups <- group
        for(gp in group) {
            fgp <- getGeneric(gp, where = where)
            if(is(fgp, "groupGenericFunction"))
                allGroups <- c(allGroups, Recall(fgp, TRUE, where))
        }
        if(length(allGroups) > 1L) {
            ids <- sapply(allGroups, function(x) {
                pkg <- packageSlot(x)
                if(is.null(pkg)) x
                else paste(x, pkg, sep=":")
            })
            allGroups <- allGroups[!duplicated(ids)]
        }
        allGroups
    }
    else
        group
}

getMethodsMetaData <- function(f, where = topenv(parent.frame()))
{
    fdef <- getGeneric(f, where = where)
    if(is.null(fdef))
        return(NULL)
    if(.noMlists()) {
        warning(sprintf("Methods list objects are not maintained in this version of R:  request for function %s may return incorrect information",
                        sQuote(fdef@generic)),
                domain = NA)
    }
    mname <- methodsPackageMetaName("M",fdef@generic, fdef@package)
    if (exists(mname, where = where, inherits = missing(where)))
        get(mname, where)
    else if(missing(where))
        .makeMlistFromTable(fdef)
    else
        .makeMlistFromTable(fdef, where)
}

assignMethodsMetaData <-
  ## assign value to be the methods metadata for generic f on database where.
  ## as of R 2.7.0 the mlist metadata is deprecated.
  ## If value is not a MethodsList,  only turns on primitives & groups
  function(f, value, fdef, where, deflt)
{
    where <- as.environment(where)
    if(is(value, "MethodsList")) {
	.MlistDeprecated()
        mname <- methodsPackageMetaName("M",fdef@generic, fdef@package)
        if(exists(mname, envir = where, inherits = FALSE) &&
           bindingIsLocked(mname, where))
          {}        # may be called from trace() with locked binding; ignore
        else
          assign(mname, value, where)
    }
    if(is.primitive(deflt))
        setPrimitiveMethods(f, deflt, "reset", fdef, NULL)
    if(is(fdef, "groupGenericFunction")) # reset or turn on members of group
        cacheGenericsMetaData(f, fdef, where = where, package = fdef@package)
}


## utility for getGenerics to return package(s)
.packageForGeneric <- function(object)
{
    if(is.list(object))                 # a list of objects
        lapply(object, .packageForGeneric)
    else if(is(object, "genericFunction"))
        object@package
    else ## ?? possibly a primitive
        "base"
}

getGenerics <- function(where, searchForm = FALSE)
{
    if(missing(where)) {
        ## all the packages cached ==? all packages with methods
        ## globally visible.  Assertion based on cacheMetaData + setMethod
        fdefs <- as.list(.genericTable, all.names=TRUE, sorted=TRUE)
        fnames <- mapply(function(nm, obj) {
            if (is.list(obj)) names(obj) else nm
        }, names(fdefs), fdefs, SIMPLIFY=FALSE)
        packages <- lapply(fdefs, .packageForGeneric)
        new("ObjectsWithPackage", unlist(fnames), package=unlist(packages))
    }
    else {
        if(is.environment(where)) where <- list(where)
        these <- unlist(lapply(where, objects, all.names=TRUE), use.names=FALSE)
        metaNameUndo(unique(these), prefix = "T", searchForm = searchForm)
    }
}

## Find the pattern for methods lists or tables
## Currently driven by mlists, but eventually these will go away
## in favor of tables.

## always returns a compatible list, with an option of  prefix
.getGenerics <- function(where, trim = TRUE)
{
    if(missing(where)) where <- .envSearch(topenv(parent.frame()))
    else if(is.environment(where)) where <- list(where)
    these <- unlist(lapply(where, objects, all.names=TRUE), use.names=FALSE)
    these <- unique(these)
    these <- these[substr(these, 1L, 6L) == ".__T__"]
    if(length(these) == 0L)
        return(character())
    funNames <- gsub(".__T__(.*):([^:]+)", "\\1", these)
    if(length(funNames) == 0L &&
       length(these[substr(these, 1L, 6L) == ".__M__"]))
        warning(sprintf("package %s seems to have out-of-date methods; need to reinstall from source",
                         sQuote(getPackageName(where[[1L]]))))
    packageNames <- gsub(".__T__(.*):([^:]+(.*))", "\\2", these)
    attr(funNames, "package") <- packageNames
    ## Would prefer following, but may be trouble bootstrapping methods
    ## funNames <- new("ObjectsWithPackage", funNames, package = packageNames)
    if(identical(trim, TRUE))
        funNames
    else {
        if(identical(trim, FALSE))
            these
        else
            gsub(".__T__", as.character(trim), these)
    }
}

cacheMetaData <-
    function(where, attach = TRUE, searchWhere = as.environment(where),
             doCheck = TRUE)
{
    ## a collection of actions performed on attach or detach
    ## to update class and method information.
    pkg <- getPackageName(where)
    classes <- getClasses(where)
    for(cl in classes) {
        cldef <- (if(attach) get(classMetaName(cl), where) # NOT getClassDef, it will use cache
                  else  getClassDef(cl, searchWhere))
        if(is(cldef, "classRepresentation")) {
            if(attach) {
                .cacheClass(cl, cldef, is(cldef, "ClassUnionRepresentation"), where)
            }
            else if(identical(cldef@package, pkg)) {
                .uncacheClass(cl, cldef)
                .removeSuperclassBackRefs(cl, cldef, searchWhere)
            }
        }
    }
    generics <- .getGenerics(where)
    packages <- attr(generics, "package")
    if(length(packages) <  length(generics))
        packages <- rep(packages, length.out = length(generics))
    if(attach && exists(".requireCachedGenerics", where, inherits = FALSE)) {
        others <- get(".requireCachedGenerics", where)
        generics <- c(generics, others)
        packages <- c(packages, attr(others, "package"))
    }
    ## check for duplicates
    dups <- duplicated(generics) & duplicated(packages)
    generics <- generics[!dups]
    for(i in seq_along(generics)) {
        f <- generics[[i]]
        fpkg <- packages[[i]]
        if(!identical(fpkg, pkg) && doCheck) {
            if(attach) {
                env <- as.environment(where)
                ## All instances of this generic in different attached packages must
                ## agree with the cached version of the generic for consistent
                ## method selection.
                if(exists(f, envir = env, inherits = FALSE)) {
                    def <- get(f, envir = env)
                    fdef <- .genericOrImplicit(f, fpkg, env)
                    if(is.function(def)) {
                        ## exclude a non-function of the same name as a primitive with methods (!)
                        if(identical(environment(def), environment(fdef)))
                            next        # the methods are identical
                        else if( is(fdef, "genericFunction")) {
                            .assignOverBinding(f, fdef,  env, FALSE)
                        }
                    }     # else, go ahead to update primitive methods
                }
                else          # either imported generic or a primitive
                    fdef <- getGeneric(f, FALSE, searchWhere, fpkg)
            }
            else
                fdef <- getGeneric(f, FALSE, searchWhere, fpkg)
        }
        else
            fdef <- getGeneric(f, FALSE, searchWhere, fpkg)
        if(!is(fdef, "genericFunction"))
            next ## silently ignores all generics not visible from searchWhere
        if(attach)
            .cacheGeneric(f, fdef)
        else
            .uncacheGeneric(f, fdef)
        methods <- .updateMethodsInTable(fdef, where, attach)
        cacheGenericsMetaData(f, fdef, attach, where, fdef@package, methods)
    }
    .doLoadActions(where, attach)
    invisible(NULL) ## as some people call this at the end of functions
}


cacheGenericsMetaData <- function(f, fdef, attach = TRUE,
                                  where = topenv(parent.frame()),
                                  package, methods)
{
    if(!is(fdef, "genericFunction")) {
	warning(gettextf("no methods found for %s; cacheGenericsMetaData() will have no effect",
			 sQuote(f)),
		domain = NA)
	return(FALSE)
    }
    if(missing(package))
        package <- fdef@package
### Assertion: methods argument unused except for primitives
### and then only for the old non-table case.
    deflt <- finalDefaultMethod(fdef@default) #only to detect primitives
    if(is.primitive(deflt)) {
	if(missing(methods)) ## "reset"
	    setPrimitiveMethods(f, deflt, "reset", fdef, NULL)
	else ## "set"
	    setPrimitiveMethods(f, deflt, "set", fdef, methods)
    }
    else if(isGroup(f, fdef = fdef)) {
	members <- fdef@groupMembers
	## do the computations for the members as well; important if the
	## members are primitive functions.
	for(ff in members) {
	    ffdef <- getGeneric(ff, where = where)
	    if(is(ffdef, "genericFunction"))
		Recall(ff, ffdef, attach, where,
                       methods = .getMethodsTable(ffdef))
	}
    }
    TRUE
}

setPrimitiveMethods <-
  function(f, fdef, code, generic, mlist = get(".Methods", envir = environment(generic)))
    .Call(C_R_M_setPrimitiveMethods, f, fdef, code, generic, mlist)

### utility to turn ALL primitive methods on or off (to avoid possible inf. recursion)
.allowPrimitiveMethods <- function(onOff) {
    code <- if(onOff) "SET" else "CLEAR"
    .Call(C_R_M_setPrimitiveMethods, "", NULL, code, NULL, NULL)
}


findUnique <- function(what, message, where = topenv(parent.frame()))
{
    where <- .findAll(what, where = where)
    if(length(where) > 1L) {
        if(missing(message))
            message <- sQuote(what)
        if(is.list(where))
            where <- unlist(where)
        if(is.numeric(where))
            where <- search()[where]
        warning(message,
                sprintf(" found on: %s; using the first one",
                        paste(sQuote(where), collapse = ", ")),
                domain = NA)
        where <- where[1L]
    }
    where
}

MethodAddCoerce <- function(method, argName, thisClass, methodClass)
{
    if(.identC(thisClass, methodClass))
        return(method)
    ext <- possibleExtends(thisClass, methodClass)
    ## if a non-simple coerce is required to get to the target class for
    ## dispatch, insert it in the method.
    if(is.logical(ext) || ext@simple)
        return(method)
    methodInsert <- function(method, addExpr) {
        if(is.function(method)) {
            newBody <- substitute({firstExpr; secondExpr},
                                  list(firstExpr = addExpr,
                                       secondExpr = body(method)))
            body(method, envir = environment(method)) <- newBody
        }
        else if(is(method, "MethodsList")) {
	    .MlistDeprecated()
            methods <- method@allMethods
            for(i in seq_along(methods))
                methods[[i]] <- Recall(methods[[i]], addExpr)
            method@allMethods <- methods
        }
        method
    }
    addExpr <- substitute(XXX <- as(XXX, CLASS),
                          list(XXX = argName, CLASS = methodClass))
    methodInsert(method, addExpr)
}

missingArg <- function(symbol, envir = parent.frame(), eval = FALSE)
    .Call(C_R_missingArg, if(eval) symbol else substitute(symbol), envir)

balanceMethodsList <- function(mlist, args, check = TRUE)
{
    .MlistDeprecated("balanceMethodsList()")
    moreArgs <- args[-1L]
    if(length(moreArgs) == 0L)
        return(mlist)
    methods <- mlist@methods
    if(check && length(methods)) {
        ## check whether the current depth is enough (i.e.,
        ## whether a method with this no. of args or more was set before
        depth <- 0
        el <- methods[[1L]]
        while(is(el, "MethodsList")) {
            mm <- el@methods
            if(length(mm) == 0L)
                break
            depth <- depth+1L
            el <- mm[[1L]]
        }
        if(depth >= length(args))
            ## already balanced to this length: An assertion
            ## relying on balance having been used consistently,
            ## which in turn relies on setMethod being called to
            ## add methods.  If you roll your own, tough luck!
            return(mlist)
    }
    for(i in seq_along(methods)) {
        el <- methods[[i]]
        if(is(el, "MethodsList"))
            el <- Recall(el, moreArgs, FALSE)
        else {
            if(is(el, "MethodDefinition")) {
                el@target[moreArgs] <- "ANY"
                el@defined[moreArgs] <- "ANY"
            }
            for(what in rev(moreArgs))
                el <- new("MethodsList", argument = as.name(what),
                          methods = list(ANY = el))
        }
        methods[[i]] <- el
    }
    mlist@methods <- methods
    mlist
}


sigToEnv <- function(signature, generic)
{
    genericSig <- generic@signature
    package <- packageSlot(signature)
    if(is.null(package))
        parent <- environment(generic)
    else
        parent <- .requirePackage(package)
    value <- new.env(parent = parent)
    classes <- as.character(signature)
    args <- names(signature)
    for(i in seq_along(args))
        assign(args[[i]], classes[[i]], envir = value)
    ## missing args in signature have class "ANY"
    if(length(args) < length(genericSig))
        for(other in genericSig[is.na(match(genericSig, args))])
            assign(other, "ANY", envir = value)
    value
}

methodSignatureMatrix <- function(object, sigSlots = c("target", "defined"))
{
    if(length(sigSlots)) {
        allSlots <- lapply(sigSlots, slot, object = object)
        mm <- unlist(allSlots)
        mm <- matrix(mm, nrow = length(allSlots), byrow = TRUE)
        dimnames(mm) <- list(sigSlots, names(allSlots[[1L]]))
        mm
    }
    else matrix(character(), 0L, 0L)
}

.valueClassTest <- function(object, classes, fname)
{
    if(length(classes)) {
        for(Cl in classes)
            if(is(object, Cl)) return(object)
        stop(gettextf("invalid value from generic function %s, class %s, expected %s",
                      sQuote(fname),
                      dQuote(class(object)),
                      paste(dQuote(classes), collapse = " or ")),
             domain = NA)
    }
    ## empty test is allowed
    object
}


.getOrMakeMethodsList <- function(f, where, genericFun)
{
    allMethods <- getMethodsMetaData(f, where = where)
    if(is.null(allMethods)) {
        argName <- genericFun@signature[[1L]]
	warning("\"MethodsList\" is defunct; allMethods now are empty")
##-        allMethods <- new("MethodsList", argument = as.name(argName))
#         other <- getMethodsMetaData(f)
#         if(is.null(other))
#             ## this utility is called AFTER ensuring the existence of a generic for f
#             ## Therefore, the case below can only happen for a primitive for which
#             ## no methods currently are attached.  Make the primitive the default
#             deflt <- getFunction(f, generic = FALSE, mustFind = FALSE)
#         else
#             ## inherit the default method, if any
#             deflt <- finalDefaultMethod(other)
#         if(!is.null(deflt))
#             allMethods <- insertMethod(allMethods, "ANY", argName, deflt)
        }
    allMethods
}

.makeCallString <- function(def, name = substitute(def), args = formalArgs(def))
{
    if(is.character(def)) {
        if(missing(name))
            name <- def
        def <- getFunction(def)
    }
    if(is(def, "function"))
        paste0(name, "(", paste(args, collapse=", "), ")")
    else
        ""
}

.ValidateValueClass <- function(fdef, name, valueClass)
{
    ## include tests for value
    fbody <- body(fdef)
    body(fdef, envir = environment(fdef)) <-
        substitute({
            ans <- EXPR
            .valueClassTest(ans, VALUECLASS, FNAME)
        }, list(EXPR = fbody, VALUECLASS = valueClass, FNAME = name))
    fdef
}

## interpret the group= argument to makeGeneric, allowing for char. argument
## and "" for compatibility.
## TO DO:  make it possible for this argument to be a group generic function
## (it may in fact work now).
.asGroupArgument <- function(group)
{
    if(is.character(group)) {
	if(identical(group, ""))
	    list()
	else
	    as.list(group) ## should we allow c(group, package) ?
    }
    else
	group
}

metaNameUndo <- function(strings, prefix, searchForm = FALSE)
{
    pattern <- methodsPackageMetaName(prefix, "")
    n <- nchar(pattern, "c")
    matched <- substr(strings, 1L, n) == pattern
    value <- substring(strings[matched], n+1L)
    pkg <- sub("^[^:]*", "", value)   # will be "" if no : in the name
    if(searchForm) {
        global <- grep(".GlobalEnv", value)
        if(length(global)) {
            pkg[-global] <- paste0("package", pkg[-global])
            pkg[global] <- substring(pkg[global], 2L)
        }
    }
    else
        pkg <- substring(pkg, 2L)
    value <- sub(":.*","", value)
    new("ObjectsWithPackage", value, package = pkg)
}

.recursiveCallTest <- function(x, fname)
{
    if(is(x, "call")) {
        if(identical(x[[1L]], quote(standardGeneric))) {
            if(!identical(x[[2L]], fname))
                warning(gettextf("the body of the generic function for %s calls 'standardGeneric' to dispatch on a different name (\"%s\")!",
                                 sQuote(fname),
                                 paste(as.character(x[[2L]]), collapse = "\n")),
                        domain = NA)
            TRUE
        }
        else {
            for(i in seq.int(from=2L, length.out = length(x)-1L)) {
                if(Recall(x[[i]], fname))
                    return(TRUE)
            }
            FALSE
        }
    }
    else if(is(x, "language")) {
        for(i in seq.int(from=2L, length.out = length(x)-1L)) {
            if(Recall(x[[i]], fname))
                return(TRUE)
        }
        FALSE
    }
    else
        FALSE
}

.NonstandardGenericTest <- function(body, fname, stdBody)
{
    if(identical(body, stdBody))
        FALSE
    else if(.recursiveCallTest(body, fname))
        TRUE
    else
        NA
}

.GenericInPrimitiveMethods <- function(mlist, f)
{
    methods <- mlist@methods
    for(i in seq_along(methods)) {
        mi <- methods[[i]]
        if(is(mi, "function")) {
            body(mi, envir = environment(mi)) <-
                substitute({.Generic <- FF; BODY},
                           list(FF = f,BODY = body(mi)))
        }
	else if(is(mi, "MethodsList")) {
	    .MlistDeprecated()
            mi <- Recall(mi, f)
	} else
            stop(sprintf("internal error: Bad methods list object in fixing methods for primitive function %s",
                          sQuote(f)),
                 domain = NA)
        methods[[i]] <- mi
    }
    mlist@methods <- methods
    mlist
}

.signatureString <- function(fdef, signature)
{
    snames <- names(signature)
    if(is.null(snames)) {
        if(is(fdef, "genericFunction")) {
            snames <- fdef@signature
            signature <- matchSignature(signature, fdef)
            if(length(snames) > length(signature))
                length(snames) <- length(signature)
        }
        else                            # shouldn't happen,...
            return(paste(signature, collapse=", "))
    }
    else
        signature <- as.character(signature)
    paste(paste0(snames, "=\"", signature, "\""), collapse = ", ")
}

.ChangeFormals <- function(def, defForArgs, msg = "<unidentified context>")
{
    if(!is(def, "function"))
        stop(gettextf("trying to change the formal arguments in %s in an object of class %s; expected a function definition",
                      msg, dQuote(class(def))),
             domain = NA)
    if(!is(defForArgs, "function"))
        stop(gettextf("trying to change the formal arguments in %s, but getting the new formals from an object of class %s; expected a function definition",
                      msg, dQuote(class(def))),
             domain = NA)
    old <- formalArgs(def)
    new <- formalArgs(defForArgs)
    if(length(old) < length(new))
        stop(gettextf("trying to change the formal arguments in %s, but the number of existing arguments is less than the number of new arguments: (%s) vs (%s)",
                      msg, paste0("\"", old, "\"", collapse=", "),
                      paste0("\"", new, "\"", collapse=", ")),
             domain = NA)
    if(length(old) > length(new))
        warning(gettextf("trying to change the formal arguments in %s, but the number of existing arguments is greater than the number of new arguments (the extra arguments won't be used): (%s) vs (%s)",
                         msg, paste0("\"", old, "\"", collapse=", "),
                         paste0("\"", new, "\"", collapse=", ")),
                domain = NA)
    if(identical(old, new))           # including the case of 0 length
        return(def)
    dlist <- as.list(def)
    slist <- lapply(c(old, new), as.name)
    names(slist) <- c(new, old)
    vlist <- dlist
    for(i in seq_along(vlist))
        vlist[[i]] <- do.call("substitute", list(vlist[[i]], slist))
    dnames <- names(dlist)
    whereNames <- match(old, dnames)
    if(anyNA(whereNames))
	stop(gettextf("in changing formal arguments in %s, some of the old names are not in fact arguments: %s",
		      msg, paste0("\"", old[is.na(match(old, names(dlist)))], "\"", collapse=", ")),
	     domain = NA)
    dnames[whereNames] <- new
    names(vlist) <- dnames
    as.function(vlist, envir = environment(def))
}

## The search list, or a namespace's static search list, or an environment
.envSearch <- function(env = topenv(parent.frame()))
{
    if(identical(env, .GlobalEnv))
        seq_along(search())
    else if(isNamespace(env) && !isBaseNamespace(env)) {
        ## the static environments for this namespace, ending with the base namespace
        value <- list(env)
        repeat {
            if(identical(env, emptyenv()))
                stop("botched namespace: failed to find 'base' namespace in its parents", domain = NA)
            env <- parent.env(env)
            value <- c(value, list(env))
            if(isBaseNamespace(env))
                break
        }
        value
    }
    else
        list(env)
}

.genericName <- function(f)
{
    if(is(f, "genericFunction"))
        f@generic
    else
        as.character(f)
}

## the environment in which to start searching for methods, etc. related
## to this generic function.  Will normally be the namespace of the generic's
## home package, or else the global environment
.genericEnv <- function(fdef)
    parent.env(environment(fdef))

## the default environment in which to start searching for methods, etc. relative to this
## call to a methods package utility.  In the absence of other information, the current
## strategy is to look at the function _calling_ the methods package utility.
##TODO:  this utility can't really work right until the methods package itself has a
## namespace, so that calls from within the package can be detected.  The
## heuristic is that all callers are skipped as long as their enviornment is  identical
## to .methodsNamespace.  But that is currently initialized to .GlobalEnv.
##
## The logic will fail if a function in a package with a namespace calls a (non-methods)
## function in a package with no namespace, and that function then calls a methods package
## function.  The right answer then is .GlobalEnv, but we will instead get the package
## namespace.
.externalCallerEnv <- function(n = 2, nmax = sys.nframe() - n + 1)
{
    ## start n generations back; by default the caller of the caller to this function
    ## go back nmax at most (e.g., a function in the methods package that knows it's never
    ## called more than nmax levels in could supply this argument
    if(nmax < 1) stop("got a negative maximum number of frames to look at")
    ev <- topenv(parent.frame()) # .GlobalEnv or the environment in which methods is being built.
    for(back in seq.int(from = -n, length.out = nmax)) {
        fun <- sys.function(back)
        if(is(fun, "function")) {
            ## Note that "fun" may actually be a method definition, and still will be counted.
            ## This appears to be the correct semantics, in
            ## the sense that, if the call came from a method, it's the method's environment
            ## where one would expect to start the search (for a class definition, e.g.)
            ev <- environment(fun)
            if(!identical(ev, .methodsNamespace))
                break
        }
    }
    ev
}

## a list of environments, starting from ev, going back to the base package,
## or else terminated by finding a namespace
.parentEnvList <- function(ev)
{
    ev <- as.environment(ev)
    value <- list(ev)
    while(!isNamespace(ev)) {
        if(identical(ev, baseenv())) {
            value[[length(value)]] <- .BaseNamespaceEnv
            break
        } else if(identical(ev, emptyenv())) {
            break
        }
        ev <- parent.env(ev)
        value <- c(value, list(ev))
    }
    value
}

.genericAssign <- function(f, fdef, methods, where, deflt)
{
    ev <- environment(fdef)
    assign(".Methods", methods, ev)
}

## Mark the method as derived from a non-generic.
.derivedDefaultMethod <- function(fdef)
{
    if(is.function(fdef) && !is.primitive(fdef)) {
        value <- new("derivedDefaultMethod")
        value@.Data <- fdef
        value@target <- value@defined <- .newSignature(.anyClassName, formalArgs(fdef))
        value
    }
    else
        fdef
}

.identC <- function(c1 = NULL, c2 = NULL)
{
    ## are the two objects identical class or genric function string names?
    .Call(C_R_identC, c1, c2)
}

## match default exprs in the method to those in the generic
## if the method does not itself specify a default, and the
## generic does
matchDefaults <- function(method, generic)
{
    changes <- FALSE
    margs <- formals(method)
    gargs <- formals(generic)
    for(arg in names(margs)) {
        ##!! weird use of missing() here is required by R's definition
        ## of a missing arg as a name object with empty ("") name
        ## This is dangerously kludgy code but seems the only way
        ## to avoid spurious errors ("xxx missing with no default")
        marg <- margs[[arg]]
        garg <- gargs[[arg]]
        if(missing(marg) && !missing(garg)) {
            changes <- TRUE
            margs[arg] <- gargs[arg] # NOT  [[]], which woud fail for NULL element
        }
    }
    if(changes)
        formals(method, envir = environment(method)) <- margs
    method
}

getGroupMembers <- function(group, recursive = FALSE, character = TRUE)
{
    .recMembers <- function(members, where) {
        all = vector("list", length(members))
        for(i in seq_along(members)) {
            what <- members[[i]]
            f <- getGeneric(what, FALSE, where)
            if(!is.null(f))
                all[[i]] <- what
            if(is(f, "groupGenericFunction")) {
                newMem <- f@groupMembers
                all <- c(all, Recall(newMem, where))
            }
        }
        all
    }
    f <- getGeneric(group)
    if(is.null(f)) {
        warning(gettextf("%s is not a generic function (or not visible here)",
                         sQuote(f)),
                domain = NA)
        return(character())
    }
    else if(!is(f, "groupGenericFunction"))
        character()
    else {
        members <- f@groupMembers
        if(recursive) {
            where <- f@package
            if(identical(where, "base")) {
                where <- "methods"      # no generics actually on base
                members <- .recMembers(members, .methodsNamespace)
            }
            else
                members <- .recMembers(members, .asEnvironmentPackage(where))
        }
        if(character)
            sapply(members, function(x){
                if(is(x, "character"))
                    x
                else if(is(x, "genericFunction"))
                    x@generic
                else
		    stop(gettextf("invalid element in the \"groupMembers\" slot (class %s)",
				  dQuote(class(x))),
                         domain = NA)
            })
        else
            members
    }
}

.primname <- function(object)
{
    ## the primitive name is 'as.double', but S4 methods are
    ## traditionally set on 'as.numeric'
    f <- .Call(C_R_get_primname, object)
    if(f == "as.double") "as.numeric" else f
}

.copyMethodDefaults <- function(generic, method)
{
    emptyDefault <- function(value) missing(value) ||
    (is.name(value) && nzchar(as.character(value)) )
    fg <- formals(generic)
    mg <- formals(method)
    mgn <- names(mg)
    changed <- FALSE
    for(what in names(fg)) {
        i <- match(what, mgn, 0L)
        if(i > 0L) {
            deflt <- mg[[i]]
            if(!(emptyDefault(deflt) || identical(deflt, fg[[what]]))) {
                fg[[what]] <- deflt
                changed <- TRUE
            }
        }
    }
    if(changed)
        formals(generic) <- fg
    generic
}

.NamespaceOrPackage <- function(what)
{
    name <- as.name(what)
    ns <-  .getNamespace(name)
    if(!is.null(ns))
        asNamespace(ns)
    else {
        i <- match(paste("package", what, sep=":"), search())
        if(is.na(i))
            .GlobalEnv
        else
            as.environment(i)
    }
}

.NamespaceOrEnvironment <- function(where)
{
    value <- NULL
    if(is.environment(where))
        value <- where
    else if(is.character(where) && nzchar(where)) {
        ns <- .getNamespace(where)
        if(isNamespace(ns))
            value <- ns
        else if(where %in% search())
            value <- as.environment(where)
        else {
            where <- paste0("package:", where)
            if(where %in% search())
                value <- as.environment(where)
        }
    }
    else if(is.numeric(where) && where %in% seq_along(search()))
        value <- as.environment(where)
    value
}


.hasS4MetaData <- function(env)
  (length(objects(env, all.names = TRUE,
                          pattern = "^[.]__[CTA]_")))

## turn ordinary generic into one that dispatches on "..."
## currently only called in one place from setGeneric()
.dotsGeneric <- function(f)
{
    if(!is(f, "genericFunction"))
        f <- getGeneric(f)
    if(!is(f, "genericFunction") || !identical(f@signature, "..."))
        stop("argument f must be a generic function with signature \"...\"")
    def <- .standardGenericDots
    fenv <- environment(f)
    environment(def) <- fenv
    assign("standardGeneric", def, envir = fenv)
    assign(".dotsCall", .makeDotsCall(formalArgs(f)), envir = fenv)
    f
}

utils::globalVariables(c(".MTable", ".AllMTable", ".dotsCall"))

.standardGenericDots <- function(name)
{
    env <- sys.frame(sys.parent())
    dots <- eval(quote(list(...)), env)
    classes <- unique(unlist(lapply(dots, methods:::.class1)))
    method <- methods:::.selectDotsMethod(classes, .MTable, .AllMTable)
    if(is.null(method))
        stop(gettextf("no method or default matching the \"...\" arguments in %s",
                      deparse(sys.call(sys.parent()), nlines = 1)), domain = NA)
    assign(".Method", method, envir = env)
    eval(.dotsCall, env)
}


.quoteCall <- quote(.Method(...))
.makeDotsCall <- function(formals)
{
    call <- .quoteCall
    if(length(formals)  > 1L) {
        idots <- match("...", formals)
        for(what in formals[-idots]) {
            ## the following nonsense is required to get the names in the call
            ## expression to be empty for ... and there for other args
            eval(substitute(call$NAME <- as.name(WHAT),
                            list(NAME = as.name(what), WHAT = what)))
        }
    }
    call
}

.selectDotsMethod <- function(classes, mtable, allmtable)
{
    .pasteC <- function(names) paste0('"', names, '"', collapse = ", ")
    found <- character()
    distances <- numeric()
    methods <- names(mtable)
    direct <- classes %in% methods
    if(all(direct)) {
        if(length(classes) > 1L) {
            warning(gettextf("multiple direct matches: %s; using the first of these", .pasteC(classes)), domain = NA)
            classes <- classes[1L]
        }
        else if(length(classes) == 0L)
            return( if(is.na(match("ANY", methods))) NULL else get("ANY", envir = mtable))
        return(mtable[[classes]])
    }
    if(is.null(allmtable))
        return(NULL)

    ## Else, look for an acceptable inherited method, which must match or be a superclass
    ## of the class of each of the arguments.
    classes <- sort(classes) # make slection depend only on the set of classes
    label <- .sigLabel(classes)
    if(exists(label, envir = allmtable, inherits = FALSE))
        ## pre-cached, but possibly NULL to indicate no match
        return(get(label, envir = allmtable))
    for(i in seq_along(classes)) {
        classi <- classes[[i]]
        defi <- getClassDef(classi)
        if(is.null(defi)) next
        extendsi <- defi@contains
        namesi <- c(classi, names(extendsi))
        if(i == 1)
            namesi <- namesi[namesi %in% methods]
        else { # only the superclass methods matching all arguments are kept
            namesi <- namesi[namesi %in% found]
            found <- namesi
            if(length(found) == 0L) break # no possible non-default match
        }
        for(namei in namesi) {
            disti <- if(identical(namei, classi)) 0 else extendsi[[namei]]@distance
            prev <- match(namei, found)
            if(is.na(prev)) {           # must be the 1st element
                found <- c(found, namei)
                distances <- c(distances, disti)
            }
            else if(disti < distances[[prev]])
                distances[[prev]] <- disti
        }
    }
    if(length(found) == 0L)
        method <-  if(is.na(match("ANY", methods))) NULL else get("ANY", envir = mtable)
    else {
        classes <- found[which.min(distances)]
        if(length(classes) > 1L) {
            warning(gettextf("multiple equivalent inherited matches: %s; using the first of these",
                             .pasteC(classes)), domain = NA)
            classes <- classes[1L]
        }
        method <- get(classes,envir = mtable)
    }
    if(!is.null(method))
        method@target <- new("signature", ... = label) # ?? not a legal class name if > 1 classes
    assign(label, method, allmtable)
    method
}

.isSingleString <- function(what)
  is.character(what) && identical(nzchar(what), TRUE)

.notSingleString <- function(what)
{
    if(identical(what, ""))
        "non-empty string; got \"\""
    else if(is.character(what))
        paste("single string; got a character vector of length", length(what))
    else
        gettextf("single string; got an object of class %s",
                 dQuote(class(what)[[1L]]))
}

.dotsClass <- function(...) {
    if(missing(..1))
      "missing"
    else
      class(..1)
}

## a utility to exclude various annoying glitches during
## loading of the methods package
.methodsIsLoaded <- function()
    identical(.saveImage, TRUE)

if(FALSE) {
## Defined but not currently used:
## utilitity to test well-defined classes in signature,
## for setMethod(), setAs() [etc.?], the result to be
## assigned in package where=
## Returns a list of signature, messages and level of error

## Has undefined ns an package
 .validSignature <- function(signature, generic, where) {
    thisPkg <- getPackageName(where, FALSE)
    checkDups <- .duplicateClassesExist()
    if(is(signature, "character")) { # including class "signature"
        classes <- as.character(signature)
        names <- allNames(signature)
        pkgs <- attr(signature, "package")
    }
    else if(is(signature, "list")) {
        classes <- sapply(signature, as.character)
        names <- names(signature)
        pkgs <- character(length(signature))
        for(i in seq_along(pkgs)) {
            pkgi <- attr(signature[[i]], "package")
            pkgs[[i]] <- if(is.null(pkgi)) "" else pkgi
        }
    }
    msgs <- character(); level <- integer()
    for(i in seq_along(classes)) {
        ## classes must be defined
        ## if duplicates exist check for them
        ## An ambiguous duplicate is a warning if it can match thisPkg
        ## else, an error
        classi <- classes[[i]]
        pkgi <- pkgs[[i]]
        classDefi <- getClass(classi, where = where)
        if(checkDups && classi %in% multipleClasses()) { # hardly ever, we hope
            clDefsi <- get(classi, envir = .classTable)
            if(nzchar(pkgi) && pkgi %in% names(clDefsi))
                ## use the chosen class, no message
                classDefi <- clDefsi[[pkgi]]
            else if(nzchar(pkgi)){
                ## this is only a warning because it just might
                ## be the result of identical class defs (e.g., from setOldClass()
                msgs <- c(msgs,
                          gettextf("multiple definitions exist for class %s, but the supplied package (%s) is not one of them (%s)",
                                   dQuote(classi), sQuote(pkgi),
                                   paste(dQuote(get(classi, envir = .classTable)), collapse = ", ")))
                level <- c(level, 2) #warn
            }
            else {
                msgs <- c(msgs,
                          gettextf("multiple definitions exist for class %s; should specify one of them (%s), e.g. by className()",
                                   dQuote(classi),
                                   paste(dQuote(get(classi, envir = .classTable)), collapse = ", ")))
            }
        }
        else {
            ## just possibly the first reference to an available
            ## package not yet loaded.  It's an error to specify
            ## a non-loadable package
            if(nzchar(pkgi)) {
                loadNamespace(pkgi)
                classDefi <- getClass(classi, where = ns)
            }
            if(is.null(classDefi)) {
                classDefi <- getClassDef
                msgi <- gettextf("no definition found for class %s",
                                 dQuote(classi))
                ## ensure only one error message
                if(length(level) && any(level == 3))
                    msgs[level == 3] <- paste(msgs[level == 3], msgi, sep = "; ")
                else
                    msgs <- c(msgs, msgi)
                level <- c(level, 3)
            }
            ## note that we do not flag a pkgi different from
            ## the package of the def., mainly because of setOldClass()
            ## which currently generates potentially multiple versions
            ## of the same S3 class.
        }
        ## except for the obscure multiple identical class case
        ## we should not get here w/o a valid class def.
        if(is.null(classDefi)) {}
        else
            pkgs[[i]] <- classDefi@package
    }
    signature <- .MakeSignature(new("signature"), generic,
                                structure(classes, names = names, package = package))
    if(length(msgs) > 1) {
        ## sort by severity, to get all messages before errror
        ii <- sort.list(level)
        msgs <- msgs[ii]; level <- level[ii]
    }
    list(signature = signature, message = msgs, level = level)
}
}

.ActionMetaPattern <- function()
    paste0("^[.]",substring(methodsPackageMetaName("A",""),2))

.actionMetaName <- function(name)
    methodsPackageMetaName("A", name)


.doLoadActions <- function(where, attach) {
    ## at the moment, no unload actions
    if(!attach)return()
    actionListName <- .actionMetaName("")
    if(!exists(actionListName, envir = where, inherits = FALSE))
        return(list())
    actions <- get(actionListName, envir = where)
    for(what in actions) {
        aname <- .actionMetaName(what)
        if(!exists(aname, envir = where, inherits = FALSE)) {
            warning(gettextf("missing function for load action: %s", what))
            next
        }
        f <- get(aname, envir = where)
        value <- eval(substitute(tryCatch(FUN(WHERE), error = function(e)e),
                            list(FUN = f, WHERE = where)), where)
        if(is(value, "error")) {
            callString <- deparse(value$call)[[1]]
            stop(gettextf("error in load action %s for package %s: %s: %s",
                          aname, getPackageName(where), callString, value$message))
        }
    }
}

setLoadAction <- function(action,
              aname = "",
              where = topenv(parent.frame())) {
    currentAnames <- .assignActionListNames(where)
    if(!nzchar(aname))
        aname <- paste0(".", length(currentAnames)+1)
    .assignActions(list(action), aname, where)
    if(is.na(match(aname, currentAnames))) {
        actionListName <- .actionMetaName("")
        assign(actionListName, c(currentAnames, aname), envir = where)
    }
}

.assignActions <- function(actions, anames, where) {
    ## check all the actions before assigning any
    for(i in seq_along(actions)) {
        f <- actions[[i]]
        fname <- anames[[i]]
        if(!is(f, "function"))
            stop(gettextf("non-function action: %s",
                          sQuote(fname)),
                 domain = NA)
        if(length(formals(f)) == 0)
            stop(gettextf("action function %s has no arguments, should have at least 1",
                          sQuote(fname)),
                 domain = NA)
    }
    for(i in seq_along(actions))
        assign(.actionMetaName(anames[[i]]), actions[[i]], envir = where)
}

.assignActionListNames <- function(where) {
    actionListName <- .actionMetaName("")
    if(exists(actionListName, envir = where, inherits = FALSE))
        get(actionListName, envir = where)
    else
        character()
}

setLoadActions <- function(..., .where = topenv(parent.frame())) {
    actionListName <- .actionMetaName("")
    currentAnames <- .assignActionListNames(.where)
    actions <- list(...)
    anames <- allNames(actions)
    ## first, replacements
    previous <- anames %in% currentAnames
    if(any(previous)) {
        .assignActions(actions[previous], anames[previous], .where)
        if(all(previous))
            return(list())
        anames <- anames[!previous]
        actions <- actions[!previous]
    }
    anon <- !nzchar(anames)
    if(any(anon)) {
        n <- length(currentAnames)
        deflts <- paste0(".",seq(from = n+1, length.out = length(actions)))
        anames[anon] <- deflts[anon]
    }
    .assignActions(actions, anames, .where)
    assign(actionListName, c(currentAnames, anames), envir = .where)
}

hasLoadAction <- function(aname, where = topenv(parent.frame()))
    exists(.actionMetaName(aname), envir = where, inherits = FALSE)

getLoadActions <- function(where = topenv(parent.frame())) {
    actionListName <- .actionMetaName("")
    if(!exists(actionListName, envir = where, inherits = FALSE))
        return(list())
    actions <- get(actionListName, envir = where)
    if(length(actions)) {
        allExists <- sapply(actions, function(what) exists(.actionMetaName(what), envir = where, inherits = FALSE))
        if(!all(allExists)) {
            warning(gettextf("some actions are missing: %s",
                             paste(actions[!allExists], collapse =", ")),
                    domain = NA)
            actions <- actions[allExists]
        }
        allFuns <- lapply(actions, function(what) get(.actionMetaName(what), envir = where))
        names(allFuns) <- actions
        allFuns
    }
    else
        list()
}

evalOnLoad <- function(expr, where = topenv(parent.frame()), aname = "") {
    f <- function(env)NULL
    body(f, where) <- substitute(eval(EXPR,ENV), list(EXPR = expr, ENV = where))
    setLoadAction(f, aname, where)
}

evalqOnLoad <- function(expr, where = topenv(parent.frame()), aname = "")
    evalOnLoad(substitute(expr), where, aname)

## a utility function used to flag non-generics at the loadNamespace phase
## The calculation there used to ignore the generic cache, which is wrong logic
## if the package being loaded had a DEPENDS on a package containing the generic
## version of the function.
.findsGeneric <- function(what, ns) {
    if(is(get(what, mode = "function", envir = ns), "genericFunction"))
        1L
    else if(!is.null(.getGenericFromCache(what, ns)))
        2L
    else
        0L
}
