#  File src/library/methods/R/MethodsList.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

MethodsList <-
  ## Create a MethodsList object out of the arguments.
  ##
  ## Conceptually, this object is a named collection of methods to be
  ## dispatched when the (first) argument in a function call matches the
  ## class corresponding to one of the names.  A final, unnamed element
  ## (i.e., with name `""') corresponds to the default method.
  ##
  ## The elements can be either a function, or another MethodsList.  In
  ## the second case, this list implies dispatching on the second
  ## argument to the function using that list, given a selection of this
  ## element on the first argument.  Thus, method dispatching on an
  ## arbitrary number of arguments is defined.
  ##
  ## MethodsList objects are used primarily to dispatch OOP-style
  ## methods and, in R, to emulate S4-style methods.
  function(.ArgName, ...)
{
    value <- makeMethodsList(list(...))
    if(is.name(.ArgName)){}
    else if(is.character(.ArgName) && length(.ArgName) == 1)
        .ArgName <- as.name(.ArgName)
    else stop("invalid first argument: should be the name of the first argument in the dispatch")
    slot(value, "argument") <- .ArgName
    value
}

makeMethodsList <- function(object, level=1)
{
    mnames <- allNames(object)
    if(.noMlists()) {
        keep <- mnames %in% c("", "ANY")
        mnames <- mnames[keep]
        object <- object[keep]
    }
    value <- new("MethodsList")
    i <- match("", mnames)
    if(!is.na(i)) {
        ## convert to ANY
        el(mnames, i) <- "ANY"
        names(object) <- mnames
    }
    if(anyDuplicated(mnames))
        stop(gettextf("duplicate element names in 'MethodsList' at level %d: %s",
             level, paste("\"", unique(mnames[duplicated(mnames)]), "\"",
                          collapse=", ")), domain = NA)
    for(i in seq_along(object)) {
        eli <- el(object, i)
        if(is(eli, "function")
           || is(eli, "MethodsList")) {}
        else if(is(eli, "list") ||
                is(eli, "named"))
            el(object, i) <- Recall(eli, NULL, level+1)
        else
            stop(gettextf("element %d at level %d (class %s) cannot be interpreted as a function or named list",
                          i, level, dQuote(class(eli))),
                 domain = NA)
    }
    slot(value, "methods") <- object
    value
}

SignatureMethod <-
  ## construct a MethodsList object containing (only) this method, corresponding
  ## to the signature; i.e., such that signature[[1L]] is the match for the first
  ## argument, signature[[2L]] for the second argument, and so on.  The string
  ## "missing" means a match for a missing argument, and "ANY" means use this as the
  ## default setting at this level.
  ##
  ## The first argument is the argument names to be used for dispatch corresponding to
  ## the signatures.
  function(names, signature, definition)
{
    n <- length(signature)
    if(n > length(names))
        stop("arguments 'names' and 'signature' must have the same length")
    if(n == 0)
        return(definition)
    Class <- el(signature,n)
    name <- el(names, n)
    m <- MethodsList(name)
    elNamed(slot(m, "methods"), Class) <- definition
    slot(m, "argument") <- as.name(name)
    SignatureMethod(names[-n], signature[-n], m)
}


insertMethod <-
  ## insert the definition `def' into the MethodsList object, `mlist', corresponding to
  ## the signature, and return the modified MethodsList.
  function(mlist, signature, args, def, cacheOnly = FALSE)
{
    if(.noMlists() && !identical(unique(signature), "ANY"))
      return(mlist)
    ## Checks for assertions about valid calls.
    ## See rev. 1.17 for the code before the assertions added.
    if(identical(args[1L], "...") && !identical(names(signature), "...")) {
        if(identical(signature[[1L]], "ANY"))
           stop(gettextf("inserting method with invalid signature matching argument '...' to class %s",
                         dQuote(signature[[1L]])),
                domain = NA)
        args <- args[-1L]
        signature <- signature[-1L]
        if(length(signature) == 0L)
            return(mlist)
    }
    if(length(signature) == 0L)
        stop("inserting method corresponding to empty signature")
    if(!is(mlist, "MethodsList"))
        stop(gettextf("inserting method into non-methods-list object (class %s)",
                      dQuote(.class1(mlist))),
             domain = NA)
    if(length(args) > 1 && !cacheOnly)
        mlist <- balanceMethodsList(mlist, args)
    Class <- el(signature, 1)
    methods <- if(cacheOnly) mlist@allMethods else mlist@methods
    current <- elNamed(methods, Class)
    if(is(current, "MethodsList")) {
        nextArg <- as.character(current@argument)
        sigArgs <- args
        n <- length(signature)
        length(sigArgs) <- n
        if(is.na(match(nextArg, sigArgs))) {
            n <- match(nextArg, args) - n
            if(is.na(n)) { ## not in args eitiher
                n <- 1
                args <- c(args, nextArg)
            }
            ## make explicit the trailing ANY's needed
            signature <- c(signature, rep("ANY", n))
        }
    }
    if(length(signature) == 1) {
        if(is.null(current)) {
            if(!is.null(def))
                elNamed(methods, Class) <- def
            ## else, no change
        }
        else {
            which <- match(Class, names(methods))
            if(is.null(def))
                ## delete the method
                methods <- methods[-which]
            else
                el(methods, which) <- def
        }
    }
    else { ## recursively merge, initializing current if necessary
        if(is.null(current))
            current <- new("MethodsList", argument = as.name(args[2L]))
        else if(is.function(current))
            current <- new("MethodsList", argument = as.name(args[2L]),
			   methods = list(ANY = current))
        elNamed(methods, Class) <-
            Recall(current, signature[-1L], args[-1L], def, cacheOnly)
    }
    mlist@allMethods <- methods
    if(!cacheOnly)
        mlist@methods <-  methods
    mlist
}


MethodsListSelect <-
  ## select the element of a MethodsList object corresponding to the
  ## actual arguments (as defined in the suppled environment),
  ## and return the object, extended to include that method if necessary.
  ##
  ## Works recursively.  At each level finds an argument name from the current `mlist'
  ## object, and evaluates this argument (if it is not missing), then uses the
  ## class of the result to select an element of `mlist'.  If such an element
  ## exists and is another `MethodsList' object, `MethodsListSelect'  calls itself recursively
  ## to resolve using further arguments.  Matching includes using a default selection or
  ## a method specifically linked to class `"missing"'.  Once a function is found, it
  ## is returned as the value.  If matching fails,  NULL is returned.
    function(f, env,
             mlist = NULL,
             fEnv = if(is(fdef, "genericFunction")) environment(fdef) else baseenv(),
             finalDefault = finalDefaultMethod(mlist),
             evalArgs = TRUE,
             useInherited = TRUE,  ## supplied when evalArgs is FALSE
             fdef = getGeneric(f, where = env), # MUST BE SAFE FROM RECUSIVE METHOD SELECTION
             resetAllowed = TRUE # FALSE when called from selectMethod, .findNextMethod
 )
{
    if(!resetAllowed) # ensure we restore the real methods for this function
	resetMlist <- .getMethodsForDispatch(fdef)
    ## look for call from C dispatch code during another call to MethodsListSelect
    if(is.null(f)) {} # Recall, not from C
    else {
	fMethods <- .getMethodsForDispatch(fdef)
        if(is.null(mlist) || (evalArgs && is.function(fMethods)))
            mlist <- fMethods
    }
    resetNeeded <- .setIfBase(f, fdef, mlist) # quickly protect against recursion -- see Methods.R
    if(resetNeeded) {
        on.exit(.setMethodsForDispatch(f, fdef, mlist))
    }
    if(!is(mlist, "MethodsList")) {
        if(is.function(mlist)) # call to f, inside MethodsListSelect
            {on.exit(); return(mlist)}
        if(is.null(f)) # recursive recall of MethodsListSelect
            stop("invalid method sublist")
        else if(!is.null(mlist)) # NULL => 1st call to genericFunction
            stop(gettextf("%f is not a valid generic function: methods list was an object of class %s",
                          sQuote(f), dQuote(class(mlist))),
                 domain = NA)
    }
    if(!is.logical(useInherited))
        stop(gettextf("%s must be TRUE, FALSE, or a named logical vector of those values; got an object of class %s",
                      sQuote("useInherited"),
                      dQuote(class(useInherited))),
             domain = NA)
    if(identical(mlist, .getMethodsForDispatch(fdef))) {
        resetNeeded <- TRUE
        ## On the initial call:
        ## turn off any further method dispatch on this function, to avoid recursive
        ## loops if f is a function used in MethodsListSelect.
        ## TODO: Using namespaces in the methods package would eliminate the need for this
        .setMethodsForDispatch(f, fdef, finalDefault)
        if(is(mlist, "MethodsList")) {
            on.exit(.setMethodsForDispatch(f, fdef, mlist))
        }
    }
    argName <- slot(mlist, "argument")
    arg <- NULL ## => don't use instance-specific inheritance
    if(evalArgs) {
        ## check for missing argument. NB: S sense, not that of R base missing()
        if(missingArg(argName, env, TRUE))
            thisClass <- "missing"
        else {
            arg <- eval(as.name(argName), env) ## DO use instance-specific inheritance
	    if(missing(arg)) ## S3 weird R code? Bail out!
		return(finalDefault)
            thisClass <- .class1(arg)
        }
    }
    else
        thisClass <- get(as.character(argName), envir = env, inherits = FALSE)
    if(identical(useInherited, TRUE) || identical(useInherited, FALSE))
        thisInherit <- nextUseInherited <- useInherited
    else {
        which <- match(as.character(argName), names(useInherited))
        if(is.na(which)) {
            nextUseInherited <- useInherited
            thisInherit <- TRUE
        }
        else {
            thisInherit <- useInherited[[which]]
            nextUseInherited <- useInherited[-which]
        }
    }
    fromClass <- thisClass ## will mark the class actually providing the method
    allMethods <- mlist@allMethods
    which <- match(thisClass, names(allMethods))
    inherited <- is.na(which)
    selection <- if(inherited) NULL else allMethods[[which]]
    if(!inherited) {
        if(is(selection, "function")) {
            if(is.null(f)) {
              ## An inherited method at the next level up.
              ## only the inherited method should be added
              mlist <- .trimMlist(mlist, fromClass)
            }
            value <- mlist ## no change
          }
        else {
            ## recursive call with NULL function name, to allow search to fail &
            ## to suppress any reset actions.
            method <- Recall(NULL, env, selection, finalDefault = finalDefault,
                   evalArgs = evalArgs, useInherited = nextUseInherited, fdef = fdef,
                             )
            if(is(method, "EmptyMethodsList"))
                value <- method
            else {
                mlist@allMethods[[which]] <- method
                value <- mlist
            }
        }
    }
    if(inherited || is(value, "EmptyMethodsList"))  {
        ## direct selection failed at this level or below
        method <- NULL
        if(thisInherit)  {
            allSelections <- inheritedSubMethodLists(arg, fromClass, mlist, env)
            allClasses <- names(allSelections)
            for(i in seq_along(allSelections)) {
                selection <- allSelections[[i]]
                fromClass <- allClasses[[i]]
                if(is(selection, "function"))
                    method <- selection
                else if(is(selection, "MethodsList")) {
                    ## go on to try matching further arguments
                    method <- Recall(NULL, env, selection, finalDefault = finalDefault,
                                     evalArgs = evalArgs,
                                     useInherited = nextUseInherited, fdef = fdef)
                    if(is(method, "EmptyMethodsList"))
                        selection <- method   ## recursive selection failed
                }
                if(!is(selection, "EmptyMethodsList"))
                    break
            }
        }
        if((is.null(selection) || is(selection, "EmptyMethodsList"))
           && !is.null(f) && !is.null(finalDefault)) {
            ## only use the final default method after exhausting all
            ## other possibilities, at all levels.
            method <- finalDefault
            fromClass <- "ANY"
        }
        if(is.null(method) || is(method, "EmptyMethodsList"))
            value <- emptyMethodsList(mlist, thisClass) ## nothing found
        else {
            method <- MethodAddCoerce(method, argName, thisClass, fromClass)
            value <- .insertCachedMethods(mlist, as.character(argName), thisClass, fromClass,
                                         method)
        }
    }
    if(!is.null(f)) {
        ## top level
        if(is(value, "EmptyMethodsList")) ## selection failed
            value <- NULL
        if(resetNeeded) {
            on.exit() # cancel the restore of the original mlist
            if(resetAllowed) {
                if(is.null(value)) resetMlist <- mlist else resetMlist <- value
            }
            .setMethodsForDispatch(f, fdef, resetMlist)
            if(is.primitive(finalDefault))
                setPrimitiveMethods(f, finalDefault, "set", fdef, resetMlist)
        }

    }
    value
}

emptyMethodsList <- function(mlist, thisClass = "ANY", sublist = list()) {
    sublist[thisClass] <- list(NULL)
    new("EmptyMethodsList", argument = mlist@argument, sublist = sublist)
}

insertMethodInEmptyList <- function(mlist, def) {
    value <- new("MethodsList", argument = mlist@argument)
    sublist <- mlist@sublist
    submethods <- sublist[[1L]]
    if(is.null(submethods))
        sublist[[1L]] <- def
    else
        sublist[[1L]] <- Recall(submethods, def)
    value@allMethods <- sublist
    value
}




finalDefaultMethod <-
  ## Return the default method from the generic (it may be NULL, a method object or a primitive.
  ## this previously searched in a MethodsList object.  Once those are gone, the loop should
  ## be irrelevant except as an error check.
  function(method)
{
    repeat {
        if(is.function(method) #somewhat liberal, but catches both methods and primitives
           || is.null(method))
          break
        value <- NULL
        if(is(method, "MethodsList"))
            method <-  elNamed(slot(method, "methods"), "ANY")
        else
          stop(gettextf("default method must be a method definition, a primitive or NULL: got an object of class %s", dQuote(class(method))),
               domain = NA)
    }
    method
}


inheritedSubMethodLists <-
  ## Utility function to match the object to the elements of a methods list.
  ##
  ## The function looks only for an inherited match, and only among
  ## the methods that are not themselves inherited.  (Inherited methods when found are
  ## stored in the session copy of the methods list, but they themselves should not be
  ## used for finding inherited matches, because an erroneous match could be found depending
  ## on which methods were previously used.  See the detailed discussion of methods.)
  function(object, thisClass, mlist, ev)
{
  methods <- slot(mlist, "methods")## only direct methods
  defaultMethod <- elNamed(methods, "ANY")## maybe NULL
  classes <- names(methods)
  value <- list()
  if(.identC(thisClass, "missing")) {
        ## no superclasses for "missing"
  }
  else {
      ## search in the superclasses, but don't use inherited methods
      ## There are two cases:  if thisClass is formally defined & unsealed, use its
      ## superclasses.  Otherwise, look in the subclasses of those classes for
      ## which methods exist.
      classDef <- getClassDef(thisClass, ev)
      useSuperClasses <- !is.null(classDef) && !classDef@sealed
      if(useSuperClasses) {
          ## for consistency, order the available methods by
          ## the ordering of the superclasses of thisClass
          superClasses <- names(classDef@contains)
          classes <- superClasses[!is.na(match(superClasses, classes))]
          for(which in seq_along(classes)) {
              tryClass <- el(classes, which)
              ## TODO:  There is potential bug here:  If the is relation is conditional,
              ## we should not cache this selection.  Needs another trick in the environment
              ## to FORCE no caching regardless of what happens elsewhere; e.g., storing a
              ## special object in .Class
              if(is.null(object) || is(object, tryClass)) {
                  elNamed(value, tryClass) <- elNamed(methods, tryClass)
              }
          }
      }
      else {
          for(which in seq_along(classes)) {
              tryClass <- el(classes, which)
              tryClassDef <- getClassDef(tryClass, ev)
              if(!is.null(tryClassDef) &&
                 !is.na(match(thisClass, names(tryClassDef@subclasses))))
                  elNamed(value, tryClass) <- el(methods, which)
          }
      }
  }
  if(!is.null(defaultMethod))
      elNamed(value, "ANY") <- defaultMethod
  value
}


matchSignature <-
  ## Match the signature object (a partially or completely named subset of the
  ## arguments of `fun', and return a vector of all the classes in the order specified
  ## by the signature slot of the generic.  The classes not specified by `signature
  ##' will be `"ANY"' in the value.
  function(signature, fun, where = baseenv())
{
    if(!is(fun, "genericFunction"))
        stop(gettextf("trying to match a method signature to an object (of class %s) that is not a generic function",
                      dQuote(class(fun))),
             domain = NA)
    anames <- fun@signature
    if(length(signature) == 0L)
        return(character())
    if(is(signature,"character")) {
        pkgs <- packageSlot(signature) # includes case of  "ObjectsWithPackage"
        if(is.null(pkgs))
            pkgs <- character(length(signature))
        else if(length(pkgs) != length(signature))
            stop("invalid 'package' slot or attribute, wrong length")
        sigClasses <- as.character(signature)
    }
    else if(is(signature, "list")) {
        sigClasses <- pkgs <- character(length(signature))
        for(i in seq_along(signature)) {
            cli <- signature[[i]]
            if(is(cli, "classRepresentation")) {
                sigClasses[[i]] <- cli@className
                pkgs[[i]] <- cli@package
            }
            else if(is(cli, "character") && length(cli) == 1) {
                sigClasses[[i]] <- cli
                pkgi <- packageSlot(cli)
                if(is.character(pkgi))
                    pkgs[[i]] <- pkgi
            }
            else
                stop(gettextf("invalid element in a list for \"signature\" argument; element %d is neither a class definition nor a class name",
                     i), domain = NA)
        }
    }
    else
        stop(gettextf("trying to match a method signature of class %s; expects a list or a character vector",
                      dQuote(class(signature))),
             domain = NA)
    if(!identical(where, baseenv())) {
        ## fill in package information, warn about undefined classes
        unknown <- !nzchar(pkgs)
        for(i in seq_along(sigClasses)[unknown]) {
            cli <- getClassDef(sigClasses[[i]], where)
            if(!is.null(cli)) {
                pkgs[[i]] <- cli@package
                unknown[[i]] <- FALSE
            }
        }
        if(any(unknown)) {
            unknown <- unique(sigClasses[unknown])
            ## coerce(), i.e., setAs() may use *one* unknown class
	    MSG <- if(identical(as.vector(coerce@generic), "coerce") &&
		      length(unknown) == 1) message
	    else function(...) warning(..., call. = FALSE)
	    MSG(.renderSignature(fun@generic, signature),
		sprintf(ngettext(length(unknown),
				 "no definition for class %s",
				 "no definition for classes %s"),
			paste(dQuote(unknown), collapse = ", ")),
		domain = NA)
        }
    }
    signature <- as.list(signature)
    if(length(sigClasses) != length(signature))
        stop(gettextf("object to use as a method signature for function %s does not look like a legitimate signature (a vector of single class names): there were %d class names, but %d elements in the signature object",
                      sQuote(fun@generic),
                      length(sigClasses),
                      length(signature)),
             domain = NA)
    if(is.null(names(signature))) {
        which <- seq_along(signature)
        if(length(which) > length(anames))
          stop(gettextf("more elements in the method signature (%d) than in the generic signature (%d) for function %s",
	       length(which), length(anames), sQuote(fun@generic)), domain = NA)
    }
    else {
    ## construct a function call with the same naming pattern  &
      ## values as signature
    sigList <- signature
    for(i in seq_along(sigList))
        sigList[[i]] <- c(sigClasses[[i]], pkgs[[i]])
    fcall <- do.call("call", c("fun", sigList))
    ## match the call to the formal signature (usually the formal args)
    if(identical(anames, formalArgs(fun)))
        smatch <- match.call(fun, fcall)
    else {
        fmatch <- fun
        ff <- as.list(anames); names(ff) <- anames
        formals(fmatch, envir = environment(fun)) <- ff
        smatch <- match.call(fmatch, fcall)
    }
    snames <- names(smatch)[-1L]
    which <- match(snames, anames)
    ## Assertion:  match.call has permuted the args into the order of formal args,
    ## and carried along the values.  Get the supplied classes in that
    ## order, from the matched args in the call object.
    if(anyNA(which))
        stop(sprintf(ngettext(sum(is.na(which)),
                              "in the method signature for function %s invalid argument name in the signature: %s",
                              "in the method signature for function %s invalid argument names in the signature: %s"),
                     sQuote(fun@generic),
                     paste(snames[is.na(which)], collapse = ", ")),
             domain = NA)
    smatch <- smatch[-1]
    for(i in seq_along(smatch)) {
        eli <- smatch[[i]]
        sigClasses[[i]] <- eli[[1]]
        pkgs[[i]] <- eli[[2]]
    }
}
    n <- length(anames)
    value <- rep("ANY", n)
    valueP <- rep("methods", n)
    names(value) <- anames
    value[which] <- sigClasses
    valueP[which] <- pkgs
    unspec <- value == "ANY"
    ## remove the trailing unspecified classes
    while(n > 1 && unspec[[n]])
        n <- n-1
    length(value) <- length(valueP) <- n
    attr(value, "package") <- valueP
    ## <FIXME> Is there a reason (bootstrapping?) why this
    ## is not an actual object from class "signature"?
    ## See .MakeSignature() </FIXME>
    value
}

showMlist <-
  ## Prints the contents of the MethodsList.  If `includeDefs' the signatures and the
  ## corresponding definitions will be printed; otherwise, only the signatures.
  ##
  ## If `includeDefs' is `TRUE', the currently known inherited methods are included;
  ## otherwise, only the directly defined methods.
function(mlist, includeDefs = TRUE, inherited = TRUE, classes = NULL, useArgNames = TRUE,
         printTo = stdout())
{
    if(identical(printTo, FALSE)) {
        tmp <- tempfile()
        con <- file(tmp, "w")
    }
    else
        con <- printTo
  object <- linearizeMlist(mlist, inherited)
  methods <- object@methods
  signatures <- object@classes
  args <- object@arguments
  if(!is.null(classes) && length(signatures)>0) {
    keep <- !vapply(signatures, function(x, y) all(is.na(match(x, y))), NA, classes)
    methods <- methods[keep]
    signatures <- signatures[keep]
    args <- args[keep]
  }
  if(length(methods) == 0)
    cat(file=con, "<Empty Methods List>\n")
  else {
   n <- length(methods)
    labels <- character(n)
    if(useArgNames) {
      for(i in 1L:n) {
        sigi <- signatures[[i]]
        labels[[i]] <- paste(args[[i]], " = \"", sigi, "\"",
                             sep = "", collapse = ", ")
      }
    }
    else {
      for(i in 1L:n)
        labels[[i]] <- paste(signatures[[i]], collapse = ", ")
    }
    for(i in seq_along(methods)) {
      cat(file=con, (if(includeDefs) "## Signature:" else ""), labels[[i]])
      method <- methods[[i]]
      if(includeDefs) {
        cat(file=con, ":\n")
        if(is(method, "MethodDefinition")) ## really an assertion
          cat(file=con, deparse(method@.Data), sep="\n")
        else
          cat(file=con, deparse(method), sep="\n")
      }
      if(is(method, "MethodDefinition") &&
         !identical(method@target, method@defined)) {
          defFrom <- method@defined
          cat(file = con, if(includeDefs) "##:" else "\n",
              "    (inherited from ",
              paste(names(defFrom), " = \"", as.character(defFrom),
                    "\"", sep = "", collapse = ", "),
               ")", if(includeDefs) "\n", sep="")
      }
      cat(file=con, "\n")
    }
  }
    if(identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
    }
}

promptMethods <- function(f, filename = NULL, methods)
{
    ## Generate information in the style of 'prompt' for the methods of
    ## the generic named 'f'.
    ##
    ## 'filename' can be a logical or NA or the name of a file to print
    ## to.  If it 'FALSE', the methods skeleton is returned, to be
    ## included in other printing (typically, the output from 'prompt').

    escape <- function(txt) gsub("%", "\\\\%", txt)
    packageString <- ""

    fdef <- getGeneric(f)
    if(!isGeneric(f, fdef=fdef))
	stop(gettextf("no generic function found corresponding to %s",
                      sQuote(f)),
	     domain = NA)
    if(missing(methods)) {
	methods <- findMethods(fdef)
	## try making  packageString
	where <- .genEnv(fdef, topenv(parent.frame()))
	if(!identical(where, .GlobalEnv))
	    packageString <-
                sprintf("in Package \\pkg{%s}", getPackageName(where))
    }
    fullName <- utils:::topicName("methods", f)
    n <- length(methods)
    labels <- character(n)
    aliases <- character(n)
    signatures <- findMethodSignatures(methods = methods, target=TRUE)
    args <- colnames(signatures) # the *same* for all
    for(i in seq_len(n)) {
        sigi <- signatures[i, ]
	labels[[i]] <-
            sprintf("\\code{signature(%s)}",
                    paste(sprintf("%s = \"%s\"", args, escape(sigi)),
                          collapse = ", "))
	aliases[[i]] <-
	    paste0("\\alias{",
		   utils:::topicName("method", c(f, signatures[i,])),
		   "}")
    }
    text <- paste0("\n\\item{", labels,
                   "}{\n%%  ~~describe this method here~~\n}")
    text <- c("\\section{Methods}{\n\\describe{", text, "}}")
    aliasText <- c(paste0("\\alias{", escape(fullName), "}"), escape(aliases))
    if(identical(filename, FALSE))
        return(c(aliasText, text))

    if(is.null(filename) || identical(filename, TRUE))
        filename <- paste0(fullName, ".Rd")

    Rdtxt <-
        list(name = paste0("\\name{", fullName, "}"),
             type = "\\docType{methods}",
             aliases = aliasText,
             ## <FIXME>
             ## Title and description are ok as auto-generated: should
             ## they be flagged as such (via '~~' which are quite often
             ## left in by authors)?
             title =
             sprintf("\\title{ ~~ Methods for Function \\code{%s} %s ~~}",
                     f, packageString),
             description =
             paste0("\\description{\n ~~ Methods for function",
                    " \\code{", f, "} ",
                    sub("^in Package", "in package", packageString),
                    " ~~\n}"),
             ## </FIXME>
             "section{Methods}" = text,
             keywords = c("\\keyword{methods}",
             "\\keyword{ ~~ other possible keyword(s) ~~ }"))

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    .message("A shell of methods documentation has been written",
             .fileDesc(filename), ".\n")
    invisible(filename)
}

linearizeMlist <-
    ## Undo the recursive nature of the methods list, making a list of
    ## function definitions, with the names of the list being the
    ## corresponding signatures (designed for printing; for looping over
    ## the methods, use `listFromMlist' instead).
    ##
    ## The function calls itself recursively.  `prev' is the previously
    ## selected class names.
    ##
    ## If argument `classes' is provided, only signatures containing one
    ## of these classes will be included.
    function(mlist, inherited = TRUE) {
        methods <- mlist@methods
        allMethods <- mlist@allMethods
        if(inherited && length(allMethods) >= length(methods)) {
            anames <- names(allMethods)
            inh <- is.na(match(anames, names(methods)))
            methods <- allMethods
        }
        preC <- function(y, x)c(x,y) # used with lapply below
        cnames <- names(methods)
        value <- list()
        classes <- list()
        arguments <- list()
        argname <- as.character(mlist@argument)
        for(i in seq_along(cnames)) {
            mi <- methods[[i]]
            if(is.function(mi)) {
                value <- c(value, list(mi))
                classes <- c(classes, list(cnames[[i]]))
                arguments <- c(arguments, list(argname))
            }
            else if(is(mi, "MethodsList")) {
                mi <- Recall(mi, inherited)
                value <- c(value, mi@methods)
                classes <- c(classes, lapply(mi@classes, preC, cnames[[i]]))
                arguments <- c(arguments, lapply(mi@arguments, preC, argname))
            }
            else
                warning(gettextf("skipping methods list element %s of unexpected class %s\n\n",
                                 paste(cnames[i], collapse = ", "),
                                 dQuote(.class1(mi))),
                        domain = NA)
        }
        new("LinearMethodsList", methods = value, classes = classes, arguments = arguments)
    }

print.MethodsList <- function(x, ...)
    showMlist(x)


listFromMlist <-
  ## linearizes the MethodsList object into list(sigs, methods); `prefix' is the partial
  ## signature (a named list of classes) to be prepended to the signatures in this object.
  ##
  ## A utility function used to iterate over all the individual methods in the object.
  function(mlist, prefix = list(), sigs. = TRUE, methods. = TRUE)
{
    methodSlot <- slot(mlist, "methods")
    mnames <- names(methodSlot)
    argName <- as.character(slot(mlist, "argument"))
    sigs <- list()
    methods <- list()
    for(i in seq_along(methodSlot)) {
        thisMethod <- el(methodSlot, i)
        thisClass <- el(mnames, i)
        elNamed(prefix, argName) <- thisClass
        if(is.function(thisMethod)) {
            if(sigs.) sigs <- c(sigs, list(prefix))
            if(methods.) methods <- c(methods, list(thisMethod))
        }
        else {
            more <- Recall(thisMethod, prefix)
            if(sigs.) sigs <- c(sigs, el(more, 1))
            if(methods.) methods <- c(methods, el(more, 2))
        }
    }
    list(sigs, methods)
}

.insertCachedMethods <- function(mlist, argName, Class, fromClass, def) {
    if(is(def, "MethodsList")) {
        ## insert all the cached methods in def
        newArg <- c(argName, as.character(def@argument))
        newDefs <- def@allMethods
        newSigs <- as.list(names(newDefs))
        for(j in seq_along(newDefs))
            mlist <- Recall(mlist, newArg, c(Class, newSigs[[j]]), fromClass,
                            newDefs[[j]])
    }
    else {
        def <- .addMethodFrom(def, argName[1L], Class[1L], fromClass)
        mlist <- insertMethod(mlist, Class, argName, def, TRUE)
    }
    mlist
}

.addMethodFrom <- function(def, arg, Class, fromClass) {
    if(is(def, "MethodDefinition")) {
        ## eventually, we may enforce method definition objects
        ## If not, just leave raw functions alone (NextMethod won't work)
        def@target[[arg]] <- Class
        def@defined[[arg]] <- fromClass
    }
    def
}

## Define a trivial version of asMethodDefinition for bootstrapping.
## The real version requires several class definitions as well as
## methods for as<-
asMethodDefinition <- function(def, signature = list(.anyClassName), sealed = FALSE, fdef = def) {
  if(is.primitive(def))
    def
  else {
    value = new("MethodDefinition")
    value@.Data <- def
    classes <- .MakeSignature(new("signature"),  def, signature, fdef)
        value@target <- classes
        value@defined <- classes
    value
  }
  }

.trimMlist <- function(mlist, fromClass) {
  mlist@methods <- mlist@methods[fromClass]
  mlist@allMethods <- mlist@allMethods[fromClass]
  mlist
}

.noMlistsFlag <- TRUE
.noMlists <- function() {
   ## if this were to be dynamically variable, but
  ## it can't, IMO
  ## identical(getOption("noMlists"), TRUE)
  ## so instead
  .noMlistsFlag
}

.MlistDepTable <- new.env()
.MlistDeprecated <- function(this = "<default>", instead) {
    if(is.character(this)) {
        if(exists(this, envir = .MlistDepTable, inherits = FALSE))
            return()
        else
            assign(this, TRUE, envir = .MlistDepTable)
    }
    if(missing(this))
        msg <-"Use of the \"MethodsList\" meta data objects is deprecated."
    else if(is.character(this))
        msg <- gettextf("%s, along with other use of the \"MethodsList\" metadata objects, is deprecated.", dQuote(this))
    else
        msg <- gettextf("in %s: use of \"MethodsList\" metadata objects is deprecated.", deparse(this))
    if(!missing(instead))
      msg <- paste(msg, gettextf("use %s instead.", dQuote(instead)))
    msg <- paste(msg, "see ?MethodsList. (This warning is shown once per session.)")
    base::.Deprecated(msg = msg)
}

