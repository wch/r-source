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
    else stop("Invalid first argument: should be the name of the first argument in the dispatch")
    slot(value, "argument") <- .ArgName
    value
}

makeMethodsList <-
  function(object, level=1)
{
    mnames <- allNames(object)
    value <- new("MethodsList")
    i <- match("", mnames)
    if(!is.na(i)) {
        ## convert to ANY
        el(mnames, i) <- "ANY"
        names(object) <- mnames
    }
    if(any(duplicated(mnames)))
        stop("duplicate element names in MethodsList at level ", level, ": ",
             paste("\"", unique(mnames[duplicated(mnames)]), "\"", collapse=", "))
    for(i in seq(along=object)) {
        eli <- el(object, i)
        if(is(eli, "function")
           || is(eli, "MethodsList")) {}
        else if(is(eli, "list") ||
                is(eli, "named"))
            el(object, i) <- Recall(eli, NULL, level+1)
        else
            stop(paste("Element ", i, " at level ", level, " (class \"",
                       class(eli), "\") can't be interpreted as a function or named list", sep=""))
    }
    slot(value, "methods") <- object
    value
}

SignatureMethod <-
  ## construct a MethodsList object containing (only) this method, corresponding
  ## to the signature; i.e., such that signature[[1]] is the match for the first
  ## argument, signature[[2]] for the second argument, and so on.  The string
  ## "missing" means a match for a missing argument, and "ANY" means use this as the
  ## default setting at this level.
  ##
  ## The first argument is the argument names to be used for dispatch corresponding to
  ## the signatures.
  function(names, signature, definition)
{
    n <- length(signature)
    if(n > length(names))
        stop("arguments \"names\" and \"signature\" must have the same length")
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
    ## Checks for assertions about valid calls.
    ## See rev. 1.17 for the code before the assertions added.
    if(identical(args[1], "...")) {
        if(!identical(signature[[1]], "ANY"))
           stop("inserting method with invalid signature matching argument \"...\" to class \"",
                signature[[1]], "\"")
        args <- args[-1]
        signature <- signature[-1]
        if(length(signature) == 0)
            return(mlist)
    }
    if(length(signature) == 0)
        stop("inserting method corresponding to empty signature")
    if(!is(mlist, "MethodsList"))
        stop(paste("inserting method into non-methods-list object (class \"",
                   .class1(mlist), "\")", sep=""))
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
            current <- new("MethodsList", argument = as.name(args[2]))
        else if(is.function(current))
            current <- new("MethodsList", argument = as.name(args[2]), methods = list(ANY = current))
        elNamed(methods, Class) <-
            Recall(current, signature[-1], args[-1], def, cacheOnly)
    }
    if(cacheOnly)
        mlist@allMethods <- methods
    else {
        mlist@methods <- methods
        ## and clear the cache, inheritance may have changed
        mlist@allMethods <- list()
    }
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
             mlist = getMethodsForDispatch(f, fdef),
             fEnv = if(is(fdef, "genericFunction")) environment(fdef) else NULL,
             finalDefault = finalDefaultMethod(mlist),
             evalArgs = TRUE,
             useInherited = TRUE,  ## supplied when evalArgs is FALSE
             fdef = getGeneric(f)
 )
{
    if(!is(mlist, "MethodsList")) {
        if(is.null(f))
            stop("Invalid method sublist")
        else
            stop(paste("\"", f, "\" is not a valid generic function", sep=""))
    }
    on.exit({
        ## the C level code turns off some recursive method selection during evaluation
        ## of a call to this function.  Make sure it's turned back on if an error occurs.
        .Call("R_clear_method_selection", PACKAGE = "methods")
        ## and remove the function from the cached method data
        resetGeneric(f, fdef)
    })
    if(!is.logical(useInherited))
        stop("useInherited must be TRUE, FALSE, or a named logical vector of T/F")
    argName <- slot(mlist, "argument")
    arg <- NULL ## => don't use instance-specific inheritance
    if(evalArgs) {
        ## check for missing argument. NB: S sense, not that of R base missing()
        if(missingArg(argName, env, TRUE))
            thisClass <- "missing"
        else {
            arg <- eval(as.name(argName), env) ## DO use instance-specific inheritance
            thisClass <- .class1(arg)
        }
    }
    else
        thisClass <- get(as.character(argName), envir = env)
    if(identical(useInherited, TRUE) || identical(useInherited, FALSE))
        thisInherit <- nextUseInherited <- useInherited
    else {
        which <- match(as.character(argName), names(useInherited))
        if(is.na(which)) {
            nextUseInherited <- useInherited
            thisInherit <- TRUE
        }
        else {
            thisInherit <- useInherited[which]
            nextUseInherited <- useInherited[-which]
        }
    }
    fromClass <- thisClass ## will mark the class actually providing the method
    allMethods <- mlist@allMethods
    which <- match(thisClass, names(allMethods))
    inherited <- is.na(which)
    selection <- if(inherited) NULL else allMethods[[which]]
    if(!inherited) {
        if(is(selection, "function"))
            value <- mlist ## no change
        else {
            method <- Recall(NULL, env, selection, finalDefault = finalDefault,
                   evalArgs = evalArgs, useInherited = nextUseInherited, fdef = fdef)
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
        allSelections <- inheritedSubMethodLists(arg, fromClass, mlist, env)
        allClasses <- names(allSelections)
        method <- NULL
        if(thisInherit) for(i in seq(along = allSelections)) {
            selection <- allSelections[[i]]
            fromClass <- allClasses[[i]]
            if(is(selection, "function"))
                method <- selection
            else if(is(selection, "MethodsList")) {
                ## go on to try matching further arguments
                method <- Recall(NULL, env, selection, finalDefault = finalDefault,
                         evalArgs = evalArgs, useInherited = nextUseInherited, fdef = fdef)
                if(is(method, "EmptyMethodsList"))
                    selection <- method   ## recursive selection failed
            }
            if(!is(selection, "EmptyMethodsList"))
                break
        }
        if(is(selection, "EmptyMethodsList") && !is.null(f) && !is.null(finalDefault)) {
            ## only use the final default method after exhausting all
            ## other possibilities, at all levels.
            method <- insertMethodInEmptyList(selection, finalDefault)
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
        else if(!is.null(fEnv)) {
            ## and later a test that this selection used no conditional inheritance
            assign(".Methods", value, envir = fEnv)
        }
    }
    ## clear the error actions
    on.exit()
    value
}

emptyMethodsList <-
  function(mlist, thisClass, sublist = list()) {
    sublist[thisClass] <- list(NULL)
    new("EmptyMethodsList", argument = mlist@argument, sublist = sublist)
  }

insertMethodInEmptyList <-
  function(mlist, def) {
    value <- new("MethodsList", argument = mlist@argument)
    sublist <- mlist@sublist
    submethods <- sublist[[1]]
    if(is.null(submethods))
      sublist[[1]] <- def
    else
      sublist[[1]] <- Recall(submethods, def)
    value@allMethods <- sublist
    value
  }




finalDefaultMethod <-
  ## The real default method of this `MethodsList' object,
  ## found by going down the default branch (i.e., class `"ANY"')
  ## until either `NULL' or a function definition is found.
  ##
  ## Also works for non-generic functions, which have NULL methods list, by
  ## returning the function definition.  (This feature is used in standardGeneric to
  ## ensure that a definition for the function can be found when method searching is
  ## turned off.)
  function(mlist, fname = "NULL")
{
    if(is.null(mlist)) ## return the function, or NULL
        getFunction(fname, mustFind = FALSE)
    else {
        while(is(mlist, "MethodsList"))
            mlist <- elNamed(slot(mlist, "methods"), "ANY")
        mlist
    }
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
  if(identical(thisClass, "missing")) {
        ## no superclasses for "missing"
  }
  else {
    ## search in the superclasses, but don't use inherited methods
    ## There are two cases:  if thisClass is formally defined, use its
    ## superclasses.  Otherwise, look in the subclasses of those classes for
    ## which methods exist.
    if(isClass(thisClass)) {
      ## for consistency, order the available methods by
      ## the (depth first) order of the superclasses of thisClass
      superClasses <- names(getExtends(getClass(thisClass)))
      classes <- superClasses[!is.na(match(superClasses, classes))]
      for(which in seq(along=classes)) {
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
      for(which in seq(along = classes)) {
        tryClass <- el(classes, which)
        if(isClass(tryClass) &&
           !is.na(match(thisClass, names(getSubclasses(getClass(tryClass))))))
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
  ## by `names'.  The classes not specified by `signature' will be `"ANY"' in the
  ## value.
  ##
  ## The formal arguments of `fun' must agree with `names' (usually the formal arguments
  ## of the generic function) as well, and `matchSignature' checks this.
  function(signature, fun)
{
    if(!is(fun, "genericFunction"))
        stop("Internal error: matchSignature was not called with a generic function object")
    names <- fun@signature
    sigClasses <- as.character(signature)
    if(length(sigClasses) > 0)
        unknown <- !sapply(sigClasses, function(x)isClass(x))
    else
        unknown <- logical()
    signature <- as.list(signature)
    if(length(sigClasses) != length(signature))
        stop("signature argument doesn't look like a legitimate signature (vector of single class names)")
    if(any(unknown)) {
        unknown <- unique(sigClasses[unknown])
        warning("Class ", paste("\"", unknown, "\"", sep="", collapse = ", "), " not defined")
    }
    ## construct a function call with the same naming pattern as signature
    fcall <- do.call("call", c("fun", signature))
    ## match the call to the supplied function
    smatch <- match.call(fun, fcall)
    snames <- names(smatch)[-1]
    which <- match(snames, names)
    if(any(is.na(which)))
        stop(paste("Invalid names in signature:",
                   paste(snames[is.na(which)], collapse = ", ")))
    n <- length(names)
    value <- rep("ANY", n)
    value[which] <- sigClasses
    while(n > 1 && value[n] == "ANY")
        n <- n-1
    length(value) <- n
    value
}

showMlist <-
  ## Prints the contents of the MethodsList.  If `includeDefs' the signatures and the
  ## corresonding definitions will be printed; otherwise, only the signatures.
  ##
  ## If `includeDefs' is `TRUE', the currently known inherited methods are included;
  ## otherwise, only the directly defined methods.
function(mlist, includeDefs = TRUE, inherited = TRUE, classes = NULL, useArgNames = TRUE,
         printTo = stdout()) {
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
    keep <- !sapply(signatures, function(x, y)all(is.na(match(x, y))), classes)
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
      for(i in 1:n) {
        sigi <- signatures[[i]]
        labels[[i]] <- paste(args[[i]], " = \"", sigi, "\"",
                             sep = "", collapse = ", ")
      }
    }
    else {
      for(i in 1:n)
        labels[[i]] <- paste(signatures[[i]], collapse = ", ")
    }
    for(i in seq(length=length(methods))) {
      cat(file=con, labels[[i]])
      method <- methods[[i]]
      if(includeDefs) {
        cat(file=con, ":\n")
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

promptMethods <-
  ## generate information in the style of `prompt' for the methods of the generic
  ## named `f'.
  ##
  ## `filename' can be a logical or the name of a file to print to.  If `file' is `FALSE',
  ## the methods skeleton is returned, to be included in other printing (typically,
  ## the output from `prompt'.
  function(f, filename = TRUE, methods) {
      paste0 <- function(...)paste(..., sep="")
      packageString = ""
      if(missing(methods)) {
          where <- find(mlistMetaName(f))
          if(length(where) == 0)
              stop("No methods found for generic \"", f, "\"")
          where <- where[1]
          methods <- getMethods(f, where)
          if(where != 1)
              packageString <- paste("in package", getPackageName(where))
      }
      object <- linearizeMlist(methods, FALSE)
      methods <- object@methods; n <- length(methods)
      args <- object@arguments
      signatures <- object@classes
      labels <- character(n)
      aliases <- character(n)
      fullName <- topicName("methods", f)
      for(i in seq(length = n)) {
          labels[[i]] <- paste(args[[i]], signatures[[i]], collapse = ", ", sep = " = ")
          aliases[[i]] <- paste0("\\alias{", topicName("method", c(f, signatures[[i]])),
                                "}")
      }
      text <- paste0(aliases, "\n\\item{", labels, "}{ ~~describe this method here }")
      text <- c("\\section{Methods}{\\describe{", text, "}}")
      aliasText <- paste0("\\alias{", fullName, "}")
      endText <- c("\\keyword{methods}", "\\keyword{ ~~ other possible keyword(s)}")
      if(identical(filename, FALSE))
          return(c(aliasText, text))
      beginText <-  c(paste0("\\name{", fullName, "}"), paste0("\\docType{methods}"),
                paste("\\title{ ~~ Methods for Function", f, packageString, "~~}"))
      if(identical(filename, TRUE))
          filename <- paste0(fullName, ".Rd")
      text <-c(beginText, aliasText, text, endText)
      cat(text, file = filename, sep="\n")
      filename
  }


linearizeMlist <-
    ## Undo the recursive nature of the methods list, making a list of function
    ## defintions, with the names of the list being the corresponding signatures
    ## (designed for printing; for looping over the methods, use `listFromMlist' instead).
    ##
    ## The function calls itself recursively.  `prev' is the previously selected class names
    ##
    ## If argument `classes' is provided, only signatures containing one of these classes
    ## will be included.
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
        argname <- mlist@argument
        for(i in seq(along = cnames)) {
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
                warning("Skipping methods list element ", paste(cnames[i], collapse = ", "),
                        " of unexpected class \"", .class1(mi),
                        "\"\n\n")
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
  function(mlist, prefix = list())
{
    methodSlot <- slot(mlist, "methods")
    mnames <- names(methodSlot)
    argName <- as.character(slot(mlist, "argument"))
    sigs <- list()
    methods <- list()
    for(i in seq(along = methodSlot)) {
        thisMethod <- el(methodSlot, i)
        thisClass <- el(mnames, i)
        elNamed(prefix, argName) <- thisClass
        if(is.function(thisMethod)) {
            sigs <- c(sigs, list(prefix))
            methods <- c(methods, list(thisMethod))
        }
        else {
            more <- Recall(thisMethod, prefix)
            sigs <- c(sigs, el(more, 1))
            methods <- c(methods, el(more, 2))
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
        for(j in seq(along = newDefs))
            mlist <- Recall(mlist, newArg, c(Class, newSigs[[j]]), fromClass,
                            newDefs[[j]])
    }
    else {
        def <- .addMethodFrom(def, argName[1], Class[1], fromClass)
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
asMethodDefinition <- function(def, signature = list())
    def
