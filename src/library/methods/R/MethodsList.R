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
  function(mlist, signature, args, def)
{
    if(length(signature) == 0)
        signature <- "ANY"
    Class <- el(signature, 1)
    if(is.null(mlist))
        mlist <- new("MethodsList", argument = as.name(args[1]))
    else if(is.function(mlist))
        mlist <- new("MethodsList", argument = as.name(args[1]), methods = list(ANY = mlist))
    current <- elNamed(slot(mlist, "methods"), Class)
    if(length(signature) == 1 && !is(current, "MethodsList")) {
        methods <- slot(mlist, "methods")
        which <- match(Class, names(methods))
        if(is.na(which)) {
            if(!is.null(def))
                elNamed(methods, Class) <- def
            ## else, no change
        }
        else {
            if(is.null(def))
                ## delete the method
                methods <- methods[-which]
            else
                el(methods, which) <- def
        }
        slot(mlist, "methods") <- methods
        mlist
    }
    else {
        ## recursively merge
        elNamed(slot(mlist, "methods"), Class) <-
            Recall(current, signature[-1], args[-1], def)
        mlist
    }
}

MethodsListSelect <-
  ## select the element of a MethodsList object corresponding to the
  ## actual arguments.
  ##
  ## Works recursively.  At each level finds an argument name from the current `mlist'
  ## object, and evaluates this argument (if it is not missing), then uses the
  ## `data.class' of the result to select an element of `mlist'.  If such an element
  ## exists and is another `MethodsList' object, `MethodsListSelect'  calls itself recursively
  ## to resolve using further arguments.  Matching includes using a default selection or
  ## a method specifically linked to class `"missing"'.  Once a function is found, it
  ## is returned as the value.  If matching fails, an error occurs.
    function(fname = NULL, ev, mustFind = TRUE,
             mlist = getMethodsForDispatch(fname),
             arg, thisClass, argName = slot(mlist, "argument")
 )
{
    ## the C level code turns off some recursive method selection during evaluation
    ## of a call to this function.  Make sure it's turned back on, even if an error occurs.
    if(!is(mlist, "MethodsList")) {
        if(is.null(fname))
            stop("Invalid method sublist")
        else
            stop(paste("\"", fname, "\" is not a valid generic function", sep=""))
    }
    on.exit(.Call("R_clear_method_selection", PACKAGE = "methods"))## in case of error
    ## in calls from the C method selection code,  arg and thisClass are already known.
    if(missing(arg)) {
        missingThisArg <-
            eval(substitute(missing(ARGNAME),  list(ARGNAME = argName)), ev)
        if(missingThisArg) {
            arg <- NULL
            thisClass <- "missing"
        }
        else {
            arg <- eval(as.name(argName), ev)
            thisClass <- data.class(arg)
            ## evaluate the argument to try class matching
            ## May also signal inexact match by storing .Classes in ev
        }
    }
    selection <- elNamed(slot(mlist, "allMethods"), thisClass)
    inherited <- is.null(selection)
    if(inherited) {
        ## look for inherited methods: should only happen once per signature
        ##
        ## First, get all the superclasses that have method(s) defined
        allSelections <- matchArg(arg, thisClass, mlist, ev)
        method <- NULL
        for(selection in allSelections) {
            if(is(selection, "function"))
                method <- selection
            else if(is(selection, "MethodsList")) {
                method <- Recall(NULL, ev, FALSE, selection)
                if(is.null(method))
                    selection <- NULL   ## recursive selection failed
            }
            if(!is.null(selection))
                break
        }
        if(is.null(selection) && mustFind) {
            ## try the overall default
            method <- selection <- finalDefaultMethod(mlist)
            if(is.null(selection))
                stop(paste("Unable to match argument \"", argName, "\" to methods", sep=""))
        }
    }
    else {                              ## this code is generally not used (C implementaton does non-inherited case)
        if(is(selection, "function"))
            method <- selection
        else {
            method <- Recall(NULL, ev, FALSE, selection)
            if(is.null(method))
                selection <- NULL       ## recursive selection failed
        }
    }
    if(inherited && !is.null(selection)
       && !is.null(fname)) {
        ## top-level call:  lower-level inherited methods (for which fname is NULL)
        ## are inserted into the corresponding element of the mlist by the C code
        ## in do_dispatch.
        elNamed(slot(mlist, "allMethods"), thisClass) <- selection
        fdef <- getFromMethodMetaData(fname)
        assign(".Methods", envir = environment(fdef), mlist)
    }
    on.exit()                           # clear error action
    method
}

MethodsListDispatch <-
  function(fname, ev, mustFind = TRUE)
  ## either the S language version:
  ## MethodsListSelect(fname, ev, mustFind)
  ## or the C version:
    .Call("R_methods_list_dispatch", fname, ev, mustFind,
          PACKAGE = "methods")



finalDefaultMethod <-
  ## The real default method of this `MethodsList' object,
  ## found by going down the default branch (i.e., class `"ANY"')
  ## until either `NULL' or a function definition is found.
  ##
  ## Also works for non-generic functions, which have NULL methods list, by
  ## returning the function definition.  (This feature is used in standardGeneric to
  ## ensure that a definition for the function can be found when method searching is
  ## turned off.)
  function(mlist, fname = "")
{
    if(is.null(mlist)) ## not a generic function; will fail if fname missing (& should)
        getFunction(fname)
    else {
        while(is(mlist, "MethodsList"))
            mlist <- elNamed(slot(mlist, "methods"), "ANY")
        mlist
    }
}


matchArg <-
  ## Utility function to match the object to the elements of a methods list.
  ##
  ## If `thisClass' is the name of one of the methods, that method is returned (including
  ## the case of `"missing"' for missing arguments.
  ##
  ## If direct matching fails, the function looks for an inherited match, but only among
  ## the methods that are not themselves inherited.  (Inherited methods when found are
  ## stored in the session copy of the methods list, but they themselves should not be
  ## used for finding inherited matches, because an erroneous match could be found depending
  ## on which methods were previously used.  See the detailed discussion of methods.)
  function(object, thisClass, mlist, ev)
{
    methods <- slot(mlist, "allMethods")## both direct and inherited
    defaultMethod <- elNamed(methods, "ANY")## maybe NULL
    classes <- names(methods)
    which <- match(thisClass, classes)
    value <- list()
    if(!is.na(which))
        elNamed(value, thisClass) <- el(methods, which)
    else if(thisClass == "missing")
        ## no superclasses for "missing"!
        elNamed(value, "ANY") <- defaultMethod
    else {
        ## search in the superclasses, but don't use inherited methods
        methods <- slot(mlist, "methods")## only direct methods
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
                if(is(object, tryClass)) {
                    elNamed(value, tryClass) <- elNamed(methods, tryClass)
                }
            }
        }
        else {
            for(which in seq(along = classes)) {
                tryClass <- el(classes, which)
                if(isClass(tryClass) && is(object, tryClass))
                    elNamed(value, tryClass) <- el(methods, which)
            }
        }
    }
    if(length(value) == 0 && !is.null(defaultMethod))
        elNamed(value, "ANY") <- defaultMethod
    value
}

matchArgClass <-
  ## utility function to match the superClasses of the specified class to the elements of a methods list.
  function(Class, classes, methods)
{
    ## test for Class extending one of the classes.  Corresponds to matchArg
    ## and to a depth-first search for inherited methods, because the elements
  ## are returned in the order of the extends (asserted to correspond to depth-first traversal
  ## of the tree of is relations).
    superClasses <- names(getExtends(getClass(Class)))
    which <- match(superClasses, classes)
    foundClasses <- superClasses[!is.na(which)] ## here's where the order is depth-first
    value <- list()
    for(cl in foundClasses)
      elNamed(value, cl) <- elNamed(methods, cl)
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
  function(names, signature, fun)
{
    sigClasses <- as.character(signature)
    signature <- as.list(signature)
    if(length(sigClasses) != length(signature))
        stop("signature argument doesn't look like a legitimate signature (vector of single class names)")
    ## construct a function call with the same naming pattern as signature
    fcall <- do.call("call", c("fun", signature))
    ## match the call to the supplied function
    smatch <- match.call(fun, fcall)
    snames <- names(smatch)[-1]
    which <- match(snames, names)
    if(any(is.na(which)))
        stop(paste("Signature names did not match supplied argument names:",
                   paste(snames[is.na(which)], collapse = ", ")))
    test <- all.equal.character(names, formalArgs(fun))
    if(is.character(test))
        warning(paste("The supplied names and the formal arguments of the function differ:",
                      test))
    n <- length(names)
    value <- rep("ANY", n)
    value[which] <- sigClasses
    while(value[n] == "ANY")
        n <- n-1
    length(value) <- n
    value
}

showMlist <-
  ## Prints the contents of the MethodsList.  If `includeDefs' the signatures and the
  ## corresonding definitions will be printed; otherwise, only the signatures.
  ##
  ## The function calls itself recursively.  `prev' is the previously selected classes.
  function(mlist, includeDefs = TRUE, inherited = TRUE, prev = character())
{
    showThisMethod <-
        function(mi,includeDefs, ci)
        {
            if(is.function(mi)) {
                cat(paste(ci, collapse=", "))
                if(includeDefs) {
                    cat(":\n  ")
                    print(mi)
                }
                cat("\n")
            }
            else if(is(mi, "MethodsList")) {
                methods <- slot(mi, "methods")
                classes <- names(methods)
                for(i in seq(along=methods))
                    Recall(el(methods, i), includeDefs, c(ci, el(classes,i)))
            }
            else
                cat(paste(ci, collapse = ", "),
                    ": <<Unexpected element of class \"", data.class(mi),
                    "\"\n\n", sep="")
        }
    methods <- slot(mlist, "methods")
    cnames <- names(methods)
    if(inherited) {
        allMethods <- slot(mlist, "allMethods")
        anames <- names(allMethods)
        inh <- is.na(match(anames, cnames))
        if(any(inh)) {
            anames <- paste(anames[inh], "(inherited)")
            methods[anames] <- allMethods[inh]
            cnames <- c(cnames, anames)
        }
    }
    for(i in seq(along = cnames)) {
        mi <- el(methods, i)
        ci <- c(prev, el(cnames, i))
        showThisMethod(mi, includeDefs, ci)
    }
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
