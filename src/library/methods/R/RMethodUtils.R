## The real version of makeGeneric, to be installed after there are some
## generic functions to boot the definition (in particular, coerce and coerce<-)

.makeGeneric <-
## Makes a generic function object corresponding to the given function name.
## and definition.
  function(f, fdef,
           fdefault = getFunction(f, generic = FALSE, mustFind = FALSE),
           group = list(), valueClass = character(), package, signature = NULL,
           genericFunction = NULL) {
      ## give the function a new environment, to cache methods later
      ev <- new.env()
      parent.env(ev) <- environment(fdef)
      environment(fdef) <- ev
      assign(".Generic", f, envir = ev)
      assign(".Methods", NULL, envir = ev)
      if(length(valueClass)>0)
          fdef <- .ValidateValueClass(fdef, f, valueClass)
      group <- .asGroupArgument(group)
      if(is.null(genericFunction))
          value <- new("genericFunction")
      else 
          value <- genericFunction
      value@.Data <- fdef
      value@generic <- f
      value@group <- group
      value@valueClass <- valueClass
      value@package <- package
      args <- formalArgs(fdef)
      if(is.null(signature))
          signature <- args
      else if(any(is.na(match(signature, args))))
          stop(paste("Non-arguments found in the signature:",
                     paste(signature[is.na(match(signature, args))], collapse = ", ")))
      dots <- match("...", signature)
      if(!is.na(dots)) ## ... is not currently supported in method signatures
          signature <- signature[-dots]
      if(length(signature) == 0)
          stop("No suitable arguments to dispatch methods in this function")
      value@signature <- signature
      name <- signature[[1]]
      if(is.null(fdefault))
          methods <- MethodsList(name)
      else {
          if(!identical(formalArgs(fdefault), formalArgs(fdef)) &&
             !is.primitive(fdefault))
              stop("The formal arguments of the generic function for \"",
                   f, "\" (",
                   paste(formalArgs(fdef), collapse = ", "),
                   ") differ from those of the non-generic to be used as the default, (",
                   paste(formalArgs(fdefault), collapse = ", "), ")")
          methods <- MethodsList(name, asMethodDefinition(fdefault))
      }
      value@default <- methods
      value@skeleton <- generic.skeleton(f, fdef, fdefault)
      value
  }

## the bootstrap version: "#----" brackets lines that replace parts of the real version
makeGeneric <-
      function(f, fdef,
           fdefault = getFunction(f, generic = FALSE, mustFind = FALSE),
           group = list(), valueClass = character(), package, signature = NULL,
           genericFunction = NULL) {
      ## give the function a new environment, to cache methods later
      ev <- new.env()
      parent.env(ev) <- environment(fdef)
      environment(fdef) <- ev
      assign(".Generic", f, envir = ev)
      assign(".Methods", NULL, envir = ev)
      if(length(valueClass)>0)
          fdef <- .ValidateValueClass(fdef, f, valueClass)
      group <- .asGroupArgument(group)
###--------
      value <- fdef
      if(is.null(genericFunction))
          class(value) <- "genericFunction"
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
          stop(paste("Non-arguments found in the signature:",
                     paste(signature[is.na(match(signature, args))], collapse = ", ")))
      dots <- match("...", signature)
      if(!is.na(dots)) ## ... is not currently supported in method signatures
          signature <- signature[-dots]
      if(length(signature) == 0)
          stop("No suitable arguments to dispatch methods in this function")
###--------
      slot(value, "signature", FALSE) <- signature
###--------
      name <- signature[[1]]
      if(is.null(fdefault))
          methods <- MethodsList(name)
      else
          methods <- MethodsList(name, asMethodDefinition(fdefault))
###--------
      slot(value, "default", FALSE) <- methods
      slot(value, "skeleton", FALSE) <- generic.skeleton(f, fdef, fdefault)
###--------
      value
  }
    

makeStandardGeneric <-
  ## a utility function that makes a valid function calling standardGeneric for name f
  ## Works (more or less) even if the actual definition, fdef, is not a proper function,
  ## that is, it's a primitive or internal
  function(f, fdef) {
    fgen <- fdef
    body(fgen) <- substitute(standardGeneric(FNAME), list(FNAME=f))
    ## detect R specials and builtins:  these don't provide an argument list
    if(typeof(fdef) != "closure") {
      ## Look in a list of pre-defined functions (and also of functions for which
      ## methods are prohibited)
      fgen <- elNamed(.BasicFunsList, f)
      if(identical(fgen, FALSE))
        stop(paste("Special function \"", f, "\" is not permitted to have methods", sep=""))
      if(is.null(fgen)) {
        warning(paste("Special function \"", f, "\" has no known argument list; will assume \"(x, ...)\"", sep=""))
        ## unknown
        fgen <- function(x, ...) {}
      }
      else {
        message("Making a generic for special function \"", f, "\"")
        setPrimitiveMethods(f, fdef, "reset", fgen, NULL)
      }
      ## Note that the body of the function comes from the list.  In a few cases ("$"),
      ## this body is not just a call to standardGeneric
    }
    fgen
  }

generic.skeleton <-
  function(name, fdef, fdefault) {
    anames <- formalArgs(fdef)
    skeleton <- lapply(as.list(c(name, anames)), as.name)
    ## any arguments after "..." have to be named
    dots <- match("...", anames)
    if(!is.na(dots) && dots < length(anames)) {
      anames[1:dots] <- ""
      names(skeleton) <- c("", anames)
    }
    if(is.null(fdefault)) {
      fdefault <- fdef
      body(fdefault) <- substitute(stop(MESSAGE), list(MESSAGE=
          paste("Invalid call in method dispatch to \"",name, "\" (no default method)",
                sep="")))
      environment(fdefault) <- NULL
    }
    skeleton[[1]] <- fdefault
    as.call(skeleton)
 }


defaultDumpName <-
  ## the default name to be used for dumping a method.
  function(generic, signature) {
  if(missing(signature))
    paste(generic, "R", sep=".", collapse =".")
  else
    paste(generic, paste(signature, collapse ="."), "R", sep=".")
}


getAllMethods <-
  ## a generic function (with methods) representing the merge of all the methods
  ## for `f' on the specified packages (anything on the current search path by default).
  ##
  ## If the generic `f' has a group generic, methods for this group generic (and further
  ## generations of group generics, if any) are also merged.  The merging rule is as follows:
  ## each generic is merged across packages, and the group generics are then merged, finally
  ## adding the directly defined methods of `f'.
  ##
  ## The effect of the merging rule is that any method directly defined for `f' on any
  ## included package overrides a method for the same signature defined for the group generic;
  ## similarly for the group generic and its group, if any, etc.
  ##
  ## For `f' or for a specific group generic, methods override in the order of the packages
  ## being searched.  A method for a particular signature on a particular package overrides
  ## any methods for the same signature on packages later on in the list of packages being
  ## searched.
  ##
  ## The slot "allMethods" of the merged methods list is set to a copy of the methods slot;
  ## this is the slot where inherited methods are stored.
  function(f, fdef = getGeneric(f, TRUE), libs = search()) {
      if(is(fdef, "genericFunction"))
          deflt <- finalDefaultMethod(fdef@default)
      else if(is.primitive(fdef)) {
          deflt <- fdef
          fdef <- getGeneric(f, TRUE)
      }
      else
          stop("Invalid \"fdef\" argument supplied to getAllMethods; expected either a genericFunction object or a primitive function, got an object of class \"",
               class(fdef), "\"")
      primCase <- is.primitive(deflt)
      groups <- getGroup(fdef, TRUE)
    ## when this function is called from methodsListDispatch (via C code),
    ## a skeleton version is assigned to prevent recursive loops:  remove this
    ## in case of errros in getAllMethods
    on.exit({message("An error occurred in collecting methods for function \"",
                     f,"\", perhaps from a C-level dispatch"); resetGeneric(f, fdef)})
    ## initialize with a check for basic functions (primitives)
      ## For all others, the initial value of methods will be NULL
    methods <- elNamed(.BasicFunsList, f)
      if(!is.null(methods))  # it better be a genericFunction object
          methods <- methods@default
    funs <- c(fdef, groups)
    for(fun in rev(funs)) {
        genericLabel <- if(primCase && !identical(fun, fdef)) f else character()
        for(where in rev(libs)) {
            mw <- getMethodsMetaData(fun, where)
            if(!is.null(mw))
                methods <- mergeMethods(methods, mw, genericLabel)
        }
    }
    ev <- environment(fdef)
    if(is.null(methods)) ## after removeMethods, e.g.
        methods <- fdef@default
    if(!is(methods, "EmptyMethodsList")) {
        methods <- setAllMethodsSlot(methods)
        assign(".Methods", methods, ev)
    }
    ## primitives are pre-cached in the method metadata (because
    ## they are not visible as generic functions from the C code).
    if(primCase)
      setPrimitiveMethods(f, deflt, "set", fdef)
    ## cancel the error cleanup
    on.exit()
    methods
  }


mergeMethods <-
  ## merge the methods in the second MethodsList object into the first,
  ## and return the merged result.
  function(m1, m2, genericLabel = character()) {
      if(length(genericLabel) > 0 && is(m2, "MethodsList"))
          m2 <- .GenericInPrimitiveMethods(m2, genericLabel)
    if(is.null(m1) || is(m1, "EmptyMethodsList"))
      return(m2)
    tmp <- listFromMlist(m2)
    sigs <- el(tmp, 1)
    methods <- el(tmp, 2)
    for(i in seq(along=sigs)) {
      sigi <- el(sigs, i)
      args <- names(sigi)
      m1 <- insertMethod(m1, as.character(sigi), args, el(methods, i), FALSE)
    }
    m1
  }

setAllMethodsSlot <- function(mlist) {
  ## initialize the "allMethods" slot to the "methods" slot, and
  ## do the same recursively to any contained MethodsList objects
  ##
  ## The current contents of the allMethods slot are ignored, so calling
  ## setAllMethodsSlot either initializes or re-intializes the object, removing
  ## any inherited methods stored in allMethods.
  if(is.null(mlist)) return(mlist)
  methods <- mlist@methods
  mnames <- allNames(methods)
  modified <- FALSE
  for(i in seq(along=methods)) {
    method <-methods[[i]]
    if(is(method, "MethodsList")) {
      methods[[i]] <- Recall(method)
      modified <- TRUE
    }
  }
  mlist@allMethods <- methods
  if(modified)
    mlist@methods <- methods
  mlist
}

doPrimitiveMethod <-
  ## do a primitive call to builtin function `name' the definition and call
  ## provided, and carried out in the environment `ev'.
  ##
  ## A call to `doPrimitiveMethod' is used when the actual method is a .Primitive.
  ##  (because primitives don't behave correctly as ordinary functions,
  ## not having either formal arguments nor a function body).
  function(name, def, call = sys.call(-1), ev = sys.frame(sys.parent(2)))
{
  cat("called doPrimitiveMethod\n\n")
    ## Store a local version of function `name' back where the current version was
    ## called.  Restore the previous state there on exit, either removing or re-assigning.
    if(exists(name, envir=ev, inherits=FALSE)) {
        prev <- get(name, envir=ev)
        on.exit(assign(name, prev, envir = ev))
    }
    else
        on.exit(rm(list=name, envir=ev))
    assign(name, def, envir = ev)
    eval(call, ev)
}

conformMethod <-
  function(signature, mnames, fnames, f = "<unspecified>")
{
    ## Desirable, but hard:  arrange for "missing" to be valid for "..." in a signature
    ## (needs a change to low-level dispatch code).
    ## Until then, allow an omitted "..." w/o checking
    if(is.na(match("...", mnames)) && !is.na(match("...", fnames)))
        fnames <- fnames[-match("...", fnames)]
    omitted <- is.na(match(fnames, mnames))
    if(!any(omitted))
        return(signature)
    label <- paste("In method for function \"", f,"\": ", sep="")
    if(!all(diff(seq(along=fnames)[!omitted]) > 0))
        stop(label, "Formal arguments in method and function don't appear in the same order")
    signature <- c(signature, rep("ANY", length(fnames)-length(signature)))
    if(any(is.na(match(signature[omitted], c("ANY", "missing"))))) {
        bad <- omitted & is.na(match(signature[omitted], c("ANY", "missing")))
        stop(label, "Formal arguments omitted in the method definition cannot be in the signature (",
                   paste(fnames[bad], " = \"", signature[bad], "\"", sep = "", collapse = ", "),
             ")")
    }
    else if(!all(signature[omitted] == "missing")) {
        message(label, "Expanding the signature to include omitted arguments in definition: ",
            paste(fnames[omitted], "= \"missing\"",collapse = ", "))
        signature[omitted] <- "missing"
    }
    ## remove trailing "ANY"'s
    n <- length(signature)
    while(identical(signature[[n]], "ANY"))
        n <- n - 1
    length(signature) <- n
    signature
}

rematchDefinition <- function(definition, generic, mnames, fnames, signature) {
    added <- is.na(match(mnames, fnames))
    if(!any(added)) {
        ## the formal args of the method must be identical to generic
        formals(definition, envir = environment(definition)) <- formals(generic)
        return(definition)
    }
    dotsPos <- match("...", fnames)
    if(is.na(dotsPos))
        stop("Methods can add arguments to the generic only if \"...\" is an argument to the generic")
    ## pass down all the names in common between method & generic, plus "..."
    ## even if the method doesn't have it.  But NOT any arguments having class
    ## "missing" implicitly (see conformMethod)
    useNames <- !is.na(match(fnames, mnames)) | fnames == "..."
    newCall <- lapply(c(".local", fnames[useNames]), as.name)
    ## leave newCall as a list while checking the trailing args
    if(dotsPos < length(fnames)) {
        ## trailing arguments are required to match.  This is a little stronger
        ## than necessary, but this is a dicey case, because the argument-matching
        ## may not be consistent otherwise (in the generic, such arguments have to be
        ## supplied by name).  The important special case is replacement methods, where
        ## value is the last argument.
        ntrail <- length(fnames) - dotsPos
        trailingArgs <- fnames[seq(to = length(fnames), length = ntrail)]
        if(!identical(mnames[seq(to = length(mnames), length = ntrail)],
                      trailingArgs))
            stop(paste("Arguments after \"...\" in the generic (",
                       paste(trailingArgs, collapse=", "),
                       ") must appear in the method, in the same place at the end of the argument list",
                       sep=""))
        newCallNames <- character(length(newCall))
        newCallNames[seq(to =length(newCallNames), length = ntrail)] <-
            trailingArgs
        names(newCall) <- newCallNames
    }
    newCall <- as.call(newCall)
    newBody <- substitute({.local <- DEF; NEWCALL},
                          list(DEF = definition, NEWCALL = newCall))
    body(generic, envir = environment(definition)) <- newBody
    generic
}

unRematchDefinition <- function(definition) {
    ## undo the effects of rematchDefiniition, if it was used.
    ## Has the obvious disadvantage of depending on the implementation.
    ## If we considered the rematching part of the API, a cleaner solution
    ## would be to include the "as given to setMethod" definition as a slot
    bdy <- body(definition)
    if(identical(class(bdy),"{") && length(bdy) > 1) {
        bdy <- bdy[[2]]
        if(identical(class(bdy), "<-") &&
           identical(bdy[[2]], as.name(".local")))
            definition <- bdy[[3]]
    }
    definition
}
    
getGeneric <-
  ## return the definition of the function named f as a generic.
  ##
  ## If there is no definition in the current search list, throws an error or returns
  ## NULL according to the value of mustFind.
### TO BE CHANGED:  Needs a package argument, which should be passed down to R_getGeneric
  function(f, mustFind = FALSE) {
    if(is.function(f) && is(f, "genericFunction"))
        return(f)
    value <- .Call("R_getGeneric", f, FALSE, PACKAGE = "methods")
    if(is.null(value) && exists(f, "package:base", inherits = FALSE)) {
      ## check for primitives
      baseDef <- get(f, "package:base")
      if(is.primitive(baseDef)) {
          value <- elNamed(.BasicFunsList, f)
          if(is.function(value) && !is(value, "genericFunction")) {
              ## initialize the generic function in the list on base
              value <- makeGeneric(f, makeStandardGeneric(f, value), value, package = "base")
              elNamed(.BasicFunsList, f) <<- value
              mlist <- elNamed(.BasicFunsMethods, f)
              if(!is.null(mlist)) {
                  ## initialize the methods for this generic with precomputed mlist
                  where <- find(".BasicFunsMethods")
                  assign(mlistMetaName(value), mlist, where)
              }
          }
      }
  }
    if(is.function(value))
        value
    else if(mustFind)
        ## the C code will have thrown an error if f is not a single string
        stop(paste("No generic function defined for \"",f,"\"", sep=""))
    else
      NULL
  }

getGroup <-
  ## return the groups to which this generic belongs.  If `recursive=TRUE', also all the
  ## group(s) of these groups.
  function(fdef, recursive = FALSE)
{
    if(is.character(fdef))
        fdef <- getGeneric(fdef)
    if(is(fdef, "genericFunction"))
        group <- fdef@group
    else
        group <- list()
    if(recursive && length(group) > 0) {
        allGroups <- group
        for(gp in group) {
            fgp <- getGeneric(gp)
            if(is(fgp, "groupGenericFunction"))
                allGroups <- c(allGroups, Recall(fgp, TRUE))
        }
        if(length(allGroups)>1) {
            ids <- sapply(allGroups, mlistMetaName)
            allGroups <- allGroups[!duplicated(ids)]
        }
        allGroups
    }
    else
        group
}

getMethodsMetaData <-
  ## get the methods meta-data for function f on database where
  function(f, where = -1) {
    mname <- mlistMetaName(f)
    if(identical(where, -1)) {
        if(exists(mname))
            get(mname)
        else
            NULL
    }
    else {
        if(exists(mname, where = where, inherits = FALSE))
            get(mname, where)
        else
            NULL
    }
  }

assignMethodsMetaData <-
  ## assign value to be the methods metadata for generic f on database where.
  ## Also resets the generic to force recomputation of session information.
  function(f, value, fdef, where, deflt = finalDefaultMethod(fdef@default)) {
    assign(mlistMetaName(fdef), value, where)
    resetGeneric(f, fdef)
    if(is.primitive(deflt))
        setPrimitiveMethods(f, deflt, "reset", fdef, NULL)
    if(is(fdef, "groupGenericFunction")) # reset or turn on members of group
        cacheGenericsMetaData(fdef@generic, package = fdef@package)
  }

mlistMetaName <-
  ## name mangling to simulate metadata for a methods definition.
  function(name = "", package = "") {
      if(is(name, "genericFunction"))
          methodsPackageMetaName("M", paste(name@generic, name@package, sep=":"))
      else if(missing(name))
          methodsPackageMetaName("M","")
      else if(is.character(name)) {
          if(nchar(package))
             methodsPackageMetaName("M", paste(name, package, sep=":"))
          else {
              fdef <- getGeneric(name)
              if(is(fdef, "genericFunction"))
                  methodsPackageMetaName("M", paste(fdef@generic, fdef@package, sep=":"))
              else
                  stop("The methods object name for \"", name, "\" must include the name of the package that contains the generic function, but there is no generic function of this name")
          }
      }
      else
          stop(paste("No way to associate a generic function with an object of class \"",
                     class(name), "\"", sep=""))
  }

getGenerics <-
  function(where = seq(along=search()), searchForm = FALSE) {
    if(is.environment(where)) where <- list(where)
    these <- character()
    for(i in where) {
      these <- c(these, objects(i, all=TRUE))
    }
    metaNameUndo(unique(these), prefix = "M", searchForm = searchForm)
  }

allGenerics <- getGenerics

is.primitive <-
  function(fdef)
    switch(typeof(fdef),
           "special" = , "builtin" = TRUE,
           FALSE)
    

cacheMetaData <-
  function(where, attach = TRUE) {
    ## a collection of actions performed on attach or detach
    ## to update class and method information.
    generics <- getGenerics(where)
    if(length(generics)>0)
      cacheGenericsMetaData(generics, attach, where)
  }

cacheGenericsMetaData <- function(generics, attach = TRUE, where, package) {
    if(missing(package)) {
        if(is(generics, "ObjectsWithPackage"))
            package <- generics@package
        else
            package <- character(length(generics))
    }
    for(i in seq(along=generics)) {
        f <- generics[[i]]
        ## FIXME:  should use package information in getGeneric call: pkg <- package[[i]]
        ## Some tests: don't cache generic if no methods defined
        fdef <- getGeneric(f)
        if(!is(fdef, "genericFunction"))
            next
        if(attach) {
            if(is.null(elNamed(.BasicFunsList, f))) {
                if( !isGeneric(fdef@generic))
                    next
                methods <- getMethods(f)
                if(!is.null(methods))
                    methods <- methods@methods
                if(length(methods)==0) {
                    resetGeneric(f, fdef)
                    next
                }
            }
            ## else, this is a primitive generic, so the assertion is that we
            ## found methods for it, or for one of its group generics.  So go
            ## ahead and turn method dispatch on
        }
        ## find the function.  It may be a generic, but will be a primitive
        ## if the internal C code is being used to dispatch methods for primitives.
        ## It may also be NULL, if no function is found (when detaching the only
        ## package defining this function, for example).
        deflt <- finalDefaultMethod(fdef@default)
        if(is.primitive(deflt)) {
            if(attach) code <- "reset"
            else {
                code <- "clear"
                if(!missing(where)) {
                    dbs <- find(mlistMetaName(f))
                    if(is.numeric(where))
                        where <- search()[where]
                    ## are there other methods for f still left?
                    if(is.environment(where)){
                        if(length(dbs)>1)
                            code <- "reset"
                    }
                    else if(any(is.na(match(dbs, where))))
                        code <- "reset"
                }
            }
            switch(code,
                   reset = setPrimitiveMethods(f, deflt, code, fdef, NULL),
                   clear = setPrimitiveMethods(f, deflt, code, NULL, NULL))
        }
        else {
            resetGeneric(f, fdef)
            if(isGroup(f, fdef = fdef)) {
                members <- fdef@groupMembers
                ## do the computations for the members as well; important if the
                ## members are primitive functions.
                if(length(members)>0)
                    Recall(members, attach, where)
            }
        }
    }
}

setPrimitiveMethods <-
  function(f, fdef, code, generic, mlist = get(".Methods", envir = environment(generic)))
    .Call("R_M_setPrimitiveMethods", f, fdef, code, generic, mlist, PACKAGE="methods")



findUnique <- function(what, doFind = find, message)
{
    where <- doFind(what)
    if(length(where) > 1) {
        if(missing(message)) {
            if(identical(doFind, findFunction))
                message <- paste("function", what)
            else
                message <- what
        }
        warning(message, " found on: ", 
                paste(search()[where], collapse = ", "),
                    "; using the first one.")
            where <- where[1]
    }
    where
}
    
MethodAddCoerce <- function(method, argName, thisClass, methodClass)
{
    if(identical(thisClass, methodClass))
        return(method)
    ext <- possibleExtends(thisClass, methodClass)
    ## if a non-simple coerce is required to get to the target class for
    ## dispatch, insert it in the method.
    if(is.logical(ext) || ext@simple)
        return(method)
    methodInsert <- function(method, addExpr) {
        if(is.function(method)) {
            newBody <- substitute({firstExpr; secondExpr},
                                  list(firstExpr = addExpr, secondExpr = body(method)))
            body(method, envir = environment(method)) <- newBody
        }
        else if(is(method, "MethodsList")) {
            methods <- method@allMethods
            for(i in seq(along=methods))
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
    .Call("R_missingArg", if(eval) symbol else substitute(symbol), envir, PACKAGE = "methods")

balanceMethodsList <- function(mlist, args, check = TRUE) {
    moreArgs <- args[-1]
    if(length(moreArgs) == 0)
        return(mlist)
    methods <- mlist@methods
    if(check && length(methods) > 0) {
        ## check whether the current depth is enough (i.e.,
        ## whether a method with this no. of args or more was set before
        depth <- 0
        el <- methods[[1]]
        while(is(el, "MethodsList")) {
            mm <- el@methods
            if(length(mm) == 0)
                break
            depth <- depth+1
            el <- mm[[1]]
        }
        if(depth >= length(args))
            ## already balanced to this length: An assertion
            ## relying on balance having been used consistently,
            ## which in turn relies on setMethod being called to
            ## add methods.  If you roll your own, tough luck!
            return(mlist)
    }
    for(i in seq(along = methods)) {
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
    
    
sigToEnv <- function(signature) {
    value <- new.env()
    classes <- as.character(signature)
    args <- names(signature)
    for(i in seq(along=args))
        assign(args[[i]], classes[[i]], envir = value)
    value
}

.methodSignatureMatrix <- function(object, sigSlots = c("target", "defined")) {
    if(length(sigSlots)>0) {
        allSlots <- lapply(sigSlots, slot, object = object)
        mm <- unlist(allSlots)
        mm <- matrix(mm, nrow = length(allSlots), byrow = TRUE)
        dimnames(mm) <- list(sigSlots, names(allSlots[[1]]))
        mm
    }
    else matrix(character(), 0, 0)
}

.valueClassTest <- function(object, classes, fname) {
    if(length(classes) > 0) {
        for(Cl in classes)
            if(is(object, Cl))
               return(object)
        stop(paste("Invalid value from generic function \"",
                   fname, "\", class \"", class(object),
                   "\", expected ",
                   paste("\"", classes, "\"", sep = "", collapse = " or "),
                   sep = ""))
    }
    ## empty test is allowed
    object
}

    
.getOrMakeMethodsList <- function(f, where, genericFun) {
    allMethods <- getMethodsMetaData(f, where = where)
    if(is.null(allMethods)) {
        argName <- genericFun@signature[[1]]
        allMethods <- new("MethodsList", argument = as.name(argName))
        other <- getMethodsMetaData(f)
        if(is.null(other))
            ## this utility is called AFTER ensuring the existence of a generic for f
            ## Therefore, the case below can only happen for a primitive for which
            ## no methods currently are attached.  Make the primitive the default
            deflt <- getFunction(f, generic = FALSE, mustFind = FALSE)
        else
            ## inherit the default method, if any
            deflt <- finalDefaultMethod(other)
        if(!is.null(deflt))
            allMethods <- insertMethod(allMethods, "ANY", argName, deflt)
        }
    allMethods
}

.makeCallString <- function(def, name = substitute(def), args = formalArgs(def)) {
    if(is.character(def)) {
        if(missing(name))
            name <- def
        def <- getFunction(def)
    }
    if(is(def, "function"))
        paste(name, "(", paste(args, collapse=", "), ")", sep="")
    else
        ""
}

.ValidateValueClass <- function(fdef, name, valueClass)
{
    ## include tests for value
    fbody <- body(fdef)
    body(fdef, envir = environment(fdef)) <-
        substitute(.valueClassTest(EXPR, VALUECLASS, FNAME),
                   list(EXPR = fbody, VALUECLASS = valueClass, FNAME = name))
    fdef
}

## interpret the group= argument to makeGeneric, allowing for char. argument
## and "" for compatibility.
## TO DO:  make it possible for this argument to be a group generic function
## (it may in fact work now).
.asGroupArgument <- function(group) {
    if(is.character(group)) {
        if(identical(group, ""))
            list()
        else
          as.list(group) ## should we allow c(group, package) ?
    }
    else
        group
}

metaNameUndo <- function(strings, prefix = "M", searchForm = FALSE) {
    pattern <- methodsPackageMetaName(prefix, "")
    n <- nchar(pattern)
    matched <- substr(strings, 1, n) == pattern
    value <- substring(strings[matched], n+1)
    pkg <- sub("^[^:]*", "", value) # will be "" if no : in the name
    if(searchForm) {
        global <- grep(".GlobalEnv", value)
        if(length(global)) {
            pkg[-global] <- paste("package", pkg[-global], sep="")
            pkg[global] <- substring(pkg[global],2)
        }
    }
    else
        pkg <- substring(pkg, 2)
    value <- sub(":.*","", value)
    new("ObjectsWithPackage", value, package = pkg)
}

.recursiveCallTest <- function(x, fname) {
    if(is(x, "call")) {
        if(identical(x[[1]], quote(standardGeneric))) {
            if(!identical(x[[2]], fname))
                warning("The body of the generic function for \"",
                        fname, "\" calls standardGeneric to dispatch on a different name (\"",
                        paste(as.character(x[[2]]), collapse = "\\n"),
                        "\")!")
            TRUE
        }
        else {
            for(i in seq(from=2, length = length(x)-1)) {
                if(Recall(x[[i]], fname))
                    return(TRUE)
            }
            FALSE
        }
    }
    else if(is(x, "language")) {
        for(i in seq(from=2, length = length(x)-1)) {
            if(Recall(x[[i]], fname))
                return(TRUE)
        }
        FALSE
    }
    else
        FALSE
}

.NonstandardGenericTest <- function(body, fname, stdBody){
    if(identical(body, stdBody))
        FALSE
    else {
        if(!.recursiveCallTest(body, fname))
            warning("The supplied generic function definition does not seem to call standardGeneric; no methods will be dispatched!")
        TRUE
    }
}
    
.GenericInPrimitiveMethods <- function(mlist, f) {
    methods <- mlist@methods
    for(i in seq(along = methods)) {
        mi <- methods[[i]]
        if(is(mi, "function")) {
            body(mi, envir = environment(mi)) <-
                substitute({.Generic <- FF; BODY},
                  list(FF = f,BODY = body(mi)))
        }
        else if(is(mi, "MethodsList"))
            mi <- Recall(mi, f)
        else
            stop("Internal error: Bad methods list object in fixing methods for prmitive function \"",
                 f, "\"")
        methods[[i]] <- mi
    }
    mlist@methods <- methods
    mlist
}

.signatureString <- function(fdef, signature) {
    snames <- names(signature)
    if(is.null(snames)) {
        if(is(fdef, "genericFunction")) {
            snames <- fdef@signature
            signature <- matchSignature(signature, fdef)
            if(length(snames)>length(signature))
                length(snames) <- length(signature)
        }
        else # shouldn't happen,...
            return(paste(signature, collapse=", "))
    }
    else
        signature <- as.character(signature)
    paste(paste(snames, "=\"", signature, "\"", sep=""), collapse = ", ")
}

.ChangeFormals <- function(def, defForArgs, msg = "<unidentified context>") {
    if(!is(def, "function"))
        stop("Trying to change the formal arguments in ", msg,", in an object of class \"",
             class(def), "\"; expected a function definition")
    if(!is(defForArgs, "function"))
        stop("Trying to change the formal arguments in ", msg,
             ", but getting the new formals from an object of class \"",
             class(def), "\"; expected a function definition")
    old <- formalArgs(def)
    new <- formalArgs(defForArgs)
    if(length(old) < length(new))
        stop("Trying to change the formal arguments in ", msg,
             ", but the number of existing arguments is less than the number of new arguments: (",
             paste("\"", old, "\"", sep ="", collapse=", "),  ") vs (",
             paste("\"", new, "\"", sep ="", collapse=", "), ")")
    if(length(old) > length(new))
        warning("Trying to change the formal arguments in ", msg,
             ", but the number of existing arguments is greater than the number of new arguments (the extra arguments won't be used): (",
             paste("\"", old, "\"", sep ="", collapse=", "),  ") vs (",
             paste("\"", new, "\"", sep ="", collapse=", "), ")")
    if(identical(old, new)) # including the case of 0 length
        return(def)
    dlist <- as.list(def)
    slist <- lapply(c(old, new), as.name)
    names(slist) <- c(new, old)
    vlist <- dlist
    for(i in seq(along = vlist))
        vlist[[i]] <- do.call("substitute", list(vlist[[i]], slist))
    dnames <- names(dlist)
    whereNames <- match(old, dnames)
    if(any(is.na(whereNames)))
        stop("In changing formal argumentsin ", msg,
             ", some of the old names are not in fact arguments: ",
             paste("\"", old[is.na(match(old, names(dlist)))], "\"", sep ="", collapse=", "))
    dnames[whereNames] <- new
    names(vlist) <- dnames
    as.function(vlist, envir = environment(def))
}
