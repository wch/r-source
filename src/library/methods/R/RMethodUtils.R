
makeGeneric <-
## Makes a generic function object corresponding to the given function name.
## and optional definition.
  function(f, fdef, keepMethods = TRUE,
           fdefault = getFunction(f, generic = FALSE, mustFind = FALSE),
           group = character(), valueClass = character()) {
  if(missing(fdef)) {
    ## either find a generic or use either a pre-defined template or a non-generic
    fdef <- getGeneric(f, FALSE)
    if(is.null(fdef))
      fdef <- getFunction(f, mustFind = TRUE)
  }
  fdef <- makeStandardGeneric(f, fdef)
  generic <- isGeneric(f, fdef = fdef)
  if(keepMethods && generic)
    return(fdef)
  anames <- formalArgs(fdef)
  if(length(anames) == 0 || (length(anames) == 1 && el(anames, 1) == "..."))
    stop("must have a named argument for a generic function.")
  name <- el(anames, 1)
  if(name == "...")
    name <- el(anames, 2)
  if(!generic) {
    env <- new.env()
    assign(".Generic", f, envir=env)
    assign(".Group", group, envir=env)
    assign(".ValueClass", valueClass, envir=env)
    ## the .Arguments object provides a template of the formal arguments
    ## in a call object, for use in dispatching primitive or internal code.
    assign(".Arguments", generic.skeleton(f, fdef, fdefault), envir=env)
    environment(fdef) <- env
  }
  else {
    env <- copyEnvironment(fdef, exceptions = ".Methods")
  }
  if(is.null(fdefault))
    methods <- MethodsList(name)
  else
    methods <- MethodsList(name, asMethodDefinition(fdefault))
  assign(".Methods", methods, envir=env)
  fdef
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
  function(f, libs = search()) {
    fdef <- getGeneric(f, TRUE)
    groups <- getGroup(fdef, TRUE)
    ## when this function is called from methodsListDispatch (via C code),
    ## a barrier version of the function is put into the metadata to prevent
    ## recursive loops.  Must remove this, if an error occurs in getAllMethods
    on.exit(removeFromMethodMetaData(f))
    methods <- NULL
    funs <- c(f, groups)
    for(fun in rev(funs))
      for(where in rev(libs)) {
        mw <- getMethodsMetaData(fun, where)
        if(!is.null(mw))
          methods <- mergeMethods(methods, mw)
      }
    ev <- copyEnvironment(fdef)
    if(is.null(methods)) ## after removeMethods, e.g.
        methods <- get(".Methods", ev)
    if(!is.null(methods)) {
        methods <- setAllMethodsSlot(methods)
        assign(".Methods", methods, ev)
    }
    environment(fdef) <- ev
    assignToMethodMetaData(f, fdef)
    ## in the current version of this function,
    ## primitives are pre-cached in the method metadata (because
    ## they are not visible as generic functions from the C code).
    fun <- getFunction(f)
    if(is.primitive(fun))
      setPrimitiveMethods(f, fun, "set", fdef)
    ## cancel the error cleanup
    on.exit()
    fdef
  }


mergeMethods <-
  ## merge the methods in the second MethodsList object into the first,
  ## and return the merged result.
  function(m1, m2) {
    if(is.null(m1))
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
  function(signature, mnames, fnames)
{
    ## TO DO:  arrange for "missing" to be a valid for "..." in a signature
    ## until then, allow an omitted "..." w/o checking
    if(is.na(match("...", mnames)) && !is.na(match("...", fnames)))
        fnames <- fnames[-match("...", fnames)]
    omitted <- is.na(match(fnames, mnames))
    if(!any(omitted))
        return(signature)
    if(!all(diff(seq(along=fnames)[!omitted]) > 0))
        stop("Formal arguments in method and function don't appear in the same order")
    specified <- omitted[seq(length=length(signature))]
    if(any(specified) &&
       any(is.na(match(signature[specified], c("ANY", "missing")))))
        stop(paste("Formal arguments omitted in the method definition cannot be in the signature:",
                   paste(fnames[is.na(match(signature[omitted], c("ANY", "missing")))], collapse = ", ")))
    message("Expanding the signature to include omitted arguments in definition: ",
            paste(fnames[omitted], "= \"missing\"",collapse = ", "))
    signature[omitted] <- "missing"
    ## there may have been some unspecified, but included, args; they go to "ANY"
    signature[nchar(signature) == 0] <- "ANY"
    ## remove trailing "ANY"'s
    n <- length(signature)
    while(identical(signature[[n]], "ANY"))
        n <- n - 1
    length(signature) <- n
    signature
}

rematchDefinition <- function(definition, generic, mnames, fnames) {
    added <- is.na(match(mnames, fnames))
    if(!any(added))
        return(definition)
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
  function(f, mustFind = FALSE) {
    value <- .Call("R_getGeneric", f, FALSE, PACKAGE = "methods")
    if(is.null(value) && exists(f, "package:base")) {
      ## check for primitives
      baseDef <- get(f, "package:base")
      if(is.primitive(baseDef))
        value <- elNamed(.BasicFunsList, f)
    }
    if(is.function(value))
      value
    else if(mustFind)
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
    if(is.function(fdef)) {
        group <- get(".Group", envir = environment(fdef))
        ## compatibility w. S-Plus allows "" for empty group
        group <- group[nchar(group)>0]
      }
    else
        group <- character()
    if(recursive && length(group) > 0) {
        allGroups <- group
        for(gp in group)
            allGroups <- c(allGroups, Recall(gp, TRUE))
        group <- unique(allGroups)
    }
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
  ## Also updates cached information about this generic.
  function(f, value, where) {
    assign(mlistMetaName(f), value, where)
    resetGeneric(f)
  }

mlistMetaName <-
  ## name mangling to simulate metadata for a methods definition.
  function(name)
  methodsMetaName("M", name)

getGenerics <-
  function(where = seq(along=search())) {
    pattern <- mlistMetaName("")
    n <- nchar(pattern)
    value <- character()
    if(is.environment(where)) where <- list(where)
    for(i in where) {
      these <- objects(i, all=TRUE)
      ## string match the pattern (NOT by a regexp in grep)
      these <- these[substr(these, 1, n) == pattern]
      value <- c(value, these)
    }
    substring(unique(value), n+1)
  }

allGenerics <- getGenerics

is.primitive <-
  function(fdef)
    switch(typeof(fdef),
           "special" = , "builtin" = TRUE,
           FALSE)
    

cacheMetaData <-
  function(envir, attach = TRUE) {
    ## a collection of actions performed on attach or detach
    ## to update class and method information.
    generics <- getGenerics(envir)
    if(length(generics)>0)
      cacheGenericsMetaData(generics, attach, envir)
  }

cacheGenericsMetaData <-
  function(generics, attach = TRUE, envir = NULL) {
    for(f in generics) {
        ## Some tests: don't cache generic if no methods defined
        if(attach) {
            if(!isGeneric(f))
                next
            methods <- getMethods(f)
            if(is.null(methods))
                next
            methods <- methods@methods
            if(length(methods)==0)
                next
        }
        if(!is.null(getFromMethodMetaData(f)))
            removeFromMethodMetaData(f)
      ## find the function.  It may be a generic, but will be a primitive
      ## if the internal C code is being used to dispatch methods for primitives.
      ## It may also be NULL, if no function is found (when detaching the only
      ## package defining this function, for example).
      fdef <- getFunction(f, mustFind = FALSE)
      if(is.primitive(fdef)) {
        if(attach) code <- "reset"
        else {
          code <- "clear"
          for(i in search()) {
            envi <- as.environment(i)
            if(identical(envi, envir))
              next
            if(!is.null(getMethodsMetaData(f, envi)))
              {code <- "reset"; break}
          }
        }
        switch(code,
               reset = setPrimitiveMethods(f, fdef, code, getGeneric(f), NULL),
               clear = setPrimitiveMethods(f, fdef, code, NULL, NULL))
      }
      else if(isGroup(f, fdef = fdef)) {
        members <- getGroupMembers(f, fdef = fdef)
        ## do the computations for the members as well; important if the
        ## members are primitive functions.
        if(length(members)>0)
          Recall(members, attach, envir)
      }
    }
  }

setPrimitiveMethods <-
  function(f, fdef, code, generic, mlist = get(".Methods", envir = environment(generic)))
    .Call("R_M_setPrimitiveMethods", f, fdef, code, generic, mlist, PACKAGE="methods")


setGroupMembers <-
  function(f, members, fdef = getGeneric(f))
{
  assign(".GroupMembers", members, envir = environment(fdef))
}

getGroupMembers <-
  function(f, fdef = getGeneric(f))
  get(".GroupMembers", envir = environment(fdef))

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
    ext <- extendsCoerce(thisClass, methodClass, formFunction = FALSE)
    ## extendsCoerce with formFunction=FALSE only returns a function if there
    ## is an explicit coerce somewhere along the line, not for direct inclusion
    ## or for the data part.
    if(!is.function(ext))
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

methodSignatureMatrix <- function(object, sigSlots = c("target", "defined")) {
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

    
.getOrMakeMethodsList <- function(f, fnames, where) {
    allMethods <- getMethodsMetaData(f, where = where)
    argName <- ""
    for(i in fnames)
        if(!identical(i, "..."))
        { argName <- i; break}
    if(nchar(argName) == 0)
        stop(paste("\"", f, "\" can't be a generic; no valid argument name", sep=""))
    if(is.null(allMethods)) {
        allMethods <- new("MethodsList", argument = as.name(argName))
        other <- getMethodsMetaData(f)
        if(is.null(other))
            ## this utility is called AFTER ensuring the existence of a generic for f
            ## Therefore, the case below can only happen for a primitive for which
            ## no methods currently are attached.  Make the prmitive the default
            deflt <- getFunction(f, generic = FALSE, mustFind = FALSE)
        else
            ## inherit the default method, if any
            deflt <- finalDefaultMethod(other)
        if(!is.null(deflt))
            allMethods <- insertMethod(allMethods, "ANY", argName, deflt)
        }
    allMethods
}

makeCallString <- function(def, name = substitute(def), args = formalArgs(def)) {
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
