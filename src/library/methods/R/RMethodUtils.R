## The real version of makeGeneric, to be installed after there are some
## generic functions to boot the definition (in particular, coerce and coerce<-)

.makeGeneric <-
## Makes a generic function object corresponding to the given function name.
## and definition.
  function(f, fdef,
           fdefault,
           group = list(), valueClass = character(), package, signature = NULL,
           genericFunction = NULL) {
      checkTrace <- function(fun, what, f) {
          if(is(fun, "traceable")) {
              warning("The function being used as ", what,
                      " in making a generic function for \"", f,
                      "\" is currently traced; the function used will have tracing removed")
              .untracedFunction(fun)
          }
          else
              fun
      }
      ## give the function a new environment, to cache methods later
      ev <- new.env()
      parent.env(ev) <- environment(fdef)
      environment(fdef) <- ev
      packageSlot(f) <- package
      assign(".Generic", f, envir = ev)
      fdef <- checkTrace(fdef)
      if(length(valueClass)>0)
          fdef <- .ValidateValueClass(fdef, f, valueClass)
      group <- .asGroupArgument(group)
      if(is.null(genericFunction))
          value <- new("standardGeneric")
      else if(is(genericFunction, "genericFunction"))
          value <- genericFunction
      else
          stop("The `genericFunction' argument must be NULL or a generic function object; got an object of class \"",
               class(genericFunction), "\"")
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
          fdefault <- checkTrace(fdefault)
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
      assign(".Methods", methods, envir = ev)
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
      packageSlot(f) <- package
      assign(".Generic", f, envir = ev)
      if(length(valueClass)>0)
          fdef <- .ValidateValueClass(fdef, f, valueClass)
      group <- .asGroupArgument(group)
###--------
      value <- fdef
      if(is.null(genericFunction))
          class(value) <- "standardGeneric"
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
      assign(".Methods", methods, envir = ev)
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
      fgen <- genericForPrimitive(f)
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
  function(f, fdef, where = topenv(parent.frame())) {
      search <- missing(fdef)
      basicDef <- genericForPrimitive(f)
      ## for basic functions (primitives), a generic is not on the search list
      if(is.null(basicDef)) {
          gwhere <- findFunction(f, where = where)
          if(length(gwhere) == 0)
              stop("\"",f, "\" is not a function visible from \"",
                   getPackageName(where), "\"")
          ## TODO: deal with multiple versions
          gwhere <- gwhere[[1]]
          if(search)
              fdef <- get(f, gwhere)
      }
      else {
          if(search)
              fdef <- basicDef
          gwhere <- "package:base" # ??
      }
      if(is(fdef, "genericFunction")) {
          deflt <- finalDefaultMethod(fdef@default)
      }
      else if(is.primitive(fdef)) {
          deflt <- fdef
          fdef <- getGeneric(f, TRUE, where)
      }
      else if(is.function(fdef) && search) {
          warning("\"",f,"\" from \"",
                  packageName(gwhere), "\" is a nongeneric function; no methods available")
          return(NULL)
      }
      else
          stop("Invalid \"fdef\" for \"", f,
               "\" in getAllMethods; expected either a genericFunction object or a primitive function, got an object of class \"",
               class(fdef), "\"")
      metaname <- mlistMetaName(fdef@generic, where)
      primCase <- is.primitive(deflt)
      ## NOTE: getGroup & getGeneric have to be called with the default
      ## topenv() here.  This may not work for installs w/o saved image TODO: check
      groups <- getGroup(fdef, TRUE)
      methods <- fdef@default # NOT getMethods(fdef) which may be out of date
      funs <- c(f, groups)
      changed <- FALSE
      for(ff in rev(funs)) {
          ## for f itself, get the metadata, otherwise use getMethods, because
          ## getAllMethods must have been called for these functions earlier
          if(identical(ff, f)) {
              libs = .findAll(metaname, where)
              for(mwhere in rev(libs)) {
                  mw <- getMethodsMetaData(f, mwhere)
                  methods <- mergeMethods(methods, mw)
                  changed <- TRUE
              }
          }
          else {
              fun <- getGeneric(ff, where = where)
              if(is.null(fun))
                  next # but really an error?
              genericLabel <- if(primCase) f else character()
              mw <- getMethods(fun)
              if(is(mw, "MethodsList") && length(mw@methods) > 0) {
                  methods <- mergeMethods(methods, mw, genericLabel)
                  changed <- TRUE
              }
          }
      }
      ev <- environment(fdef)
      if(changed || !identical(getMethods(fdef), methods))
          .genericAssign(f, fdef, methods, gwhere, deflt)
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

doPrimitiveMethod <-
  ## do a primitive call to builtin function `name' the definition and call
  ## provided, and carried out in the environment `ev'.
  ##
  ## A call to `doPrimitiveMethod' is used when the actual method is a .Primitive.
  ##  (because primitives don't behave correctly as ordinary functions,
  ## not having either formal arguments nor a function body).
  function(name, def, call = sys.call(sys.parent()), ev = sys.frame(sys.parent(2)))
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
    while(.identC(signature[[n]], "ANY"))
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
    if(.identC(class(bdy),"{") && length(bdy) > 1) {
        bdy <- bdy[[2]]
        if(.identC(class(bdy), "<-") &&
           identical(bdy[[2]], as.name(".local")))
            definition <- bdy[[3]]
    }
    definition
}

getGeneric <-
  ## return the definition of the function named f as a generic.
  ##
  ## If there is no definition, throws an error or returns
  ## NULL according to the value of mustFind.
  function(f, mustFind = FALSE, where = .genEnv(f, topenv(parent.frame()))) {
    if(is.function(f) && is(f, "genericFunction"))
        return(f)
    value <- if(is.null(where))
        NULL
      else
          .getGeneric( f, as.environment(where))
    if(is.null(value) && exists(f, "package:base", inherits = FALSE)) {
      ## check for primitives
      baseDef <- get(f, "package:base")
      if(is.primitive(baseDef)) {
          value <- genericForPrimitive(f)
          if(!is.function(value) && mustFind)
              stop("Methods cannot be defined for the primitive function \"",
                   f, "\"")

      }
  }
    if(is.function(value))
        value
    else if(mustFind)
        ## the C code will have thrown an error if f is not a single string
        stop("No generic function found for \"",f,"\"")
    else
      NULL
  }

## low-level version
.getGeneric <- function(f, where) {
    .Call("R_getGeneric", f, FALSE, as.environment(where), PACKAGE = "methods")
}
           
getGroup <-
  ## return the groups to which this generic belongs.  If `recursive=TRUE', also all the
  ## group(s) of these groups.
  function(fdef, recursive = FALSE, where = topenv(parent.frame()))
{
    if(is.character(fdef))
        fdef <- getGeneric(fdef, where = where)
    if(is(fdef, "genericFunction"))
        group <- fdef@group
    else
        group <- list()
    if(recursive && length(group) > 0) {
        allGroups <- group
        for(gp in group) {
            fgp <- getGeneric(gp, where = where)
            if(is(fgp, "groupGenericFunction"))
                allGroups <- c(allGroups, Recall(fgp, TRUE, where))
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
  function(f, where = topenv(parent.frame())) {
        mname <- mlistMetaName(f, where)
        if(exists(mname, where = where, inherits = missing(where)))
            get(mname, where)
        else
            NULL
  }


assignMethodsMetaData <-
  ## assign value to be the methods metadata for generic f on database where.
  function(f, value, fdef, where, deflt = finalDefaultMethod(value)) {
    assign(mlistMetaName(fdef) # use generic function to get package correct
           , value, where)
    if(is.primitive(deflt))
        setPrimitiveMethods(f, deflt, "reset", fdef, NULL)
    if(is(fdef, "groupGenericFunction")) # reset or turn on members of group
        cacheGenericsMetaData(f, fdef, where = where, package = fdef@package)
  }

mlistMetaName <-
  ## name mangling to simulate metadata for a methods definition.
  function(name = "", package = "") {
      if(!is.character(package)) { # find the generic starting from envir. or search list
          fdef <- getGeneric(name, where = package)
          if(is.null(fdef))
              package <- ""
          else
              package <- fdef@package
      }
      else # delay finding the generic until we need it
          fdef <- NULL
      if(is(name, "genericFunction"))
          methodsPackageMetaName("M", paste(name@generic, name@package, sep=":"))
      else if(missing(name))
          methodsPackageMetaName("M","")
      else if(is.character(name)) {
          if(length(name) > 1 && !identical(package, "")) {
              value <- name
              name <- paste(name, package, sep=":")
              for(i in seq(along = value))
                  value[[i]] = methodsPackageMetaName("M", name[[i]])
          }
          else if(nchar(package))
             methodsPackageMetaName("M", paste(name, package, sep=":"))
          else {
              if(is.null(fdef)) {
                  if(nchar(package)>0)
                      where <- .requirePackage(package)
                  else
                      where <- topenv(parent.frame())
                  fdef <- getGeneric(name, where = where)
              }
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
  function(where, searchForm = FALSE) {
      if(missing(where))
          where <- .envSearch(topenv(parent.frame()))
      else if(is.environment(where)) where <- list(where)
    these <- character()
    for(i in where) {
      these <- c(these, objects(i, all=TRUE))
    }
    metaNameUndo(unique(these), prefix = "M", searchForm = searchForm)
  }

allGenerics <- getGenerics

## faster version for use outside methods
.getGenerics <- function(where)
{
    if(missing(where)) where <- .envSearch(topenv(parent.frame()))
    else if(is.environment(where)) where <- list(where)
    these <- character()
    for(i in where) these <- c(these, objects(i, all=TRUE))
    these <- unique(these)
    these <- these[substr(these, 1, 6) == ".__M__"]
    gsub(".__M__(.*):([^:]+)", "\\1", these)
}

is.primitive <-
  function(fdef)
    switch(typeof(fdef),
           "special" = , "builtin" = TRUE,
           FALSE)


cacheMetaData <- function(where, attach = TRUE, searchWhere = as.environment(where)) {
    ## a collection of actions performed on attach or detach
    ## to update class and method information.
    generics <- .getGenerics(where)
    for(f in generics) {
        fdef <- getGeneric(f, FALSE, searchWhere)
        ## silently ignores all generics not visible from searchWhere
        ## (certainly reasonable for attach=FALSE, maybe for namespaces ?)
        if(is(fdef, "genericFunction")) {
            methods <- getAllMethods(f, fdef, searchWhere)
            cacheGenericsMetaData(f, fdef, attach, where, fdef@package, methods)
        }
    }
}

cacheGenericsMetaData <- function(f, fdef, attach = TRUE, where = topenv(parent.frame()),
                                  package, methods = getAllMethods(f, fdef, where)) {
    if(!is(fdef, "genericFunction")) {
        warning("No methods found for \"", f, "\"; cacheGenericsMetaData will have no effect")
        return(FALSE)
    }
    if(missing(package))
        package <- fdef@package
    deflt <- finalDefaultMethod(methods)
    if(is.primitive(deflt)) {
        if(attach) {
            if(missing(methods))
                code <- "reset"
            else
                code <- "set"
        }
        else {
            ## the methods supplied may not be correct (until primitives have their own
            ## separate generics in namespaces) so must delete or reset methods explicitly
            methods <- deletePrimMethods(f, where)
            code <- "set"
        }
        switch(code,
               reset = setPrimitiveMethods(f, deflt, code, fdef, NULL),
               set = setPrimitiveMethods(f, deflt, code, fdef, methods),
##               clear = setPrimitiveMethods(f, deflt, code, NULL, NULL),
               stop("internal error: bad code for setPrimitiveMethods: ", code))
    }
    else if(isGroup(f, fdef = fdef)) {
        members <- fdef@groupMembers
        ## do the computations for the members as well; important if the
        ## members are primitive functions.
        for(ff in members) {
            ffdef <- getGeneric(ff, where = where)
            if(is(ffdef, "genericFunction")) {
                ## reform the methods list for the group member, to include
                ## any group-inherited methods
                mm <- getAllMethods(ff, ffdef, where = where)
                Recall(ff, ffdef, attach, where, methods = mm)
            }
        }
    }
    TRUE
}

setPrimitiveMethods <-
  function(f, fdef, code, generic, mlist = get(".Methods", envir = environment(generic)))
    .Call("R_M_setPrimitiveMethods", f, fdef, code, generic, mlist, PACKAGE="methods")



findUnique <- function(what, message, where = topenv(parent.frame()))
{
    where <- .findAll(what, where = where)
    if(length(where) > 1) {
        if(missing(message)) {
            if(identical(doFind, findFunction))
                message <- paste("function ", dQuote(what))
            else
                message <- dQuote(what)
        }
        if(is.list(where))
            where <- unlist(where)
        if(is.numeric(where))
            where <- search()[where]
        warning(message, " found on: ",
                paste(where, collapse = ", "),
                    "; using the first one.")
            where <- where[1]
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


sigToEnv <- function(signature, generic) {
    genericSig <- generic@signature
    package <- packageSlot(signature)
    if(is.null(package))
        parent <- environment(generic)
    else
        parent <- .requirePackage(package)
    value <- new.env(parent = parent)
    classes <- as.character(signature)
    args <- names(signature)
    for(i in seq(along=args))
        assign(args[[i]], classes[[i]], envir = value)
    ## missing args in signature have class "ANY"
    if(length(args) < length(genericSig))
        for(other in genericSig[is.na(match(genericSig, args))])
            assign(other, "ANY", envir = value)
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

## The search list, or a namespace's static search list, or an environment
.envSearch <- function(env = topenv(parent.frame())) {
    if(identical(env, .GlobalEnv))
        seq(along = search())
    else if(isNamespace(env) && !isBaseNamespace(env)) {
        ## the static environments for this namespace, ending with the base namespace
        value <- list(env)
        repeat {
            if(is.null(env))
                stop("Botched namespace: Failed to find base namespace in its parents")
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

.genericName <- function(f) {
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
## function in a pacakge with no namespace, and that function then calls a methods package
## function.  The right answer then is .GlobalEnv, but we will instead get the package
## namespace.
.externalCallerEnv <- function(n = 2, nmax = sys.nframe() - n +1) {
    ## start n generations back; by default the caller of the caller to this function
    ## go back nmax at most (e.g., a function in the methods package that knows it's never
    ## called more than nmax levels in could supply this argument
    if(nmax < 1) stop("Got a negative maximum number of frames to look at")
    ev <- topenv(parent.frame()) # .GlobalEnv or the environment in which methods is being built.
    for(back in seq(start = -n, length = nmax)) {
        fun <- sys.function(back)
        if(is(fun, "function")) {
            ## Note that "fun" may actually be a method definition, and still will be counted.
            ## This appears to be the correct semantics, in
            ## the sense that, if the call came from a method, it's the method's environment
            ## where one would expect to start the search (for a class defintion, e.g.)
            ev <- environment(fun)
            if(!identical(ev, .methodsNamespace))
                break
        }
    }
    ev
}

## a list of environments, starting from ev, going back to NULL (the base package),
## or else terminated by finding a namespace
.parentEnvList <- function(ev) {
    ev <- as.environment(ev)
    value <- list(ev)
    while(!isNamespace(ev)) {
        if(is.null(ev)) {
            value[[length(value)]] <- .BaseNamespaceEnv
            break
        }
        ev <- parent.env(ev)
        value <- c(value, list(ev))
    }
    value
}

.genericAssign <- function(f, fdef, methods, where, deflt) {
    primCase <- is.primitive(deflt)
    ev <- environment(fdef)
    assign(".Methods", methods, ev)
}

## Mark the method as derived from a non-generic.
## FIXME:  This can't work for primitives, which cannot have slots
## However, primitives are not to be restored locally in any case.
.derivedDefaultMethod <- function(fdef) {
    if(is.function(fdef) && !is.primitive(fdef)) {
        value <- new("derivedDefaultMethod")
        value@.Data <- fdef
        value
    }
    else
        fdef
}

.identC <- function(c1 = NULL, c2 = NULL) {
    ## are the two objects identical class references?
    ## FIXME:  without the preliminary test, this segfaults when
    ## called in the form .identC("logical", FALSE) ????
    if(is.character(c1) && is.character(c2))
        .Call("R_identC", c1, c2, PACKAGE="methods")
    else
        FALSE
}

## a version of match that avoids the is.factor() junk: faster & safe for bootstrapping
.matchBasic <- function(x, table, nomatch = NA) .Internal(match(x, table, nomatch))

## match default exprs in the method to those in the generic
## if the method does not itself specify a default, and the
## generic does
matchDefaults <- function(method, generic) {
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
            margs[[arg]] <- garg
        }
    }
    if(changes)
        formals(method, envir = environment(method)) <- margs
    method
}

getGroupMembers <- function(group, recursive = FALSE, character = TRUE) {
    .recMembers <- function(members, where) {
        all = vector("list", length(members))
        for(i in seq(along=members)) {
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
        warning("\"", f, "\" is not a generic function (or not visible here)" )
        return(character())
    }
    else if(!is(f, "groupGenericFunction"))
        character()
    else {
        members <- f@groupMembers
        if(recursive)
            members <- .recMembers(members, f@package)
        if(character)
            sapply(members, function(x){
                if(is(x, "character"))
                    x
                else if(is(x, "genericFunction"))
                    x@generic
                else
                    stop("Invalid element in the groupMembers slot (class \"",
                         class(x), "\")")
            })
        else
            members
    }
}

deletePrimMethods <- function(f, env) {
    fdef <- genericForPrimitive(f)
    mlist <- getMethods(f)
    allM <- getAllMethods(f, fdef, .GlobalEnv)
    toDelete <- getMethodsMetaData(f, env)
    if(!is.null(toDelete)) {
        toDelete <- linearizeMlist(toDelete)
        sigs  <- toDelete@classes
        args <- toDelete@arguments
        for(i in seq(along=sigs)) {
            sig <- sigs[[i]]; arg <- args[[i]]
            def <- getMethod(f, sig, optional=TRUE, mlist = allM)
            ## def is either the now-visible method or NULL (=> delete the method)
            mlist <- insertMethod(mlist, sig, arg, def)
        }
        .genericAssign(f, fdef, mlist, .GlobalEnv, get(f))
    }
    mlist
}
