
makeGeneric <-
## Makes a generic function object corresponding to the given function name.
## and optional definition.
  function(f, fdef, keepMethods = TRUE, useAsDefault = NA,
           group = character(), valueClass = character()) {
  if(missing(fdef)) {
    ## either find a generic or use either a pre-defined template or a non-generic
    fdef <- getGeneric(f, FALSE)
    if(is.null(fdef))
      fdef <- getFunction(f, mustFind = TRUE)
  }
  generic <- isGeneric(f, fdef = fdef)
  if(is.na(useAsDefault))
    useAsDefault <- !generic
  if(keepMethods && generic)
    return(fdef)
  if(!useAsDefault || generic) {
    ## create a generic with the same body (usually
    ## standardGeneric(f)) but NULL default method
    fdefault <- NULL
  }
  else {
    ## a non-generic function becomes the default
    fdefault <- getFunction(f, generic = FALSE, mustFind = FALSE)
    fdef <- makeStandardGeneric(f, fdef)
  }
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
    methods <- MethodsList(name, fdefault)
  assign(".Methods", methods, envir=env)
  fdef
}

makeStandardGeneric <-
  ## a utility function that makes a valid function calling standardGeneric for name f
  ## Works (more or less) even if the actual definition, fdef, is not a proper function,
  ## that is, it's a primitive or internal
  function(f, fdef) {
    ## detect R specials and builtins:  these don't provide an argument list
    if(typeof(fdef) != "closure") {
      ## Look in a list of pre-defined functions (and also of functions for which
      ## methods are prohibited)
      fdef <- elNamed(.BasicFunsList, f)
      if(identical(fdef, FALSE))
        stop(paste("Special function \"", f, "\" is not permitted to have methods", sep=""))
      if(is.null(fdef)) {
        warning(paste("Special function \"", f, "\" has no known argument list; will assume \"(x, ...)\"", sep=""))
        ## unknown
        fdef <- function(x, ...) {}
      }
      else
        message("Making a generic for special function \"", f, "\"")
      ## Note that the body of the function comes from the list.  In a few cases ("$"),
      ## this body is not just a call to standardGeneric
    }
    body(fdef) <- substitute(standardGeneric(FNAME), list(FNAME=f))
    fdef
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


mergeGenericFunctions <-
  ## a generic function (with methods) representing the merge of all the versions
  ## of `f' on the specified packages (anything on the current search path by default).
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
    ## when this function is called from methodsListDispatch (via C code),
    ## a barrier version of the function is put into the metadata to prevent
    ## recursive loops.  Must remove this, if an error occurs in mergeGenericFunctions
    fdef <- getGeneric(f, TRUE)
    ev <- copyEnvironment(fdef)
    groups <- getGroup(fdef, TRUE)
    on.exit(removeFromMethodMetaData(f))
    methods <- NULL
    funs <- c(f, groups)
    for(fun in rev(funs))
      for(where in rev(libs)) {
        fw <- getFunction(fun, where=where, mustFind=FALSE)
        if(is.null(fw) || !isGeneric(fun, fdef = fw))
          next
        methods <- mergeMethods(methods, getMethods(fw))
      }
    methods <- setAllMethodsSlot(methods)
    assign(".Methods", methods, ev)
    environment(fdef) <- ev
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
      m1 <- insertMethod(m1, as.character(sigi), args, el(methods, i))
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
  methods <- slot(mlist, "methods")
  allMethods <- list()
  modified <- FALSE
  for(i in names(methods)) {
    method <- elNamed(methods, i)
    if(is(method, "MethodsList")) {
      method <- Recall(method)
      elNamed(methods, i) <- method
      modified <- TRUE
    }
    elNamed(allMethods, i) <- method
  }
  slot(mlist, "allMethods") <- allMethods
  if(modified)
    slot(mlist, "methods") <- methods
  mlist
}

doPrimitiveMethod <-
  ## do a primitive call to builtin function `name' the definition and call
  ## provided, and carried out in the environment `ev'.
  ##
  ## A call to `doPrimitiveMethod' is used when the actual method is a .Primitive.
  ##  (because primitives don't behave correctly as ordinary functions,
  ## not having either formal arguments nor a function body).
  function(name, def, call = sys.call(-1), ev = sys.frame(-2)) {
    ## Store a local version of function `name' back where the current version was
    ## called.  Restore the previous state there on exit, either removing or re-assigning.
    if(exists(name, envir=ev, inherits=F)) {
      prev <- get(name, envir=ev)
      on.exit(assign(name, prev, envir = ev))
    }
    else
      on.exit(rm(list=name, envir=ev))
    assign(name, def, envir = ev)
    eval(call, ev)
  }

conformMethodArgs <-
  function(def, fdef, envir) {
    newDef <- fdef
    newCall <- get(".Arguments",  envir=envir)
    languageEl(newCall, 1) <- as.name(".Method")
    body(newDef) <- substitute({.Method <- DEF; CALL},
                      list(DEF = def, CALL = newCall))
    environment(newDef) <- NULL
    newDef
  }

getGeneric <-
  ## return the definition of the function named f as a generic.
  ##
  ## If there is no definition in the current search list, throws an error or returns
  ## NULL according to the value of mustFind.
  function(f, mustFind = FALSE)
  .Call("R_getGeneric", f, mustFind)

getGroup <-
  ## return the groups to which this generic belongs.  If `recursive=TRUE', also all the
  ## group(s) of these groups.
  function(fdef, recursive = FALSE) {
    if(is.character(fdef))
      fdef <- getGeneric(fdef)
    if(is.function(fdef))
      group <- get(".Group", envir = environment(fdef))
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

        
