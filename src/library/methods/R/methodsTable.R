### merge version called from namespace imports code.  Hope to avoid using generic
.mergeMethodsTable2 <- function(table, newtable, envir, metaname) {
    old <- objects(table, all=TRUE)
    mm <- 1
    for( what in old) {
      mm = get(what, envir =table)
      if(is(mm, "MethodDefinition")) {
          mm = length(mm@defined)
          break
      }
    }
    new = objects(newtable, all=TRUE)
    ## check that signature length doesn't change
    canStore <- TRUE
    for(what in new) {
        obj <- get(what, envir = newtable)
        if(is(obj, "MethodDefinition") &&
           length(obj@defined) != mm) {
            canStore <- FALSE
            break
        }
    }
    if(canStore) {
        for(what in new)
          assign(what, get(what, envir = newtable), envir = table)
        table
    }
    else { # rats! have to get the generic function
        f <- gsub(".__T__(.*):([^:]+)", "\\1", metaname)
        package <- gsub(".__T__(.*):([^:]+(.*))", "\\2", metaname)
        generic <- getGeneric(f, TRUE, envir, package)
        .mergeMethodsTable(generic, table, newtable, TRUE)
        table
    }
}

## action on attach, detach to merge methods tables
.mergeMethodsTable <- function(generic, table, newtable, add = TRUE) {
  fenv <- environment(generic)
  signature <- generic@signature
  if(!exists(".SigLength", envir = fenv, inherits = FALSE))
     .setupMethodsTables(generic)
  n <- get(".SigLength", envir = fenv)
  anySig <- rep("ANY", n)
  anyLabel <- .sigLabel(anySig)
  newMethods <- objects(newtable, all=TRUE)
  for(what in newMethods) {
    obj <- get(what, envir = newtable)
    if(is.primitive(obj))
      sig <- anySig ## Assert doesn't need to be a signature obj., won't change ns
    else if(is(obj, "MethodDefinition"))
      sig <- obj@defined
    else
      stop(gettextf(
                    "Invalid object in meta table of methods for \"%s\", label \"%s\", had class \"%s\"",
                    generic@generic, what, class(obj)))
    ns <- length(sig)
    if(ns == n) {}
    else {
      if(ns < n) {
        sig <-  new("signature", generic, c(sig@.Data, rep("ANY", n-ns)))
        obj@defined <- sig
        obj@target <- sig
        what <- .sigLabel(sig)
        ns <- n
      }
      else if(add) { # and ns > n
        signames <- generic@signature
        length(signames) <- ns
        .resetTable(table, ns, signames)
        assign(".SigLength", ns, envir = fenv)
        n <- ns
      }
    }
    if(add)
      assign(what, obj, envir = table)
    else if(exists(what, envir = table, inherits = FALSE) &&
            !all(obj@defined == "ANY") ) # remove methods, but not the default
      remove(list = what, envir = table)
    ## else warning?
  }
}


.mlistAddToTable <- function(generic, mlist, table = new.env(TRUE, fenv), add = TRUE) {
  fenv <- environment(generic)
  signature <- generic@signature
    if(!exists(".SigLength", envir = fenv, inherits = FALSE))
     .setupMethodsTables(generic)
  n <- get(".SigLength", envir = fenv, inherits = FALSE)
  .storeMlist(table, rep("ANY", n), mlist, 1,  add, fenv)
  ## check for more args in the mlist than in the table
  nNow <- get(".SigLength", envir = fenv, inherits = FALSE)
  if(nNow > n) {
    length(signature) <- nNow
    .resetTable(table, nNow, signature)
  }
  table
}

.storeMlist <- function(table, sig, mlist, i, add, fenv) {
  ## the methods slot is a list named by class, with elements either
  ## method definitions or mlists
  m <- mlist@methods
  classes <- names(m)
  for(j in seq_along(m)) {
    el <- m[[j]]
    sig[[i]] <- classes[[j]]
    if(is(el, "MethodDefinition") || is.primitive(el)) {
      if(add)
        assign(.sigLabel(sig), el, envir = table)
      else
        remove(list = .sigLabel(sig), envir = table)
    }
    else if(is(el,"MethodsList")) {
      i1 <- i+1
      if(i1 >= length(sig)) {
        ## a reset of the labels will be needed
        assign(".SigLength", i1, envir = fenv)
        sig <- c(sig, rep("ANY", i1-length(sig)))
      }
      Recall(table, sig, el, i1, add, fenv)
    }
    else
      stop("Invalid mlist element  for signature \"", classes[[j]], "\" at level ", i,
           "( should be methods list or method, had class \"",
           class(el), "\")")
  }
  table
}

.cacheMethodInTable <- function(fdef, sig, def,
                                table = get(".AllMTable", envir = fenv)) {
    ## store method in cache table.
    ## called from setMethod()
    ## also Called from cacheMethod (from as(),
  ## as<-())
  fenv <- environment(fdef)
  if(!exists(".AllMTable", envir = fenv, inherits = FALSE))
    .setupMethodsTables(fdef)
  sig <- .matchSigLength(sig, fdef, fenv, TRUE)
  label <- .sigLabel(sig)
  if(is.null(def)) {
    if(exists(label, envir = table, inherits = FALSE)) # remove the method (convention for setMethod)
      remove(list = label, envir = table)
  }
  else
  assign(label, def, envir = table)
}


.resetTable <- function(table, n, signames) {
  ## after updating a methods table, the maximum no. of arguments in
  ## the signature increased to n.  Reassign any objects whose label
  ## does not match n classes from the defined slot
  anyLabel <- rep("ANY", n); seqN <- 1:n
  labels <- objects(table, all.names = TRUE)
  for(what in labels) {
    method <- get(what, envir = table)
    if(is.primitive(method)) # stored as default ?
      label <- anyLabel
    else if(is(method, "MethodDefinition"))
      label <- method@defined
    else
      stop("Invalid object in methods table (\"", what,
           "\"), expected a method, got an object of class \"",
           class(method), "\"")
    if(length(label) < n) {
      label@.Data  <- ifelse(seqN>length(label), anyLabel, label@.Data)
      label@names <- signames
      method@defined <- method@target <- label
    }
    newLabel <- .sigLabel(label)
    if(!identical(what, newLabel)) {
      assign(newLabel, method, envir = table)
      remove(list=what, envir = table)
    }
  }
}

### the tag associated with a method signature.
### Should eventually use the same C code as dispatch, for consistency
.sigLabel <- function(sig)
  ## TODO:  should include package; i.e., class:package#class:...
  ## BUT:  not until defined, target slots have classes with package attribute
  paste(sig, collapse = "#")

.findInheritedMethods <-
    function(classes, fdef, mtable = NULL,
             table = get(".MTable", envir = environment(fdef)),
             excluded = NULL, useInherited, verbose = FALSE,
             returnAll = !(doMtable || doExcluded) )
{
  ## classes is a list of the class(x) for each arg in generic
  ## signature, with "missing" for missing args
  if(!is.environment(table)) {
    if(is(fdef, "standardGeneric"))
      stop("Invalid or unset methods table in generic function \"", fdef@generic,"\"")
    else
      stop("Trying to find a methods table in a non-generic function")
  }
  hasGroup <- length(fdef@group) > 0
  if(hasGroup)
        groupGenerics <- .getAllGroups(list(fdef))
  doMtable <- is.environment(mtable)
  doExcluded <- length(excluded) > 0
  nargs <- length(classes)
  if(verbose)
      cat(sprintf("* .findInheritedMethods(): nargs=%d, returnAll=%s\n",
                  nargs, returnAll))
  methods <- list()
  if(!missing(useInherited) && length(useInherited) < nargs)
    useInherited <- rep(useInherited, length = nargs)
  if(hasGroup && !doExcluded) {
    ## first try for an exact match in a group generic
    ## If this matches &  is cached, it then will be treated as a non-inherited method
    ## so no further calls here should occur.
    ##
    ## doExcluded is the findNextMethod case; we don't regard group methods as
    ## inherited in the nextMethod sense, since they have the same signature
    label <- .sigLabel(classes)
    direct <- .getGroupMethods(label, groupGenerics, FALSE)
##M doExcluded must be FALSE here
##M     if(doExcluded && length(direct) > 0)
##M       direct <- direct[is.na(match(names(direct), excluded))]
    if(length(direct) > 0 && doMtable) {
        assign(label, direct[[1]], envir = mtable)
        if(verbose) cat("* found (and cached) direct group method for", label,"\n")
        return(direct)
##M	  ## else must be returnAll, so include the group direct method
##M	  methods <- direct
    }
    ## else, continue because we may want all defined methods
  }
  def <- getClass(classes[[1]], .Force = TRUE)
  labels <-
      if(missing(useInherited) || useInherited[[1]])
          c(classes[[1]], names(def@contains), "ANY")
      else classes[[1]]
  if(nargs > 1) {
      classDefs <- vector("list", nargs)
      classDefs[[1]] <- def
      for(i in 2:nargs) {
          cc <- classDefs[[i]] <- getClass(classes[[i]], .Force = TRUE)
          allLabels <- if(missing(useInherited) || useInherited[[i]])
              c(cc@className, names(cc@contains), "ANY") else cc@className
          labels <- outerLabels(labels, allLabels)
      }
  }
  if(!returnAll)
    labels <- labels[-1] # drop exact match
  labels <- unique(labels) # only needed while contains slot can have duplicates(!)
  allMethods <- objects(table, all.names=TRUE)
  found <- match(labels, allMethods, 0) > 0
  for(label in labels[found])
      methods[[label]] <- get(label, envir = table)
  if(hasGroup) {
      ##  add the  group methods recursively found but each time
      ## only those not already included in found.
      groupmethods <- .getGroupMethods(labels, groupGenerics, found)
      fromGroup <- c(rep(FALSE, length(methods)), rep(TRUE, length(groupmethods)))
      methods <- c(methods, groupmethods)
  }
  else
      fromGroup <- rep(FALSE, length(methods))
  if(verbose) cat("*", length(methods), "preliminary methods:",
		  substr(deparse(names(methods)), 2, 1e6), "\n")
  ## remove default if its not the only method
  if(length(methods) > 1 && !returnAll) {
      defaultLabel <- paste(rep("ANY", nargs), collapse = "#")
      i <- match(defaultLabel, names(methods), 0)
      if(i > 0) {
          methods <- methods[-i]
          fromGroup <- fromGroup[-i]
      }
  }
  if(doExcluded)
    methods <- methods[is.na(match(names(methods), as.character(excluded)))]
  if(length(methods) > 1 && !returnAll) {
      if(nargs == 1)
          classDefs <- list(def)
      ## else, defined before
      if(verbose) cat("* getting best methods, reducing from remaining preliminary",
		      length(methods),"ones\n")
      methods <- .getBestMethods(methods, classDefs, fromGroup, verbose = verbose)
      if(length(methods) > 1)
	warning(gettextf(paste("Ambiguous method selection for \"%s\", target \"%s\"",
                               "(the first of the signatures shown will be used)\n%s\n"),
			 fdef@generic, .sigLabel(classes),
			 paste("   ", names(methods), collapse = "\n")))
      methods <- methods[1]
  }
  if(doMtable  && length(methods)>0) {
    tlabel <- .sigLabel(classes)
    m <- methods[[1]]
    if(is(m, "MethodDefinition"))  { # else, a primitive
      m@target <- .newSignature(classes, fdef@signature)
      methods[[1]] <- m
    }
    assign(tlabel, m, envir = mtable)
  }
  methods
}

.newSignature <- function(classes, names) {
  ## a simple version to deal with boostrapping stage
  i <- seq_len(min(length(classes), length(names)))
  value <- as.character(classes)[i]
  class(value) <- getClass("signature")@className
  value@names <- as.character(names)[i]
  value
}

.findNextFromTable <- function(method, f, optional, envir, prev = character()) {
  fdef <- getGeneric(f)
  env <- environment(fdef)
  target <- method@target
  n <- get(".SigLength", envir = env)
  defined <- method@defined
  m <- length(defined)
  if(m > n)
    length(defined) <- n
  else if(n > m)
    ## will only really need this to be a signature when the elements
    ## have package attribute--see .sigLabel
    defined <-  new("signature", fdef, c(defined@.Data, rep("ANY", n-m)))
  excluded <- c(prev, .sigLabel(defined))
  methods <- .findInheritedMethods(defined, fdef, mtable = NULL, excluded = excluded)
  if(length(methods)==0) # use default method, maybe recursively.
    methods <- list(finalDefaultMethod(fdef@default)) #todo: put a label on it?
  if(length(methods)>1)
    warning("found ", length(methods), " equally good next methods") #todo: better message
  ## excluded slot is a list, but with methods tables, elements are just labels
  new("MethodWithNext", method, nextMethod = methods[[1]], excluded =as.list(excluded))
}


  ## get the classes of the args

.InheritForDispatch <- function(classes, fdef, mtable) {
  methods <- .findInheritedMethods(classes, fdef, mtable)
  if(length(methods)==1)
    return(methods[[1]]) # the method
  else if(length(methods)==0) {
    cnames <- paste("\"", sapply(classes, as.character), "\"", sep = "", collapse = ", ")
    stop("unable to find an inherited method for function \"", fdef@generic,
         "\", for signature ", cnames)
  }
  else
    stop("Internal error in finding inherited methods; didn't return a unique method")
}

.findMethodInTable <- function(signature, table, fdef = NULL) {
  allMethods <- objects(table, all.names=TRUE)
  if(is(fdef, "genericFunction"))
    signature <- .matchSigLength(signature, fdef, environment(fdef), FALSE)
  label <- .sigLabel(signature)
  found <- match(label, allMethods, 0)
  if(found>0)
    get(label, envir = table)
  else
    NULL
}

## inheritance distances:  0 for the class, 1 for immediate contains, 2 for other contains
##    and 3 for ANY
.inhDistances <- function(classDef) {
  contains <- classDef@contains
  allNames <-  unique(names(contains)) # bug allows duplicates in contains
  dist <- rep(2, length(allNames))
  for(i in seq_along(dist)) {
    ci <- contains[[i]]
    dist[[i]] <- ci@distance
  }
  dist <- c(0, dist, NA)
  names(dist) <- c(classDef@className, allNames, "ANY")
  dist
}

.getBestMethods <- function(methods, classDefs, fromGroup, verbose = FALSE) {
    n <- length(methods)
    dist <- rep(0, n)
    nArg <- length(classDefs)
    defClasses <- matrix("ANY", nArg, n)
    for(j in 1:n) {
	cl <- methods[[j]]@defined@.Data
	defClasses[seq_along(cl), j] <- cl
    }
    containsDist <- lapply(classDefs, .inhDistances)
    maxDist <- max(unlist(containsDist), na.rm = TRUE) + 1
    if(verbose) { cat("** individual arguments' distances:\n"); print(containsDist) }
    ## add up the inheritance distances for each argument (row of defClasses)
    for(i in 1:nArg) {
	ihi <- containsDist[[i]]
	ihi[is.na(ihi)] <- maxDist
	cli <- defClasses[i,]
	dist <- dist + ihi[match(cli, names(ihi))]
    }
    if(verbose) cat("** final methods' distances: (",
		    paste(formatC(dist), collapse= ", "), ")\n", sep='')
    best <- dist == min(dist)
    ## of the least distance methods, choose direct, rather than group
    ## methods, unless all the best methods are from group generics
    if(!all(fromGroup[best]))
	best <- best & !fromGroup
    methods[best]
}

# add objects to the generic function's environment that allow
# table-based dispatch of methods
.setupMethodsTables <- function(generic,
                               initialize = !exists(".Methods", envir = env, inherits = FALSE)) {
  env <- environment(generic)
  if(initialize)
    mlist <- generic@default
  else
    mlist <- get(".Methods", envir = env, inherits = FALSE)
  if(initialize || !exists(".SigLength", envir = env, inherits = FALSE)) {
      nsig <- 1
      ## check that groups of generics agree on .SigLength; otherwise
      ## labels won't match
     for(gp in generic@group) {
         gpDef <- getGeneric(gp)
         if(is(gpDef, "genericFunction")) {
             .getMethodsTable(gpDef) # force initialization
             nsig <- max(nsig, get(".SigLength", envir = environment(gpDef)))
         }
     }
      assign(".SigLength", nsig, envir = env)
  }
  argSyms <- lapply(generic@signature, as.name)
  assign(".SigArgs", argSyms, envir = env)
  mtable <- .mlistAddToTable(generic, mlist)
  assign(".MTable", mtable, envir = env)
  .resetInheritedMethods(env, mtable)
  if(is(generic, "groupGenericFunction")) {
      for(gp in generic@groupMembers) {
          gpDef <- getGeneric(gp)
          if(is(gpDef, "genericFunction"))
              .getMethodsTable(gpDef) # force initialization w. group methods
      }
  }
}

.updateMethodsInTable <- function(generic, where, attach) {
  fenv <- environment(generic)
  reset <- identical(attach, "reset")
  if(!exists(".MTable", envir = fenv, inherits = FALSE))
    .setupMethodsTables(generic)
  mtable <- get(".MTable", envir = fenv)
  if(!reset) {
    env <- as.environment(where)
    tname <- .TableMetaName(generic@generic, generic@package)
    if(exists(tname, envir = env, inherits = FALSE)) {
      .mergeMethodsTable(generic, mtable, get(tname, envir = env), attach)
    }
    else {
      warning(gettextf(
              "Couldn't find methods table for \"%s\", package \"%s\" may be out of date",
                       generic@generic, generic@package))
      metaName <- mlistMetaName(generic)
      if(exists(metaName, envir = env, inherits = FALSE))
        .mlistAddToTable(generic, get(metaName, envir = env), mtable, attach)
      else
        warning("Methods list for generic \"", generic@generic,
                "\" not found")
    }
  }
  if(length(generic@group)>0) {
      groups <- as.list(generic@group)
      generics <- vector("list", length(groups))
      for(i in seq_along(groups))
        generics[[i]] <- getGeneric(groups[[i]])
    .checkGroupSigLength(groups, generics)
  }
  else if(is(generic, "groupGenericFunction"))
    .checkGroupSigLength(list(generic@generic), list(generic))
  .resetInheritedMethods(fenv, mtable)
  mtable
}

.resetInheritedMethods <- function(fenv, mtable) {
    allObjects <- character()
    direct <- objects(mtable, all=TRUE)
    if(exists(".AllMTable", envir = fenv, inherits = FALSE)) {
        ## remove all inherited methods.  Note that code (e.g. setMethod) that asigns
        ## a new method to mtable is responsible for copying it to allTable as well.
        allTable <- get(".AllMTable", envir = fenv)
        allObjects <- objects(allTable, all=TRUE)
        remove(list= allObjects[is.na(match(allObjects, direct))], envir = allTable)
    }
    else {
        allTable <- new.env(TRUE, fenv)
        assign(".AllMTable", allTable, envir = fenv)
    }
    ## check for missing direct objects; usually a non-existent AllMTable?
    if(any(is.na(match(direct, allObjects)))) {
        direct <- objects(mtable, all=TRUE)
        for(what in direct)
          assign(what, get(what, envir = mtable), envir = allTable)
    }
}

.UseMethodsTables <- TRUE

.UsingMethodsTables <- function(onOff = .UseMethodsTables, where = .methodsNamespace)
{
    prev <- .UseMethodsTables
    if(nargs()) {
	onOff <- as.logical(onOff)	# TRUE, FALSE or NA
	if(!is.na(onOff))
	    .assignOverBinding(".UseMethodsTables", onOff, where = where, FALSE)
	else
	    stop(gettextf(".UsingMethodsTables: 'onOff' is not TRUE or FALSE"))
    }
    prev
}

## In the following, consider separate "compute" and "print" functions/methods:
## Wish: alternative to 'classes' allow  "wild-card signature", e.g.,
##       showMethods("coerce", signature = c("dgeMatrix", "*"))
.showMethodsTable <- function(generic, includeDefs = FALSE, inherited = FALSE,
                              classes = NULL, showEmpty = TRUE, printTo = stdout())
{
    cf <- function(...) cat(file = printTo, sep = "", ...)
    sigString <- function(sig) paste(sig@names, "=\"", as.character(sig), "\"",
				     sep = "", collapse = ", ")
    qs <- function(what) paste('"', what, '"', collapse = ", ", sep = "")
    doFun <- function(func, pkg) cf("Function: ", func, " (package ", pkg, ")\n")
    env <- environment(generic)
    signature <- generic@signature
    table <- get(if(inherited) ".AllMTable" else ".MTable", envir = env)
    f <- generic@generic
    p <- packageSlot(f)
    if(is.null(p)) p <- "base"
    deflt <- new("signature", generic, "ANY")
    labels <- objects(table, all = TRUE)
    if(!is.null(classes) && length(labels) > 0) {
	sigL <- strsplit(labels, split = "#")
	keep <- !sapply(sigL, function(x, y) all(is.na(match(x, y))), classes)
	labels <- labels[keep]
    }
    if(length(labels) == 0) {
	if(showEmpty) {
	    doFun(f,p)
	    cf("<No methods>\n\n")
	}
	return(invisible())
    }
    ## else: non-empty methods list
    doFun(f,p)
    for(what in labels) {
	m <- get(what, envir = table)
	if( is(m, "MethodDefinition")) {
	    t <- m@target
	    if(length(t) == 0)
		t <- deflt
	    d <- m@defined
	    if(length(d) == 0)
		d <- deflt
	    cf(sigString(t), "\n")
	    if(!identical(t, d))
		cf("    (inherited from: ", sigString(d), ")\n")
            if(!.identC(m@generic, f) && length(m@generic) == 1 && nchar(m@generic)>0)
		cf("    (definition from function \"", m@generic, "\")\n")
	}
	if(includeDefs) {
	    if(is(m, "MethodDefinition"))
		m <- m@.Data
	    cat(deparse(m), sep="\n", "\n", file = printTo)
	}
    }
    cat("\n", file = printTo)
}

## temporary switch for tables
useMTable <- function(onOff = NA)
  .Call("R_set_method_dispatch", as.logical(onOff), PACKAGE = "methods")

## get all the group generic functions, in breadth-first order since
## direct group inheritance is closer than indirect (all existing
## groups are mutually exclusive, but multiple group membership is
## allowed)
.getAllGroups <- function(funs) {
  start <- length(funs)
  for(i in seq_along(funs)) {
    groups <- funs[[i]]@group
    funs <- c(funs, lapply(groups,
                           function(what) {
                             f <- getGeneric(what)
                             if(!is.function(f))
                               stop("Failed to find expected group generic function: ",
                                    what)
                             f
                           }))
    }
    # now the next generations recusively
    if(length(funs) > start) {
      nmore <- length(funs) - start
      more <- Recall(funs[(start+1):length(funs)])
      ## did we add any groups?
      if(length(more) > nmore)
        funs <- c(funs, more[(nmore+1):length(more)])
    }
    funs
}

.getGroupMethods <- function(labels, generics, found) {
  methods <- list()
  for(i in seq_along(generics)) {
    gen <- generics[[i]]
    if(!is(gen,"genericFunction"))
      stop("Invalid group generic function in search for inherited method (class \"",
           class(gen), "\"")
    table <- .getMethodsTable(gen)
    allMethods <- objects(table, all.names = TRUE)
    ## TODO:  possible for .SigLength to differ between group &
    ## members.  Requires expanding labels to max. length
    newFound <- rep(FALSE, length(found))
    newFound[!found] <- (match(labels[!found], allMethods, 0)>0)
    found <- found | newFound
    for(what in labels[newFound])
      methods[[what]] <- get(what, envir = table)
  }
  methods
}

.getMethodsTable <- function(fdef, env = environment(fdef),
                             check = TRUE, inherited = FALSE)
{
    name <- if(inherited) ".AllMTable" else ".MTable"
    if(check && !exists(name, envir = env, inherits = FALSE))
      .setupMethodsTables(fdef)
    get(name, envir = env)
}

.getGenericSigLength <- function(fdef, env = environment(fdef), check = TRUE) {
    if(check && !exists(".SigLength", envir = env, inherits = FALSE))
      .setupMethodsTables(fdef)
    get(".SigLength", envir = env)
}



.checkGroupSigLength <- function(gnames, generics = lapply(gnames, getGeneric)) {
  funs <- gnames
  recall <- FALSE
  for(i in seq_along(gnames)) {
    what <- gnames[[i]]
    fdef <- generics[[i]]
    if(!is(fdef, "groupGenericFunction")) {
      warning("Trying to check signature length of group generic \"",
              what, "\", but it is not a group generic")
      next
    }
    if(length(fdef@group) > 0)  {# push up the check one level
      gnames[[i]] <- fdef@group
      generics[[i]] <- lapply(fdef@group, getGeneric)
      recall <- TRUE
      next
    }
    funs <- c(funs, getGroupMembers(fdef, TRUE, FALSE))
  }
  if(recall)
    return(Recall(unlist(gnames, FALSE), unlist(generics, FALSE)))
  funs <- unique(funs)
  fdefs <- lapply(funs, function(x) {
    if(is.character(x) && length(x) == 1) getGeneric(x)
    else x})
  ## now compare the sig lengths
  sigs <- rep(0,length(funs))
  for(i in seq_along(sigs)) {
    what <- funs[[i]]
    fdef <- fdefs[[i]]
    if(is.null(fdef))
      next # getGroupMembers returns NULL if  member is not defined
    if(!is(fdef, "genericFunction"))
      warning("Trying to check signature length of generic \"",
              what, "\", but it is not a  generic function: i = ", i,
              ", funs = ", paste(unlist(funs), collapse = ", "),
              "; gnames = ",
              paste(as.character(gnames), collapse = ", "))
    else {
      ev <- environment(fdef)
      if(!exists(".SigLength", envir = ev, inherits = FALSE))
        .setupMethodsTables(fdef)
      sigs[i] <- get(".SigLength", envir = ev)
    }
  }
  n <- max(sigs)
    reset <- sigs < n & sigs > 0 # all the  sigs  for defined funs & less than max.
  if(any(reset)) {
    funs <- funs[reset]
    fdefs <- fdefs[reset]
    for(fdef in fdefs) {
        .resetSigLength(fdef, n)
    }
  }
  funs
}

## a simplified outer of paste
outerLabels <- function(labels, new) {
    ## WARNING: This code incorporates the definition of .sigLabel
    ## and so must change if that does (e.g. to include package)
    n <- length(labels)
    m <- length(new)
    paste(labels[rep(1:n, rep(m,n))], new[rep(1:m,n)], sep ="#")
}


.matchSigLength <- function(sig, fdef, fenv, reset = FALSE) {
  nargs <- .getGenericSigLength(fdef, fenv, TRUE)
  n <- length(sig)
  if(n < nargs)
    sig <- c(as.character(sig), rep("ANY", nargs - n))
  else if(n > nargs) { #reset table?
    if(all(sig[(nargs+1):n] == "ANY"))
      length(sig) <- nargs
    else {
      while(sig[[n]] == "ANY")
        n <- n-1
      if(reset)
        .resetSigLength(fdef, n)
      length(sig) <- n
    }
  }
  sig
}

.resetSigLength <- function(fdef, n) {
      fenv <- environment(fdef)
      assign(".SigLength", n, envir = fenv)
      mtable <- .getMethodsTable(fdef, fenv, check = FALSE)
      signames <- fdef@signature
      length(signames) <- n
      .resetTable(mtable, n, signames)
      .resetInheritedMethods(fenv, mtable)
}

.TableMetaName <- function(name, package)
  methodsPackageMetaName("T", paste(name, package, sep=":"))

.TableMetaPrefix <- function()
  methodsPackageMetaName("T","")

.addToMetaTable <- function(fdef, signature, definition, where, nSig) {
  return()
}

# the real version
..addToMetaTable <- function(fdef, signature, definition, where,
                             nSig = .getGenericSigLength(fdef, fenv)) {
    # TODO:  nSig should be a slot in the table
  tname <- .TableMetaName(fdef@generic, fdef@package)
  where <- as.environment(where)
  if(exists(tname, envir =where, inherits = FALSE)) {
     table <- get(tname, envir = where)
     if(length(signature) > nSig)
       .resetTable(table, length(signature), fdef@signature[seq_along(signature)])
  }
  else {
    table <- new.env(TRUE, environment(fdef))
    assign(tname, table, envir = where)
  }
  assign(.sigLabel(signature), definition, envir = table)
}

.makeGenericTables <- function(where) {
  generics <- .getGenerics(where)
  mtables <- .getGenerics(where, FALSE)
  for(i in seq_along(generics)) {
    name <- generics[[i]]
    mtable <- get(mtables[[i]], envir = where)
    generic <- .getGeneric(name, where = where)
    if(is.null(generic)) {
      warning(gettextf(
       "Could not find generic function \"%s\" to initialize cached methods",
                       name))
      next
    }
    table <- .getMethodsTable(generic)
    .mergeMethodsTable(generic, table, mtable)
    assign(.TableMetaName(generic@generic, generic@package), table, envir = where)
  }
}

.assignMethodsMetaTable <- function(mlist, generic, where, overwrite = TRUE) {
    tname <- .TableMetaName(generic@generic, generic@package)
    if(overwrite || !exists(tname, envir = where, inherits = FALSE)) {
        table <- .mlistAddToTable(generic, mlist)
        assign(tname, table, envir = where)
    }
}

.getGenericSigArgs <- function(fdef, env = environment(fdef), check = TRUE) {
    if(check && !exists(".SigLength", envir = env, inherits = FALSE))
      .setupMethodsTables(fdef)
    n <- get(".SigLength", envir = env)
    args <-  get(".SigArgs", envir = env)
    length(args) <- n
    args
}


## the most simple part of listFromMethods() below; not yet exported
tableNames <- function(generic, where, table) {
    fdef <- getGeneric(generic)
    if(missing(table))
	table <-
	    if(missing(where)) .getMethodsTable(fdef)
	    else get(.TableMetaName(fdef@generic, fdef@package),
                     envir = as.environment(where), inherits = FALSE)
    objects(table, all.names=TRUE)
}

listFromMethods <- function(generic, where, table) {
    fdef <- getGeneric(generic)
    if(missing(table))
	table <-
	    if(missing(where)) .getMethodsTable(fdef)
	    else get(.TableMetaName(fdef@generic, fdef@package),
		     envir = as.environment(where), inherits = FALSE)
    fev <- environment(fdef)
    nSigArgs <- .getGenericSigLength(fdef, fev)
    names <- objects(table, all=TRUE)
    methods <- lapply(names, function(x)get(x, envir = table))
    if(nSigArgs > 1) {
        n <- length(names)
        sigs <- vector("list", n)
        namesCon <- textConnection(names)
        for(i in seq_len(n))
            sigs[[i]] <- scan(namesCon, "", sep ="#", nmax = nSigArgs, quiet=TRUE)
    }
    else
      sigs <- as.list(names)
    new("LinearMethodsList", classes=sigs, methods=methods,
        arguments = .getGenericSigArgs(fdef, fev), generic = fdef)
}
