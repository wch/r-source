#  File src/library/methods/R/methodsTable.R
#  Part of the R package, http://www.R-project.org
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

### merge version called from namespace imports code.  Hope to avoid using generic
.mergeMethodsTable2 <- function(table, newtable, envir, metaname) {
    old <- objects(table, all.names=TRUE)
    mm <- 1
    for( what in old) {
      mm <- get(what, envir =table)
      if(is(mm, "MethodDefinition")) {
          mm <- length(mm@defined)
          break
      }
    }
    new <- objects(newtable, all.names=TRUE)
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
  newMethods <- objects(newtable, all.names=TRUE)
  for(what in newMethods) {
    obj <- get(what, envir = newtable)
    if(is.primitive(obj))
      sig <- anySig ## Assert doesn't need to be a signature obj., won't change ns
    else if(is(obj, "MethodDefinition"))
      sig <- obj@defined
    else
      stop(gettextf(
                    "Invalid object in meta table of methods for \"%s\", label \"%s\", had class \"%s\"",
                    generic@generic, what, class(obj)), domain=NA)
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
  NULL
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
    ## once generic functions are installed from 2.11.0 or later, this should
    ## only be called with mlist a method or NULL.
    if(is.null(mlist)) return(table)
    m <- (if(is(mlist, "MethodsList")) mlist@methods
        else list(ANY=mlist)
        )
  ## once MethodsList is defunct, this should be rewritten (and renamed!)

  ## the methods slot is a list named by class, with elements either
  ## method definitions or mlists
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
      stop("Invalid mlist element for signature \"", classes[[j]],
	   "\" at level ", i,
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
  if(missing(table) && !exists(".AllMTable", envir = fenv, inherits = FALSE))
    .setupMethodsTables(fdef)
  sig <- .matchSigLength(sig, fdef, fenv, TRUE)
  label <- .sigLabel(sig)
  if(is.null(def)) {
      if(exists(label, envir = table, inherits = FALSE))
          ## remove the method (convention for setMethod)
          remove(list = label, envir = table)
  }
  else
      assign(label, def, envir = table)
}


.resetTable <- function(table, n, signames) {
  ## after updating a methods table, the maximum no. of arguments in
  ## the signature increased to n.  Reassign any objects whose label
  ## does not match n classes from the defined slot
  anyLabel <- rep("ANY", n); seqN <- 1L:n
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
      label@.Data <- ifelse(seqN > length(label), anyLabel, label@.Data)
      label@names <- signames
      method@defined <- method@target <- label
    }
    newLabel <- .sigLabel(label)
    if(!identical(what, newLabel)) {
      assign(newLabel, method, envir = table)
      remove(list=what, envir = table)
    }
  }
  NULL
}

### the tag associated with a method signature.
### Should perhaps use the same C code as dispatch, for consistency,
### however, that code breaks out early in the collapse loop if no match.
### This code is not used for quick matching, so efficiency less critical.
.sigLabel <- function(sig)
  ## TODO:  should include package; i.e., class:package#class:...
  ## BUT:  not until defined, target slots have classes with package attribute
  paste(sig, collapse = "#")

## workhorse of selectMethod() [ -> ../Methods.R ] "
.findInheritedMethods <-
    function(classes, fdef, mtable = NULL,
             table = get(".MTable", envir = environment(fdef)),
             excluded = NULL, useInherited,
             returnAll = !(doCache || doExcluded),
             simpleOnly = .simpleInheritanceGeneric(fdef), verbose = FALSE,
             doCache = is.environment(mtable))
{
    ## to avoid infinite recursion, and somewhat for speed, turn off S4 methods for primitives
    primMethods <- .allowPrimitiveMethods(FALSE)
    on.exit(.allowPrimitiveMethods(primMethods))
    ## classes is a list of the class(x) for each arg in generic
    ## signature, with "missing" for missing args
    if(!is.environment(table)) {
        if(is(fdef, "standardGeneric"))
          stop("Invalid or unset methods table in generic function \"",
               fdef@generic,"\"")
        else
          stop("Trying to find a methods table in a non-generic function")
    }
    hasGroup <- length(fdef@group) > 0L
    if(hasGroup)
      groupGenerics <- .getAllGroups(list(fdef))
    doExcluded <- length(excluded) > 0L
    if(verbose)
      cat(" .findInheritedMethods(): (hasGroup, doCache, doExcluded)= (",
	  paste(c("f","T")[1+c(hasGroup, doCache, doExcluded)],collapse=", "),
	  ")\n", sep='')
    nargs <- length(classes)
    if(!missing(useInherited) && length(useInherited) < nargs)
      useInherited <- rep(useInherited, length.out = nargs)
    if(hasGroup && !doExcluded) {
        ## first try for an exact match in a group generic
        ## If this matches &  is cached, it then will be treated as a non-inherited method
        ## so no further calls here should occur.
        ##
        ## doExcluded is the findNextMethod case; we don't regard group methods as
        ## inherited in the nextMethod sense, since they have the same signature
        label <- .sigLabel(classes)
        direct <- .getGroupMethods(label, groupGenerics, FALSE)
        if(length(direct) && doCache) {
            assign(label, direct[[1L]], envir = mtable)
            return(direct)
        }
        ## else, continue because we may want all defined methods
    }
    cl1 <- classes[[1L]]
    def <- getClass(cl1, .Force = TRUE)
    labels <-
      if(missing(useInherited) || useInherited[[1L]])
          c(cl1, .eligibleSuperClasses(def@contains, simpleOnly), "ANY")
      else cl1
    supersList <- list(labels)
    if(nargs > 1) { ## further arguments
        classDefs <- vector("list", nargs)
        classDefs[[1L]] <- def
        for(i in 2:nargs) {
            cc <- classDefs[[i]] <- getClass(classes[[i]], .Force = TRUE)
            allLabels <- if(missing(useInherited) || useInherited[[i]])
                c(cc@className, .eligibleSuperClasses(cc@contains, simpleOnly),
                  "ANY")
            else cc@className
            labels <- outerLabels(labels, allLabels)
            supersList <- c(supersList, list(allLabels))
        }
    }
    if(!returnAll)
      labels <- labels[-1L] # drop exact match
    labels <- unique(labels)# only needed while contains slot can have duplicates(!)
    if(verbose) cat(" .fI> length(unique(method labels)) = ", length(labels))
    allMethods <- objects(table, all.names=TRUE)
    found <- match(labels, allMethods, 0L) > 0L
    nFound <- length(lab.found <- labels[found])
    methods <- list() # =?= vector("list", nFound) ; but fails??
    for(label in lab.found)
      methods[[label]] <- get(label, envir = table)
    if(verbose) cat(" >> found: ", nFound, "\n")
    if(hasGroup) {
        ##  add the  group methods recursively found but each time
        ## only those not already included in found.
        groupmethods <- .getGroupMethods(labels, groupGenerics, found)
        fromGroup <- c(rep(FALSE, length(methods)),
                       rep(TRUE,  length(groupmethods)))
        if(verbose) cat(" .fI> #{additional group methods}:",
                        length(groupmethods),"\n")
        methods <- c(methods, groupmethods)
    }
    else
      fromGroup <- rep(FALSE, length(methods))
    ## remove default (ANY,..,ANY) if its not the only method:
    if(length(methods) > 1L && !returnAll) {
        defaultLabel <- paste(rep.int("ANY", nargs), collapse = "#")
        i <- match(defaultLabel, names(methods), 0L)
        if(i > 0L) {
            methods <- methods[-i]
            fromGroup <- fromGroup[-i]
        }
    }
    if(doExcluded)
      methods <- methods[is.na(match(names(methods), as.character(excluded)))]
    if(length(methods) > 1L && !returnAll) {
        if(verbose) cat(" .fI> length(methods) = ", length(methods),
                        " --> ambiguity\n")
        ## have ambiguity to resolve
        select <- .getBestMethods(methods, supersList, fromGroup, verbose=verbose)
        ##         --------------
        if(length(select) > 1L) {
            if(verbose) cat(" .fI> found", length(select)," best methods\n")

            target <- .sigLabel(classes)
            condAction <- getOption("ambiguousMethodSelection")
            if(is.null(condAction))
              condAction <- .ambiguousMethodMessage
            else if(!is(condAction, "function"))
              stop(gettextf("The \"ambiguousMethodSelection\" option should be a function to be called as the condition action; got an object of class \"%s\"",
                            class(condAction)), domain = NA)

            select <- withCallingHandlers(
                                          .disambiguateMethods(classes, select, fdef@generic,
                                                               methods, supersList, fromGroup,
                                                               classDefs, verbose),
                                          ambiguousMethodSelection=condAction)
        }
        methods <- methods[select]
    }
    if(simpleOnly && length(methods) == 0L) {
        methods <- Recall(classes, fdef, mtable, table, excluded, useInherited,
                          verbose, returnAll, FALSE)
        if(length(methods) > 0L)
          message(gettextf("No simply inherited methods found for function \"%s\"; using non-simple method",
                           fdef@generic), domain = NA)
    }
    if(doCache && length(methods)) { ## Cache the newly found one
        if(verbose) cat(" .fI> caching newly found methods ..\n")
        tlabel <- .sigLabel(classes)
        m <- methods[[1L]]
        if(is(m, "MethodDefinition"))  { # else, a primitive
            m@target <- .newSignature(classes, fdef@signature)
            ## if any of the inheritance is not simple, must insert coerce's in method body
            coerce <- .inheritedArgsExpression(m@target, m@defined, body(m))
            if(!is.null(coerce))
              body(m) <- coerce
            methods[[1L]] <- m
        }
        assign(tlabel, m, envir = mtable)
    }
    methods
}

.ambiguousMethodMessage <- function(cond) {
  selected <- attr(cond, "selected")
  if(is.null(selected)) {# not properly set up, so just use the message
    message(cond$message)
  }
  else {
    possible <- attr(cond, "candidates")
    message(gettextf('Note: Method with signature "%s" chosen for function "%s",\n target signature "%s".\n %s would also be valid',
                     selected, attr(cond, "generic"), attr(cond, "target"),
                     paste('"', possible[is.na(match(possible, selected))], '"',
                           sep="", collapse=", ")),
            domain = NA)
  }
}

.simpleInheritanceGeneric <- function(fdef) {
    identical(attr(fdef@signature, "simpleOnly"), TRUE)
}

.eligibleSuperClasses <- function(contains, simpleOnly) {
    what <- names(contains)
    if(!length(what))
      what
    else {
        if(simpleOnly)
            eligible <- sapply(contains, function(x) (is.logical(x) && x) || x@simple)
        else # eliminate conditional inheritance
            eligible <- sapply(contains, function(x) (is.logical(x) && x) || x@simple || identical(x@test, .simpleExtTest))
        what[eligible]
    }
}

.newSignature <- function(classes, names) {
  ## a simple version to deal with boostrapping stage, used in new() etc
  i <- seq_len(min(length(classes), length(names)))

  ## Simplified version ...
  structure(as.character(classes)[i],
            class = getClass("signature")@className,
            names = as.character(names)[i])
  ## ... of the following (which uses `@<-` in non-standard way):
  ## value <- as.character(classes)[i]
  ## class(value) <- getClass("signature")@className
  ## attr(value,"names") <- as.character(names)[i]
  ## value@names <- as.character(names)[i]
  ## value
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
  if(length(methods) == 0L) # use default method, maybe recursively.
    methods <- list(finalDefaultMethod(fdef@default)) #todo: put a label on it?
  if(length(methods) > 1L)
    warning("found ", length(methods), " equally good next methods") #todo: better message
  ## excluded slot is a list, but with methods tables, elements are just labels
  new("MethodWithNext", method, nextMethod = methods[[1L]],
      excluded = as.list(excluded))
}


  ## get the classes of the args

.InheritForDispatch <- function(classes, fdef, mtable) {
  methods <- .findInheritedMethods(classes, fdef, mtable)
  if(length(methods) == 1L)
    return(methods[[1L]]) # the method
  else if(length(methods) == 0L) {
    cnames <- paste("\"", sapply(classes, as.character), "\"",
                    sep = "", collapse = ", ")
    stop("unable to find an inherited method for function \"", fdef@generic,
         "\", for signature ", cnames)
  }
  else
    stop("Internal error in finding inherited methods; didn't return a unique method")
}

.findMethodInTable <- function(signature, table, fdef = NULL)
{
    if(is(fdef, "genericFunction"))
        signature <- .matchSigLength(signature, fdef, environment(fdef), FALSE)
    label <- .sigLabel(signature)
##     allMethods <- objects(table, all.names=TRUE)
##     if(match(label, allMethods, nomatch = 0L))
    if(exists(label, envir = table, inherits = FALSE))
        get(label, envir = table) ## else NULL
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

.leastMethodDistance <- function(methods, supersList, classDefs, fromGroup, verbose = FALSE) {
    n <- length(methods)
    dist <- rep(0, n)
    nArg <- length(classDefs)
    defClasses <- matrix("ANY", nArg, n)
    for(j in 1L:n) {
	cl <- methods[[j]]@defined@.Data
	defClasses[seq_along(cl), j] <- cl
    }
    containsDist <- lapply(classDefs, .inhDistances)
    maxDist <- max(unlist(containsDist), na.rm = TRUE) + 1
    if(verbose) { cat("** individual arguments' distances:\n"); print(containsDist) }
    ## add up the inheritance distances for each argument (row of defClasses)
    for(i in 1L:nArg) {
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
    if(any(fromGroup[best]) && !all(fromGroup[best]))
	best <- best & !fromGroup
    (1:n)[best]
}

## currently called exactly once from .findInheritedMethods() :
.getBestMethods <- function(methods, supersList, fromGroup, verbose = FALSE) {
    n <- length(methods)      ## >= 2
    nArg <- length(supersList)## >= 1
    sigs <- matrix("ANY", nArg, n)
    for(i in 1:n) {
      sig <- methods[[i]]@defined
      if(length(sig) < nArg) { # is this still possible? --> show 'verbose'
	if(verbose) cat(sprintf(" .. method %d: length(sig) = %d < nArg = %d\n",
				i, length(sig), nArg))
	sigs[seq_along(sig), i] <- sig
      }
      else
        sigs[,i] <- sig
    }
    if(nArg < 2) { # the easy case
      return(which.min(match(sigs[1L,], supersList[[1L]])))
    }
    ## else  nArg >= 2
    best      <- rep.int(TRUE,  n)
    dominated <- rep.int(FALSE, n)
    pos <- matrix(0L, nArg, n)
    for(i in 1:nArg) {
        pos[i,] <- match(sigs[i,], supersList[[i]])
    }
    ## pairwise comparison of columns of pos.  Any way to vectorize?
    seqn <- seq_len(n)
    for(i in seqn) {
      for(j in seqn[-i]) {
        diffs <- pos[,j] - pos[,i]
	if(any(diffs < 0))  { best[i] <- FALSE; if(dominated[i]) break }
	if(all(diffs <= 0)) { dominated[i] <- TRUE; if(!best[i]) break }
      }
    }
    if(verbose)
	cat(if(any(best)) paste(" have best ones",
				paste(format(seqn[best]),collapse=","))
	    else if(any(dominated)) paste(" can eliminate dominated ones,",
				    paste(format(seqn[dominated]),collapse=",")),
	    "\n")
    ## a best method is as early in the superclasses as any other on all arguments
    ## Because the signatures are not duplicated, there can be at most one.
    if(any(best))
      seqn[best]
    ## eliminate those methods dominated by another
    else
      seqn[!dominated]
}

## currently called exactly once from .findInheritedMethods() :
.disambiguateMethods <- function(target, which, generic, methods, supersList,
                                 fromGroup, classDefs, verbose)
{
  ## save full set of possibilities for condition object
  candidates <- methods[which]
  note <- character()
  ## choose based on total generational distance
  which2 <- .leastMethodDistance(candidates, supersList, classDefs,
                                 fromGroup[which])
  if(length(which2) < length(which)) {
    note <- c(note, gettextf("Selecting %d methods of min. distance", which2))
    which <- which[which2]
  }
  ## if some are group methods, eliminate those
  if(length(which) > 1 && any(fromGroup[which]) && !all(fromGroup[which])) {
    which <- which[!fromGroup]
    note <- c(note, gettextf("Selecting %d non-group methods", length(which)))
  }
  ## prefer partially direct methods
  if(length(which) > 1) {
    direct <- sapply(methods[which], function(x, target)
                     (is(x, "MethodDefinition") && any(target == x@defined)),
                     target = target)
    if(any(direct) && !all(direct)) {
      which <- which[direct]
      note <- c(note, gettextf("Selecting %d partially exact-matching method(s)",
                               length(which)))
    }
  }
  which <- which[[1L]]
  selected <- names(methods)[[which]]
  ## FIXME (?): This is not shown to the user
  msg <- gettextf("Choosing method %s from %d ambiguous possibilities",
                      selected, length(candidates))
  condObject <- simpleCondition(msg)
  ## would be nice to use an S4 class eventually
  class(condObject) <- c("ambiguousMethodSelection", class(condObject))
  attributes(condObject) <-
      c(attributes(condObject),
	list("candidates" = names(candidates),
	     "target"	  = .sigLabel(target),
	     "selected"	  = selected,
	     "generic"	  = generic,
	     "notes" = if(length(note)) paste(note, collapse ="; ") else ""))
  if(verbose) cat("   .disambiguateM*(): notes =\n\t",
		  attr(condObject, "notes"), "\n")
  signalCondition(condObject)
  which
}

# add objects to the generic function's environment that allow
# table-based dispatch of methods
.setupMethodsTables <- function(generic,
		initialize = !exists(".MTable", envir = env, inherits = FALSE))
{
    env <- environment(generic)
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
    if(initialize) {
        mlist <- generic@default # from 2.11.0: method, primitive or NULL, not MethodsList
        mtable <- .mlistAddToTable(generic, mlist) # by default, adds to an empty table
        assign(".MTable", mtable, envir = env)
    }
    else ## the current .MTable
        mtable <- getMethodsForDispatch(generic)
    .resetInheritedMethods(env, mtable)
    if(is(generic, "groupGenericFunction")) {
        for(gp in generic@groupMembers) {
            gpDef <- getGeneric(gp)
            if(is(gpDef, "genericFunction"))
                .getMethodsTable(gpDef) # force initialization w. group methods
        }
    }
    NULL
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
    ## else used to warn, but the generic may be implicitly required
    ## by class inheritance, without any explicit methods in this package
  }
  if(length(generic@group)) {
      groups <- as.list(generic@group)
      generics <- vector("list", length(groups))
      for(i in seq_along(groups))
        generics[[i]] <- getGeneric(groups[[i]])
    .checkGroupSigLength(groups, generics)
  }
  if(is(generic, "groupGenericFunction")) {
      .checkGroupSigLength(list(generic@generic), list(generic))
      for(g in getGroupMembers(generic))
          .updateMethodsInTable(getGeneric(g), where, attach)
  }
  .resetInheritedMethods(fenv, mtable)
  mtable
}

.resetInheritedMethods <- function(fenv, mtable) {
    allObjects <- character()
    direct <- objects(mtable, all.names=TRUE)
    if(exists(".AllMTable", envir = fenv, inherits = FALSE)) {
        ## remove all inherited methods.  Note that code (e.g. setMethod) that asigns
        ## a new method to mtable is responsible for copying it to allTable as well.
        allTable <- get(".AllMTable", envir = fenv)
        allObjects <- objects(allTable, all.names=TRUE)
        remove(list= allObjects[is.na(match(allObjects, direct))], envir = allTable)
    }
    else {
        allTable <- new.env(TRUE, fenv)
        assign(".AllMTable", allTable, envir = fenv)
    }
    ## check for missing direct objects; usually a non-existent AllMTable?
    if(any(is.na(match(direct, allObjects)))) {
        direct <- objects(mtable, all.names=TRUE)
        for(what in direct)
          assign(what, get(what, envir = mtable), envir = allTable)
    }
    NULL
}

## In the following, consider separate "compute" and "print" functions/methods:
## Wish: alternative to 'classes' allow  "wild-card signature", e.g.,
##       showMethods("coerce", signature = c("dgeMatrix", "*"))
.showMethodsTable <- function(generic, includeDefs = FALSE, inherited = FALSE,
                              classes = NULL, showEmpty = TRUE, printTo = stdout())
{
    cf <- function(...) cat(file = printTo, sep = "", ...)
    sigString <- function(sig) paste(names(sig), "=\"", as.character(sig), "\"",
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
    labels <- objects(table, all.names = TRUE)
    if(!is.null(classes) && length(labels)) {
	sigL <- strsplit(labels, split = "#")
	keep <- !sapply(sigL, function(x, y) all(is.na(match(x, y))), classes)
	labels <- labels[keep]
    }
    if(length(labels) == 0L) {
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
	    if(length(t) == 0L)
		t <- deflt
	    d <- m@defined
	    if(length(d) == 0L)
		d <- deflt
	    cf(sigString(t), "\n")
	    if(!identical(t, d))
		cf("    (inherited from: ", sigString(d), ")\n")
            if(!.identC(m@generic, f) && length(m@generic) == 1L &&
               nzchar(m@generic))
		cf("    (definition from function \"", m@generic, "\")\n")
	}
	if(includeDefs && is(m, "function")) {
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
    ## now the next generations recusively
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
    newFound[!found] <- (match(labels[!found], allMethods, 0L) > 0L)
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
    if(check && !exists(name, envir = env, inherits = FALSE)) {
	.setupMethodsTables(fdef, initialize = TRUE)
	if(!exists(name, envir = env, inherits = FALSE))
	    stop("Invalid methods table request")
    }
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
    if(length(fdef@group))  {# push up the check one level
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
    if(is.character(x) && length(x) == 1L) getGeneric(x)
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
    paste(labels[rep.int(1L:n, rep.int(m,n))], new[rep.int(1L:m,n)], sep ="#")
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

# regexp for matching table names; semi-general but assumes the
# meta pattern starts with "." and has no other special characters
.TableMetaPattern <- function()
    paste("^[.]",substring(methodsPackageMetaName("T",""),2), sep = "")

.addToMetaTable <- function(fdef, signature, definition, where, nSig) {
  return()
}

## the real version
..addToMetaTable <- function(fdef, signature, definition, where,
                             nSig = .getGenericSigLength(fdef)) {
    ## TODO:  nSig should be a slot in the table
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
  .cacheMethodInTable(fdef, signature, definition, table)
}

## Assertion: following is unused
.assignMethodsMetaTable <- function(mlist, generic, where, overwrite = TRUE) {
    .MlistDeprecated(".assignMethodsMetaTable")
    tname <- .TableMetaName(generic@generic, generic@package)
    if(overwrite || !exists(tname, envir = where, inherits = FALSE)) {
        table <- .mlistAddToTable(generic, mlist) # asserted never to be called.
        assign(tname, table, envir = where)
    }
}

.removeMethodsMetaTable <- function(generic, where) {
    ## does not warn if none exists, on the theory that a generic may be created
    ## but no methods defined to create a table.  The use of implicitGeneric's is an example.
    tname <- .TableMetaName(generic@generic, generic@package)
    if(exists(tname, where, inherits = FALSE))
      rm(list=tname, pos = where)
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
    names <- objects(table, all.names=TRUE)
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

.makeMlist1 <- function(arg, objects, j = 1) {
    mnames <- character(length(objects))
    for(i in seq_along(objects)) {
        what <- objects[[i]]
        if(is.primitive(what))
          sig <- "ANY"
        else
          sig <- what@defined
        mnames[[i]] <- (if(length(sig) < j) "ANY" else sig[[j]])
    }
    names(objects) <- mnames
    new("MethodsList", argument = arg, methods = objects, allMethods = objects)
}

.makeMlist2 <- function(args, objects, j = 1) {
    ## make a list according to  argument j, convert these as needed
    mlists <- list()
    for(what in objects) {
        sig <- if(!is.primitive(what)) what@defined # else NULL
        if(length(sig) <= j)
            arg1 <- arg2 <- "ANY"
        else {
            arg1 <- sig[[j]]
            arg2 <- sig[[j+1]]
        }
        x <- list(what)
        el <- mlists[[arg1, exact = TRUE]]
        mlists[[arg1]] <- (if(is.null(el)) x else c(el, x))
    }
    jNext <- j+1
    if(jNext < length(args))
      for(i in seq_along(mlists))
          mlists[[i]] <- .makeMlist2(args, mlists[[i]], jNext)
    else {
        arg2 <- as.name(args[[jNext]])
        for(i in seq_along(mlists))
          mlists[[i]] <- .makeMlist1(arg2, mlists[[i]], jNext)
    }
    new("MethodsList", argument = as.name(args[[1L]]),
        methods = mlists, allMethods = mlists)
}

.makeMlistFromTable <- function(generic, where = NULL) {
    .getAll <- function(what, table) {
        value <- list(length(what))
        for(i in seq_along(what))
          value[[i]] <- get(what[[i]], envir = table)
        value
    }
    if(is.null(where)) {
        what <- ".MTable"
        where <- environment(generic)
    }
    else {
        where <- as.environment(where)
        what <-  .TableMetaName(generic@generic, generic@package)
    }
    if(exists(what, envir = where, inherits= FALSE))
        table <- get(what, envir = where)
    else
        table <- new.env()
    value <- new("MethodsList", argument = as.name(generic@signature[[1]]))
    allNames <- objects(table, all.names = TRUE)
    if(length(allNames) == 0L)
      return(value)
    argNames <- generic@signature
    ## USES THE PATTERN OF class#class#.... in the methods tables
    nargs <- nchar(unique(gsub("[^#]","", allNames)))+1
    if(length(nargs) > 1L) {
        warning("Something weird:  inconsistent number of args in methods table strings:", paste(nargs,collapse = ", ")," (using the largest value)")
        nargs <- max(nargs)
    }
    length(argNames) <- nargs # the number of args used
    if(nargs == 1)
        .makeMlist1(as.name(argNames[[1L]]), .getAll(allNames, table))
    else
      .makeMlist2(argNames, .getAll(allNames, table))
 }

## assign a methods meta-data table, by default (and usually) a copy of the table
## from the generic function with the initial methods, if any.
.assignMethodsTableMetaData <- function(name, generic, where, table) {
    what <-  .TableMetaName(generic@generic, generic@package)
    if(missing(table))
          table <- .copyEnv(.getMethodsTable(generic))
    assign(what, table, envir = as.environment(where))
}

.getMethodsTableMetaData <-  function(generic, where, optional = FALSE) {
    what <-  .TableMetaName(generic@generic, generic@package)
    if(exists(what, envir = where, inherits = FALSE))
      get(what, envir = where )
    else if(optional)
      NULL
    else
      stop(gettextf("No methods table for generic \"%s\" from package \"%s\" in package \"%s\"",
                    generic@generic, generic@package, getPackageName(where)),
           domain = NA)
}

.inheritedArgsExpression <- function(target, defined, body) {
    expr <- substitute({}, list(DUMMY = "")) # bug if you use quote({})--is overwritten!!
    args <- names(defined)
    for(i in seq_along(defined)) {
        ei <- extends(target[[i]], defined[[i]], fullInfo = TRUE)
        if(is(ei, "SClassExtension")  && !ei@simple)
          expr[[length(expr) + 1L]] <-
            substitute(ARG <- as(ARG, DEFINED, strict = FALSE),
                       list(ARG = as.name(args[[i]]),
                            DEFINED = as.character(defined[[i]])))
    }
    if(length(expr) > 1L) {
       expr[[length(expr) + 1L]] <- body
       expr
   }
    else
      NULL
}

testInheritedMethods <- function(f, signatures, test = TRUE,  virtual = FALSE,
                                 groupMethods = TRUE,  where = .GlobalEnv)
{
  getSigs <- function(fdef)
      objects(methods:::.getMethodsTable(fdef), all.names = TRUE)

  ## Function relevantClasses is defined here to set object .undefClasses
  ## in testInheritedMethods as a marker to warn about undefined subclasses
  .relevantClasses <- function(classes, excludeVirtual, where, doinheritance) {
    classDefs <- lapply(classes, getClassDef, where)
    undefs <- sapply(classDefs, is.null)
    if(any(undefs)) {
      .undefClasses <<- unique(c(.undefClasses, classes[undefs]))
      classes <- classes[!undefs]
      classDefs <- classDefs[!undefs]
    }
    if(doinheritance) {
      allSubs <- lapply(classDefs,  function(what) names(what@subclasses))
      allSubs <- unique(unlist(allSubs))
      pattern <- sapply(allSubs, .matchSubsPattern, classes, excludeVirtual)
      ## exclude virtuals
      if(excludeVirtual) {
        excl <- nzchar(pattern)
        pattern <- pattern[excl]
        allSubs <- allSubs[excl]
      }
      if(length(allSubs)>0)
        allSubs <- sapply(split(allSubs, pattern), `[[`,1)
      else
        allSubs <- character()
    }
    else
      allSubs <- character()
    ## prepend the classes themselves, as appropriate
    iAny <- match( "ANY", classes, 0)
    if(iAny > 0) {
      classes[[iAny]] <- ".Other" # non-virtual placeholder for ANY
      classDefs[[iAny]] <- getClassDef(".Other")
    }
    if(excludeVirtual)
      classes <- classes[sapply(classDefs, function(def) identical(def@virtual, FALSE))]
    unique(c(classes, allSubs))
  }
  ## end of .relevantClasses

  if(!is(f, "genericFunction"))
    f <- getGeneric(f)
  fname <- f@generic
  if(missing(signatures)) {
    mdefs <- findMethods(f)
    mnames <- names(mdefs)
    sigs <-  findMethodSignatures(methods = mdefs)
    if(groupMethods) {
      groups <- getGroup(f, recursive = TRUE)
      for(group in groups) {
        fg <- getGeneric(group)
        mg <- findMethods(fg)
        sigsg <- findMethodSignatures(methods = mg)
        newSigs <- is.na(match(names(mg), mnames))
        mg <- mg[newSigs]
        mdefs <- c(mdefs, mg[newSigs])
        sigs <- rbind(sigs, sigsg[newSigs,])
        mnames <- c(mnames, names(mg)[newSigs])
      }
    }
    if(length(sigs) == 0)
      return(new("MethodSelectionReport", generic = fname))
    ## possible selection of which args to include with inheritance
    ok <- if(fname %in% c("coerce", "coerce<-"))
	match(colnames(sigs), "from", 0) > 0 else rep.int(TRUE, ncol(sigs))
    for(j in seq_len(ncol(sigs))) {
      classesj <-unique(sigs[,j])
      .undefClasses <- character()
      subclasses <- .relevantClasses(classesj, !virtual, where, ok[[j]])
      nj <- length(subclasses)
      ##       if(nj == 0) {  ##FIXME, wrong test
      ##         warning(gettextf("No elligible subclasses for argument \"%s\" found, so no contribution to analysis",
      ##                          colnames(sigs)[[j]]), domain  = NA)
      ##         next
      ##       }
      if(j > 1) {
        ## replicate all the previous elements of subclasses a la outer
        subclasses <- rep(subclasses, rep.int(ncomb, nj))
        ncomb <- ncomb * nj
        sigLabels <- paste(rep(sigLabels, times = nj), subclasses, sep = "#")
      }
      else {
        sigLabels <- subclasses
        ncomb <- nj
      }

      if(length(.undefClasses)) {
        warning("Undefined classes (", paste('"',unique(.undefClasses),'"', sep="", collapse=", "),
                ") will be ignored, for argument ", colnames(sigs)[[j]])
        .undefClasses <- character()
      }
    } ## loop on j
    ## now split the individual labels back into signatures
    signatures <- strsplit(sigLabels, "#", fixed = TRUE)
  } ## end of missing(signatures) case
  else if(is(signatures, "matrix") && identical(typeof(signatures), "character")
       && ncol(signatures) <= length(f@signature)) {
      ## turn signatures back into a list
      siglist <- vector("list", nrow(signatures))
      for(i in seq_len(nrow(signatures)))
        siglist[[i]] <- signatures[i,]
      signatures <- siglist
  }
  else stop('Argument "signatures" must be a character matrix whose rows are method signatures')
  ambig_target <- character()
  ambig_candidates <- list()
  ambig_selected <- character()
  ambig_note <- character()
  if(test) {
    ## define a handler that accumulates the attributes from the condition object
    warninghandler <- function(cond) {
      ambig_target <<- c(ambig_target, attr(cond, "target"))
      ambig_candidates <<- c(ambig_candidates, list(attr(cond, "candidates")))
      ambig_selected <<- c(ambig_selected, attr(cond, "selected"))
      ambig_note <<- c(ambig_note, attr(cond, "note"))
    }
    ambigOpt <- options(ambiguousMethodSelection = warninghandler)
    on.exit(options(ambigOpt))
    doSelect <-  function(sig) {
      x <- selectMethod(f = f, sig, optional = TRUE)
      if(is(x, "MethodDefinition")) {
        nsig <- x@defined
        if(length(nsig) < length(sig))
          c(nsig, rep("ANY", length(sig) - length(nsig)))
        else
          nsig
      }
      else if(is.null(x))
        rep("<NONE>", length(sig))
      else # primitive
        rep("ANY", length(sig))
    }
    signatures <- lapply(signatures, doSelect)
  }
  signatures <- sapply(signatures, function(x)paste(x, collapse = "#"))
  names(signatures) <- sigLabels

  new("MethodSelectionReport", generic = fname, allSelections = signatures,
      target = ambig_target, selected = ambig_selected,
      candidates = ambig_candidates, note = ambig_note)
}

.matchSubsPattern <- function(what, matchto, excludeVirtual) {
  def <- getClass(what)
  if(excludeVirtual & def@virtual)
    return("")
  matches <- match(names(def@contains), matchto, 0)
  matches <- matches[matches>0]
  paste(matches, collapse=".")
}

