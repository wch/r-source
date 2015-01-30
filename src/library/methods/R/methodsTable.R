#  File src/library/methods/R/methodsTable.R
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

### merge version called from namespace imports code.  Hope to avoid using generic
.mergeMethodsTable2 <- function(table, newtable, envir, metaname) {
    old <- objects(envir=table, all.names=TRUE)
    mm <- 1
    for( what in old) {
      mm <- get(what, envir =table)
      if(is(mm, "MethodDefinition")) {
          mm <- length(mm@defined)
          break
      }
    }
    new <- objects(envir=newtable, all.names=TRUE)
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
  allTable <- if(!add) get(".AllMTable", envir = fenv) ## else NULL
                                        # .AllMTable but only if required
  n <- get(".SigLength", envir = fenv)
  anySig <- rep("ANY", n) # assert doesn't need to be a real signature
  anyLabel <- .sigLabel(anySig)
  newMethods <- objects(envir=newtable, all.names=TRUE)
  for(what in newMethods) {
    obj <- get(what, envir = newtable)
    if(is.primitive(obj))
      sig <- anySig
    else if(is(obj, "MethodDefinition"))
      sig <- obj@defined
    else if(is.environment(obj)) {
       objsWhat <- objects(obj, all.names=TRUE)
       if(length(objsWhat) == 0)
           next # empty environment, ignore
       sig <- NULL
       for(ww in objsWhat) {
           objw <- get(ww, envir = obj)
           if(is(objw, "MethodDefinition"))
               sig <- objw@defined
       }
       if(is.null(sig))
           sig <- anySig
    }
    else
      stop(gettextf("invalid object in meta table of methods for %s, label %s, had class %s",
                    sQuote(generic@generic),
                    sQuote(what),
                    dQuote(class(obj))),
           domain = NA)
    ns <- length(sig)
    if(ns == n) {}
    else {
      if(ns < n) {
        nadd <- n - ns
        sigPackage <- packageSlot(sig)
        if(length(sigPackage)< ns) # probably out of date?
            sigPackage <- c(sigPackage, rep("", ns - length(sigPackage)))
        sig <-  .simpleSignature(c(sig, rep("ANY", nadd)),
                    names = generic@signature[1:n],
                    packages = c(sigPackage, rep("methods", nadd)))
        obj <- .xpdSignature(obj, sig, n-ns)
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
    if(add) {
        if(exists(what, envir = table, inherits = FALSE)) {
            obj <- .newOrMultipleMethod(obj, what, table)
            ## must replace in .AllMTable also
            if(is.null(allTable))
                allTable <- get(".AllMTable", envir = fenv)
            assign(what, obj, envir = allTable)
        }
        assign(what, obj, envir = table)
    }
    else if(exists(what, envir = table, inherits = FALSE) &&
            !all(obj@defined == "ANY") ) {
        ## remove methods, but not the default
        remove(list = what, envir = allTable)
        remove(list = what, envir = table)
    }
    ## else warning?
  }
  NULL
}

.xpdSignature <- function(obj, sig, nadd) {
    if(is(obj, "MethodDefinition")) {
        obj@defined <- sig
        obj@target <- sig
    }
    else if(is.environment(obj)) {
        xtrPkg <- rep("methods", nadd)
        for(what in objects(obj)) {
            objw <- get(what, envir = obj)
            if(is(objw, "MethodDefinition")) {
                sigw <- objw@defined
                pkgw <- packageSlot(sigw)
                if(length(pkgw) < length(sigw))
                    pkgw <- c(pkgw, rep("", length(sigw) - length(pkgw)))
                sigw <- .simpleSignature( c(sigw, rep("ANY", nadd)),
                        names = names(sig),
                        packages = c(pkgw, rep("methods", nadd)))
                objw@defined <- objw@target <- sigw
                remove(list = what, envir = obj)
                var <- .pkgMethodLabel(objw)
                if(nzchar(var)) assign(var, objw, envir = obj)
            }
        }
    }
    obj
}

## a simpler version of setting up a signature object
## For better or worse, the initialize() method expects
## a function definition and calls .MakeSignature()
.simpleSignature <- function(classes, names, packages) {
    object <- new("signature")
    object@.Data <- classes
    object@names <- names
    object@package <- packages
    object
}

.newOrMultipleMethod <- function(obj, what, table) {
    if(!.duplicateClassesExist())
        return(obj)
    current <- get(what, envir = table)
    if(is.environment(current)) {
        if(is.environment(obj))
            for(whatObj in objects(obj, all.names = TRUE))
                assign(whatObj, get(whatObj, envir = obj),
                       envir = current)
        else if(is(obj, "MethdodDefinition")) {
            var <- .pkgMethodLabel(obj)
            if(nzchar(var)) assign(var, obj, envir = current)
        }
        current
    }
    else if(is(current, "MethodDefinition")) {
        curPkg <- packageSlot(current@defined)
        if(is(obj, "MethodDefinition")) {
            objPkg <- packageSlot(obj@defined)
            if(is.null(curPkg) || is.null(objPkg) ||
               identical(curPkg, objPkg))
                return(obj)
            else {
                merge <- new.env()
                var <- .pkgMethodLabel(obj)
                if(nzchar(var)) assign(var, obj, envir = merge)
                var <- .pkgMethodLabel(current)
                if(nzchar(var)) assign(var, current, envir = merge)
                return(merge)
            }
        }
        else if(is.environment(obj)) {
            merge <- new.env()
            assign(.pkgMethodLabel(current), current, envir = merge)
            for(whatObj in objects(obj, all.names = TRUE))
                assign(whatObj, get(whatObj, envir = obj),
                       envir = merge)
            return(merge)
        }
        ## else adding a primitive, should do nothing
        else
            current
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
      stop(gettextf("invalid mlist element for signature %s at level %d (should be methods list or method, had class %s)",
                    sQuote(classes[[j]]),
                    i,
                    dQuote(class(el))),
           domain = NA)
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
  isCurrent <- exists(label, envir = table, inherits = FALSE)
  if(is.null(def)) { # remove the method (convention for setMethod)
      if(isCurrent)
          remove(list = label, envir = table)
  }
  else {
      dupl <- .duplicateClassesExist()
      ## ensure that a valid object is assigned:  if duplicate classes
      ## exist, may need a table by package label; else, make sure
      ## the target and defined slots are complete
      ## IF we believed all methods up to date, the call could be conditional
      ##    if(dupl || isCurrent)
      def <- .methodPackageSlots(def, label, table, dupl, isCurrent)
      assign(label, def, envir = table)
  }
}

## check for duplicate classes and embed method in an environment if so
.methodPackageSlots <- function(def, ...) def

## the real version
..methodPackageSlots <- function(def, label, table, duplicatesExist, isCurrent) {
    sig <- def@target
    dups <- FALSE
    if(duplicatesExist) {
        def <- .fixPackageSlot(def, sig)
        for(cl in sig) {
            if(exists(cl, envir = .classTable, inherits = FALSE) && is.list(get(cl, envir = .classTable))) {
                dups <- TRUE
                break
            }
        }
        if(isCurrent) { # check that this is overwriting identical signature
            current <- get(label, envir = table)
            dups <- dups || !identical(current@target, sig)
        }
        if(dups) {
            if(isCurrent) {
                if(is(current, "MethodDefinition")) {
                    pkg <- attr(current@target, "package")
                    if(length(pkg) == 0)
                        current <- .fixPackageSlot(current, current@target)
                    env <- new.env()
                    ## zero-length seen 2011-07-29
                    var <- .pkgMethodLabel(current)
                    if(nzchar(var)) assign(var, current, envir = env)
                }
                else if(is.environment(current))
                    env <- current
                else
                    stop(
                         gettextf("bad method object stored in method table, class %s",
                                  dQuote(class(current))),
                         domain = NA)
            }
            else
                env <- new.env()
            assign(.pkgMethodLabel(def), def, envir = env)
            env
        }
        else # no change
            def
    }
    else # no duplicate classes
        def
}

.fixPackageSlot <- function(def, sig) {
    ## check the pkg slot
    pkgs <- attr(sig, "package")
    if(is.null(pkgs))
        pkgs <- character(length(sig))
    fixme <- !nzchar(pkgs)
    if(any(fixme)) {
        for(i in seq_along(pkgs)[fixme])
            pkgs[[i]] <- getClass(sig[[i]], .Force = TRUE)@package
        attr(sig, "package") <- pkgs
        def@target <- sig
        ## check the defined signature as well
        sig <- def@defined
        pkgs <- attr(sig, "package")
        if(is.null(pkgs))
            pkgs <- character(length(sig))
        fixme <- !nzchar(pkgs)
        if(any(fixme)) {
            for(i in seq_along(pkgs)[fixme])
                pkgs[[i]] <- getClass(sig[[i]], .Force = TRUE)@package
            attr(sig, "package") <- pkgs
            def@defined <- sig
        }
    }
    def
}

.okMethodLabel <- function(method) {
    if(is(method, "MethodDefinition")) {
        pkgs <- packageSlot(method@target)
        length(pkgs) > 0 && all(nzchar(pkgs))
    }
    else
        TRUE # primitive or environment
}


.pkgMethodLabel <- function(method) {
    sig <- method@target
    pkgs <- packageSlot(sig)
    if( (length(pkgs) < length(as.character(sig))) || any(!nzchar(pkgs)))
        stop("package slot missing from signature for generic ",
             sQuote(method@generic), "\n",
             "and classes ", paste(sig, collapse = ", "), "\n",
             "cannot use with duplicate class names (the package may need to be re-installed)",
             call. = FALSE, domain = NA)
    paste(pkgs, collapse = "#")
}

.resetTable <- function(table, n, signames) {
    ## protect this computation, in case it's resetting
    ## something used in the computation
    primMethods <- .allowPrimitiveMethods(FALSE)
    on.exit(.allowPrimitiveMethods(primMethods))
    ## after updating a methods table, the maximum no. of arguments in
    ## the signature increased to n.  Reassign any objects whose label
    ## does not match n classes from the defined slot
    anyLabel <- rep("ANY", n)
    anyPkg <- rep("methods", n)
    seqN <- 1L:n
    labels <- objects(envir=table, all.names = TRUE)
    for(what in labels) {
        method <- get(what, envir = table)
        if(is.primitive(method)) # stored as default ?
            newSig <- anyLabel
        else if(is(method, "MethodDefinition"))
            newSig <- method@defined
        else if(is(method, "environment")) {
            newSig <- strsplit(what, "#", fixed = TRUE)[[1]]
            .resetTable(method, n, signames)
        }
        else
            stop(gettextf("invalid object in methods table (%s), expected a method, got an object of class %s",
                          sQuote(what),
                          dQuote(class(method))),
                 domain = NA)

        if(is(method, "MethodDefinition")) {
            pkgs <- packageSlot(newSig)
            newSig <- as(ifelse(seqN > length(newSig), anyLabel, newSig), "signature")
            newSig@names <- signames
            newSig@package <-  ifelse(seqN > length(pkgs), anyPkg, pkgs)
            method@defined <- method@target <- newSig
            newLabel <- .sigLabel(newSig)
        }
        else
            newLabel <- .sigLabel(ifelse(seqN > length(newSig), anyLabel, newSig))
        remove(list=what, envir = table)
        assign(newLabel, method, envir = table)
    }
    NULL
}

### the tag associated with a method signature.
### Should perhaps use the same C code as dispatch, for consistency,
### however, that code breaks out early in the collapse loop if no match.
### This code is not used for quick matching, so efficiency less critical.
.sigLabel <- function(sig)
  paste(sig, collapse = "#")

## workhorse of selectMethod() [ -> ../Methods.R ] "
.findInheritedMethods <-
    function(classes, fdef, mtable = NULL,
             table = get(".MTable", envir = environment(fdef)),
             excluded = NULL, useInherited,
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
          stop(gettextf("invalid or unset methods table in generic function %s", sQuote(fdef@generic)), damain = NA)
        else
          stop("trying to find a methods table in a non-generic function")
    }
    hasGroup <- length(fdef@group) > 0L
    if(hasGroup)
      groupGenerics <- .getAllGroups(list(fdef))
    doExcluded <- length(excluded) > 0L
    if(verbose) {
	plist <- function(x) paste(x, collapse = ", ")
	cat(" .findInheritedMethods(): (hasGroup, doCache, doExcluded)= (",
	    plist(c("f","T")[1+c(hasGroup, doCache, doExcluded)]), ")\n",
	    if(hasGroup) paste0(" Group generics: ",
				plist(vapply(groupGenerics, slot,
					     character(1), "generic")), "\n"),
	    sep='')
    }
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
	if(length(direct)) {
	    if(doCache)
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
    classDefs <- vector("list", nargs)
    classDefs[[1L]] <- def
    if(nargs > 1) { ## further arguments
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
    labels <- labels[-1L] # drop exact match
    labels <- unique(labels)# only needed while contains slot can have duplicates(!)
    if(verbose) {
	cat(" .fI> length(unique(method labels)) = ", length(labels))
	if(verbose >= 2) { cat(";  labels = \n") ; print(labels) }
    }
    allMethods <- objects(envir=table, all.names=TRUE)
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
    ## resolve any duplicate-class ambiguities
    if(.duplicateClassesExist()) {
        found <- integer()
        nm <- names(methods)
        for(i in seq_along(methods)) {
            m <- methods[[i]]
            if(is.environment(m)) {
                methods[[i]] <- .checkDuplicateMethodClasses(classDefs, m, nm[[i]])
                found <- c(found, i)
            }
        }
        if(length(found))
            methods <- unlist(methods, recursive = FALSE)
        if(!is.list(methods)) # reduced to a single method?
            methods <- list(methods)
    }
    if(doExcluded)
      methods <- methods[is.na(match(names(methods), as.character(excluded)))]
    ## remove default (ANY,..,ANY) if its not the only method:
    if(length(methods) > 1L) {
        defaultLabel <- paste(rep.int("ANY", nargs), collapse = "#")
        i <- match(defaultLabel, names(methods), 0L)
        if(i > 0L) {
            methods <- methods[-i]
            fromGroup <- fromGroup[-i]
        }
    }
    if(length(methods) > 1L) {
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
              stop(gettextf("the \"ambiguousMethodSelection\" option should be a function to be called as the condition action; got an object of class %s",
                            dQuote(class(condAction))),
                   domain = NA)

            select <- withCallingHandlers(
                                          .disambiguateMethods(classes, select, fdef@generic,
                                                               methods, supersList, fromGroup,
                                                               classDefs, verbose),
                                          ambiguousMethodSelection=condAction)
        }
        methods <- methods[select]
    }
    if(simpleOnly && length(methods) == 0L) {
	## Seems to be *unused* [below, 'simpleOnly' argument was missing for years!]
	methods <- Recall(classes, fdef, mtable, table, excluded, useInherited,
			  simpleOnly, verbose, FALSE)
        if(length(methods) > 0L)
          message(gettextf("No simply inherited methods found for function %s; using non-simple method",
                           sQuote(fdef@generic)),
                  domain = NA)
    }
    if(length(methods)) {
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
	if(doCache) {
	    if(verbose) cat(" .fI> caching newly found methods ..\n")
	    assign(tlabel, m, envir = mtable)
	}
    }
    methods
}

.checkDuplicateMethodClasses <- function(classDefs, env, label){
    matches <- list()
    supers <- strsplit(label, "#", TRUE)[[1]]
    plabels <- strsplit(objects(env, all.names = TRUE), "#", TRUE)
    for(plabel in plabels) {
        if(.hasThisSubclass(classDefs, supers, plabel))
            matches[[plabel]] <- get(plabel, envir = env)
    }
    matches
}

.hasThisSubclass <- function(classDefs, supers, plabel) {
    for(i in seq_along(plabel)) {
        pkg <- classDefs[[i]]@package
        cl <- classDefs[[i]]@className
        si <- supers[[i]]
        pki <- plabel[[i]]
        if(identical(si, "ANY") ||
           (identical(cl, si) && identical(pkg, pki)))
            next
        cli <- getClassDef(si, package = pki)
        if(is.null(cli)) return(FALSE)
        sub <- cli@subclasses[[cl]]
        if(is.null(sub) || !identical(pkg, sub@package))
            return(FALSE)
    }
    TRUE
}

.ambiguousMethodMessage <- function(cond) {
  selected <- attr(cond, "selected")
  if(is.null(selected)) {# not properly set up, so just use the message
    message(cond$message)
  }
  else {
    possible <- attr(cond, "candidates")
    message(gettextf("Note: method with signature %s chosen for function %s,\n target signature %s.\n %s would also be valid",
                     sQuote(selected),
                     sQuote(attr(cond, "generic")),
                     sQuote(attr(cond, "target")),
		     paste0('"', possible[is.na(match(possible, selected))], '"',
			    collapse=", ")),
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
	eligible <-
	    vapply(contains,
		   if(simpleOnly)
		   function(x) (is.logical(x) && x) || x@simple
		   else # eliminate conditional inheritance
		   function(x) (is.logical(x) && x) || x@simple || identical(body(x@test), TRUE), NA)
	what[eligible]
    }
}

.newSignature <- function(classes, names) {
  ## a simple version to deal with boostrapping stage, used in new() etc
    n <- min(length(classes), length(names))
  i <- seq_len(n)
    ## a corresponding set of package names
    ## <FIXME> There should be a "<unknown>" package name instead of "methods"
    ## but this requires a way to deal with that generally </FIXME>
    pkgs <- c(packageSlot(classes), rep("methods", n))[i]

  ## Simplified version ...
  .asS4(structure(as.character(classes)[i],
            class = .signatureClassName,
            names = as.character(names)[i],
            package = pkgs ))
 }

.findNextFromTable <- function(method, f, optional, envir, prev = character())
{
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
        warning(sprintf(ngettext(length(methods),
                                 "found %d equally good next method",
                                 "found %d equally good next methods"),
                        length(methods)),
                domain = NA)
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
    cnames <- paste0("\"", vapply(classes, as.character, ""), "\"",
		     collapse = ", ")
    stop(gettextf("unable to find an inherited method for function %s for signature %s",
                  sQuote(fdef@generic),
                  sQuote(cnames)),
         domain = NA)
  }
  else
    stop("Internal error in finding inherited methods; didn't return a unique method", domain = NA)
}

.findMethodInTable <- function(signature, table, fdef = NULL)
{
    if(is(fdef, "genericFunction"))
        signature <- .matchSigLength(signature, fdef, environment(fdef), FALSE)
    label <- .sigLabel(signature)
##     allMethods <- objects(table, all.names=TRUE)
##     if(match(label, allMethods, nomatch = 0L))
    if(!is.null(value <- table[[label]])) {
        if(is.environment(value)) {
            pkgs <- objects(value, all.names = TRUE)
            if(length(pkgs) == 1)
                value <- get(pkgs, envir = value)
            else if(length(pkgs) == 0)
                value <- NULL
            ## else, return the environment indicating multiple possibilities
        }
        value
    } # else, NULL
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
    ## These should be integers, so we do not need to worry about a decimal point
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
    note <- c(sprintf(ngettext(which2,
                               "Selecting %d method of minimum distance",
                               "Selecting %d methods of minimum distance"),
                      which2))
    which <- which[which2]
  }
  ## if some are group methods, eliminate those
  if(length(which) > 1 && any(fromGroup[which]) && !all(fromGroup[which])) {
    which <- which[!fromGroup]
    note <- c(note,  sprintf(ngettext(length(which),
                                      "Selecting %d non-group method",
                                      "Selecting %d non-group methods"),
                             length(which)))
  }
  ## prefer partially direct methods
  if(length(which) > 1) {
    direct <- vapply(methods[which], function(x, target)
                     (is(x, "MethodDefinition") && any(target == x@defined)),
		     NA, target = target)
    if(any(direct) && !all(direct)) {
      which <- which[direct]
      note <- c(note, sprintf(ngettext(length(which),
                                       "Selecting %d partially exact-matching method",
                                       "Selecting %d partially exact-matching methods"),
                              length(which)))
    }
  }
  which <- which[[1L]]
  if(identical(as.character(generic), "coerce"))
      return(which) # as() computations not currently consistent w. selection (R 2.15.2)
  selected <- names(methods)[[which]]
  ## FIXME (?): This is not shown to the user
  msg <- sprintf(ngettext(length(candidates),
                          "Choosing method %s from %d ambiguous possibility",
                          "Choosing method %s from %d ambiguous possibilities"),
                 sQuote(selected), length(candidates))
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
  if(is.null(mtable <- fenv$.MTable)) {
      .setupMethodsTables(generic)
      mtable <- get(".MTable", envir = fenv)
  }
  if(!reset) {
    env <- as.environment(where)
    tname <- .TableMetaName(generic@generic, generic@package)
    if(!is.null(tt <- env[[tname]])) {
      .mergeMethodsTable(generic, mtable, tt, attach)
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
    if(!is.null(allTable <- fenv$.AllMTable)) {
        ## remove all inherited methods.  Note that code (e.g. setMethod) that asigns
        ## a new method to mtable is responsible for copying it to allTable as well.
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
    sigString <- function(sig)
	paste0(names(sig), "=\"", as.character(sig), "\"", collapse = ", ")
    qs <- function(what) paste0('"', what, '"', collapse = ", ")
    doFun <- function(func, pkg) cf("Function: ", func, " (package ", pkg, ")\n")
    env <- environment(generic)
    signature <- generic@signature
    table <- get(if(inherited) ".AllMTable" else ".MTable", envir = env)
    f <- generic@generic
    p <- packageSlot(f)
    if(is.null(p)) p <- "base"
    deflt <- new("signature", generic, "ANY")
    labels <- objects(envir=table, all.names = TRUE)
    if(!is.null(classes) && length(labels)) {
	sigL <- strsplit(labels, split = "#")
	keep <- !vapply(sigL, function(x, y) all(is.na(match(x, y))), NA, y=classes)
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
        if(is.environment(m)) {  ## duplicate class case -- compare .findMethodInTable()
            pkgs <- objects(m)
            if(length(pkgs) == 1)
                m <- get(pkgs, envir = m)
            else if(length(pkgs) > 1)
                cf("  (", length(pkgs), " methods defined for this signature, with different packages)\n")
        }
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
  .Call(C_R_set_method_dispatch, as.logical(onOff))

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
                               stop("failed to find expected group generic function: ",
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
      stop(gettextf("invalid group generic function in search for inherited method (class %s)",
                    dQuote(class(gen))),
           domain = NA)
    table <- .getMethodsTable(gen)
    allMethods <- objects(envir=table, all.names = TRUE)
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
	    stop("invalid methods table request")
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
      warning(gettextf("trying to check signature length of group generic '%s', but it is not a group generic", what),
              domain = NA)
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
      warning(gettextf("trying to check signature length of generic '%s', but it is not a generic function: i = %d, funs = %s, gnames = %s",
                       what,  i, paste(unlist(funs), collapse = ", "),
                       paste(as.character(gnames), collapse = ", ")),
              domain = NA)
    else {
      ev <- environment(fdef)
      if(is.null(sigl <- ev$.SigLength)) {
	  .setupMethodsTables(fdef)
	  sigl <- get(".SigLength", envir = ev)
      }
      sigs[i] <- sigl
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
  pkgs <- packageSlot(sig)
  if(n < nargs) {
      more <- nargs - n
      pkgs <- c(pkgs, rep("methods", more))
      sig <- c(as.character(sig), rep("ANY", more))
  }
  else if(n > nargs) { #reset table?
    if(all(sig[(nargs+1):n] == "ANY"))
      length(sig) <- length(pkgs) <- nargs
    else {
      while(sig[[n]] == "ANY")
        n <- n-1
      if(reset)
        .resetSigLength(fdef, n)
      length(sig) <- length(pkgs) <- n
    }
  }
  packageSlot(sig) <- pkgs
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
    paste0("^[.]",substring(methodsPackageMetaName("T",""),2))

.addToMetaTable <- function(fdef, signature, definition, where, nSig) {
  return()
}

## the real version
..addToMetaTable <- function(fdef, signature, definition, where,
                             nSig = .getGenericSigLength(fdef)) {
    ## TODO:  nSig should be a slot in the table
  tname <- .TableMetaName(fdef@generic, fdef@package)
  where <- as.environment(where)
  if(!is.null(table <- where[[tname]])) {
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
    objects(envir=table, all.names=TRUE)
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
    names <- objects(envir=table, all.names=TRUE)
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
    allNames <- objects(envir=table, all.names = TRUE)
    if(length(allNames) == 0L)
      return(value)
    argNames <- generic@signature
    ## USES THE PATTERN OF class#class#.... in the methods tables
    nargs <- nchar(unique(gsub("[^#]","", allNames)))+1
    if(length(nargs) > 1L) {
        warning("something weird:  inconsistent number of args in methods table strings:", paste(nargs,collapse = ", ")," (using the largest value)",
                domain = NA)
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
      stop(gettextf("no methods table for generic %s from package %s in package %s",
                    sQuote(generic@generic),
                    sQuote(generic@package),
                    sQuote(getPackageName(where))),
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
    undefs <- vapply(classDefs, is.null, NA)
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
      classes <- classes[vapply(classDefs, function(def) identical(def@virtual, FALSE), NA)]
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
      classesj <- unique(sigs[,j])
      .undefClasses <- character()
      subclasses <- .relevantClasses(classesj, !virtual, where, ok[[j]])
      nj <- length(subclasses)
      ##       if(nj == 0) {  ##FIXME, wrong test
      ##         warning(gettextf("No eligible subclasses for argument '%s' found, so no contribution to analysis",
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
        warning(gettextf("undefined classes (%s) will be ignored for argument '%s'",
                         paste0('"',unique(.undefClasses),'"', collapse=", "),
                         colnames(sigs)[[j]]), domain = NA)
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
  else stop("argument 'signatures' must be a character matrix whose rows are method signatures")
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
        rep_len("<NONE>", length(sig))
      else # primitive
        rep_len("ANY", length(sig))
    }
    signatures <- lapply(signatures, doSelect)
  }
  signatures <- sapply(signatures, paste0, collapse = "#")
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

