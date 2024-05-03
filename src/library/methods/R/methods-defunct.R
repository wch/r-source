#  File src/library/methods/R/methods-deprecated.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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
#  https://www.R-project.org/Licenses/


## <entry>
## Deprecated in 2.10.0
## Defunct in 2.11.0
## Removed in 3.0.0
## trySilent <- function(expr) .Defunct("try(silent = TRUE)")
## </entry>

## <entry>
## Defunct in 3.0.0
## Removed in 4.1.0
## traceOn <- function(what, tracer = browseAll, exit = NULL) {
##     browseAll <- function() .Defunct()
##     .Defunct("trace")
## }
## traceOff <- function(whatL) .Defunct("untrace")
## </entry>


## <entry>
## Defunct in 4.5.0
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
    .MlistDefunct("MethodsListSelect()")
}
## </entry>


## <entry>
## Defunct in 4.5.0
mergeMethods <-
    ## merge the methods in the second MethodsList object into the first,
    ## and return the merged result.
    function(m1, m2, genericLabel = character())
{
    .MlistDefunct("mergeMethods()")
}
## </entry>

## <entry>
## Defunct in 4.5.0
## Removed in 4.5.0
## --------- <----- 'sub-entry' separator
## showMlist <-
##   ## Prints the contents of the MethodsList.  If `includeDefs' the signatures and the
##   ## corresponding definitions will be printed; otherwise, only the signatures.
##   ##
##   ## If `includeDefs' is `TRUE', the currently known inherited methods are included;
##   ## otherwise, only the directly defined methods.
## function(mlist, includeDefs = TRUE, inherited = TRUE, classes = NULL, useArgNames = TRUE,
##          printTo = stdout())
## {
##     .MlistDeprecated("showMlist()")
##     if(isFALSE(printTo)) {
##         tmp <- tempfile()
##         con <- file(tmp, "w")
##     }
##     else
##         con <- printTo
##   object <- linearizeMlist(mlist, inherited)
##   methods <- object@methods
##   signatures <- object@classes
##   args <- object@arguments
##   if(!is.null(classes) && length(signatures)>0) {
##     keep <- !vapply(signatures, function(x, y) all(is.na(match(x, y))), NA, classes)
##     methods <- methods[keep]
##     signatures <- signatures[keep]
##     args <- args[keep]
##   }
##   if(length(methods) == 0)
##     cat(file=con, "<Empty Methods List>\n")
##   else {
##    n <- length(methods)
##     labels <- character(n)
##     if(useArgNames) {
##       for(i in 1L:n) {
##         sigi <- signatures[[i]]
##         labels[[i]] <- paste0(args[[i]], " = \"", sigi, "\"",
##                               collapse = ", ")
##       }
##     }
##     else {
##       for(i in 1L:n)
##         labels[[i]] <- paste(signatures[[i]], collapse = ", ")
##     }
##     for(i in seq_along(methods)) {
##       cat(file=con, (if(includeDefs) "## Signature:" else ""), labels[[i]])
##       method <- methods[[i]]
##       if(includeDefs) {
##         cat(file=con, ":\n")
##         if(is(method, "MethodDefinition")) ## really an assertion
##           cat(file=con, deparse(method@.Data), sep="\n")
##         else
##           cat(file=con, deparse(method), sep="\n")
##       }
##       if(is(method, "MethodDefinition") &&
##          !identical(method@target, method@defined)) {
##           defFrom <- method@defined
##           cat(file = con, if(includeDefs) "##:" else "\n",
##               "    (inherited from ",
##               paste0(names(defFrom), " = \"",
##                      as.character(defFrom), "\"",
##                      collapse = ", "),
##                ")", if(includeDefs) "\n", sep="")
##       }
##       cat(file=con, "\n")
##     }
##   }
##     if(isFALSE(printTo)) {
##         close(con)
##         value <- readLines(tmp)
##         unlink(tmp)
##         value
##     }
## }
## ---------
## ##' only called from showMlist() above, which has been deprecated in R 3.2.0 (Apr.2015):
## linearizeMlist <-
##     ## Undo the recursive nature of the methods list, making a list of
##     ## function definitions, with the names of the list being the
##     ## corresponding signatures (designed for printing; for looping over
##     ## the methods, use `listFromMlist' instead).
##     ##
##     ## The function calls itself recursively.  `prev' is the previously
##     ## selected class names.
##     ##
##     ## If argument `classes' is provided, only signatures containing one
##     ## of these classes will be included.
##     function(mlist, inherited = TRUE) {
##         methods <- mlist@methods
##         allMethods <- mlist@allMethods
##         if(inherited && length(allMethods) >= length(methods)) {
## ##            anames <- names(allMethods)
## ##            inh <- is.na(match(anames, names(methods)))
##             methods <- allMethods
##         }
##         preC <- function(y, x)c(x,y) # used with lapply below
##         cnames <- names(methods)
##         value <- list()
##         classes <- list()
##         arguments <- list()
##         argname <- as.character(mlist@argument)
##         for(i in seq_along(cnames)) {
##             mi <- methods[[i]]
##             if(is.function(mi)) {
##                 value <- c(value, list(mi))
##                 classes <- c(classes, list(cnames[[i]]))
##                 arguments <- c(arguments, list(argname))
##             }
##             else if(is(mi, "MethodsList")) {
## 		.MlistDeprecated()
##                 mi <- Recall(mi, inherited)
##                 value <- c(value, mi@methods)
##                 classes <- c(classes, lapply(mi@classes, preC, cnames[[i]]))
##                 arguments <- c(arguments, lapply(mi@arguments, preC, argname))
##             }
##             else
##                 warning(gettextf("skipping methods list element %s of unexpected class %s\n\n",
##                                  paste(cnames[i], collapse = ", "),
##                                  dQuote(.class1(mi))),
##                         domain = NA)
##         }
##         new("LinearMethodsList", methods = value, classes = classes, arguments = arguments)
##     }
## ---------
## print.MethodsList <- function(x, ...)
##     showMlist(x)
## ---------
## ## In R's own code, this is *only* used in mergeMethods(), deprecated in R 3.2.0 (Apr.2015)
## listFromMlist <-
##   ## linearizes the MethodsList object into list(sigs, methods); `prefix' is the partial
##   ## signature (a named list of classes) to be prepended to the signatures in this object.
##   ##
##   ## A utility function used to iterate over all the individual methods in the object.
##   function(mlist, prefix = list(), sigs. = TRUE, methods. = TRUE)
## {
##     methodSlot <- slot(mlist, "methods")
##     mnames <- names(methodSlot)
##     argName <- as.character(slot(mlist, "argument"))
##     sigs <- list()
##     methods <- list()
##     for(i in seq_along(methodSlot)) {
##         thisMethod <- methodSlot[i]
##         thisClass <- mnames[[i]]
##         prefix[[argName]] <- thisClass
##         if(is.function(thisMethod)) {
##             if(sigs.) sigs <- c(sigs, list(prefix))
##             if(methods.) methods <- c(methods, list(thisMethod))
##         }
##         else {
##             more <- Recall(thisMethod, prefix)
##             if(sigs.) sigs <- c(sigs, more[[1]])
##             if(methods.) methods <- c(methods, more[[2]])
##         }
##     }
##     list(sigs, methods)
## }
## ---------
## .insertCachedMethods <- function(mlist, argName, Class, fromClass, def) {
##     if(is(def, "MethodsList")) {
##         .MlistDeprecated()
##         ## insert all the cached methods in def
##         newArg <- c(argName, as.character(def@argument))
##         newDefs <- def@allMethods
##         newSigs <- as.list(names(newDefs))
##         for(j in seq_along(newDefs))
##             mlist <- Recall(mlist, newArg, c(Class, newSigs[[j]]), fromClass,
##                             newDefs[[j]])
##     }
##     else {
##         def <- .addMethodFrom(def, argName[1L], Class[1L], fromClass)
##         mlist <- insertMethod(mlist, Class, argName, def, TRUE)
##     }
##     mlist
## }
## ---------
## </entry>
