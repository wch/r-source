#  File src/library/methods/R/addedFunctions.R
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


functionBody <- base::body #was get("body", mode = "function")

`functionBody<-` <- base::`body<-`
## was
## .ff <- function(fun, envir = environment(fun), value) fun
## body(.ff, envir = .GlobalEnv) <- body(get("body<-"))
## "functionBody<-" <- .ff
## rm(.ff)

allNames <-
  ## the character vector of names (unlike names(), never returns NULL)
  function(x)
{
    value <- names(x)
    if(is.null(value))
        character(length(x))
    else
        value
}

getFunction <- function(name, generic = TRUE, mustFind = TRUE,
                        where = topenv(parent.frame()))
      ## find the object as a function.
{
    if(!nzchar(name))
        stop(gettextf('expected a non-empty character string for argument name'), domain = NA)
    found <- FALSE
    where <- as.environment(where)
    f <- NULL
    ## parent.env sequence of a namespace ends in the base package namespace,
    ## of a non-namespace ends in NULL (equiv. to base environment) [sigh]
    lastEnv <- if(isNamespace(where)) function(where) isBaseNamespace(where) else
    function(where) identical(where, baseenv())
    repeat {
        if(!is.null(f <- get0(name, envir = where, mode = "function", inherits = FALSE)))
            found <- generic || !is(f, "genericFunction")
        if(found || lastEnv(where))
            break
        where <- parent.env(where)
    }
    if(!found && mustFind)
	stop(if(generic)
	     gettextf("no function %s found", sQuote(name)) else
	     gettextf("no non-generic function %s found", sQuote(name)),
	     domain = NA)
    f
}

el <-
  function(object, where)
  ## element of a vector; numeric index only.
  ##
  ## the definition allows indexing beyond current length of vector
  ## (consistent with [[]] in S but not in R).
  object[where][[1L]]

"el<-" <-
  ## set the element of a vector; numeric index only.
  .Primitive("[[<-")

elNamed <-
  ## get the element of the vector corresponding to name.  No partial matching.
  function(x, name, mustFind=FALSE)
{
    i <- match(name, names(x))
    if(is.na(i)) {
        if(mustFind)
            stop(gettextf("%s is not one of the element names",
                          sQuote(name)),
                 domain = NA)
        else NULL
    }
    else
        el(x,i)
}

"elNamed<-" <-
    ## set the element of the vector corresponding to name.
    function(x, name, value)
{
    x[[name]] <- value
    x
}

formalArgs <-
    ## Returns the names of the formal arguments of this function.
    function(def) names(formals(def))


findFunction <-
    ## return a list of all the places where a function
    ## definition for `name' exists.  If `generic' is FALSE, ignore generic
    ## functions.
    function(f, generic = TRUE, where = topenv(parent.frame()))
{
    allWhere <- .findAll(f, where) # .findAll() in ./ClassExtensions.R
    ok <- logical(length(allWhere))
    for(i in seq_along(allWhere)) {
	wherei <- allWhere[[i]]
	if(!is.null(fdef <- get0(f, wherei, inherits = FALSE))) {
	    ok[i] <- is.function(fdef) && (generic || is.primitive(fdef) || !isGeneric(f, wherei, fdef))
	}## else ok[i] <- FALSE
    }
    allWhere[ok]
}

existsFunction <- function(f, generic=TRUE, where = topenv(parent.frame()))
    length(findFunction(f, generic, where)) > 0L

Quote <- base::quote #was get("quote" , mode = "function")

.message <- function(..., domain = NULL, appendLF = TRUE) {
    ## Output all the arguments, pasted together with no intervening spaces,
    ## wrapping long lines
    text <- paste0(..., collapse="")
    lines <- strwrap(text, width = max(20, 7 * getOption("width") %/% 8))
    message(paste(lines, collapse="\n"), domain = domain, appendLF = appendLF)
}

hasArg <- function(name) {
    aname <- as.character(substitute(name))
    fnames <- names(formals(sys.function(sys.parent())))
    if(is.na(match(aname, fnames))) {
        if(is.na(match("...", fnames)))
            FALSE
        else {
            dotsCall <- eval(quote(substitute(list(...))), sys.parent())
            !is.na(match(aname, names(dotsCall)))
        }
    }
    else
        eval(substitute(!missing(name)), sys.frame(sys.parent()))
}
