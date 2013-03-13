#  File src/library/base/R/dynload.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

if(.Platform$OS.type == "windows") {
    dyn.load <- function(x, local = TRUE, now = TRUE, ...) {
        inDL <- function(x, local, now, ..., DLLpath = "")
            .Internal(dyn.load(x, local, now, DLLpath))
        inDL(x, as.logical(local), as.logical(now), ...)
    }
} else {
    dyn.load <- function(x, local = TRUE, now = TRUE, ...)
        .Internal(dyn.load(x, as.logical(local), as.logical(now), ""))
}

dyn.unload <- function(x)
    .Internal(dyn.unload(x))

is.loaded <- function(symbol, PACKAGE = "", type = "")
    .Internal(is.loaded(symbol, PACKAGE, type))

getNativeSymbolInfo <- function(name, PACKAGE, unlist = TRUE,
                                 withRegistrationInfo = FALSE)
{
    if(missing(PACKAGE)) PACKAGE <- ""

    if(is.character(PACKAGE))
        pkgName <- PACKAGE
    else if(inherits(PACKAGE, "DLLInfo")) {
        pkgName <- PACKAGE[["path"]]
        PACKAGE <- PACKAGE[["info"]]
    } else if(inherits(PACKAGE, "DLLInfoReference")) {
        pkgName <- character()
    } else
        stop(gettextf("must pass a package name, %s or %s object",
                      dQuote("DLLInfo"),
                      dQuote("DllInfoReference")),
             domain = NA)

    syms <- lapply(name, function(id) {
	v <- .Internal(getSymbolInfo(as.character(id), PACKAGE,
                                     as.logical(withRegistrationInfo)))
	if(is.null(v)) {
	    msg <- paste("no such symbol", id)
	    if(length(pkgName) && nzchar(pkgName))
		msg <- paste(msg, "in package", pkgName)
	    stop(msg, domain = NA)
	}
	names(v) <- c("name", "address", "package", "numParameters")[seq_along(v)]
	v
    })

   if(length(name) == 1L && unlist)
     syms <- syms[[1L]]
   else
     names(syms) <- name

   syms
}

getLoadedDLLs <- function() .Internal(getLoadedDLLs())


getDLLRegisteredRoutines <- function(dll, addNames = TRUE)
    UseMethod("getDLLRegisteredRoutines")


getDLLRegisteredRoutines.character <- function(dll, addNames = TRUE)
{
    dlls <- getLoadedDLLs()
    w <- vapply(dlls, function(x) x[["name"]] == dll || x[["path"]] == dll, NA)

    if(!any(w))
        stop(gettextf("No DLL currently loaded with name or path %s", sQuote(dll)),
             domain = NA)

    dll <- which(w)[1L]
    if(sum(w) > 1L)
        warning(gettextf("multiple DLLs match '%s'. Using '%s'",
                         dll, dll[["path"]]), domain = NA)

    getDLLRegisteredRoutines(dlls[[dll]], addNames)
}


getDLLRegisteredRoutines.DLLInfo <- function(dll, addNames = TRUE)
{
    ## Provide methods for the different types.
    if(!inherits(dll, "DLLInfo"))
        stop(gettextf("must specify DLL via a %s object. See getLoadedDLLs()",
                      dQuote("DLLInfo")),
             domain = NA)

    info <- dll[["info"]]
    els <- .Internal(getRegisteredRoutines(info))
    ## Put names on the elements by getting the names from each element.
    if(addNames) {
      els <- lapply(els, function(x) {
                              if(length(x))
                                 names(x) <- vapply(x, function(z) z$name, "")
                              x
                         })
    }
    class(els) <- "DLLRegisteredRoutines"
    els
}


print.NativeRoutineList <-
function(x, ...)
{
    m <- data.frame(numParameters = sapply(x, function(x) x$numParameters),
                    row.names = sapply(x, function(x) x$name))
    print(m, ...)
    invisible(x)
}

### This is arranged as a ragged data frame.  It may be confusing
### if one reads it row-wise as the columns are related in pairs
### but not across pairs.  We might leave it as  a list of lists
### but that spans a great deal of vertical space and involves
### a lot of scrolling for the user.
print.DLLRegisteredRoutines <-
function(x, ...)
{
    ## Create a data frame with as many rows as the maximum number
    ## of routines in any category. Then fill the column with ""
    ## and then the actual entries.

    n <- vapply(x, length, 1L)
    x <- x[n > 0]
    n <- max(n)
    d <- list()
    sapply(names(x),
             function(id) {
		d[[id]] <<- rep.int("", n)
		names <- vapply(x[[id]], function(x) x$name, "")
                if(length(names)) d[[id]][seq_along(names)] <<- names
                d[[paste(id, "numParameters")]] <<- rep.int("", n)
                names <- sapply(x[[id]], function(x) x$numParameters)
                if(length(names))
                    d[[paste(id, "numParameters")]][seq_along(names)] <<- names
             })
    print(as.data.frame(d), ...)
    invisible(x)
}

getCallingDLLe <- function(e)
{
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) &&
       length(env$DLLs))
        return(env$DLLs[[1L]])
    NULL
}

getCallingDLL <-
function(f = sys.function(-1), doStop = FALSE)
{
    e <- environment(f)

    if(!isNamespace(e)) {
        if(doStop)
            stop("function is not in a namespace, so cannot locate associated DLL")
        else
            return(NULL)
    }

    ## Please feel free to replace with a more encapsulated way to do this.
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) && length(env$DLLs))
        return(env$DLLs[[1L]])
    else {
        if(doStop)
            stop("looking for DLL for native routine call, but no DLLs in namespace of call")
        else
            NULL
    }
    NULL
}

print.DLLInfo <- function(x, ...)
{
    tmp <- as.data.frame.list(x[c("name", "path", "dynamicLookup")])
    names(tmp) <- c("DLL name", "Filename", "Dynamic lookup")
    write.dcf(tmp, ...)
    invisible(x)
}

print.DLLInfoList <- function(x, ...)
{
    if(length(x)) {
        m <- data.frame(Filename = sapply(x, function(x) x[["path"]]),
                        "Dynamic Lookup" =
                        sapply(x, function(x) x[["dynamicLookup"]]))
        print(m, ...)
    }
    invisible(x)
}


`$.DLLInfo` <- function(x, name)
    getNativeSymbolInfo(as.character(name), PACKAGE = x)




