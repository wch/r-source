dyn.load <- function(x, local=TRUE, now=TRUE)
    .Internal(dyn.load(x, as.logical(local), as.logical(now)))

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
        stop("must pass a package name, DLLInfo or DllInfoReference object")


    syms = lapply(name, function(id) {
       v <- .Call("R_getSymbolInfo", as.character(id), PACKAGE, as.logical(withRegistrationInfo), PACKAGE = "base")
       if(is.null(v)) {
           msg <- paste("no such symbol", id)
           if(length(pkgName) && nzchar(pkgName))
               msg <- paste(msg, "in package", pkgName)
           stop(msg)
       }
       names(v) <- c("name", "address", "package", "numParameters")[1:length(v)]
       v
      })


   if(length(name) == 1 && unlist == TRUE)
     syms = syms[[1]]
   else
     names(syms) = name

   syms
}

getLoadedDLLs <- function()
{
    els <- .Call("R_getDllTable", PACKAGE = "base")
    names(els) = sapply(els, function(x) x[["name"]])
    els
}


getDLLRegisteredRoutines <- function(dll, addNames = TRUE)
    UseMethod("getDLLRegisteredRoutines")


getDLLRegisteredRoutines.character <- function(dll, addNames = TRUE)
{
    dlls <- getLoadedDLLs()
    w <- sapply(dlls, function(x) x[["name"]] == dll || x[["path"]] == dll)

    if(!any(w))
        stop("No DLL currently loaded with name or path ", dll)

    dll <- which(w)[1]
    if(sum(w) > 1)
        warning(gettextf("multiple DLLs match '%s'. Using '%s'",
                         dll, dll[["path"]]), domain = NA)

    getDLLRegisteredRoutines(dlls[[dll]], addNames)
}


getDLLRegisteredRoutines.DLLInfo <- function(dll, addNames = TRUE)
{
    ## Provide methods for the different types.
    if(!inherits(dll, "DLLInfo"))
        stop("must specify DLL via a DLLInfo object. See getLoadedDLLs()")

    info <- dll[["info"]]
    els <- .Call("R_getRegisteredRoutines", info, PACKAGE = "base")
    ## Put names on the elements by getting the names from each element.
    if(addNames) {
      els <- lapply(els, function(x) {
                              if(length(x))
                                 names(x) <- sapply(x, function(z) z$name)
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

print.DLLRegisteredRoutines <-
      # This is arranged as a ragged data frame.  It may be confusing
      # if one reads it row-wise as the columns are related in pairs
      # but not across pairs.  We might leave it as  a list of lists
      # but that spans a great deal of vertical space and involves
      # a lot of scrolling for the user.
function(x, ...)
{
      # Create a data frame with as many rows as the maximum number
      # of routines in any category. Then fill the column with ""
      # and then the actual entries.

    n <- max(sapply(x, length))
    d <- list()
    sapply(names(x),
             function(id) {
                d[[id]] <<- rep("", n)
                names <- sapply(x[[id]], function(x) x$name)
                if(length(names))
                    d[[id]][1:length(names)] <<- names

                d[[paste(id, "numParameters")]] <<- rep("", n)
                names <- sapply(x[[id]], function(x) x$numParameters)
                if(length(names))
                    d[[paste(id, "numParameters")]][1:length(names)] <<- names
             })
    print(as.data.frame(d), ...)
    invisible(x)
}

getCallingDLLe <- function(e)
{
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) &&
       length(env$DLLs))
        return(env$DLLs[[1]])
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

       # Please feel free to replace with a more encapsulated way to do this.
    if (is.null(env <- e$".__NAMESPACE__.")) env <- baseenv()
    if(exists("DLLs", envir = env) && length(env$DLLs))
        return(env$DLLs[[1]])
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
{
  getNativeSymbolInfo(as.character(name), PACKAGE = x)
}



