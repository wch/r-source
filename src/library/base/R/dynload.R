##dyn.load <- function(x)
##{
##	x <- as.character(x)
##	y <- substr(x, 1, 1)
##	if (y == "/") {
##		.Internal(dyn.load(x))
##	}
##	else {
##		.Internal(dyn.load(
##		paste(system("pwd", intern = TRUE), x, sep = "/", collapse="")))
##	}
##}
dyn.load <- function(x, local=TRUE, now=TRUE)
    .Internal(dyn.load(x, as.logical(local), as.logical(now)))

dyn.unload <- function(x)
    .Internal(dyn.unload(x))

getNativeSymbolInfo <- function(name, PACKAGE)
{
    if(missing(PACKAGE)) PACKAGE <- ""
    if(!is.character(PACKAGE) && !inherits(PACKAGE, "DLLInfoReference"))
      stop("must pass a package name or DllInfoReference object")
    
    v <- .Call("R_getSymbolInfo", as.character(name), PACKAGE,
               PACKAGE = "base")
    if(is.null(v)) {
        msg <- paste("no such symbol", name)
        if(length(PACKAGE) && nchar(PACKAGE[1]))
            msg <- paste(msg, "in package", PACKAGE[1])
        stop(msg)
    }
    names(v) <- c("name", "address", "package", "numParameters")[1:length(v)]
    v
}


getLoadedDLLs <- function()
{
    els = .Call("R_getDllTable", PACKAGE = "base")
    names(els) = sapply(els, function(x) x[["name"]])
    els
}

getDLLRegisteredRoutines =
function(dll)  
{
   # Provide methods for the different types.
 if(!inherits(dll, "DLLInfo"))
   stop("Must specify DLL via a DLLInfo object. See getLoadedDLLs()")

 info = dll$info

 els = .Call("R_getRegisteredRoutines", info, PACKAGE = "base")
   # Put names on the elements by getting the names from each element.
 els = lapply(els, function(x) {
                        if(length(x))
                           names(x) = sapply(x, function(z) z$name)
                        x
                    })

 els
}

