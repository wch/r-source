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
    v <- .Call("R_getSymbolInfo", as.character(name), as.character(PACKAGE),
               PACKAGE = "base")
    if(is.null(v)) {
        msg <- paste("no such symbol",name)
        if(length(PACKAGE) && nchar(PACKAGE[1]))
            msg <- paste(msg, "in package",PACKAGE[1])
        stop(msg)
    }
    names(v) <- c("name", "address", "package", "numParameters")[1:length(v)]
    v
}
