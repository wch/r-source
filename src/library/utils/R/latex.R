bibtex <- function(object, ...) UseMethod("bibtex")

print.bibtex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible()
}

latex <- function(object, ...) UseMethod("latex")
    
print.latex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible()
}
