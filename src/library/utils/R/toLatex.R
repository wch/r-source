toBibtex <- function(object, ...) UseMethod("toBibtex")

print.Bibtex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible()
}

toLatex <- function(object, ...) UseMethod("toLatex")
    
print.Latex <- function(x, prefix="", ...)
{
    writeLines(paste(prefix, unclass(x), sep=""))
    invisible()
}
