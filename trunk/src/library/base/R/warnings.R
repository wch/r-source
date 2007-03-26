warnings <- function(...)
{
    if(!exists("last.warning", envir=baseenv())) return()
    last.warning <- get("last.warning", envir=baseenv())
    if(!(n <- length(last.warning))) return()
    structure(last.warning, dots=list(...), class="warnings")
}

print.warnings <- function(x, ...)
{
    if(n<- length(x)) {
        cat(ngettext(n, "Warning message:\n", "Warning messages:\n"))
        names <- names(x)
        for(i in 1:n) {
            out <- if(n == 1) names[i] else paste(i,": ", names[i], sep="")
            if(length(x[[i]])) {
                temp <- deparse(x[[i]])
                out <- paste(out, "in:", temp[1], if(length(temp) > 1) " ...")
            }
            do.call("cat", c(list(out), attr(x, "dots"), fill=TRUE))
        }
    }
    invisible(x)
}
