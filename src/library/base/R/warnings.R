warnings <- function(...)
{
    if(!exists("last.warning", envir=baseenv())) return()
    last.warning <- get("last.warning", envir=baseenv())
    if(!(n <- length(last.warning))) return()
    structure(last.warning, dots=list(...), class="warnings")
}

print.warnings <- function(x, ...)
{
    if(n <- length(x)) {
        cat(ngettext(n, "Warning message:\n", "Warning messages:\n"))
        msgs <- names(x)
        for(i in seq_len(n)) {
            ind <- if(n == 1) "" else paste(i, ": ", sep="")
            out <- if(length(x[[i]])) {
                ## deparse can overshoot cutoff
                temp <- deparse(x[[i]], width.cutoff = 50)
                ## Put on one line if narrow enough.
                sm <- strsplit(msgs[i], "\n")[[1]]
                nl <- if(nchar(ind, "w") + nchar(temp[1], "w") +
                         nchar(sm[1], "w") <= 75)
                    " " else "\n  "
                paste(ind, "In ",
                      temp[1], if(length(temp) > 1) " ...",
                      " :", nl, msgs[i], sep = "")
            } else paste(ind, msgs[i], sep = "")
            do.call("cat", c(list(out), attr(x, "dots"), fill=TRUE))
        }
    }
    invisible(x)
}
