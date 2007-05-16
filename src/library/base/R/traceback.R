traceback <-
function(x = NULL, max.lines = getOption("deparse.max.lines"))
{
    if(is.null(x) && (exists(".Traceback", envir = baseenv())))
	x <- get(".Traceback", envir = baseenv())
    n <- length(x)
    if(n == 0)
        cat(gettext("No traceback available"), "\n")
    else {
        for(i in 1:n) {
            label <- paste(n-i+1, ": ", sep="")
            m <- length(x[[i]])
            if(m > 1)
                label <- c(label, rep(substr("          ", 1,
                                             nchar(label, type="w")),
                                      m - 1))
            if(is.numeric(max.lines) && max.lines > 0 && max.lines < m) {
                cat(paste(label[1:max.lines], x[[i]][1:max.lines], sep = ""),
                    sep = "\n")
                cat(label[max.lines+1], " ...\n")
            } else
            cat(paste(label, x[[i]], sep=""), sep="\n")
        }
    }
    invisible()
}
