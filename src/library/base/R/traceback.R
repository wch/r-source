traceback <-
function(x = NULL)
{
    if(is.null(x) && (exists(".Traceback", env = .GlobalEnv)))
	x <- get(".Traceback", env = .GlobalEnv)
    if(is.null(x) || length(x) == 0)
        cat(gettext("No traceback available"), "\n")
    else {
        n <- length(x)
        m0 <- getOption("deparse.max.lines")
        for(i in 1:n) {
            label <- paste(n-i+1, ": ", sep="")
            m <- length(x[[i]])
            if(m > 1)
                label <- c(label, rep(substr("          ", 1,
                                             nchar(label, type="w")),
                                      m - 1))
            if(is.numeric(m0) && m0 > 0 && m0 < m) {
                cat(paste(label[1:m0], x[[i]][1:m0], sep=""), sep="\n")
                cat(label[m0+1], " ...\n")
            } else
            cat(paste(label, x[[i]], sep=""), sep="\n")
        }
    }
    invisible()
}
