traceback <-
function(x = NULL)
{
    if(is.null(x) && (exists(".Traceback", env = .GlobalEnv)))
	x <- get(".Traceback", env = .GlobalEnv)
    if(is.null(x) || length(x) == 0)
        cat(gettext("No traceback available"), "\n")
    else {
        n <- length(x)
        for(i in 1:n) {
            label <- paste(n-i+1, ": ", sep="")
            if((m <- length(x[[i]])) > 1)
                label <- c(label, rep(substr("          ", 1,
                                             nchar(label, type="w")),
                                      m - 1))
            cat(paste(label, x[[i]], sep=""), sep="\n")
        }
    }
    invisible()
}
