traceback <- function()
{
    if (exists(".Traceback", env = .GlobalEnv))
	.Traceback <- get(".Traceback", env = .GlobalEnv)
    else .Traceback <- NULL
    if(is.null(.Traceback) || length(.Traceback) == 0)
        cat("No traceback available\n")
    else {
        n <- length(.Traceback)
        for(i in 1:n) {
            label <- paste(n-i+1, ": ", sep="")
            if((m <- length(.Traceback[[i]])) > 1)
                label <- c(label, rep(substr("          ", 1,
                                             nchar(label, type="w")),
                                      m - 1))
            cat(paste(label, .Traceback[[i]], sep=""), sep="\n")
        }
    }
    invisible()
}
