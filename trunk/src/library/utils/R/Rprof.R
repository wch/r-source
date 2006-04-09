Rprof <- function(filename = "Rprof.out", append = FALSE, interval = 0.02)
{
    if(is.null(filename)) filename <- ""
    invisible(.Internal(Rprof(filename, append, interval)))
}

