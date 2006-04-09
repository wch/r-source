dump <- function (list, file = "dumpdata.R", append = FALSE,
		  control = "all", envir = parent.frame(),
                  evaluate = TRUE)
{
    digits <- options("digits")
    on.exit(options(digits))
    options(digits = 12)
    if(is.character(file)) {
        ## avoid opening a file if there is nothing to dump
        ex <- sapply(list, exists, envir=envir)
        if(!any(ex)) return(invisible(character(0)))
        if(nchar(file) > 0) {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file), add = TRUE)
        } else file <- stdout()
    }
    opts <- .deparseOpts(control)
    .Internal(dump(list, file, envir, opts, evaluate))
}

