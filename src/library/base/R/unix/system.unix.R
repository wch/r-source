system <- function(command, intern = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = FALSE, minimized = FALSE,
                   invisible = FALSE) {
    if(!missing(show.output.on.console) || !missing(minimized) || !missing(invisible))
        warning("arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only")
    if(ignore.stderr) command <- paste(command, "2>/dev/null")
    if(!is.null(input)) {
        if(!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        cat(input, file=f, sep="\n")
        command <- paste(command, "<", f)
    }
    if(!wait) command <- paste(command, "&")
    .Internal(system(command, intern))
}

unix <- function(call, intern = FALSE) {
    .Deprecated("system")
    system(call, intern)
}

##--- The following should/could really be done in C [platform !] :
unlink <- function(x, recursive = FALSE) {
    if(!is.character(x)) stop("argument must be character")
    if(recursive)
        system(paste("rm -rf ", paste(x, collapse = " ")))
    else
        system(paste("rm -f ", paste(x, collapse = " ")))
}
