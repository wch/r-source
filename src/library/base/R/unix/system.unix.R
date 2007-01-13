system <- function(command, intern = FALSE, ignore.stderr = FALSE,
                   wait = TRUE, input = NULL,
                   show.output.on.console = FALSE, minimized = FALSE,
                   invisible = FALSE) {
    if(!missing(wait) || !missing(input) || !missing(show.output.on.console) ||
       !missing(minimized) || !missing(invisible))
        warning("the only arguments used by system() on this platform are\n  'command', 'intern' and 'ignore.stderr'")
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
		     command, intern))
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
