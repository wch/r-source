system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
		     command, intern))

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
