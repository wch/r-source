system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
		     command, intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system")
    system(call, intern)
}

tempfile <- function(pattern = "file") .Internal(tempfile(pattern))

##--- The following should/could really be done in C [platform !] :
unlink <- function(x, recursive = FALSE) {
    if(!is.character(x)) stop("argument must be character")
    if(recursive)
        system(paste("rm -rf ", paste(x, collapse = " ")))
    else
        system(paste("rm -f ", paste(x, collapse = " ")))
}

dir.create <- function(path)
{
    if(!is.character(path) || (length(path) > 1) || !nchar(path))
	stop("invalid `path' argument")
    invisible(system(paste("mkdir", path)) == 0)
}
