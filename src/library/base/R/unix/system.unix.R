system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
		     command, intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system")
    system(call, intern)
}

##--- The following 2  should/could really be done in C [platform !] :
tempfile <- function(pattern = "file") {
    if(!is.character(pattern)) stop("`pattern' must be character")
    if(!length(pattern)) stop("`pattern' must be of length >= 1")
    if(!nchar(tmpdir <- getenv("TMPDIR"))) tmpdir <- "/tmp"
    paste(tmpdir, "/", pattern, system("echo $$", intern = TRUE), sep = "")
}

unlink <- function(x) {
    if(!is.character(x)) stop("argument must be character")
    system(paste("rm -rf ", paste(x, collapse = " ")))
}

dir.create <- function(path)
{
    if(!is.character(path) || (length(path) > 1) || !nchar(path))
	stop("invalid `path' argument")
    invisible(system(paste("mkdir", path)) == 0)
}
