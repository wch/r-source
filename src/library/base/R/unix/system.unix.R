system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
                     command, intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system")
    system(call, intern)
}

##--- The following 2  should/could really be done in C [platform !] :
tempfile <- function(pattern = "file") {
    system(paste("for p in", paste(pattern, collapse = " "), ";",
		 "do echo /tmp/$p$$; done"),
	   intern = TRUE)
}

unlink <- function(x) { system(paste("rm -rf ", paste(x, collapse = " "))) }

