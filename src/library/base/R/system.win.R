.Platform <-
    list(OS.type = "Windows", file.sep = "\\\\", dynlib.ext = ".dll")

getenv <- function(names) .Internal(getenv(names))

system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

system.time <- function(expr) .NotYetImplemented()

tempfile <- function(pattern = "file") {
    .Internal(tempfile(pattern))
}

unlink <- function(x) .NotYetImplemented()
