## .Platform <- list(OS.type = "mac", file.sep = ":", dynlib.ext = ".dll")
.Platform <- Platform()# via config.h

getenv <- function(names) .Internal(getenv(names))

system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

system.date <- function() .NotYetImplemented()

system.time <- function(expr) .NotYetImplemented()

tempfile <- function(pattern = "file")
.NotYetImplemented()

unlink <- function(x) .NotYetImplemented()
