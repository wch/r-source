.Platform <-
    list(OS.type = "Macintosh",
         file.sep = ":",
         dynlib.ext = ".dll",
         ## The next few are from Guido's:
	 show.file = function(filename) .NotYetImplemented(),
	 append.file = function(f1,f2) .NotYetImplemented(),
	 show.data = function(package,lib.loc,fsep) .NotYetImplemented()
         )

getenv <- function(names) .Internal(getenv(names))

help <- function(topic, package = .packages(), lib.loc = .lib.loc)
        .NotYetImplemented()

library <- function(name, help, lib.loc = .lib.loc,
		    character.only = FALSE, logical.return = FALSE) {
    .NotYetImplemented()
}

system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

system.date <- function() .NotYetImplemented()

system.file <- function(file = "", pkg = .packages(), lib = .lib.loc)
.NotYetImplemented()

system.time <- function(expr) .NotYetImplemented()

tempfile <- function(pattern = "file")
.NotYetImplemented()

unlink <- function(x) .NotYetImplemented()
