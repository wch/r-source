.Platform <-
    list(OS.type = "Windows",
         file.sep = "\\\\",
         dynlib.ext = ".dll",
         ## The next few are from Guido's:
	 show.file = function(filename) {
                       a <- scan(filename,what="c",sep="\n",quiet=TRUE)
                       for (i in 1:length(a))
                       cat(a[i],"\n")},
	 append.file = function(f1,f2) {# append to 'f1' the file 'f2':
             a <- scan(f1, what = "c", sep = "\n", quiet = TRUE)
             for (i in 1:length(a)) cat(a[i], "\n",file=f2,append=TRUE)
         },
	 show.data = function(package,lib.loc,fsep) {
	 ## give `index' of all possible data sets
             for (lib in lib.loc)
             for (pkg in package) {
	      INDEX <- system.file(paste("data", "index.doc", sep = fsep),
			      pkg, lib)
	      if (INDEX != "") {
	       cat(paste("\n\nData sets in package `", pkg, "':\n\n",
                         sep = fsep))
               .Platform$ show.file(INDEX)
              }}},	 
         )

getenv <- function(names) .Internal(getenv(names))

help <- function(topic, package = .packages(), lib.loc = .lib.loc) {
    cat("Please use the Help menu.\n")
}

library <- function(name, help, lib.loc = .lib.loc,
		    character.only = FALSE, logical.return = FALSE) {
    .NotYetImplemented()
}

system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

system.date <- function() .NotYetImplemented()

system.file <- function(file = "", pkg = .packages(), lib = .lib.loc) {
    FILES <- paste(t(outer(lib, pkg, paste, sep = "/")),
		   file, sep = "/", collapse = " ")
    .Internal(system.file(FILES))
}

system.time <- function(expr) .NotYetImplemented()

tempfile <- function(pattern = "file") {
    .Internal(tempfile(pattern))
}

unlink <- function(x) .NotYetImplemented()
