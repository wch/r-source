.Platform <-
    list(OS.type = "Windows",
         file.sep = "\\\\",
         dynlib.ext = ".dll",
         show.file = function(file) .NotYetImplemented(),
         append.file = function(f1,f2) .NotYetImplemented(), # concat(f1,f2)
         show.libraries = function(lib.loc, fsep) .NotYetImplemented(),
         )

data <- function(..., list = character(0), package = .packages(),
		 lib.loc = .lib.loc) {
    ## FIXME add support for package and lib.loc args
    names <- c(as.character(substitute(list(...))[-1]), list)
    if (length(names) == 0) {
	datafile<-system.file("data","index.doc")
	if( datafile == "" )
	    stop("no index file for data")
	xx<-scan(datafile,skip=3,what="",sep="\t")
	cat("	R DATA SETS \n")
	cat(t(matrix(xx[!is.na(xx)],nc=2,byrow=TRUE)),sep=c("\t\t","\n"))
    }
    else
	for (name in names) {
	    file <- system.file("data", name)
	    if(file == "") stop(paste("no data set called", name))
	    else source(file)
	}
    invisible(names)
}

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
