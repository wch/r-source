data <- function(..., list = character(0), package = .packages(),
		 lib.loc = .lib.loc) {
  # FIXME add support for package and lib.loc args
  names <- c(as.character(substitute(list(...))[-1]), list)
  if (length(names) == 0) {
    datafile<-system.file("data","index.doc")
    if( datafile == "" )
      stop("no index file for data")
    xx<-scan(datafile,skip=3,what="",sep="\t")
    cat("   R DATA SETS \n")
    cat(t(matrix(xx[!is.na(xx)],nc=2,byrow=T)),sep=c("\t\t","\n"))
  }
  else
    for (name in names) {
      file <- system.file("data", name)
      if(file == "") stop(paste("no data set called", name))
      else source(file)
    }
  invisible(names)
}

getenv <-function(names) .Internal(getenv(names))

help <- function(topic, package = .packages(), lib.loc = .lib.loc) {
  cat("Please use the Help menu.\n")
}

library <- function(name, help, lib.loc = .lib.loc,
		    character.only = FALSE, logical.return = FALSE) {
  .NotYetImplemented()
}

library.dynam <- function(chname, package = .packages(), lib.loc = .lib.loc) {
  # FIXME (this is == Unix  with changes 1) .dll instead of .so	 2) "\\" for "/"
  if (!exists(".Dyn.libs"))
    assign(".Dyn.libs", character(0), envir = .AutoloadEnv)
  if(missing(chname) || (LEN <- nchar(chname)) == 0)
    return(.Dyn.libs)
  if (substr(chname, LEN - 3, LEN) == ".dll") {
    chname <- substr(chname, 1, LEN - 4)
  }
  if (is.na(match(chname, .Dyn.libs))) {
    file <- system.file(paste("libs", "\\", chname, ".", "dll", sep = ""),
			package, lib.loc)
    if (file == "") {
      stop(paste("dynamic library `", chname, "' not found", sep = ""))
    }
    .Internal(dyn.load(file))
    assign(".Dyn.libs", c(.Dyn.libs, chname), envir = .AutoloadEnv)
  }
  invisible(.Dyn.libs)
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
