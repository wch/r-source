data <-
function(..., list = character(0), package =c(.packages(),.Autoloaded),
	 lib.loc = .lib.loc, trace = FALSE) {
  names <- c(as.character(substitute(list(...))[-1]), list)
  if (!missing(package))
    if (is.name(y <- substitute(package)))# && !is.character(package))
      package <- as.character(y)
  found <- FALSE
  if (length(names) == 0) { #-- give 'index' of all possible data sets
    file <- tempfile("Rdata.")
    on.exit(unlink(file))
    for (lib in lib.loc)
      for (pkg in package) {
	INDEX <- system.file(paste("data", "index.doc", sep = "/"),
			     pkg, lib)
	if (INDEX != "") {
	  cat(paste(ifelse(found, "\n", ""),
		    "Data sets in package `", pkg, "':\n\n", sep = ""),
	      file = file, append = TRUE)
	  system(paste("cat", INDEX, ">>", file, "2>/dev/null"))
	  if(!found) found <- TRUE
	}
      }
    if (found)
      system(paste("$RHOME/cmd/pager", file))
  }
  else for (name in names) {
    dn <- paste("data/", name, sep = "")
    files <- system.file(paste(dn, ".*", sep = ""), package, lib.loc)
    found <- FALSE
    if (files != "") {
      for (file in files) {
	if(trace)
	  cat("name=",name,":\t file= .../",sub(".*/","",file),"::\t",sep="")
	if (found) break
	found <- TRUE
        ext <- sub(".*\\.", "", file)
        ## make sure the match is really for `name.ext'
        if (sub(".*/", "", file) != paste(name, ".", ext, sep = ""))
          found <- FALSE
        else
          switch(ext,
                 "R" =, "r" = source(file),
                 "RData" =, "rdata" =, "rda" = load(file),
                 "TXT" =, "txt" =, "tab" =
                 assign(name, read.table(file, header = TRUE),
                        env = .GlobalEnv),
                 "CSV" =, "csv" =
                 assign(name, read.table(file, header = TRUE, sep = ";"),
                        env = .GlobalEnv),
                 ## otherwise
                 found <- FALSE)
	if (trace) cat(if(!found) "*NOT* ", "found\n")
      }
    }
    if (!found)
      warning(paste("Data set `", name, "' not found", sep = ""))
  }
  invisible(names)
}

getenv <- function(x) {
  if (missing(x)) {
    x <- strsplit(.Internal(getenv(character())), "=")
    v <- n <- character(LEN <- length(x))
    for (i in 1:LEN) {
      n[i] <- x[[i]][1]
      v[i] <- paste(x[[i]][-1], collapse = "=")
    }
    structure(v, names = n)
  } else {
    structure(.Internal(getenv(x)), names = x)
  }
}

help <-
function(topic, package = c(.packages(),.Autoloaded), lib.loc =.lib.loc) {
  if (!missing(package))
    if (is.name(y <- substitute(package)))# && !is.character(package))
      package <- as.character(y)
  if (!missing(topic)) {
    topic <- substitute(topic)
    if (is.name(topic))
      topic <- as.character(topic)
    else if (!is.character(topic))
      stop("Unimplemented help feature")
    if (!is.na(match(topic, c("+", "-", "*", "/", "^", "%%"))))
      topic <- "Arithmetic"
    else if (!is.na(match(topic, c("<", ">", "<=", ">=", "==", "!="))))
      topic <- "Comparison"
    else if (!is.na(match(topic, c("[", "[[", "$"))))
      topic <- "Extract"
    else if (!is.na(match(topic, c("&", "&&", "|", "||", "!"))))
      topic <- "Logic"
    else if (!is.na(match(topic,c("%*%"))))
      topic<- "matmult"
    topic <- gsub("\\[","\\\\[", topic)#- for cmd/help ..
    INDICES <- paste(t(outer(lib.loc, package, paste, sep = "/")),
		     "help", "AnIndex", sep = "/", collapse = " ")
    file <- system(paste("${RHOME}/cmd/help '", topic, "' ", INDICES, sep=""),
		   intern = TRUE)
    if (file == "") { # try data .doc -- this is OUTDATED
      file <- system.file(paste("data", "/", topic, ".doc", sep = ""),
			  package, lib.loc)
    }
    if (length(file) && file != "") {
      if (!is.null(.Options$trace) && .Options$trace)
        cat ("\t\t\t\t\t\tHelp file name '", sub(".*/","",file),".Rd'\n",sep="")
      system(paste("${RHOME}/cmd/pager", file))
    } else
      stop(paste("No documentation for `", topic, "'", sep = ""))
  }
  else if (!missing(package))
    library(help = package, lib = lib.loc, character.only = TRUE)
  else if (!missing(lib.loc))
    library(lib = lib.loc)
  else
    help("help", "base", .Library)
}

library <-
function (name, help, lib.loc = .lib.loc, character.only = FALSE,
	  logical.return = FALSE)
{
  if (!missing(name)) {
    if (!character.only)
      name <- as.character(substitute(name))
    lib.source <- function(file, env) {
      exprs <- parse(n = -1, file = file)
      if (length(exprs) == 0)
	return(invisible())
      for (i in exprs) yy <- eval(i, env)
      invisible()
    }
    pkgname <- paste("package", name, sep = ":")
    if (is.na(match(pkgname, search()))) {
      packagedir <- system.file("", name, lib.loc)
      if (packagedir == "") {
	txt <- paste("There is no package called `", name, "'", sep = "")
	if (logical.return) {
	  warning(txt)
	  return(FALSE)
	}
	else stop(txt)
      }
      which.lib.loc <-
        lib.loc[match(packagedir[1], paste(lib.loc, name, "", sep = "/"))]
      if (length(packagedir) > 1) {
        warning(paste("Package `", name, "' found more than once,\n  ",
                      "using the one found in `", which.lib.loc, "'",
                      sep = ""))
      }
      file <- system.file(paste("R", name, sep = "/"), name, lib.loc)
      env <- attach(NULL, name = pkgname)
      if (file == "")
	warning(paste("Package `", name, "' contains no R code", sep = ""))
      else
	lib.source(file, env)
      lib.fixup(env, .GlobalEnv)
      if (exists(".First.lib", envir = env, inherits = FALSE)) {
	firstlib <- get(".First.lib", envir = env, inherits = FALSE)
	firstlib(which.lib.loc, name)
      }
    }
  } else if (!missing(help)) {
    if (!character.only)
      help <- as.character(substitute(help))
    file <- system.file("INDEX", help, lib.loc)
    if (file == "")
      stop(paste("No documentation for package `",
		 help, "'", sep = ""))
    else
      system(paste("$RHOME/cmd/pager", file))

  } else {
    file <- tempfile("R.")
    on.exit(unlink(file))
    first <- TRUE
    for (lib in lib.loc) {
      cat(paste(ifelse(first, "", "\n"), "Packages in library `",
		lib, "':\n\n", sep = ""), file = file,
	  append = TRUE)
      INDEX <- paste(lib, "LibIndex", sep = "/")
      system(paste("cat", INDEX, ">>", file,
		   "2>/dev/null"))
      first <- FALSE
    }
    system(paste("$RHOME/cmd/pager", file))
  }
  if (logical.return)
    TRUE
  else invisible(.packages())
}

library.dynam <-
function(chname, package = .packages(), lib.loc = .lib.loc) {
  if (!exists(".Dyn.libs"))
    assign(".Dyn.libs", character(0), envir = .AutoloadEnv)
  if(missing(chname) || (LEN <- nchar(chname)) == 0)
    return(.Dyn.libs)
  if (substr(chname, LEN - 2, LEN) == ".so") {
    chname <- substr(chname, 1, LEN - 3)
  }
  if (is.na(match(chname, .Dyn.libs))) {
    file <- system.file(paste("libs", "/", chname, ".", "so", sep = ""),
			package, lib.loc)
    if (file == "") {
      stop(paste("dynamic library `", chname, "' not found", sep = ""))
    }
    .Internal(dyn.load(file))
    assign(".Dyn.libs", c(.Dyn.libs, chname), envir = .AutoloadEnv)
  }
  invisible(.Dyn.libs)
}

system <- function(call, intern = FALSE) .Internal(system(call, intern))

system.date <- function() { system("date", intern = TRUE) }

system.file <- function(file = "", pkg = .packages(), lib = .lib.loc) {
	FILES <- paste(t(outer(lib, pkg, paste, sep = "/")),
		       file, sep = "/", collapse = " ")
	system(paste("${RHOME}/cmd/filename", FILES), intern = TRUE)
}

system.time <- function(expr) {
	## Purpose: Return CPU (and other) times that `expr' used ..
	## Modelled after S "unix.time"
	## -----------------------------------------------------------------
	## Arguments: expr: `any' valid R expression
	## -----------------------------------------------------------------
	loc.frame <- sys.frame(sys.parent(1))
	on.exit(cat("Timing stopped at:", proc.time() - time, "\n"))
	expr <- substitute(expr)
	time <- proc.time()
	eval(expr, envir = loc.frame)
	new.time <- proc.time()
	on.exit()
	if(length(new.time) == 3)	new.time <- c(new.time, 0, 0)
	if(length(time) == 3)		time	 <- c(	  time, 0, 0)
	new.time - time
}

unix.time <- system.time

tempfile <- function(pattern = "file") {
  system(paste("for p in", paste(pattern, collapse = " "), ";",
	       "do echo /tmp/$p$$; done"),
	 intern = TRUE)
}

unlink <- function(x) {
  system(paste("rm -rf ", paste(x, collapse = " ")))
}
