##-- Keep  'library' and 'library.dynam'  PLATFORM-Independent !
##-- Use  .Platform  (== Platform() from config.h ) to configure!
##	  ~~~~~~~~~

library <-
  function (package, help, lib.loc = .lib.loc, character.only = FALSE,
	    logical.return = FALSE, warn.conflicts = package != "MASS",
            keep.source = getOption("keep.source.pkgs"))

{
    if (!missing(package)) {
	if (!character.only)
	    package <- as.character(substitute(package))
	pkgname <- paste("package", package, sep = ":")
	if (is.na(match(pkgname, search()))) {
	    packagedir <- system.file("", pkg = package, lib = lib.loc)
	    if (packagedir == "") {
		txt <- paste("There is no package called `",
			     package, "'", sep = "")
		if (logical.return) {
		    warning(txt)
		    return(FALSE)
		}
		else stop(txt)
	    }
            lib.loc <- unique(lib.loc)
	    which.lib.loc <-
		lib.loc[match(packagedir[1], file.path(lib.loc, package))]
	    if (length(packagedir) > 1) {
		warning(paste("Package `", package,
                              "' found more than once,\n  ",
			      "using the one found in `", which.lib.loc,
			      "'", sep = ""))
	    }
	    file <- system.file("R", package, pkg = package, lib = lib.loc)
	    ## allowed zipped R source files
	    if (file == "") {
		tfile <- file.path(which.lib.loc, package, "R", package)
		zfile <- zip.file.extract(tfile)
		if (zfile != tfile) {
		    file <- zfile
		    on.exit(unlink(file))
		}
	    }
	    ## create environment
	    env <- attach(NULL, name = pkgname)
            ## detach does not allow character vector args
            on.exit(detach(2))
            lastbit<- file.path("", "R", package)
            path <- gsub(paste(lastbit, "$", sep=""), "", file)
            attr(env, "path") <- path
	    ## "source" file into env
	    if (file == "")
		warning(paste("Package `", package, "' contains no R code",
			      sep = ""))
	    else sys.source(file, env, keep.source = keep.source)
	    .Internal(lib.fixup(env, .GlobalEnv))
	    if(exists(".First.lib", envir = env, inherits = FALSE)) {
		firstlib <- get(".First.lib", envir = env, inherits = FALSE)
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(".First.lib failed")
	    }
            if(!is.null(firstlib <- getOption(".First.lib")[[package]])){
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(".First.lib failed")
            }
	    if (warn.conflicts &&
		!exists(".conflicts.OK",  envir = env, inherits = FALSE)) {
		##-- Check for conflicts
		dont.mind <- c("last.dump", "last.warning", ".Last.value",
			       ".Random.seed")
		## Currently, package is ALWAYS at "pos=2"
		lib.pos <- 2
		ob <- objects(lib.pos)
		fst <- TRUE
		ipos <- seq(along = sp <- search())[-c(lib.pos,
			    match("Autoloads", sp))]
		for (i in ipos) {
		    obj.same <- match(objects(i), ob, nomatch = 0)
		    if (any(obj.same > 0) &&
			length(same <- (obs <- ob[obj.same])
			       [!obs %in% dont.mind])) {
			if (fst) {
			    fst <- FALSE
			    cat("\nAttaching Package \"", pkgname,
				"\":\n\n", sep = "")
			}
			cat("\n\tThe following object(s) are masked",
			    if (i < lib.pos) "_by_" else "from", sp[i],
			    ":\n\n\t", same, "\n\n")
		    }
		}
	    }
            on.exit()
	}
	else {
	    if (getOption("verbose"))
		warning(paste("Package",pkgname,"already present in search()"))
	}
    }
    else if (!missing(help)) {
	if (!character.only)
	    help <- as.character(substitute(help))
        help <- help[1]                 # only give help on one package
	file <- system.file("INDEX", pkg=help, lib=lib.loc)
	if (file == "")
	    stop(paste("No documentation for package `", help, "'", sep = ""))
        if(length(file) > 1) {
	    which.lib.loc <-
                lib.loc[match(system.file("", pkg = help, lib =
                                          lib.loc)[1],
                              file.path(lib.loc, help))]
            warning(paste("Package `", help, "' found more than once,\n  ",
                          "using the one found in `", which.lib.loc,
                          "'", sep = ""))
        }
	file.show(file[1], title = paste("Contents of package", help))
    }
    else {
	## library():
	libfil <- tempfile("R.")
	avail <- NULL
	for (lib in lib.loc) {
	    cat("\nPackages in library `", lib, "':\n\n", sep = "",
		file = libfil, append = TRUE)
	    if (file.exists(libind <- file.path(lib, "LibIndex")))
	    {
		file.append(libfil, libind)
		## This gives warnings and partly garbage,
		## since contrib's LibIndex isn't really "clean":
		## scan(libind, what=list("",""), sep="\t",
		a <- NULL
	    }
	    else {
		a <- .packages(all.available = TRUE, lib.loc = lib)
		for (i in sort(a)) {
		    title <- system.file("TITLE", pkg=i, lib=lib)
		    if (title != "")
			file.append(libfil, title)
		    else cat(i, "\n", file = libfil, append = TRUE)
		}
	    }
	    avail <- c(avail, a)
	}
	file.show(libfil, delete.file = TRUE, title = "R packages available")
	return(invisible(avail))
    }
    if (logical.return)
	TRUE
    else invisible(.packages())
}

library.dynam <-
  function (chname, package = .packages(), lib.loc = .lib.loc,
	    verbose = getOption("verbose"), file.ext = .Platform$dynlib.ext)
{
  if (!exists(".Dyn.libs"))
    assign(".Dyn.libs", character(0), envir = .AutoloadEnv)
  if (missing(chname) || (LEN <- nchar(chname)) == 0)
    return(.Dyn.libs)
  nc.ext <- nchar(file.ext)
  if (substr(chname, LEN - nc.ext + 1, LEN) == file.ext)
    chname <- substr(chname, 1, LEN - nc.ext)
  if (is.na(match(chname, .Dyn.libs))) {
    file <- system.file(file.path("libs", paste(chname, file.ext,
			      sep = "")), pkg = package, lib = lib.loc)
    if (file == "") {
      stop(paste("dynamic library `", chname, "' not found",
		 sep = ""))
    }
    if (verbose)
      cat("now dyn.load(", file, ")..\n", sep = "")
    dyn.load(file)
    assign(".Dyn.libs", c(.Dyn.libs, chname), envir = .AutoloadEnv)
  }
  invisible(.Dyn.libs)
}

require <- function(package, quietly = FALSE, warn.conflicts = TRUE,
                    keep.source = getOption("keep.source.pkgs"))
{
    package <- as.character(substitute(package)) # allowing "require(eda)"
    if (is.na(match(paste("package", package, sep = ":"), search())))
        if(!exists(".Provided") || is.na(match(package, .Provided))) {
	if (!quietly)
	    cat("Loading required package:", package, "\n")
	library(package, char = TRUE, logical = TRUE,
		warn.conflicts = warn.conflicts, keep.source = keep.source)
    } else TRUE
    else TRUE
}

.packages <- function(all.available = FALSE, lib.loc = .lib.loc) {
    if(all.available) {
	a <- list.files(lib.loc[file.exists(lib.loc)], all.files =
			FALSE, full.names = FALSE)
	ans <- character(0)
	for (nam in a) {
	    pkg <- system.file(file.path("R", nam), pkg = nam, lib =
			       lib.loc)
	    if (pkg != "") ans <- c(ans,nam)
	}
	return(ans)
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1, 8) == "package:"], 9)))
}

.path.package <- function(package = .packages())
{
    if(length(package) == 0) return(character(0))
    s <- search()
    searchpaths <- lapply(1:length(s),
                          function(i) attr(pos.to.env(i), "path"))
    searchpaths[[length(s)]] <- system.file()
    pkgs <- paste("package", package, sep=":")
    pos <- match(pkgs, s)
    if(any(m <- is.na(pos))) {
        miss <- paste(package[m], collapse=", ")
        if(all(m)) stop(paste("none of the packages are not loaded"))
        else warning(paste("package(s)", miss, "are not loaded"))
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names=FALSE)
}
