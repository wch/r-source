##-- Keep  'library' and 'library.dynam'  PLATFORM-Indepedent !
##-- Use  .Platform  in	 ./system.unix.R [./system.win.R , ...] to configure!
##	  ~~~~~~~~~

library <- function (name, help, lib.loc = .lib.loc, character.only = FALSE,
		     logical.return = FALSE, warn.conflicts = name != "MASS")
{
    fsep <- .Platform$file.sep
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
		txt <- paste("There is no package called `", name, "'", sep= "")
		if (logical.return) {
		    warning(txt)
		    return(FALSE)
		}
		else stop(txt)
	    }
	    which.lib.loc <-
		lib.loc[match(packagedir[1], paste(lib.loc, name, "",sep=fsep))]
	    if (length(packagedir) > 1) {
		warning(paste("Package `", name, "' found more than once,\n  ",
			      "using the one found in `", which.lib.loc, "'",
			      sep = ""))
	    }
	    file <- system.file(paste("R", name, sep = fsep), name, lib.loc)
	    env <- attach(NULL, name = pkgname)# create environment
	    if (file == "")
		warning(paste("Package `",name,"' contains no R code", sep=""))
	    else
		lib.source(file, env)# "source" file into env
	    lib.fixup(env, .GlobalEnv)
	    if (exists(".First.lib", envir = env, inherits = FALSE)) {
		firstlib <- get(".First.lib", envir = env, inherits = FALSE)
		firstlib(which.lib.loc, name)
	    }
            if(warn.conflicts) {
                ##-- Check for conflicts
                dont.mind <- c("last.dump", "last.warning",
                               ".Last.value", ".Random.seed")
                lib.pos <- 2## Currently, package is ALWAYS at "pos=2"
                ob <- objects(lib.pos)
                ipos <- seq(along = sp <- search())[
                            -c(lib.pos, match("Autoloads",sp))]
                for(i in ipos) {
                    obj.same <- match(objects(i), ob, nomatch = 0)
                    fst <- TRUE
                    if(any(obj.same > 0) && 
                       length(same<-(ob <- ob[obj.same])[!ob %in% dont.mind])) {
                        if(fst){fst <- FALSE; cat("\nAttaching Package \"",
                                                  pkgname,"\":\n\n", sep="")}
                        cat("\n\tThe following object(s) are masked",
                            if(i < lib.pos) "by" else "from", sp[i], ":\n\n\t",
                            same, "\n\n")
                    }
                }
            }
	} else {
            if(options()$verbose)
                warning(paste("Package",pkgname,"already present in search()"))
        }
    } else if (!missing(help)) {
	if (!character.only)
	    help <- as.character(substitute(help))
	file <- system.file("INDEX", help, lib.loc)
	if (file == "")
	    stop(paste("No documentation for package `",
		       help, "'", sep = ""))
	else
	    .Platform$ show.file(file)
    } else { ## library():
        for (lib in lib.loc) {
            cat(paste("\nPackages in library `", lib,"':\n\n", sep = ""))
            a <- .packages(all.available = TRUE, lib.loc=lib)
            for (i in a) {
                title <- system.file("TITLE",i,lib)
                if (title != "") 
	            .Platform$ show.file(title)
                else
	            cat(i,"\n")
            }
        }
        return(invisible(a))
    }
    if (logical.return)
	TRUE
    else invisible(.packages())
}

library.dynam <-
    function(chname, package = .packages(), lib.loc = .lib.loc,
	     verbose = .Options$verbose, file.ext = .Platform$dynlib.ext)
{
    if (!exists(".Dyn.libs"))
	assign(".Dyn.libs", character(0), envir = .AutoloadEnv)
    if(missing(chname) || (LEN <- nchar(chname)) == 0)
	return(.Dyn.libs)
    fsep <- .Platform$file.sep
    nc.ext <- nchar(file.ext)
    if (substr(chname, LEN - nc.ext+1, LEN) == file.ext)
	chname <- substr(chname, 1, LEN - nc.ext)

    if (is.na(match(chname, .Dyn.libs))) {
	file <- system.file(paste("libs", fsep, chname, file.ext, sep = ""),
			    package, lib.loc)
	if (file == "") {
	    stop(paste("dynamic library `", chname, "' not found", sep = ""))
	}
	if(verbose) cat("now dyn.load(",file,")..\n", sep="")
	.Internal(dyn.load(file))
	assign(".Dyn.libs", c(.Dyn.libs, chname), envir = .AutoloadEnv)
    }
    invisible(.Dyn.libs)
}

require <- function(name, quietly = FALSE) {
    name <- as.character(substitute(name)) # allowing "require(eda)"
    if (!exists(".Provided", inherits = TRUE))
	assign(".Provided", character(0), envir = .GlobalEnv)
    if (is.na(match(paste("package", name, sep = ":"), search()))
	&& is.na(match(name, .Provided))) {
	if (!quietly)
	    cat("Loading required package:", name, "\n")
	library(name, char = TRUE, logical = TRUE)
    }
    else
	TRUE
}

provide <- function(name) {
    if (!exists(".Provided", inherits = TRUE))
	assign(".Provided", character(0), envir = .GlobalEnv)
    if (missing(name))
	.Provided
    else {
	name <- as.character(substitute(name))
	if (is.na(match(name, .packages())) &&
	    is.na(match(name, .Provided))) {
	    assign(".Provided", c(name, .Provided), envir = .GlobalEnv)
	    TRUE
	}
	else
	    FALSE
    }
}

.packages <- function(all.available = FALSE, lib.loc = .lib.loc) {
    if(all.available) {
        fsep <- .Platform$ file.sep
        a <- strsplit(system.file("*","",lib.loc), fsep)
        ans <- character(0)
        for (i in a) {
            name <- i[length(i)]
            pkg <- system.file(paste("R",name, sep=fsep), name, lib.loc) 
            if (pkg != "") ans <- c(ans,name)
        }
        return(ans)
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1, 8) == "package:"], 9)))
}

