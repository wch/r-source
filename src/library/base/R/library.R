library <-
    function(package, help, lib.loc = .lib.loc, character.only = FALSE,
             logical.return = FALSE, warn.conflicts = TRUE,
             keep.source = getOption("keep.source.pkgs"))
{
    fQuote <- function(s) paste("`", s, "'", sep = "")
    if(!missing(package)) {
	if(!character.only)
	    package <- as.character(substitute(package))
	pkgname <- paste("package", package, sep = ":")
	if(is.na(match(pkgname, search()))) {
            pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
            if(length(pkgpath) == 0) {
                txt <- paste("There is no package called",
                             fQuote(package))
                if (logical.return) {
                    warning(txt)
		    return(FALSE)
		}
		else stop(txt)
            }
            which.lib.loc <- dirname(pkgpath)
            codeFile <- file.path(which.lib.loc, package, "R", package)
	    ## create environment
	    env <- attach(NULL, name = pkgname)
            ## detach does not allow character vector args
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
	    ## source file into env
	    if(file.exists(codeFile))
                sys.source(codeFile, env, keep.source = keep.source)
            else
		warning(paste("Package ",
                              fQuote(package),
                              "contains no R code"))
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
		lib.pos <- match(pkgname, search())
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
			    cat("\nAttaching package ", fQuote(package),
                                ":\n\n", sep = "")
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
		warning(paste("Package",
                              pkgname,
                              "already present in search()"))
	}
    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        help <- help[1]                 # only give help on one package

        pkgpath <- .find.package(help, lib.loc)
        outFile <- tempfile("Rlibrary")
        outConn <- file(outFile, open = "w")
        docFiles <- file.path(pkgpath,
                              c("TITLE", "DESCRIPTION", "INDEX"))
        headers <- c("", "Description:\n\n", "Index:\n\n")
        footers <- c("\n", "\n", "")
        for(i in which(file.exists(docFiles))) {
            writeLines(headers[i], outConn, sep="")
            writeLines(readLines(docFiles[i]), outConn)
            writeLines(footers[i], outConn, sep="")
        }
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = paste("Documentation for package",
                  fQuote(help)))
    }
    else {
	## library():
	outFile <- tempfile("Rlibrary")
        outConn <- file(outFile, open = "w")
	avail <- NULL
	for(lib in lib.loc) {
	    cat("\nPackages in library `", lib, "':\n\n", sep = "",
		file = outConn, append = TRUE)
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                title <- file.path(lib, i, "TITLE")
                if(file.exists(title))
                    writeLines(readLines(title), outConn)
                else
                    writeLines(i, outConn)
	    }
	    avail <- c(avail, a)
	}
        close(outConn)
	file.show(outFile, delete.file = TRUE,
                  title = "R packages available")
	return(invisible(avail))
    }
    if (logical.return)
	TRUE
    else invisible(.packages())
}

library.dynam <-
function(chname, package = .packages(), lib.loc = .lib.loc, verbose =
         getOption("verbose"), file.ext = .Platform$dynlib.ext, ...)
{
    if (!exists(".Dyn.libs"))
        assign(".Dyn.libs", character(0), envir = .AutoloadEnv)
    if (missing(chname) || (LEN <- nchar(chname)) == 0)
        return(.Dyn.libs)
    nc.ext <- nchar(file.ext)
    if (substr(chname, LEN - nc.ext + 1, LEN) == file.ext)
        chname <- substr(chname, 1, LEN - nc.ext)
    if (is.na(match(chname, .Dyn.libs))) {
        for(pkg in .find.package(package, lib.loc, missing(lib.loc),
                                 quiet = TRUE)) {
            file <- file.path(pkg, "libs",
                              paste(chname, file.ext, sep = ""))
            if(file.exists(file)) break
            else
                file <- ""
        }
        if(file == "") {
            stop(paste("dynamic library `", chname, "' not found",
                       sep = ""))
        }
        if (verbose)
            cat("now dyn.load(", file, ")..\n", sep = "")
        dyn.load(file, ...)
        assign(".Dyn.libs", c(.Dyn.libs, chname), envir = .AutoloadEnv)
    }
    invisible(.Dyn.libs)
}

require <- function(package, quietly = FALSE, warn.conflicts = TRUE,
                    keep.source = getOption("keep.source.pkgs"))
{
    package <- as.character(substitute(package)) # allowing "require(eda)"
    if (is.na(match(paste("package", package, sep = ":"), search()))) {
	if (!quietly) cat("Loading required package:", package, "\n")
	library(package, char = TRUE, logical = TRUE,
		warn.conflicts = warn.conflicts, keep.source = keep.source)
    } else TRUE
}

.packages <- function(all.available = FALSE, lib.loc = .lib.loc) {
    if(all.available) {
	ans <- character(0)
        lib.loc <- lib.loc[file.exists(lib.loc)]
        for(lib in lib.loc) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            for(nam in a) {
                if(file.exists(file.path(lib, nam, "R", nam))
                   || file.exists(file.path(lib, nam, "data")))
                    ans <- c(ans, nam)
            }
        }
        return(unique(ans))
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1, 8) == "package:"], 9)))
}

.path.package <- function(package = .packages(), quiet = FALSE)
{
    if(length(package) == 0) return(character(0))
    s <- search()
    searchpaths <- lapply(1:length(s),
                          function(i) attr(pos.to.env(i), "path"))
    searchpaths[[length(s)]] <- system.file()
    pkgs <- paste("package", package, sep=":")
    pos <- match(pkgs, s)
    if(any(m <- is.na(pos))) {
        if(!quiet) {
            miss <- paste(package[m], collapse=", ")
            if(all(m)) stop(paste("none of the packages are not loaded"))
            else warning(paste("package(s)", miss, "are not loaded"))
        }
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names=FALSE)
}

.find.package <-
function(package, lib.loc = .lib.loc, use.attached, quiet = FALSE) {

    if(missing(use.attached))
        use.attached <- missing(lib.loc)
    else if(is.null(use.attached))
        use.attached <- FALSE
    else if(!is.logical(use.attached))
        stop("incorrect value for `use.attached'")

    fQuote <- function(s) paste("`", s, "'", sep = "")

    n <- length(package)
    if(n == 0)
        return(character(0))

    bad <- character(0)                 # names of packages not found
    paths <- character(0)               # paths to packages found

    for(pkg in package) {
        fp <- file.path(lib.loc, pkg)
        if(use.attached)
            fp <- c(.path.package(pkg, TRUE), fp)
        fp <- unique(fp[file.exists(fp)])
        if(length(fp) == 0) {
            bad <- c(bad, pkg)
            next
        }
        if(length(fp) > 1) {
            fp <- fp[1]
            warning(paste("package `", pkg, "' found more than once,\n",
                          "using the one found in `", dirname(fp), "'",
                          sep = ""))
        }
        paths <- c(paths, fp)
    }

    if(!quiet && (length(bad) > 0)) {
        if(length(paths) == 0)
            stop("none of the packages were found")
        for(pkg in bad)
            warning(paste("there is no package called", fQuote(pkg)))
    }

    paths
}
