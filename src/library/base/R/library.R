library <-
function(package, help, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         verbose = getOption("verbose"))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")
    if(!missing(package)) {
	if(!character.only)
	    package <- as.character(substitute(package))
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
	pkgname <- paste("package", package, sep = ":")
	if(is.na(match(pkgname, search()))) {
            ## check for the methods package before attaching this package.
            ## Only if it is _already_ here do we do cacheMetaData
            ## (otherwise, the methods package does it instead, perhaps
            ## from a requires() in this package's .First.lib).
            hasMethods <- !is.na(match("package:methods", search()))
            pkgpath <- .find.package(package, lib.loc, quiet = TRUE,
                                     verbose = verbose)
            if(length(pkgpath) == 0) {
                txt <- paste("There is no package called",
                             sQuote(package))
                if(logical.return) {
                    warning(txt)
		    return(FALSE)
		}
		else stop(txt)
            }
            which.lib.loc <- dirname(pkgpath)
            ## if the name space mechanism is available and the package
            ## has a name space, then the name space loading mechanism
            ## takes over.
            if (exists("packageHasNamespace") &&
                packageHasNamespace(package, which.lib.loc))
                return(doNamespaceLibrary(package, which.lib.loc, lib.loc,
                                          logical.return))
            codeFile <- file.path(which.lib.loc, package, "R", package)
	    ## create environment (not attached yet)
	    loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
	    ## source file into loadenv
	    if(file.exists(codeFile))
                sys.source(codeFile, loadenv, keep.source = keep.source)
            else
		warning(paste("Package ",
                              sQuote(package),
                              "contains no R code"))
            ## now transfer contents of loadenv to an attached frame
	    env <- attach(NULL, name = pkgname)
            ## detach does not allow character vector args
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
            for (name in ls(loadenv, all = TRUE)) {
                val <- get(name, env = loadenv)
                rm(list=name, envir = loadenv, inherits = FALSE)
	        if (typeof(val) == "closure" &&
                    identical(environment(val), loadenv))
                    environment(val) <- .GlobalEnv
                assign(name, val, env = env)
            }
            ## run .First.lib
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
	    if(warn.conflicts &&
               !exists(".conflicts.OK", envir = env, inherits = FALSE)) {
		## Check for conflicts
		dont.mind <- c("last.dump", "last.warning",
                               ".Last.value", ".Random.seed")
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
			    cat("\nAttaching package ", sQuote(package),
                                ":\n\n", sep = "")
			}
			cat("\n\tThe following object(s) are masked",
			    if (i < lib.pos) "_by_" else "from", sp[i],
			    ":\n\n\t", same, "\n\n")
		    }
		}
	    }
            if(hasMethods
               && !identical(pkgname, "package:methods"))
               cacheMetaData(env, TRUE)
            on.exit()
	}
	else if(verbose)
            warning(paste("Package", sQuote(package),
                          "already present in search()"))
    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        help <- help[1]                 # only give help on one package

        pkgpath <- .find.package(help, lib.loc, verbose = verbose)
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
                  sQuote(help)))
    }
    else {
	## library():
        if(is.null(lib.loc))
            lib.loc <- .libPaths()
        db <- matrix(character(0), nr = 0, nc = 3)
        nopkgs <- character(0)

        for(lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for(i in sort(a)) {
                INDEX <- file.path(lib, i, "TITLE")
                title <- if(file.exists(INDEX))
                    read.00Index(INDEX)[, 2]
                else ""
                db <- rbind(db, cbind(i, lib, title))
            }
            if(length(a) == 0)
                nopkgs <- c(nopkgs, lib)
        }
        colnames(db) <- c("Package", "LibPath", "Title")

        y <- list(header = NULL, results = db, footer = NULL)
        ## <FIXME>
        ## Should do something about libraries without packages, as
	## recorded in nopkgs.
        ## </FIXME>
        class(y) <- "libraryIQR"
        return(y)
    }

    if (logical.return)
	TRUE
    else invisible(.packages())
}

library.dynam <-
function(chname, package = .packages(), lib.loc = NULL, verbose =
         getOption("verbose"), file.ext = .Platform$dynlib.ext, ...)
{
    ## <FIXME>
    ## Versions of R prior to 1.4.0 had .Dyn.libs in .AutoloadEnv
    ## (and did not always ensure getting it from there).
    ## We now consistently use the base environment.
    if(!exists(".Dyn.libs", envir = NULL)) {
        assign(".Dyn.libs", character(0), envir = NULL)
    }
    if(missing(chname) || (LEN <- nchar(chname)) == 0)
        return(get(".Dyn.libs", envir = NULL))
    nc.ext <- nchar(file.ext)
    if(substr(chname, LEN - nc.ext + 1, LEN) == file.ext)
        chname <- substr(chname, 1, LEN - nc.ext)
    if(is.na(match(chname, get(".Dyn.libs", envir = NULL)))) {
        ## <FIXME>
        ## Do we really want `quiet = TRUE'?
        for(pkg in .find.package(package, lib.loc, quiet = TRUE,
                                 verbose = verbose)) {
            file <- file.path(pkg, "libs",
                              paste(chname, file.ext, sep = ""))
            if(file.exists(file)) break
            else
                file <- ""
        }
        ## </FIXME>
        if(file == "") {
            stop(paste("dynamic library `", chname, "' not found",
                       sep = ""))
        }
        if(verbose)
            cat("now dyn.load(", file, ")..\n", sep = "")
        dyn.load(file, ...)
        assign(".Dyn.libs",
               c(get(".Dyn.libs", envir = NULL), chname),
               envir = NULL)
    }
    invisible(get(".Dyn.libs", envir = NULL))
    ## </FIXME>
}

require <-
function(package, quietly = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"))
{
    package <- as.character(substitute(package)) # allowing "require(eda)"
    if (is.na(match(paste("package", package, sep = ":"), search()))) {
	if (!quietly) cat("Loading required package:", package, "\n")
	library(package, char = TRUE, logical = TRUE,
		warn.conflicts = warn.conflicts, keep.source = keep.source)
    } else TRUE
}

.packages <-
function(all.available = FALSE, lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
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

.path.package <-
function(package = .packages(), quiet = FALSE)
{
    if(length(package) == 0) return(character(0))
    s <- search()
    searchpaths <-
        lapply(1:length(s), function(i) attr(as.environment(i), "path"))
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
function(package, lib.loc = NULL, use.attached, quiet = FALSE,
         verbose = getOption("verbose"))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")
    
    if(!missing(use.attached))
        warning(paste("argument", sQuote("use.attached"),
                      "is deprecated"))
    use.attached <- FALSE
    if(is.null(lib.loc)) {
        use.attached <- TRUE
        lib.loc <- .libPaths()
    }
    
    n <- length(package)
    if(n == 0) return(character(0))

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
            if(verbose) {
                warning(paste("package ", sQuote(pkg),
                              " found more than once,\n",
                              "using the one found in ",
                              sQuote(dirname(fp)),
                              sep = ""))
            }
        }
        paths <- c(paths, fp)
    }

    if(!quiet && (length(bad) > 0)) {
        if(length(paths) == 0)
            stop("none of the packages were found")
        for(pkg in bad)
            warning(paste("there is no package called", sQuote(pkg)))
    }

    paths
}

.libPaths <-
function(new)
{
    if(!missing(new))
        assign(".lib.loc", unique(c(new, .Library)), envir = NULL)
    get(".lib.loc", envir = NULL)
}
