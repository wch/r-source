library <-
function(package, help, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         verbose = getOption("verbose"))
{
    testRversion <- function(descfile)
    {
        current <- paste(R.version[c("major", "minor")], collapse = ".")
        fields <- read.dcf(descfile, fields =
                           c("Package", "Depends", "Built"))
        ## depends on R version?
        if(!package.dependencies(fields, check = TRUE)) {
            dep <- package.dependencies(fields)[[1]][1,]
            stop(paste("This is R ", current, ", package ",
                       fields[1, "Package"],
                       " needs ", dep[2], " ", dep[3], sep=""),
                 call. = FALSE)
        }
        ## which version was this package built under?
        if(!is.na(built <- fields[1, "Built"])) {
            builtFields <- strsplit(built, ";")[[1]]
            builtunder <- substring(builtFields[1], 3)
            if(nchar(builtunder) &&
               compareVersion(current, builtunder) < 0) {
                warning(paste("package", fields[1, "Package"],
                              "was built under R version", builtunder),
                        call. = FALSE)
            }
            if(.Platform$OS.type == "unix") {
                platform <- builtFields[2]
                ## allow for small mismatches, e.g. OS version number.
                m <- agrep(platform, R.version$platform)
                if(!length(m))
                    stop(paste("package", fields[1, "Package"],
                               "was built for", platform),
                         call. = FALSE)
            }
        }
        else
            stop(paste("This package has not been installed properly\n",
                       "See the Note in ?library"))
    }

    sQuote <- function(s) paste("`", s, "'", sep = "")
    if(!missing(package)) {
	if(!character.only)
	    package <- as.character(substitute(package))
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
	pkgname <- paste("package", package, sep = ":")
	if(is.na(match(pkgname, search()))) {
            ## Check for the methods package before attaching this
            ## package.
            ## Only if it is _already_ here do we do cacheMetaData.
            ## The methods package caches all other libs when it is
            ## attached. 
            ## Note for detail: this does _not_ test whether dispatch is
            ## currently on, but rather whether the package is attached
            ## (cf .isMethodsDispachOn).
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
            descfile <- system.file("DESCRIPTION", package = package,
                                    lib.loc = which.lib.loc)
            if(nchar(descfile)) testRversion(descfile)
            else stop("This is not a valid package -- no DESCRIPTION exists")
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
            else if(verbose)
		warning(paste("Package ",
                              sQuote(package),
                              "contains no R code"))
            ## now transfer contents of loadenv to an attached frame
	    env <- attach(NULL, name = pkgname)
            ## detach does not allow character vector args
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
	    ## the actual copy has to be done by C code to avoid forcing
            ## promises that might have been created using delay().
            .Internal(lib.fixup(loadenv, env))
            ## save the package name in the environment
            assign(".packageName", package, envir = env)
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
        pkgName <- help[1]              # only give help on one package
        pkgPath <- .find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- file.path(pkgPath,
                              c("TITLE", "DESCRIPTION", "INDEX",
                                file.path("doc", "00Index.dcf")))
        pkgInfo <- vector(length = 4, mode = "list")
        readDocFile <- function(f) {
            if(basename(f) %in% c("DESCRIPTION", "00Index.dcf")) {
                ## This should be in valid DCF format ...
                txt <- try(read.dcf(f))
                if(inherits(txt, "try-error")) {
                    warning(paste("file",
                                  sQuote(f),
                                  "is not in valid DCF format"))
                    return(NULL)
                }
                ## Return a list so that the print method knows to
                ## format as a description list (if non-empty).
                txt <- if(all(dim(txt) >= 1))
                    list(colnames(txt), as.character(txt[1, ]))
                else
                    NULL
            }
            else
                txt <- readLines(f)
            txt
        }
        for(i in which(file.exists(docFiles)))
            pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
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
                if(length(title) == 0) title <- ""
                db <- rbind(db, cbind(i, lib, title))
            }
            if(length(a) == 0)
                nopkgs <- c(nopkgs, lib)
        }
        colnames(db) <- c("Package", "LibPath", "Title")
        if((length(nopkgs) > 0) && !missing(lib.loc)) {
            if(length(nopkgs) > 1)
                warning(paste("libraries",
                              paste(sQuote(nopkgs), collapse = ", "),
                              "contain no packages"))
            else
                warning(paste("library",
                              paste(sQuote(nopkgs)),
                              "contains no package"))
        }

        y <- list(header = NULL, results = db, footer = NULL)
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
    sQuote <- function(s) paste("`", s, "'", sep = "")

    .Dyn.libs <- .dynLibs()
    if(missing(chname) || (ncChname <- nchar(chname)) == 0)
        return(.Dyn.libs)
    ncFileExt <- nchar(file.ext)
    if(substr(chname, ncChname - ncFileExt + 1, ncChname) == file.ext)
        chname <- substr(chname, 1, ncChname - ncFileExt)
    if(is.na(match(chname, .Dyn.libs))) {
        for(pkg in .find.package(package, lib.loc, verbose = verbose)) {
            file <- file.path(pkg, "libs",
                              paste(chname, file.ext, sep = ""))
            if(file.exists(file)) break
            else
                file <- ""
        }
        if(file == "") {
            stop(paste("dynamic library", sQuote(chname), "not found"))
        }
        if(verbose)
            cat("now dyn.load(", file, ") ...\n", sep = "")
        dyn.load(file, ...)
        .dynLibs(c(.Dyn.libs, chname))
    }
    invisible(.dynLibs())
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
                if(file.exists(file.path(lib, nam, "DESCRIPTION")))
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
    pkgs <- paste("package", package, sep = ":")
    pos <- match(pkgs, s)
    if(any(m <- is.na(pos))) {
        if(!quiet) {
            if(all(m))
                stop(paste("none of the packages are loaded"))
            else
                warning(paste("package(s)",
                              paste(package[m], collapse=", "),
                              "are not loaded"))
        }
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names = FALSE)
}

.find.package <-
function(package, lib.loc = NULL, quiet = FALSE,
         verbose = getOption("verbose"))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")

    useAttached <- FALSE
    if(is.null(lib.loc)) {
        useAttached <- TRUE
        lib.loc <- .libPaths()
    }

    n <- length(package)
    if(n == 0) return(character(0))

    bad <- character(0)                 # names of packages not found
    paths <- character(0)               # paths to packages found

    for(pkg in package) {
        fp <- file.path(lib.loc, pkg)
        if(useAttached)
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

print.packageInfo <-
function(x, ...)
{
    if(!inherits(x, "packageInfo")) stop("wrong class")
    sQuote <- function(s) paste("`", s, "'", sep = "")
    outFile <- tempfile("RpackageInfo")
    outConn <- file(outFile, open = "w")
    vignetteMsg <-
        paste("Further information is available in the following ",
              "vignettes in directory ",
              sQuote(file.path(x$path, "doc")),
              ":",
              sep = "")
    headers <- c("", "Description:\n\n", "Index:\n\n",
                 paste(paste(strwrap(vignetteMsg), collapse = "\n"),
                       "\n\n", sep = ""))
    footers <- c("\n", "\n", "\n", "")
    formatDocEntry <- function(entry) {
        if(is.list(entry))
            formatDL(entry, style = "list")
        else
            entry
    }
    for(i in which(!sapply(x$info, is.null))) {
        writeLines(headers[i], outConn, sep = "")
        writeLines(formatDocEntry(x$info[[i]]), outConn)
        writeLines(footers[i], outConn, sep = "")
    }
    close(outConn)
    file.show(outFile, delete.file = TRUE,
              title = paste("Documentation for package",
              sQuote(x$name)))
    invisible(x)
}
