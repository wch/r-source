testPlatformEquivalence <- function(built, run)
{
    ## args are "cpu-vendor-os", but os might be 'linux-gnu'!
    ## remove vendor field
    built <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", built)
    run <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", run)
    ## allow for small mismatches, e.g. OS version number and i686 vs i586.
    length(agrep(built, run)) > 0
}

library <-
function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         verbose = getOption("verbose"), version)
{
    testRversion <- function(pkgInfo, pkgname)
    {
        current <- getRversion()
        ## depends on R version?
        if(length(Rdeps <- pkgInfo$Rdepends) > 1) {
            target <- Rdeps$version
            res <- eval(parse(text=paste("current", Rdeps$op, "target")))
            if(!res)
                stop(sprintf(gettext("This is R %s, package '%s' needs %s %s"),
                             current, pkgname, Rdeps$op, target),
                     call. = FALSE, domain = NA)
        }
        ## which version was this package built under?
        if(!is.null(built <- pkgInfo$Built)) {
            ## must be >= 2.0.0
            if(built$R < "2.0.0")
                stop("package ", sQuote(pkgname),
                     " was built before R 2.0.0: please re-install it",
                     call. = FALSE)
            ## warn if later than this version
            if(built$R > current)
                warning("package ", sQuote(pkgname),
                        " was built under R version ", built$R,
                        call. = FALSE)
            if(.Platform$OS.type == "unix") {
                platform <- built$Platform
		if(length(grep("\\w", platform)) &&
                   !testPlatformEquivalence(platform, R.version$platform))
                    stop("package ", sQuote(pkgname), " was built for ",
                         platform,
                         call. = FALSE)
            }
        }
        else
            stop("package ", sQuote(pkgname),
                 " has not been installed properly\n",
                 "See the Note in ?library", call. = FALSE)
    }

    checkNoGenerics <- function(env, pkg)
    {
        nenv <- env
        ns <- .Internal(getRegisteredNamespace(as.name(libraryPkgName(pkg))))
        if(!is.null(ns)) nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            ## A package will have created a generic
            ## only if it has created a formal method.
            length(objects(env, pattern="^\\.__M", all=TRUE)) == 0
        }
    }

    checkConflicts <- function(package, pkgname, pkgpath, nogenerics)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".First.lib", ".Last.lib",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ## ignore generics not defined for the package
        ob <- objects(lib.pos, all = TRUE)
        if(!nogenerics && .isMethodsDispatchOn()) {
            these <- objects(lib.pos, all = TRUE)
            these <- these[substr(these, 1, 6) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
        ipos <- seq(along = sp)[-c(lib.pos, match("Autoloads", sp))]
        for (i in ipos) {
            obj.same <- match(objects(i, all = TRUE), ob, nomatch = 0)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if(length(Classobjs)) same <- same[-Classobjs]
                if(length(same)) {
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
    }

    libraryPkgName <- function(pkgName, sep = "_")
	unlist(strsplit(pkgName, sep, fixed=TRUE))[1]

    libraryPkgVersion <- function(pkgName, sep = "_")
    {
        splitName <- unlist(strsplit(pkgName, sep, fixed=TRUE))
	if (length(splitName) > 1) splitName[2] else NULL
    }

    libraryMaxVersPos <- function(vers)
    {
	## Takes in a character vector of version numbers
        ## returns the position of the maximum version.
        if(length(vers) == 0) return(integer(0))
        vers <- package_version(vers)
        min(which(vers == max(vers)))
    }

    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach")) # might be list()
        for(fun in hook) try(fun(pkgname, pkgpath))
    }

    bindTranslations <- function(pkgname, pkgpath)
    {
        popath <- file.path(pkgpath, "po")
        if(!file.exists(popath)) return()
        bindtextdomain(pkgname, popath)
        bindtextdomain(paste("R", pkgname, sep="-"), popath)
    }

    if(!missing(package)) {
        if (is.null(lib.loc)) lib.loc <- .libPaths()

	if(!character.only)
	    package <- as.character(substitute(package))

        if(package %in% c("ctest", "eda", "modreg", "mva", "nls",
                          "stepfun", "ts")) {
            have.stats <- "package:stats" %in% search()
            if(!have.stats) require("stats")
            warning("package ", sQuote(package),
                    " has been merged into 'stats'", call. = FALSE)
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if(package  == "mle") {
            have.stats4 <- "package:stats4" %in% search()
            if(!have.stats4) require("stats4")
            warning("package ", sQuote(package),
                    " has been merged into 'stats4'", call. = FALSE)
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if(package == "lqs") {
            warning("package 'lqs' has been moved back to package 'MASS'",
                    call. = FALSE, immediate. = TRUE)
            have.VR <- "package:MASS" %in% search()
            if(!have.VR) {
                if(require("MASS", quietly=TRUE))
                    warning("package 'MASS' has now been loaded",
                            call. = FALSE, immediate. = TRUE)
                else {
                    if(logical.return) return(FALSE)
                    else
                        stop("package 'MASS' seems to be missing from this R installation")
                }
            }
            return(if (logical.return) TRUE else invisible(.packages()))
        }

	if (!missing(version)) {
	     package <- manglePackageName(package, version)
        } else {
	   ## Need to find the proper package to install
	   pkgDirs <- list.files(lib.loc,
                                 pattern = paste("^", package, sep=""))
           ## See if any directories in lib.loc match the pattern of
           ## 'package', if none do, just continue as it will get caught
           ## below.  Otherwise, if there is actually a 'package', use
           ## that, and if not, then use the highest versioned dir.
	   if (length(pkgDirs) > 0) {
	       if (!(package %in% pkgDirs)) {
		   ## Need to find the highest version available
		   vers <- unlist(lapply(pkgDirs, libraryPkgVersion))
		   vpos <- libraryMaxVersPos(vers)
		   if (length(vpos) > 0) package <- pkgDirs[vpos]
               }
           }
        }

        ## NB from this point on `package' is either the original name or
        ## something like ash_1.0-8
        if(length(package) != 1)
            stop("'package' must be of length 1")
	pkgname <- paste("package", package, sep = ":")
	newpackage <- is.na(match(pkgname, search()))
	if(newpackage) {
            ## Check for the methods package before attaching this
            ## package.
            ## Only if it is _already_ here do we do cacheMetaData.
            ## The methods package caches all other libs when it is
            ## attached.

            pkgpath <- .find.package(package, lib.loc, quiet = TRUE,
                                     verbose = verbose)
            if(length(pkgpath) == 0) {
               txt <- paste(gettext("There is no package called"),
                            sQuote(libraryPkgName(package)))
		vers <- libraryPkgVersion(package)
		if (!is.null(vers))
		   txt <- paste(txt, ", version ", vers, sep="")
                if(logical.return) {
                    warning(txt, domain=NA)
		    return(FALSE)
		} else stop(txt)
            }
            which.lib.loc <- dirname(pkgpath)
            pfile <- system.file("Meta", "package.rds", package = package,
                                 lib.loc = which.lib.loc)
            if(!nchar(pfile))
            	stop(sQuote(libraryPkgName(package)),
                     " is not a valid package -- installed < 2.0.0?")
            pkgInfo <- .readRDS(pfile)
            testRversion(pkgInfo, package)

            ## The check for inconsistent naming is now in .find.package

            if(is.character(pos)) {
                npos <- match(pos, search())
                if(is.na(npos)) {
                    warning(sQuote(pos),
                            " not found on search path, using pos = 2")
                    pos <- 2
                } else pos <- npos
            }
            .getRequiredPackages2(pkgInfo)
#                .getRequiredPackages2(pkgInfo, lib.loc = lib.loc)
            ## If the name space mechanism is available and the package
            ## has a name space, then the name space loading mechanism
            ## takes over.
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                    ns <- loadNamespace(package, c(which.lib.loc, lib.loc))
                    dataPath <- file.path(which.lib.loc, package, "data")
                    env <- attachNamespace(ns, pos = pos,
                                           dataPath = dataPath)
                })
                if (inherits(tt, "try-error"))
                    if (logical.return)
                        return(FALSE)
                    else stop("package/namespace load failed for ",
                              sQuote(libraryPkgName(package)))
                else {
                    on.exit(do.call("detach", list(name = pkgname)))
                    nogenerics <- checkNoGenerics(env, package)
                    if(warn.conflicts &&
                       !exists(".conflicts.OK", envir = env, inherits = FALSE))
                        checkConflicts(package, pkgname, pkgpath, nogenerics)

                    if(!nogenerics && .isMethodsDispatchOn() &&
                       !identical(pkgname, "package:methods"))
                        methods::cacheMetaData(env, TRUE,
                                               searchWhere = .GlobalEnv)
                    runUserHook(package, pkgpath)
                    on.exit()
                    if (logical.return)
                        return(TRUE)
                    else
                        return(invisible(.packages()))
                }
            }
            codeFile <- file.path(which.lib.loc, package, "R",
                                  libraryPkgName(package))
            ## create environment (not attached yet)
            loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
            ## save the package name in the environment
            assign(".packageName", package, envir = loadenv)
            ## source file into loadenv
            if(file.exists(codeFile)) {
                res <- try(sys.source(codeFile, loadenv,
                                     keep.source = keep.source))
                if(inherits(res, "try-error"))
                    stop("unable to load R code in package ",
                         sQuote(libraryPkgName(package)),
                         call. = FALSE)
            } else if(verbose)
                warning("package ", sQuote(libraryPkgName(package)),
                        " contains no R code")
            ## lazy-load data sets if required
            dbbase <- file.path(which.lib.loc, package, "data", "Rdata")
            if(file.exists(paste(dbbase, ".rdb", sep="")))
                lazyLoad(dbbase, loadenv)
            ## lazy-load a sysdata database if present
            dbbase <- file.path(which.lib.loc, package, "R", "sysdata")
            if(file.exists(paste(dbbase, ".rdb", sep="")))
                lazyLoad(dbbase, loadenv)
            ## now transfer contents of loadenv to an attached frame
            env <- attach(NULL, pos = pos, name = pkgname)
            ## detach does not allow character vector args
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
            ## the actual copy has to be done by C code to avoid forcing
            ## promises that might have been created using delay().
            .Internal(lib.fixup(loadenv, env))

            ## Do this before we use any code from the package
            bindTranslations(libraryPkgName(package), pkgpath)

            ## run .First.lib
            if(exists(".First.lib", mode = "function",
                      envir = env, inherits = FALSE)) {
                firstlib <- get(".First.lib", mode = "function",
                                envir = env, inherits = FALSE)
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(".First.lib failed for ",
                              sQuote(libraryPkgName(package)))
            }
            if(!is.null(firstlib <- getOption(".First.lib")[[package]])){
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(".First.lib failed for ",
                              sQuote(libraryPkgName(package)))
            }
            nogenerics <- checkNoGenerics(env, package)
            if(warn.conflicts &&
               !exists(".conflicts.OK", envir = env, inherits = FALSE))
                checkConflicts(package, pkgname, pkgpath, nogenerics)

            if(!nogenerics && .isMethodsDispatchOn() &&
               !identical(pkgname, "package:methods"))
                methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
            runUserHook(package, pkgpath)
            on.exit()
	}
	if (verbose && !newpackage)
            warning("package ", sQuote(libraryPkgName(package)),
                    " already present in search()")
    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        pkgName <- help[1]              # only give help on one package
        pkgPath <- .find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
                      file.path(pkgPath, "INDEX"))
        if(file.exists(vignetteIndexRDS <-
                       file.path(pkgPath, "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector(length = 4, mode = "list")
        pkgInfo[[1]] <- paste("\n\t\tInformation on package",
                              sQuote(pkgName))
        readDocFile <- function(f) {
            if(basename(f) %in% "package.rds") {
                txt <- .readRDS(f)$DESCRIPTION
                nm <- paste(names(txt), ":", sep="")
                formatDL(nm, txt, indent = max(nchar(nm, type="w")) + 3)
            } else if(basename(f) %in% "vignette.rds") {
                txt <- .readRDS(f)
                ## New-style vignette indexes are data frames with more
                ## info than just the base name of the PDF file and the
                ## title.  For such an index, we give the names of the
                ## vignettes, their titles, and indicate whether PDFs
                ## are available.
                ## The index might have zero rows.
                if(is.data.frame(txt) && nrow(txt))
                    cbind(basename(gsub("\\.[[:alpha:]]+$", "",
                                        txt$File)),
                          paste(txt$Title,
                                paste(rep.int("(source", NROW(txt)),
                                      ifelse(txt$PDF != "",
                                             ", pdf",
                                             ""),
                                      ")", sep = "")))
                else NULL
            } else
                readLines(f)
        }
        for(i in which(file.exists(docFiles)))
            pkgInfo[[i+1]] <- readDocFile(docFiles[i])
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
                ## All packages installed under 2.0.0 should have
                ## 'package.rds' but we have not checked.
                file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
                title <- if(file != "") {
                    tmp <- .readRDS(file)
                    if(is.list(tmp)) tmp <- tmp$DESCRIPTION
                    tmp["Title"]
                } else NA
                if(is.na(title))
                    title <- " ** No title available (pre-2.0.0 install?)  ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if(length(a) == 0)
                nopkgs <- c(nopkgs, lib)
        }
        colnames(db) <- c("Package", "LibPath", "Title")
        if((length(nopkgs) > 0) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs),
                                    "library %s contains no packages",
                                    "libraries %s contain no packages"),
                           pkglist)
            warning(msg, domain=NA)
        }

        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }

    if (logical.return)
	TRUE
    else invisible(.packages())
}

print.libraryIQR <-
function(x, ...)
{
    db <- x$results
    ## Split according to LibPath.
    out <- if(nrow(db) == 0)
        NULL
    else lapply(split(1 : nrow(db), db[, "LibPath"]),
                function(ind) db[ind, c("Package", "Title"),
                                 drop = FALSE])
    outFile <- tempfile("RlibraryIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(lib in names(out)) {
        writeLines(paste(ifelse(first, "", "\n"),
                         "Packages in library ", sQuote(lib), ":\n",
                         sep = ""),
                   outConn)
        writeLines(formatDL(out[[lib]][, "Package"],
                            out[[lib]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines("no packages found")
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = "R packages available")
    }
    invisible(x)
}

library.dynam <-
function(chname, package = NULL, lib.loc = NULL, verbose =
         getOption("verbose"), file.ext = .Platform$dynlib.ext, ...)
{
    #.Dyn.libs <- .dynLibs()
    .Dyn.libs <- getLoadedDLLs()

    if(missing(chname) || (ncChname <- nchar(chname)) == 0)
        return(.Dyn.libs)
    ncFileExt <- nchar(file.ext)
    if(substr(chname, ncChname - ncFileExt + 1, ncChname) == file.ext)
        chname <- substr(chname, 1, ncChname - ncFileExt)
    if(TRUE || is.na(match(chname, .Dyn.libs))) {
        for(pkg in .find.package(package, lib.loc, verbose = verbose)) {
            file <- file.path(pkg, "libs",
                              paste(chname, file.ext, sep = ""))
            if(file.exists(file)) break else file <- ""
        }
        if(file == "")
            stop("shared library ", sQuote(chname), " not found")
        which <- sapply(.Dyn.libs, function(x) x$path == file)
        if(any(which)) {
            if(verbose)
                cat("DLL", file, "already loaded\n")
            return(.Dyn.libs[[ seq(along=.Dyn.libs)[which] ]])
        }
        if(verbose)
            cat("now dyn.load(", file, ") ...\n", sep = "")
        dll <- dyn.load(file, ...)
        .dynLibs(c(.Dyn.libs, chname))
        return(dll)
    }
#XXX
    invisible(.dynLibs())
}

library.dynam.unload <-
function(chname, libpath, verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext)
{
    .Dyn.libs <- .dynLibs()
    if(missing(chname) || (ncChname <- nchar(chname)) == 0)
        stop("no shared library was specified")
    ncFileExt <- nchar(file.ext)
    if(substr(chname, ncChname - ncFileExt + 1, ncChname) == file.ext)
        chname <- substr(chname, 1, ncChname - ncFileExt)
    num <- match(chname, .Dyn.libs, 0)
    if(is.na(num))
        stop("shared library ", sQuote(chname), " was not loaded")
    file <- file.path(libpath, "libs", paste(chname, file.ext, sep = ""))
    if(!file.exists(file))
        stop("shared library ", sQuote(chname), " not found")
    if(verbose)
        cat("now dyn.unload(", file, ") ...\n", sep = "")
    dyn.unload(file)
    .dynLibs(.Dyn.libs[-num])
    invisible(.dynLibs())
}

require <-
function(package, quietly = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         character.only = FALSE, version, save = TRUE)
{
    if( !character.only )
        package <- as.character(substitute(package)) # allowing "require(eda)"
    if (missing(version)) {
        pkgName <- package
        ## dont' care about versions, so accept any
        s <- sub("_[0-9.-]*", "", search())
        loaded <- paste("package", pkgName, sep = ":") %in% s
    } else {
        pkgName <- manglePackageName(package, version)
        loaded <- paste("package", pkgName, sep = ":") %in% search()
    }

    if (!loaded) {
	if (!quietly)
            cat(sprintf(gettext("Loading required package: %s\n"), package))
	value <- library(package, character.only = TRUE, logical = TRUE,
		warn.conflicts = warn.conflicts, keep.source = keep.source,
                version = version)
    } else value <- TRUE

    if(identical(save, FALSE)) {}
    else {
        ## update the ".required" variable
        if(identical(save, TRUE)) {
            save <- topenv(parent.frame())
            ## (a package namespace, topLevelEnvironment option or
            ## .GlobalEnv)
            if(identical(save, .GlobalEnv)) {
                ## try to detect call from .First.lib in  a package
                ## <FIXME>
                ## Although the docs have long and perhaps always had
                ##   .First.lib(libname, pkgname)
                ## the majority of CRAN packages seems to use arguments
                ## 'lib' and 'pkg'.
                objectsInParentFrame <- sort(objects(parent.frame()))
                if(identical(sort(c("libname", "pkgname")),
                             objectsInParentFrame))
                    save <-
                        as.environment(paste("package:",
                                             get("pkgname",
                                                 parent.frame()),
                                             sep = ""))
                else if(identical(sort(c("lib", "pkg")),
                                  objectsInParentFrame))
                    save <-
                        as.environment(paste("package:",
                                             get("pkg",
                                                 parent.frame()),
                                             sep = ""))
                ## </FIXME>
                ## else either from prompt or in the source for install
                ## with saved image ?
            }
        }
        else
            save <- as.environment(save)
        hasDotRequired <- exists(".required", save, inherits=FALSE)
        if(!isNamespace(save) || hasDotRequired) { ## so assignment allowed
            if(hasDotRequired)
                packages <- unique(c(package, get(".required", save)))
            else
                packages <- package
            assign(".required", packages, save)
        }
    }
    value
}

.packages <- function(all.available = FALSE, lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(all.available) {
	ans <- character(0)
        lib.loc <- lib.loc[file.exists(lib.loc)]
        valid_package_version_regexp <-
            .standard_regexps()$valid_package_version
        for(lib in lib.loc) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            for(nam in a) {
                ## match .find.packages as to what is a package
                if(!file.exists(file.path(lib, nam, "DESCRIPTION")))
                    next
                ## ("If there is no 'DESCRIPTION' file, it ain't a
                ## package.  And that's the only check we have ...")
                ## <FIXME PRE-R-NG>
                ## All packages usable in R-ng must have 'package.rds'.
                ## (And we do not need to validate these meta data.)
                ## Should be simply ignore the others?
                ## (See also above ...)
                pfile <- file.path(lib, nam, "Meta", "package.rds")
                info <- if(file.exists(pfile))
                    .readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else
                    try(read.dcf(file.path(lib, nam, "DESCRIPTION"),
                                 c("Package", "Version"))[1, ],
                        silent = TRUE)
                ## In fact, info from 'package.rds' should be validated.
                if(inherits(info, "try-error") || any(is.na(info)))
                    next
                if(regexpr(valid_package_version_regexp,
                           info["Version"]) == -1)
                    next
                ans <- c(ans, nam)
                ## </FIXME>
            }
        }
        return(unique(ans))
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1, 8) == "package:"], 9)))
}

.path.package <- function(package = NULL, quiet = FALSE)
{
    if(is.null(package)) package <- .packages()
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
                stop("none of the packages are loaded")
            else
                warning(sprintf(ngettext(as.integer(sum(m)),
                                         "package %s is not loaded",
                                         "packages %s are not loaded"),
                                paste(package[m], collapse=", ")),
                        domain = NA)
        }
        pos <- pos[!m]
    }
    unlist(searchpaths[pos], use.names = FALSE)
}

.find.package <-
function(package = NULL, lib.loc = NULL, quiet = FALSE,
         verbose = getOption("verbose"))
{
    if(is.null(package) && is.null(lib.loc) && !verbose) {
        ## We only want the paths to the attached packages.
        return(.path.package())
    }

    use_attached <- FALSE
    if(is.null(package)) {
        package <- .packages()
    }
    if(is.null(lib.loc)) {
        use_attached <- TRUE
        lib.loc <- .libPaths()
    }

    if(!length(package)) return(character())

    bad <- character(0)
    out <- character(0)

    for(pkg in package) {
        if(any(grep("_", pkg))) {
            ## The package "name" contains the version info.
            ## Note that .packages() is documented to return the "base
            ## names" of all currently attached packages.  In the case
            ## of versioned installs, this seems to contain both the
            ## package name *and* the version number (not sure if this
            ## is a bug or a feature).
            pkg_has_version <- TRUE
            pkg_regexp <- paste(pkg, "$", sep = "")
        }
        else {
            pkg_has_version <- FALSE
            pkg_regexp <- paste(pkg, "($|_)", sep = "")
        }
        paths <- character()
        for(lib in lib.loc) {
            dirs <- list.files(lib,
                               pattern = paste("^", pkg_regexp,
                               sep = ""),
                               full = TRUE)
            ## Note that we cannot use tools::file_test() here, as
            ## cyclic name space dependencies are not supported.  Argh.
            paths <- c(paths,
                       dirs[file.info(dirs)$isdir &
                            file.exists(file.path(dirs,
                                                  "DESCRIPTION"))])
        }
        if(use_attached
           && any(pos <- grep(paste("^package:", pkg_regexp,
                                    sep = ""),
                              search()))) {
            dirs <- sapply(pos, function(i) {
                if(is.null(env <- as.environment(i)))
                    system.file()
                else
                    attr(env, "path")
            })
            paths <- c(as.character(dirs), paths)
        }
        ## As an extra safety measure, only use the paths we found if
        ## their DESCRIPTION file registers the given package and has a
        ## valid version.  Actually, we should really exclude all
        ## candidates with "bad" DESCRIPTION metadata, but we cannot use
        ## tools:::.check_package_description() for a full check here.
        ## (But then packages installed with R 2.0.0 or later must have
        ## valid DESCRIPTION metadata anyways.)
        if(length(paths)) {
            paths <- unique(paths)
            valid_package_version_regexp <-
                .standard_regexps()$valid_package_version
            db <- lapply(paths, function(p) {
                ## <FIXME PRE-R-NG>
                ## All packages usable in R-ng must have 'package.rds'.
                ## (And we do not need to validate these meta data.)
                ## Should be simply ignore the others?
                ## (See also above ...)
                pfile <- file.path(p, "Meta", "package.rds")
                info <- if(file.exists(pfile))
                    .readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else
                    try(read.dcf(file.path(p, "DESCRIPTION"),
                                 c("Package", "Version"))[1, ],
                        silent = TRUE)
                if(inherits(info, "try-error"))
                    c(Package=NA, Version=NA) # need dimnames below
                else
                    info
                ## </FIXME>
            })
            db <- do.call("rbind", db)
            ok <- (apply(!is.na(db), 1, all)
                   & (db[, "Package"] == sub("_.*", "", pkg))
                   & (regexpr(valid_package_version_regexp,
                              db[, "Version"])) > -1)
            paths <- paths[ok]
        }
        if(length(paths) == 0) {
            bad <- c(bad, pkg)
            next
        }
        if(length(paths) > 1) {
            ## If a package was found more than once ...
            ## * For the case of an exact version match (if the "name"
            ##   already contained the version), use the first path;
            ## * Otherwise, be consistent with the current logic in
            ##   library(): if there are matching non-versioned paths,
            ##   use the first of these; otherwise, use the first path
            ##   with the highest version.  (Actually, we should really
            ##   return the path to the highest version which has
            ##   resolvable dependencies against the current version of
            ##   R ...)
            paths <- if(pkg_has_version) {
                paths[1]
            }
            else if(any(pos <- which(basename(paths) == pkg)))
                paths[pos][1]
            else {
                versions <- package_version(db[ok, "Version"])
                pos <- min(which(versions == max(versions)))
                paths <- paths[pos][1]
            }
            if(verbose)
                warning("package ", sQuote(pkg), " found more than once,\n",
                        "using the one found in ", sQuote(paths))
        }
        out <- c(out, paths)
    }

    if(!quiet && (length(bad) > 0)) {
        if(length(out) == 0)
            stop("none of the packages were found")
        for(pkg in bad)
            warning("there is no package called ", sQuote(pkg))
    }

    out
}

print.packageInfo <- function(x, ...)
{
    if(!inherits(x, "packageInfo")) stop("wrong class")
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
        if(is.list(entry) || is.matrix(entry))
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

manglePackageName <- function(pkgName, pkgVersion)
    paste(pkgName, "_", pkgVersion, sep = "")

.getRequiredPackages <-
    function(file="DESCRIPTION", quietly = FALSE, useImports = FALSE)
{
    ## OK to call tools as only used during installation.
    pkgInfo <- tools:::.split_description(tools:::.read_description(file))
    .getRequiredPackages2(pkgInfo, quietly, , useImports)
    invisible()
}

.getRequiredPackages2 <-
    function(pkgInfo, quietly = FALSE, lib.loc = NULL, useImports = FALSE)
{
    pkgs <- names(pkgInfo$Depends)
    if (length(pkgs)) {
        sch <- search()
        pkgname <- pkgInfo$DESCRIPTION["Package"]
        for(pkg in pkgs) {
            z <- pkgInfo$Depends[[pkg]]
            if ( !paste("package", pkg, sep = ":") %in% sch ) {
                if (length(z) > 1) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    if(nchar(pfile) == 0)
                        stop("package ", sQuote(pkg), " required by ",
                             sQuote(pkgname), " could not be found",
                             call. = FALSE)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    if (!eval(parse(text=paste("current", z$op, "z$version"))))
                        stop("package ", sQuote(pkg), " ", current,
                             " was found, but ", z$op, " ", z$version,
                             " is required by ", sQuote(pkgname),
                             call. = FALSE)
                }

                if (!quietly)
                    cat(sprintf(gettext("Loading required package: %s\n"), pkg))
                library(pkg, character.only = TRUE, logical = TRUE,
                        lib.loc = lib.loc) ||
                stop("package ", sQuote(pkg), " could not be loaded",
                     call. = FALSE)
            } else {
                ## check the required version number, if any
                if (length(z) > 1) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    if (!eval(parse(text=paste("current", z$op, "z$version"))))
                        stop("package ", sQuote(pkg), " ", current,
                             " is loaded, but ", z$op, " ", z$version,
                             " is required by ", sQuote(pkgname),
                             call. = FALSE)
                }
            }
        }
    }
    if(useImports) {
        nss <- names(pkgInfo$Imports)
        for(ns in nss) loadNamespace(ns, lib.loc)
    }
}
