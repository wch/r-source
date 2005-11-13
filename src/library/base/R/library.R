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
                stop(gettextf("This is R %s, package '%s' needs %s %s",
                             current, pkgname, Rdeps$op, target),
                     call. = FALSE, domain = NA)
        }
        ## which version was this package built under?
        if(!is.null(built <- pkgInfo$Built)) {
            ## must be >= 2.0.0
            if(built$R < "2.0.0")
                stop(gettextf("package '%s' was built before R 2.0.0: please re-install it",
                              pkgname), call. = FALSE, domain = NA)
            ## warn if later than this version
            if(built$R > current)
                warning(gettextf("package '%s' was built under R version %s",
                                 pkgname, as.character(built$R)),
                        call. = FALSE, domain = NA)
            if(.Platform$OS.type == "unix") {
                platform <- built$Platform
		if(length(grep("\\w", platform)) &&
                   !testPlatformEquivalence(platform, R.version$platform))
                    stop(gettextf("package '%s' was built for %s",
                                  pkgname, platform),
                         call. = FALSE, domain = NA)
            }
        }
        else
            stop(gettextf("package '%s' has not been installed properly\n",
                          pkgname),
                 gettext("See the Note in ?library"),
                 call. = FALSE, domain = NA)
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
                        cat(gettextf("\nAttaching package: '%s'\n\n", package))
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
        ## remove any non-existent directories
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]

	if(!character.only)
	    package <- as.character(substitute(package))

        if(package %in% c("ctest", "eda", "modreg", "mva", "nls",
                          "stepfun", "ts")) {
            have.stats <- "package:stats" %in% search()
            if(!have.stats) require("stats")
            old <- "stats"
            warning(gettextf("package '%s' has been merged into '%s'",
                             package, old),
                    call. = FALSE, domain = NA)
            return(if (logical.return) TRUE else invisible(.packages()))
        }
        if(package  == "mle") {
            have.stats4 <- "package:stats4" %in% search()
            if(!have.stats4) require("stats4")
            old <- "stats4"
            warning(gettextf("package '%s' has been merged into '%s'",
                             package, old),
                    call. = FALSE, domain = NA)
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
        } else { # Need to find the package version to install
            ## this throws a warning if lib.loc has not been cleaned.
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
                if(length(lib.loc)) {
                    vers <- libraryPkgVersion(package)
                    txt <- if (!is.null(vers))
                        gettextf("there is no package called '%s', version %s",
                                 libraryPkgName(package), vers)
                    else
                        gettextf("there is no package called '%s'",
                                 libraryPkgName(package))
                } else {
                    txt <- gettext("no library trees found in 'lib.loc'")
                }
                if(logical.return) {
                    warning(txt, domain = NA)
		    return(FALSE)
		} else stop(txt, domain = NA)
            }
            which.lib.loc <- dirname(pkgpath)
            pfile <- system.file("Meta", "package.rds", package = package,
                                 lib.loc = which.lib.loc)
            if(!nchar(pfile))
            	stop(gettextf("'%s' is not a valid package -- installed < 2.0.0?",
                     libraryPkgName(package)), domain = NA)
            pkgInfo <- .readRDS(pfile)
            testRversion(pkgInfo, package)

            ## The check for inconsistent naming is now in .find.package

            if(is.character(pos)) {
                npos <- match(pos, search())
                if(is.na(npos)) {
                    warning(gettextf("'%s' not found on search path, using pos = 2", pos), domain = NA)
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
                    ns <- loadNamespace(package, c(which.lib.loc, lib.loc),
                                        keep.source = keep.source)
                    dataPath <- file.path(which.lib.loc, package, "data")
                    env <- attachNamespace(ns, pos = pos,
                                           dataPath = dataPath)
                })
                if (inherits(tt, "try-error"))
                    if (logical.return)
                        return(FALSE)
                    else stop(gettextf("package/namespace load failed for '%s'",
                                       libraryPkgName(package)),
                              call. = FALSE, domain = NA)
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
                    stop(gettextf("unable to load R code in package '%s'",
                                  libraryPkgName(package)),
                         call. = FALSE, domain = NA)
            } else if(verbose)
                warning(gettextf("package '%s' contains no R code",
                                 libraryPkgName(package)), domain = NA)
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
            ## promises that might have been created using delayedAssign().
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
                    else stop(gettextf(".First.lib failed for '%s'",
                                       libraryPkgName(package)), domain = NA)
            }
            if(!is.null(firstlib <- getOption(".First.lib")[[package]])){
                tt<- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(gettextf(".First.lib failed for '%s'",
                                       libraryPkgName(package)), domain = NA)
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
            warning(gettextf("package '%s' already present in search()",
                             libraryPkgName(package)), domain = NA)

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
        pkgInfo <- vector(length = 3, mode = "list")
        readDocFile <- function(f) {
            if(basename(f) %in% "package.rds") {
                txt <- .readRDS(f)$DESCRIPTION
                if("Encoding" %in% names(txt)) {
                    to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT"else ""
                    tmp <- try(iconv(txt, from=txt["Encoding"], to=to))
                    if(!inherits(tmp, "try-error"))
                        txt <- tmp
                    else
                        warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                }
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
                ## All packages installed under 2.0.0 should have
                ## 'package.rds' but we have not checked.
                file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
                title <- if(file != "") {
                    txt <- .readRDS(file)
                    if(is.list(txt)) txt <- txt$DESCRIPTION
                    ## we may need to re-encode here.
                    if("Encoding" %in% names(txt)) {
                        to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT" else ""
                        tmp <- try(iconv(txt, txt["Encoding"], to, "?"))
                        if(!inherits(tmp, "try-error"))
                            txt <- tmp
                        else
                            warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                    }
                    txt["Title"]
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
        writeLines(gettextf("%sPackages in library '%s':\n",
                            ifelse(first, "", "\n"),
                            lib),
                   outConn)
        writeLines(formatDL(out[[lib]][, "Package"],
                            out[[lib]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        message("no packages found")
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = gettext("R packages available"))
    }
    invisible(x)
}

library.dynam <-
function(chname, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext, ...)
{
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname)) == 0)
        return(dll_list)

    ## Be defensive about possible system-specific extension for shared
    ## libraries, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext)
    if(substr(chname, nc_chname - nc_file_ext + 1, nc_chname)
       == file.ext)
        chname <- substr(chname, 1, nc_chname - nc_file_ext)

    for(pkg in .find.package(package, lib.loc, verbose = verbose)) {
        file <- file.path(pkg, "libs",
                          paste(chname, file.ext, sep = ""))
        if(file.exists(file)) break else file <- ""
    }
    if(file == "")
        stop(gettextf("shared library '%s' not found", chname), domain = NA)
    ind <- sapply(dll_list, function(x) x[["path"]] == file)
    if(any(ind)) {
        if(verbose)
            message(gettextf("shared library '%s' already loaded", chname),
                    domain = NA)
        return(invisible(dll_list[[ seq(along = dll_list)[ind] ]]))
    }
    if(.Platform$OS.type == "windows") {
        ## Make it possible to find other DLLs in the same place as
        ## @code{file}, so that e.g. binary packages can conveniently
        ## provide possibly missing DLL dependencies in this place
        ## (without having to bypass the default package dynload
        ## mechanism).  Note that this only works under Windows, and a
        ## more general solution will have to be found eventually.
        PATH <- Sys.getenv("PATH")
        Sys.putenv("PATH" =
                   paste(gsub("/", "\\\\", dirname(file)), PATH, sep=";"))
        on.exit(Sys.putenv("PATH" = PATH))
    }
    if(verbose)
        message(gettextf("now dyn.load(\"%s\") ...", file), domain = NA)
    dll <- dyn.load(file, ...)
    .dynLibs(c(dll_list, list(dll)))
    invisible(dll)
}

library.dynam.unload <-
function(chname, libpath, verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext)
{
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname)) == 0)
        stop("no shared library was specified")

    ## Be defensive about possible system-specific extension for shared
    ## libraries, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext)
    if(substr(chname, nc_chname - nc_file_ext + 1, nc_chname)
       == file.ext)
        chname <- substr(chname, 1, nc_chname - nc_file_ext)

    file <- file.path(libpath, "libs",
                      paste(chname, file.ext, sep = ""))
    pos <- which(sapply(dll_list, function(x) x[["path"]] == file))
    if(!length(pos))
        stop(gettextf("shared library '%s' was not loaded", chname),
             domain = NA)

    if(!file.exists(file))
        stop(gettextf("shared library '%s' not found", chname), domain = NA)
    if(verbose)
        message(gettextf("now dyn.unload(\"%s\") ...", file), domain = NA)
    dyn.unload(file)
    .dynLibs(dll_list[-pos])
    invisible(dll_list[[pos]])
}

require <-
function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
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
            cat(gettextf("Loading required package: %s\n", package))
	value <- library(package, lib.loc = lib.loc, character.only = TRUE,
                         logical = TRUE, warn.conflicts = warn.conflicts,
                         keep.source = keep.source, version = version)
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
                ## In principle, info from 'package.rds' should be
                ## validated, but we already had counterexamples ...
                ## <FIXME>
                ## Shouldn't we warn about packages with bad meta data?
                if(inherits(info, "try-error")
                   || (length(info) != 2)
                   || any(is.na(info)))
                    next
                if(regexpr(valid_package_version_regexp,
                           info["Version"]) == -1)
                    next
                ## </FIXME>
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
                if(identical(env <- as.environment(i), baseenv()))
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
                ## In principle, info from 'package.rds' should be
                ## validated, but we already had counterexamples ...
                if(inherits(info, "try-error")
                   || (length(info) != 2)
                   || any(is.na(info)))
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
                warning(gettextf("package '%s' found more than once,\nusing the one found in '%s'",
                                 pkg, paths), domain = NA)
        }
        out <- c(out, paths)
    }

    if(!quiet && (length(bad) > 0)) {
        if(length(out) == 0)
            stop("none of the packages were found")
        for(pkg in bad)
            warning(gettextf("there is no package called '%s'", pkg),
                    domain = NA)
    }

    out
}

print.packageInfo <- function(x, ...)
{
    if(!inherits(x, "packageInfo")) stop("wrong class")
    outFile <- tempfile("RpackageInfo")
    outConn <- file(outFile, open = "w")
    vignetteMsg <-
        gettextf("Further information is available in the following vignettes in directory '%s':",
                 file.path(x$path, "doc"))
    headers <- c(gettext("Description:\n\n"),
                 gettext("Index:\n\n"),
                 paste(paste(strwrap(vignetteMsg), collapse = "\n"),
                       "\n\n", sep = ""))
    footers <- c("\n", "\n", "")
    formatDocEntry <- function(entry) {
        if(is.list(entry) || is.matrix(entry))
            formatDL(entry, style = "list")
        else
            entry
    }
    writeLines(gettextf("\n\t\tInformation on package '%s'\n", x$name),
               outConn)
    for(i in which(!sapply(x$info, is.null))) {
        writeLines(headers[i], outConn, sep = "")
        writeLines(formatDocEntry(x$info[[i]]), outConn)
        writeLines(footers[i], outConn, sep = "")
    }
    close(outConn)
    file.show(outFile, delete.file = TRUE,
              title = gettextf("Documentation for package '%s'", x$name))
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
                        stop(gettextf("package '%s' required by '%s' could not be found",
                                      pkg, pkgname),
                             call. = FALSE, domain = NA)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    if (!eval(parse(text=paste("current", z$op, "z$version"))))
                        stop(gettextf("package '%s' %s was found, but %s %s is required by '%s'",
                             pkg, current, z$op, z$version, pkgname),
                             call. = FALSE, domain = NA)
                }

                if (!quietly)
                    cat(gettextf("Loading required package: %s\n", pkg))
                library(pkg, character.only = TRUE, logical = TRUE,
                        lib.loc = lib.loc) ||
                stop(gettextf("package '%s' could not be loaded", pkg),
                     call. = FALSE, domain = NA)
            } else {
                ## check the required version number, if any
                if (length(z) > 1) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    if (!eval(parse(text=paste("current", z$op, "z$version"))))
                        stop(gettextf("package '%s' %s is loaded, but %s %s is required by '%s'",
                             pkg, current, z$op, z$version, pkgname),
                             call. = FALSE, domain = NA)
                }
            }
        }
    }
    if(useImports) {
        nss <- names(pkgInfo$Imports)
        for(ns in nss) loadNamespace(ns, lib.loc)
    }
}
