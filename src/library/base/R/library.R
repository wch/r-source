#  File src/library/base/R/library.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

testPlatformEquivalence <- function(built, run)
{
    ## args are "cpu-vendor-os", but os might be 'linux-gnu'!
    ## remove vendor field
    built <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", built)
    run <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", run)
    ## Mac OS X supports multiple CPUs by using 'universal' binaries
    if (length(grep("^universal-darwin", built)) && nzchar(.Platform$r_arch))
        built <- sub("^universal", R.version$arch, built)
    ## allow for small mismatches, e.g. OS version number and i686 vs i586.
    length(agrep(built, run)) > 0
}

library <-
function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
	 quietly = FALSE, keep.source = getOption("keep.source.pkgs"),
         verbose = getOption("verbose"))
{
    paste0 <- function(...) paste(..., sep="")
    testRversion <- function(pkgInfo, pkgname, pkgpath)
    {
        if(is.null(built <- pkgInfo$Built))
            stop(gettextf("package '%s' has not been installed properly\n", pkgname),
                 call. = FALSE, domain = NA)

        ## which version was this package built under?
        ## must be >= 2.10.0 (new help system)
        R_version_built_under <- as.numeric_version(built$R)
        if(R_version_built_under < "2.10.0")
            stop(gettextf("package '%s' was built before R 2.10.0: please re-install it",
                          pkgname), call. = FALSE, domain = NA)

        current <- getRversion()
        ## depends on R version?
        ## as it was installed >= 2.7.0 it will have Rdepends2
        if(length(Rdeps <- pkgInfo$Rdepends2)) {
            for(dep in Rdeps)
                if(length(dep) > 1L) {
                    target <- as.numeric_version(dep$version)
                    res <- eval(parse(text=paste("current", dep$op, "target")))
                    if(!res)
                        stop(gettextf("This is R %s, package '%s' needs %s %s",
                                      current, pkgname, dep$op, target),
                             call. = FALSE, domain = NA)
                }
        }
        ## warn if installed under a later version of R
        if(R_version_built_under > current)
            warning(gettextf("package '%s' was built under R version %s",
                             pkgname, as.character(built$R)),
                    call. = FALSE, domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if(.Platform$OS.type == "unix") {
            ## allow mismatches if r_arch is in use, e.g.
            ## i386-gnu-linux vs x86-gnu-linux depending on
            ## build system.
            if(!nzchar(r_arch) && length(grep("\\w", platform)) &&
               !testPlatformEquivalence(platform, R.version$platform))
                stop(gettextf("package '%s' was built for %s",
                              pkgname, platform),
                     call. = FALSE, domain = NA)
        } else {  # Windows
            ## a check for 'mingw' suffices, since i386 and x86_64
            ## have DLLs in different places.  This allows binary packages
            ## to be merged.
            if(nzchar(platform) && !grepl("mingw", platform))
                stop(gettextf("package '%s' was built for %s",
                              pkgname, platform),
                     call. = FALSE, domain = NA)
        }
        ## if using r_arch subdirs, check for presence
        if(nzchar(r_arch)
           && file.exists(file.path(pkgpath, "libs"))
           && !file.exists(file.path(pkgpath, "libs", r_arch)))
            stop(gettextf("package '%s' is not installed for 'arch=%s'",
                          pkgname, r_arch),
                 call. = FALSE, domain = NA)
    }

    checkLicense <- function(pkg, pkgInfo, pkgPath)
    {
        L <- tools:::analyze_license(pkgInfo$DESCRIPTION["License"])
        if(!L$is_empty && !L$is_verified) {
            site_file <- path.expand(file.path(R.home("etc"), "licensed.site"))
            if(file.exists(site_file) &&
               pkg %in% readLines(site_file)) return()
            personal_file <- path.expand("~/.R/licensed")
            if(file.exists(personal_file)) {
                agreed <- readLines(personal_file)
                if(pkg %in% agreed) return()
            } else agreed <- character()
            if(!interactive())
                stop(gettextf("package '%s' has a license that you need to accept in an interactive session", pkg), domain = NA)
            lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
            lfiles <- lfiles[file.exists(lfiles)]
            if(length(lfiles)) {
                message(gettextf("Package '%s' has a license that you need to accept after viewing", pkg), domain = NA)
                readline("press RETURN to view license")
                encoding <- pkgInfo$DESCRIPTION["Encoding"]
                if(is.na(encoding)) encoding <- ""
                ## difR and EVER have a Windows' 'smart quote' LICEN[CS]E file
                if(encoding == "latin1") encoding <- "cp1252"
                file.show(lfiles[1L], encoding = encoding)
            } else {
                message(gettextf("Package '%s' has a license that you need to accept:\naccording to the DESCRIPTION file it is", pkg), domain = NA)
                message(pkgInfo$DESCRIPTION["License"])
            }
            choice <- menu(c("accept", "decline"),
                           title = paste("License for", sQuote(pkg)))
            if(choice != 1)
                stop(gettextf("License for package '%s' not accepted", package),
                     domain = NA, call. = FALSE)
            dir.create(dirname(personal_file), showWarnings=FALSE)
            writeLines(c(agreed, pkg), personal_file)
        }
    }

    checkNoGenerics <- function(env, pkg)
    {
        nenv <- env
        ns <- .Internal(getRegisteredNamespace(as.name(pkg)))
        if(!is.null(ns)) nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            ## A package will have created a generic
            ## only if it has created a formal method.
            length(objects(env, pattern="^\\.__[MT]", all.names=TRUE)) == 0L
        }
    }

    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".First.lib", ".Last.lib",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics", ".Depends")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ## ignore generics not defined for the package
        ob <- objects(lib.pos, all.names = TRUE)
        if(!nogenerics) {
            ##  Exclude generics that are consistent with implicit generic
            ## from another pacakge.  A better test would be to move this
            ## down into the loop and test against specific other package name
            ## but subtle conflicts like that are likely to be found elsewhere
            these <- objects(lib.pos, all.names = TRUE)
            these <- these[substr(these, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
	ipos <- seq_along(sp)[-c(lib.pos,
				 match(c("Autoloads", "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob, nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if(length(Classobjs)) same <- same[-Classobjs]
                ## report only objects which are both functions or
                ## both non-functions.
		same.isFn <- function(where)
		    sapply(same, exists,
                           where = where, mode = "function", inherits = FALSE)
		same <- same[same.isFn(i) == same.isFn(lib.pos)]
                ## if a package imports, and re-exports, there's no problem
		if(length(same))
		    same <- same[sapply(same, function(.)
					!identical(get(., i),
						   get(., lib.pos)))]
                if(length(same)) {
                    if (fst) {
                        fst <- FALSE
                        packageStartupMessage(gettextf("\nAttaching package: '%s'\n",
                                                       package),
                                              domain = NA)
                    }

                    objs <- strwrap(paste(same, collapse=", "), indent=4,
                                    exdent=4)
                    msg <- sprintf("The following object(s) are masked %s '%s':\n\n%s\n",
                                   if (i < lib.pos) "_by_" else "from",
                                   sp[i], paste(objs, collapse="\n"))
		    packageStartupMessage(msg)
                }
            }
        }
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

    if(verbose && quietly)
	message("'verbose' and 'quietly' are both true; being verbose then ..")
    if(!missing(package)) {
        if (is.null(lib.loc)) lib.loc <- .libPaths()
        ## remove any non-existent directories
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]

	if(!character.only)
	    package <- as.character(substitute(package))
        if(length(package) != 1L)
            stop("'package' must be of length 1")
        if(is.na(package) || (package == ""))
            stop("invalid package name")

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
            if(length(pkgpath) == 0L) {
                txt <- if(length(lib.loc))
                    gettextf("there is no package called '%s'", package)
                else
                    gettext("no library trees found in 'lib.loc'")
                if(logical.return) {
                    warning(txt, domain = NA)
		    return(FALSE)
		} else stop(txt, domain = NA)
            }
            abs_path <- function(x) {cwd <- setwd(x);on.exit(setwd(cwd));getwd()}
            which.lib.loc <- abs_path(dirname(pkgpath))
            pfile <- system.file("Meta", "package.rds", package = package,
                                 lib.loc = which.lib.loc)
            if(!nzchar(pfile))
            	stop(gettextf("'%s' is not a valid installed package",
                              package), domain = NA)
            pkgInfo <- .readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            ## avoid any bootstrapping issues by these exemptions
            if(!package %in% c("datasets", "grDevices", "graphics", "methods",
                               "splines", "stats", "stats4", "tcltk", "tools",
                               "utils") &&
               isTRUE(getOption("checkPackageLicense", FALSE)))
                checkLicense(package, pkgInfo, pkgpath)

            ## The check for inconsistent naming is now in .find.package

            if(is.character(pos)) {
                npos <- match(pos, search())
                if(is.na(npos)) {
                    warning(gettextf("'%s' not found on search path, using pos = 2", pos), domain = NA)
                    pos <- 2
                } else pos <- npos
            }
            .getRequiredPackages2(pkgInfo, quietly = quietly)
            deps <- unique(names(pkgInfo$Depends))
            ## If the name space mechanism is available and the package
            ## has a name space, then the name space loading mechanism
            ## takes over.
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                    ns <- loadNamespace(package, c(which.lib.loc, lib.loc),
                                        keep.source = keep.source)
                    dataPath <- file.path(which.lib.loc, package, "data")
                    env <- attachNamespace(ns, pos = pos,
                                           dataPath = dataPath, deps)
                })
                if (inherits(tt, "try-error"))
                    if (logical.return)
                        return(FALSE)
                    else stop(gettextf("package/namespace load failed for '%s'",
                                       package),
                              call. = FALSE, domain = NA)
                else {
                    on.exit(detach(pos=pos))
                    ## If there are S4 generics then the package should
                    ## depend on methods
                    nogenerics <-
                        !.isMethodsDispatchOn() || checkNoGenerics(env, package)
                    if(warn.conflicts &&
                       !exists(".conflicts.OK", envir = env, inherits = FALSE))
                        checkConflicts(package, pkgname, pkgpath,
                                       nogenerics, ns)
                    runUserHook(package, pkgpath)
                    on.exit()
                    if (logical.return)
                        return(TRUE)
                    else
                        return(invisible(.packages()))
                }
            }

            ## non-namespace branch
            codeFile <- file.path(which.lib.loc, package, "R", package)
            ## create environment (not attached yet)
            loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
            ## save the package name in the environment
            assign(".packageName", package, envir = loadenv)
            if(length(deps)) assign(".Depends", deps, envir = loadenv)
            ## source file into loadenv
            if(file.exists(codeFile)) {
                res <- try(sys.source(codeFile, loadenv,
                                      keep.source = keep.source))
                if(inherits(res, "try-error"))
                    stop(gettextf("unable to load R code in package '%s'",
                                  package),
                         call. = FALSE, domain = NA)
            } else if(verbose)
                warning(gettextf("package '%s' contains no R code",
                                 package), domain = NA)
            ## lazy-load data sets if required
            dbbase <- file.path(which.lib.loc, package, "data", "Rdata")
            if(file.exists(paste0(dbbase, ".rdb")))
                lazyLoad(dbbase, loadenv)
            ## lazy-load a sysdata database if present
            dbbase <- file.path(which.lib.loc, package, "R", "sysdata")
            if(file.exists(paste0(dbbase, ".rdb")))
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
            bindTranslations(package, pkgpath)

            ## run .First.lib
            if(exists(".First.lib", mode = "function",
                      envir = env, inherits = FALSE)) {
                firstlib <- get(".First.lib", mode = "function",
                                envir = env, inherits = FALSE)
                tt <- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(gettextf(".First.lib failed for '%s'",
                                       package), domain = NA)
            }
            if(!is.null(firstlib <- getOption(".First.lib")[[package]])) {
                tt <- try(firstlib(which.lib.loc, package))
                if(inherits(tt, "try-error"))
                    if (logical.return) return(FALSE)
                    else stop(gettextf(".First.lib failed for '%s'",
                                       package), domain = NA)
            }
            ## If there are generics then the package should
            ## depend on methods and so have turned methods dispatch on.
            nogenerics <-
                !.isMethodsDispatchOn() || checkNoGenerics(env, package)
            if(warn.conflicts &&
               !exists(".conflicts.OK", envir = env, inherits = FALSE))
                checkConflicts(package, pkgname, pkgpath, nogenerics, env)

            if(!nogenerics)
                methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
            runUserHook(package, pkgpath)
            on.exit()
	}
	if (verbose && !newpackage)
            warning(gettextf("package '%s' already present in search()",
                             package), domain = NA)

    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        pkgName <- help[1L]            # only give help on one package
        pkgPath <- .find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
                      file.path(pkgPath, "INDEX"))
        if(file.exists(vignetteIndexRDS <-
                       file.path(pkgPath, "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector(length = 3L, mode = "list")
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
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 3)
            } else if(basename(f) %in% "vignette.rds") {
                txt <- .readRDS(f)
                ## New-style vignette indices are data frames with more
                ## info than just the base name of the PDF file and the
                ## title.  For such an index, we give the names of the
                ## vignettes, their titles, and indicate whether PDFs
                ## are available.
                ## The index might have zero rows.
                if(is.data.frame(txt) && nrow(txt))
                    cbind(basename(gsub("\\.[[:alpha:]]+$", "",
                                        txt$File)),
                          paste(txt$Title,
                                paste0(rep.int("(source", NROW(txt)),
                                       ifelse(txt$PDF != "",
                                              ", pdf",
                                              ""),
                                       ")")))
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
        db <- matrix(character(0L), nrow = 0L, ncol = 3L)
        nopkgs <- character(0L)

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
                    title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if(length(a) == 0L)
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if(length(nopkgs) && !missing(lib.loc)) {
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
    ## Split according to LibPath, preserving order of libraries.
    libs <- db[, "LibPath"]
    libs <- factor(libs, levels=unique(libs))
    out <- if(nrow(db) == 0)
        NULL
    else lapply(split(1 : nrow(db), libs),
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
    abs_path <- function(x) {cwd <- setwd(x);on.exit(setwd(cwd));getwd()}
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname, "c")) == 0)
        return(dll_list)

    ## Be defensive about possible system-specific extension for shared
    ## objects, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext, "c")
    if(substr(chname, nc_chname - nc_file_ext + 1L, nc_chname) == file.ext)
        chname <- substr(chname, 1L, nc_chname - nc_file_ext)

    r_arch <- .Platform$r_arch
    for(pkg in .find.package(package, lib.loc, verbose = verbose)) {
        DLLpath <- if(nzchar(r_arch)) file.path(pkg, "libs", r_arch)
	else    file.path(pkg, "libs")
        file <- file.path(DLLpath, paste(chname, file.ext, sep = ""))
        if(file.exists(file)) break else file <- ""
    }
    if(file == "")
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL '%s' not found: maybe not installed for this architecture?", chname), domain = NA)
        else
            stop(gettextf("shared object '%s' not found", chname), domain = NA)
    ## for consistency with library.dyn.unload:
    file <- file.path(abs_path(DLLpath), paste(chname, file.ext, sep = ""))
    ind <- sapply(dll_list, function(x) x[["path"]] == file)
    if(length(ind) && any(ind)) {
        if(verbose)
            if(.Platform$OS.type == "windows")
                message(gettextf("DLL '%s' already loaded", chname),
                        domain = NA)
            else
                message(gettextf("shared object '%s' already loaded", chname),
                        domain = NA)
        return(invisible(dll_list[[ seq_along(dll_list)[ind] ]]))
    }
    if(.Platform$OS.type == "windows") {
        ## Make it possible to find other DLLs in the same place as
        ## @code{file}, so that e.g. binary packages can conveniently
        ## provide possibly missing DLL dependencies in this place
        ## (without having to bypass the default package dynload
        ## mechanism).  Note that this only works under Windows, and a
        ## more general solution will have to be found eventually.
        ##
        ## 2.7.0: there's a more general mechanism in DLLpath=,
        ## so not clear if this is still needed.
        PATH <- Sys.getenv("PATH")
        Sys.setenv(PATH = paste(gsub("/", "\\\\", DLLpath), PATH, sep=";"))
        on.exit(Sys.setenv(PATH = PATH))
    }
    if(verbose)
        message(gettextf("now dyn.load(\"%s\") ...", file), domain = NA)
    dll <- if("DLLpath" %in% names(list(...))) dyn.load(file, ...)
    else dyn.load(file, DLLpath = DLLpath, ...)
    .dynLibs(c(dll_list, list(dll)))
    invisible(dll)
}

library.dynam.unload <-
function(chname, libpath, verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext)
{
    abs_path <- function(x) {cwd <- setwd(x);on.exit(setwd(cwd));getwd()}
    dll_list <- .dynLibs()

    if(missing(chname) || (nc_chname <- nchar(chname, "c")) == 0)
        if(.Platform$OS.type == "windows")
            stop("no DLL was specified")
        else
            stop("no shared object was specified")

    ## Be defensive about possible system-specific extension for shared
    ## objects, although the docs clearly say they should not be
    ## added.
    nc_file_ext <- nchar(file.ext, "c")
    if(substr(chname, nc_chname - nc_file_ext + 1L, nc_chname)
       == file.ext)
        chname <- substr(chname, 1L, nc_chname - nc_file_ext)

    ## We need an absolute path here.
    libpath <- abs_path(libpath)
    file <- if(nzchar(.Platform$r_arch))
             file.path(libpath, "libs", .Platform$r_arch,
                       paste(chname, file.ext, sep = ""))
     else    file.path(libpath, "libs",
                       paste(chname, file.ext, sep = ""))

    pos <- which(sapply(dll_list, function(x) x[["path"]] == file))
    if(!length(pos))
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL '%s' was not loaded", chname), domain = NA)
        else
            stop(gettextf("shared object '%s' was not loaded", chname),
                 domain = NA)

    if(!file.exists(file))
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL '%s' not found", chname), domain = NA)
        else
            stop(gettextf("shared object '%s' not found", chname), domain = NA)
    if(verbose)
        message(gettextf("now dyn.unload(\"%s\") ...", file), domain = NA)
    dyn.unload(file)
    .dynLibs(dll_list[-pos])
    invisible(dll_list[[pos]])
}

require <-
function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
         keep.source = getOption("keep.source.pkgs"),
         character.only = FALSE, save = FALSE)
{
    if( !character.only )
        package <- as.character(substitute(package)) # allowing "require(eda)"
    loaded <- paste("package", package, sep = ":") %in% search()

    if (!loaded) {
	if (!quietly)
            packageStartupMessage(gettextf("Loading required package: %s",
                                           package), domain = NA)
	value <- tryCatch(library(package, lib.loc = lib.loc,
                                  character.only = TRUE,
                                  logical.return = TRUE,
                                  warn.conflicts = warn.conflicts,
				  quietly = quietly,
                                  keep.source = keep.source),
                          error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ",
                    sQuote(msg), "\n", file = stderr(), sep = "")
                .Internal(printDeferredWarnings())
            }
            return(invisible(FALSE))
        }
        if (!value) return(invisible(FALSE))
    } else value <- TRUE

    if(identical(save, FALSE)) {}
    else {
        warning("use of 'save' is deprecated")
        ## update the ".Depends" variable
        ## We no longer use '.required' since some packages set that.
        if(identical(save, TRUE)) {
            save <- topenv(parent.frame())
            ## (a package namespace, topLevelEnvironment option or
            ## .GlobalEnv)
            if(identical(save, .GlobalEnv)) {
                ## try to detect call from .First.lib in a package
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
            }
        }
        else
            save <- as.environment(save)
        ## detach() only uses .Depends from a package environment.
        ## so only save it there
        if(!is.null(nm <- attr(save, "name")) && grepl("^package:", nm)) {
            hasDotDepends <- exists(".Depends", save, inherits=FALSE)
            packages <- if(hasDotDepends) unique(c(package, get(".Depends", save))) else package
            assign(".Depends", packages, save)
        }
    }
    invisible(value)
}

.packages <- function(all.available = FALSE, lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(all.available) {
	ans <- character(0L)
        for(lib in lib.loc[file.exists(lib.loc)]) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            pfile <- file.path(lib, a, "Meta", "package.rds")
            ans <- c(ans, a[file.exists(pfile)])
        }
        return(unique(ans))
    } ## else
    s <- search()
    return(invisible(substring(s[substr(s, 1L, 8L) == "package:"], 9)))
}

.path.package <- function(package = NULL, quiet = FALSE)
{
    if(is.null(package)) package <- .packages()
    if(length(package) == 0L) return(character(0L))
    s <- search()
    searchpaths <-
        lapply(seq_along(s), function(i) attr(as.environment(i), "path"))
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

## As from 2.9.0 ignore versioned installs
.find.package <-
function(package = NULL, lib.loc = NULL, quiet = FALSE,
         verbose = getOption("verbose"))
{
    if(is.null(package) && is.null(lib.loc) && !verbose) {
        ## We only want the paths to the attached packages.
        return(.path.package())
    }

    ## don't waste time looking for the standard packages:
    ## we know where they are and this can take a significant
    ## time with 1000+ packages installed.
    if(length(package) == 1L  &&
       package %in% c("base", "tools", "utils", "grDevices", "graphics",
                      "stats", "datasets", "methods", "grid", "splines",
                      "stats4", "tcltk"))
        return(file.path(.Library, package))

    use_attached <- FALSE
    if(is.null(package)) package <- .packages()
    if(is.null(lib.loc)) {
        use_attached <- TRUE
        lib.loc <- .libPaths()
    }

    if(!length(package)) return(character())

    bad <- character(0L)
    out <- character(0L)

    for(pkg in package) {
        paths <- character()
        for(lib in lib.loc) {
            dirs <- list.files(lib,
                               pattern = paste("^", pkg, "$", sep = ""),
                               full.names = TRUE)
            ## Note that we cannot use tools::file_test() here, as
            ## cyclic name space dependencies are not supported.  Argh.
            paths <- c(paths,
                       dirs[file.info(dirs)$isdir &
                            file.exists(file.path(dirs,
                                                  "DESCRIPTION"))])
        }
        if(use_attached
           && length(pos <- grep(paste("^package:", pkg, "$", sep = ""),
                                 search()))) {
            dirs <- sapply(pos, function(i) {
                if(identical(env <- as.environment(i), baseenv()))
                    system.file()
                else
                    attr(env, "path")
            })
            ## possibly NULL if no path attribute.
            dirs <- dirs[!sapply(dirs, is.null)]
            paths <- c(as.character(dirs), paths)
        }
        if(length(paths)) {
            paths <- unique(paths)
            valid_package_version_regexp <-
                .standard_regexps()$valid_package_version
            db <- lapply(paths, function(p) {
                ## Note that this is sometimes used for source
                ## packages, e.g. by promptPackage from package.skeleton
                pfile <- file.path(p, "Meta", "package.rds")
                info <- if(file.exists(pfile))
                    ## this must have these fields to get installed
                    .readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else {
                    info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"),
                                              c("Package", "Version"))[1, ],
                                     error = identity)
                    if(inherits(info, "error")
                       || (length(info) != 2L)
                       || any(is.na(info)))
                        c(Package = NA, Version = NA) # need dimnames below
                    else
                        info
                }
            })
            db <- do.call("rbind", db)
            ok <- (apply(!is.na(db), 1L, all)
                   & (db[, "Package"] == pkg)
                   & (grepl(valid_package_version_regexp, db[, "Version"])))
            paths <- paths[ok]
        }
        if(length(paths) == 0L) {
            bad <- c(bad, pkg)
            next
        }
        if(length(paths) > 1L) {
            ## If a package was found more than once ...
            paths <- paths[1L]
            if(verbose)
                warning(gettextf("package '%s' found more than once,\nusing the one found in '%s'",
                                 pkg, paths), domain = NA)
        }
        out <- c(out, paths)
    }

    if(!quiet && length(bad)) {
        if(length(out) == 0L) {
            if(length(bad) == 1L) {
                stop(gettextf("there is no package called '%s'", pkg),
                     domain = NA)
            } else {
                stop(ngettext(length(bad),
                              "there is no package called",
                              "there are no packages called"), " ",
                     paste(shQuote(bad), collapse = ", "), domain = NA)

            }
        }
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

.getRequiredPackages <-
    function(file="DESCRIPTION", lib.loc = NULL, quietly = FALSE, useImports = FALSE)
{
    ## OK to call tools as only used during installation.
    pkgInfo <- tools:::.split_description(tools:::.read_description(file))
    .getRequiredPackages2(pkgInfo, quietly, lib.loc, useImports)
    invisible()
}

.getRequiredPackages2 <-
function(pkgInfo, quietly = FALSE, lib.loc = NULL, useImports = FALSE)
{
    pkgs <- unique(names(pkgInfo$Depends))
    if (length(pkgs)) {
        pkgname <- pkgInfo$DESCRIPTION["Package"]
        for(pkg in pkgs) {
            ## allow for multiple occurrences
            zs <- pkgInfo$Depends[names(pkgInfo$Depends) == pkg]
            have_vers <- any(sapply(zs, length) > 1L)
            if ( !paste("package", pkg, sep = ":") %in% search() ) {
                if (have_vers) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    if(nzchar(pfile) == 0)
                        stop(gettextf("package '%s' required by '%s' could not be found",
                                      pkg, pkgname),
                             call. = FALSE, domain = NA)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    for(z in zs)
                        if(length(z) > 1L) {
                            target <- as.numeric_version(z$version)
                            if (!eval(parse(text=paste("current", z$op, "target"))))
                                stop(gettextf("package '%s' %s was found, but %s %s is required by '%s'",
                                              pkg, current, z$op, target, pkgname),
                                     call. = FALSE, domain = NA)
                        }
                }

                if (!quietly)
                    packageStartupMessage(gettextf("Loading required package: %s",
                                     pkg), domain = NA)
                library(pkg, character.only = TRUE, logical.return = TRUE,
                        lib.loc = lib.loc) ||
                stop(gettextf("package '%s' could not be loaded", pkg),
                     call. = FALSE, domain = NA)
            } else {
                ## check the required version number, if any
                if (have_vers) {
                    pfile <- system.file("Meta", "package.rds",
                                         package = pkg, lib.loc = lib.loc)
                    current <- .readRDS(pfile)$DESCRIPTION["Version"]
                    for(z in zs)
                        if (length(z) > 1L) {
                            target <- as.numeric_version(z$version)
                            if (!eval(parse(text=paste("current", z$op, "target"))))
                                stop(gettextf("package '%s' %s is loaded, but %s %s is required by '%s'",
                                              pkg, current, z$op, target, pkgname),
                                     call. = FALSE, domain = NA)
                        }
                }
            }
        }
    }
    if(useImports) {
        nss <- names(pkgInfo$Imports)
        for(ns in nss) loadNamespace(ns, lib.loc)
    }
}

.expand_R_libs_env_var <-
function(x)
{
    v <- paste(R.version[c("major", "minor")], collapse = ".")

    expand <- function(x, spec, expansion)
        gsub(paste("(^|[^%])(%%)*%", spec, sep = ""),
             sprintf("\\1\\2%s", expansion), x)

    ## %V => version x.y.z
    x <- expand(x, "V", v)
    ## %v => version x.y
    x <- expand(x, "v", sub("\\.[^.]*$", "", v))
    ## %p => platform
    x <- expand(x, "p", R.version$platform)
    ## %a => arch
    x <- expand(x, "a", R.version$arch)
    ## %o => os
    x <- expand(x, "o", R.version$os)

    gsub("%%", "%", x)
}
