#  File src/library/base/R/library.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

testPlatformEquivalence <-
function(built, run)
{
    ## args are "cpu-vendor-os", but os might be 'linux-gnu'!
    ## remove vendor field
    built <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", built)
    run <- gsub("([^-]*)-([^-]*)-(.*)", "\\1-\\3", run)
    ## Mac OS X supports multiple CPUs by using 'universal' binaries
    if (grepl("^universal-darwin", built) && nzchar(.Platform$r_arch))
        built <- sub("^universal", R.version$arch, built)
    ## allow for small mismatches, e.g. OS version number and i686 vs i586.
    length(agrep(built, run)) > 0
}

library <-
function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
         logical.return = FALSE, warn.conflicts = TRUE,
	 quietly = FALSE, verbose = getOption("verbose"))
{
    testRversion <- function(pkgInfo, pkgname, pkgpath)
    {
        if(is.null(built <- pkgInfo$Built))
            stop(gettextf("package %s has not been installed properly\n",
                          sQuote(pkgname)),
                 call. = FALSE, domain = NA)

        ## which version was this package built under?
        R_version_built_under <- as.numeric_version(built$R)
        if(R_version_built_under < "3.0.0")
            stop(gettextf("package %s was built before R 3.0.0: please re-install it",
                          sQuote(pkgname)), call. = FALSE, domain = NA)

        current <- getRversion()
        ## depends on R version?
        ## as it was installed >= 2.7.0 it will have Rdepends2
        if(length(Rdeps <- pkgInfo$Rdepends2)) {
            for(dep in Rdeps)
                if(length(dep) > 1L) {
                    target <- dep$version
                    res <- if(is.character(target)) {
                        do.call(dep$op, # these are both strings
                                list(as.numeric(R.version[["svn rev"]]),
                                     as.numeric(sub("^r", "", dep$version))))
                    } else {
                        do.call(dep$op,
                                list(current, as.numeric_version(target)))
##                        target <- as.numeric_version(dep$version)
##                        eval(parse(text=paste("current", dep$op, "target")))
                    }
                    if(!res)
                        stop(gettextf("This is R %s, package %s needs %s %s",
                                      current, sQuote(pkgname), dep$op, target),
                             call. = FALSE, domain = NA)
                }
        }
        ## warn if installed under a later version of R
        if(R_version_built_under > current)
            warning(gettextf("package %s was built under R version %s",
                             sQuote(pkgname), as.character(built$R)),
                    call. = FALSE, domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if(.Platform$OS.type == "unix") {
            ## allow mismatches if r_arch is in use, e.g.
            ## i386-gnu-linux vs x86-gnu-linux depending on
            ## build system.
            if(!nzchar(r_arch) && length(grep("\\w", platform)) &&
               !testPlatformEquivalence(platform, R.version$platform))
                stop(gettextf("package %s was built for %s",
                              sQuote(pkgname), platform),
                     call. = FALSE, domain = NA)
        } else {  # Windows
            ## a check for 'mingw' suffices, since i386 and x86_64
            ## have DLLs in different places.  This allows binary packages
            ## to be merged.
            if(nzchar(platform) && !grepl("mingw", platform))
                stop(gettextf("package %s was built for %s",
                              sQuote(pkgname), platform),
                     call. = FALSE, domain = NA)
        }
        ## if using r_arch subdirs, check for presence
        if(nzchar(r_arch)
           && file.exists(file.path(pkgpath, "libs"))
           && !file.exists(file.path(pkgpath, "libs", r_arch)))
            stop(gettextf("package %s is not installed for 'arch = %s'",
                          sQuote(pkgname), r_arch),
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
                stop(gettextf("package %s has a license that you need to accept in an interactive session", sQuote(pkg)), domain = NA)
            lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
            lfiles <- lfiles[file.exists(lfiles)]
            if(length(lfiles)) {
                message(gettextf("package %s has a license that you need to accept after viewing", sQuote(pkg)), domain = NA)
                readline("press RETURN to view license")
                encoding <- pkgInfo$DESCRIPTION["Encoding"]
                if(is.na(encoding)) encoding <- ""
                ## difR and EVER have a Windows' 'smart quote' LICEN[CS]E file
                if(encoding == "latin1") encoding <- "cp1252"
                file.show(lfiles[1L], encoding = encoding)
            } else {
                message(gettextf("package %s has a license that you need to accept:\naccording to the DESCRIPTION file it is", sQuote(pkg)), domain = NA)
                message(pkgInfo$DESCRIPTION["License"], domain = NA)
            }
            choice <- menu(c("accept", "decline"),
                           title = paste("License for", sQuote(pkg)))
            if(choice != 1)
                stop(gettextf("license for package %s not accepted",
                              sQuote(package)), domain = NA, call. = FALSE)
            dir.create(dirname(personal_file), showWarnings=FALSE)
            writeLines(c(agreed, pkg), personal_file)
        }
    }

    checkNoGenerics <- function(env, pkg)
    {
        nenv <- env
        ns <- .getNamespace(as.name(pkg))
        if(!is.null(ns)) nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            ## A package will have created a generic
            ## only if it has created a formal method.
            length(objects(env, pattern="^\\.__T", all.names=TRUE)) == 0L
        }
    }

    ## FIXME: ./attach.R 's attach() has *very* similar checkConflicts(), keep in sync
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, env)
    {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
                       ".Random.seed", ".Last.lib", ".onDetach",
                       ".packageName", ".noGenerics", ".required",
                       ".no_S3_generics", ".Depends", ".requireCachedGenerics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ## ignore generics not defined for the package
        ob <- objects(lib.pos, all.names = TRUE)
        if(!nogenerics) {
            ##  Exclude generics that are consistent with implicit generic
            ## from another package.  A better test would be to move this
            ## down into the loop and test against specific other package name
            ## but subtle conflicts like that are likely to be found elsewhere
	    these <- ob[substr(ob, 1L, 6L) == ".__T__"]
            gen  <- gsub(".__T__(.*):([^:]+)", "\\1", these)
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
		    vapply(same, exists, NA,
                           where = where, mode = "function", inherits = FALSE)
		same <- same[same.isFn(i) == same.isFn(lib.pos)]
		## if a package imports and re-exports, there's no problem
		not.Ident <- function(ch, TRAFO=identity, ...)
		    vapply(ch, function(.)
                           !identical(TRAFO(get(., i)),
                                      TRAFO(get(., lib.pos)), ...),
                           NA)
		if(length(same)) same <- same[not.Ident(same)]
		## if the package is 'base' it cannot be imported and re-exported,
		## allow a "copy":
		if(length(same) && identical(sp[i], "package:base"))
		    same <- same[not.Ident(same, ignore.environment = TRUE)]
                if(length(same)) {
                    if (fst) {
                        fst <- FALSE
                        packageStartupMessage(gettextf("\nAttaching package: %s\n",
                                                       sQuote(package)),
                                              domain = NA)
                    }
		    msg <- .maskedMsg(same, pkg = sQuote(sp[i]), by = i < lib.pos)
		    packageStartupMessage(msg, domain = NA)
                }
            }
        }
    }

    if(verbose && quietly)
	message("'verbose' and 'quietly' are both true; being verbose then ..")
    if(!missing(package)) {
        if (is.null(lib.loc)) lib.loc <- .libPaths()
        ## remove any non-existent directories
        lib.loc <- lib.loc[dir.exists(lib.loc)]

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
            ## The methods package caches all other pkgs when it is
            ## attached.

            pkgpath <- find.package(package, lib.loc, quiet = TRUE,
                                    verbose = verbose)
            if(length(pkgpath) == 0L) {
                txt <- if(length(lib.loc))
                    gettextf("there is no package called %s", sQuote(package))
                else
                    gettext("no library trees found in 'lib.loc'")
                if(logical.return) {
                    warning(txt, domain = NA)
		    return(FALSE)
		} else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath), "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package,
                                 lib.loc = which.lib.loc)
            if(!nzchar(pfile))
            	stop(gettextf("%s is not a valid installed package",
                              sQuote(package)), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            ## avoid any bootstrapping issues by these exemptions
            if(!package %in% c("datasets", "grDevices", "graphics", "methods",
                               "splines", "stats", "stats4", "tcltk", "tools",
                               "utils") &&
               isTRUE(getOption("checkPackageLicense", FALSE)))
                checkLicense(package, pkgInfo, pkgpath)

            ## The check for inconsistent naming is now in find.package

            if(is.character(pos)) {
                npos <- match(pos, search())
                if(is.na(npos)) {
                    warning(gettextf("%s not found on search path, using pos = 2", sQuote(pos)), domain = NA)
                    pos <- 2
                } else pos <- npos
            }
            .getRequiredPackages2(pkgInfo, quietly = quietly)
            deps <- unique(names(pkgInfo$Depends))

            ## If the namespace mechanism is available and the package
            ## has a namespace, then the namespace loading mechanism
            ## takes over.
            if (packageHasNamespace(package, which.lib.loc)) {
		if (isNamespaceLoaded(package)) {
                    # Already loaded.  Does the version match?
                    newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
                    oldversion <- as.numeric_version(getNamespaceVersion(package))
                    if (newversion != oldversion) {
                    	# No, so try to unload the previous one
                    	res <- try(unloadNamespace(package))
                    	if (inherits(res, "try-error"))
                    	    stop(gettextf("Package %s version %s cannot be unloaded", sQuote(package), oldversion, domain = "R-base"))
                    }
                }
                tt <- try({
                    ns <- loadNamespace(package, c(which.lib.loc, lib.loc))
                    env <- attachNamespace(ns, pos = pos, deps)
                })
                if (inherits(tt, "try-error"))
                    if (logical.return)
                        return(FALSE)
                    else stop(gettextf("package or namespace load failed for %s",
                                       sQuote(package)),
                              call. = FALSE, domain = NA)
                else {
                    on.exit(detach(pos = pos))
                    ## If there are S4 generics then the package should
                    ## depend on methods
                    nogenerics <-
                        !.isMethodsDispatchOn() || checkNoGenerics(env, package)
                    if(warn.conflicts && # never will with a namespace
                       !exists(".conflicts.OK", envir = env, inherits = FALSE))
                        checkConflicts(package, pkgname, pkgpath,
                                       nogenerics, ns)
                    on.exit()
                    if (logical.return)
                        return(TRUE)
                    else
                        return(invisible(.packages()))
                }
            } else
            stop(gettextf("package %s does not have a namespace and should be re-installed",
                          sQuote(package)), domain = NA)
	}
	if (verbose && !newpackage)
            warning(gettextf("package %s already present in search()",
                             sQuote(package)), domain = NA)

    }
    else if(!missing(help)) {
	if(!character.only)
	    help <- as.character(substitute(help))
        pkgName <- help[1L]            # only give help on one package
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
                      file.path(pkgPath, "INDEX"))
        if(file.exists(vignetteIndexRDS <-
                       file.path(pkgPath, "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if(basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if("Encoding" %in% names(txt)) {
                    to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT"else ""
                    tmp <- try(iconv(txt, from=txt["Encoding"], to=to))
                    if(!inherits(tmp, "try-error"))
                        txt <- tmp
                    else
                        warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", call.=FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 3)
            } else if(basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
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
                                       ifelse(nzchar(txt$PDF),
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
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()

        for(lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for(i in sort(a)) {
                ## All packages installed under 2.0.0 should have
                ## 'package.rds' but we have not checked.
                file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
                title <- if(nzchar(file)) {
                    txt <- readRDS(file)
                    if(is.list(txt)) txt <- txt$DESCRIPTION
                    ## we may need to re-encode here.
                    if("Encoding" %in% names(txt)) {
                        to <- if(Sys.getlocale("LC_CTYPE") == "C") "ASCII//TRANSLIT" else ""
                        tmp <- try(iconv(txt, txt["Encoding"], to, "?"))
                        if(!inherits(tmp, "try-error"))
                            txt <- tmp
                        else
                            warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", call.=FALSE)
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

format.libraryIQR <-
function(x, ...)
{
    db <- x$results
    if(!nrow(db)) return(character())
    ## Split according to LibPath, preserving order of libraries.
    libs <- db[, "LibPath"]
    libs <- factor(libs, levels = unique(libs))
    out <- lapply(split(1 : nrow(db), libs),
                  function(ind) db[ind, c("Package", "Title"),
                                   drop = FALSE])
    c(unlist(Map(function(lib, sep) {
        c(gettextf("%sPackages in library %s:\n", sep, sQuote(lib)),
          formatDL(out[[lib]][, "Package"],
                   out[[lib]][, "Title"]))
    },
                 names(out),
                 c("", rep.int("\n", length(out) - 1L)))),
      x$footer)
}

print.libraryIQR <-
function(x, ...)
{
    s <- format(x)
    if(!length(s)) {
        message("no packages found")
    } else {
        outFile <- tempfile("RlibraryIQR")
        writeLines(s, outFile)
        file.show(outFile, delete.file = TRUE,
                  title = gettext("R packages available"))
    }
    invisible(x)
}

library.dynam <-
function(chname, package, lib.loc, verbose = getOption("verbose"),
         file.ext = .Platform$dynlib.ext, ...)
{
    dll_list <- .dynLibs()

    if(missing(chname) || !nzchar(chname)) return(dll_list)

    ## For better error messages, force these to be evaluated.
    package
    lib.loc

    r_arch <- .Platform$r_arch
    chname1 <- paste0(chname, file.ext)
    ## it is not clear we should allow this, rather require a single
    ## package and library.
    for(pkg in find.package(package, lib.loc, verbose = verbose)) {
        DLLpath <- if(nzchar(r_arch)) file.path(pkg, "libs", r_arch)
	else    file.path(pkg, "libs")
        file <- file.path(DLLpath, chname1)
        if(file.exists(file)) break else file <- ""
    }
    if(file == "")
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL %s not found: maybe not installed for this architecture?", sQuote(chname)), domain = NA)
        else
            stop(gettextf("shared object %s not found", sQuote(chname1)),
                 domain = NA)
    ## for consistency with library.dyn.unload:
    file <- file.path(normalizePath(DLLpath, "/", TRUE), chname1)
    ind <- vapply(dll_list, function(x) x[["path"]] == file, NA)
    if(length(ind) && any(ind)) {
        if(verbose)
            if(.Platform$OS.type == "windows")
                message(gettextf("DLL %s already loaded", sQuote(chname1)),
                        domain = NA)
            else
                message(gettextf("shared object '%s' already loaded",
                                 sQuote(chname1)), domain = NA)
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
    dll_list <- .dynLibs()

    if(missing(chname) || nchar(chname, "c") == 0L)
        if(.Platform$OS.type == "windows")
            stop("no DLL was specified")
        else
            stop("no shared object was specified")

    ## We need an absolute path here, and separators consistent with
    ## library.dynam
    libpath <- normalizePath(libpath, "/", TRUE)
    chname1 <- paste0(chname, file.ext)
    file <- if(nzchar(.Platform$r_arch))
             file.path(libpath, "libs", .Platform$r_arch, chname1)
     else    file.path(libpath, "libs", chname1)

    pos <- which(vapply(dll_list, function(x) x[["path"]] == file, NA))
    if(!length(pos))
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL %s was not loaded", sQuote(chname1)),
                 domain = NA)
        else
            stop(gettextf("shared object %s was not loaded", sQuote(chname1)),
                 domain = NA)

    if(!file.exists(file))
        if(.Platform$OS.type == "windows")
            stop(gettextf("DLL %s not found", sQuote(chname1)), domain = NA)
        else
            stop(gettextf("shared object '%s' not found", sQuote(chname1)),
                 domain = NA)
    if(verbose)
        message(gettextf("now dyn.unload(\"%s\") ...", file), domain = NA)
    dyn.unload(file)
    .dynLibs(dll_list[-pos])
    invisible(dll_list[[pos]])
}

require <-
function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
         character.only = FALSE)
{
    if(!character.only)
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
				  quietly = quietly),
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
    invisible(value)
}

.packages <-
function(all.available = FALSE, lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(all.available) {
	ans <- character()
        for(lib in lib.loc[file.exists(lib.loc)]) {
            a <- list.files(lib, all.files = FALSE, full.names = FALSE)
            pfile <- file.path(lib, a, "Meta", "package.rds")
            ans <- c(ans, a[file.exists(pfile)])
        }
        return(unique(ans))
    } ## else
    s <- search()
    invisible(.rmpkg(s[substr(s, 1L, 8L) == "package:"]))
}

path.package <-
function(package = NULL, quiet = FALSE)
{
    if(is.null(package)) package <- .packages()
    if(length(package) == 0L) return(character())
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
find.package <-
function(package = NULL, lib.loc = NULL, quiet = FALSE,
         verbose = getOption("verbose"))
{
    if(is.null(package) && is.null(lib.loc) && !verbose) {
        ## We only want the paths to the attached packages.
        return(path.package())
    }

    ## don't waste time looking for the standard packages:
    ## we know where they are and this can take a significant
    ## time with 1000+ packages installed.
    if(length(package) == 1L  &&
       package %in% c("base", "tools", "utils", "grDevices", "graphics",
                      "stats", "datasets", "methods", "grid", "parallel",
                      "splines", "stats4", "tcltk"))
        return(file.path(.Library, package))

    use_loaded <- FALSE
    if(is.null(package)) package <- .packages()
    if(is.null(lib.loc)) {
        use_loaded <- TRUE
        lib.loc <- .libPaths()
    }

    if(!length(package)) return(character())

    bad <- character()
    out <- character()

    for(pkg in package) {
	paths <- file.path(lib.loc, pkg)
	paths <- paths[ file.exists(file.path(paths, "DESCRIPTION")) ]
	if(use_loaded && isNamespaceLoaded(pkg)) {
	    dir <- if (pkg == "base") system.file()
		   else .getNamespaceInfo(asNamespace(pkg), "path")
            paths <- c(dir, paths)
        }
        ## trapdoor for tools:::setRlibs
        if(length(paths) &&
           file.exists(file.path(paths[1], "dummy_for_check"))) {
            bad <- c(bad, pkg)
            next
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
                    readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                else {
                    info <- tryCatch(read.dcf(file.path(p, "DESCRIPTION"),
                                              c("Package", "Version"))[1, ],
                                     error = identity)
                    if(inherits(info, "error")
                       || (length(info) != 2L)
                       || anyNA(info))
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
                warning(gettextf("package %s found more than once,\nusing the one found in %s",
                                 sQuote(pkg), sQuote(paths)), domain = NA)
        }
        out <- c(out, paths)
    }

    if(!quiet && length(bad)) {
        if(length(out) == 0L) {
            if(length(bad) == 1L) {
                stop(gettextf("there is no package called %s", sQuote(pkg)),
                     domain = NA)
            } else {
                stop(ngettext(length(bad),
                              "there is no package called",
                              "there are no packages called"), " ",
                     paste(sQuote(bad), collapse = ", "), domain = NA)

            }
        }
        for(pkg in bad)
            warning(gettextf("there is no package called %s", sQuote(pkg)),
                    domain = NA)
    }

    out
}

format.packageInfo <-
function(x, ...)
{
    if(!inherits(x, "packageInfo")) stop("wrong class")
    vignetteMsg <-
        gettextf("Further information is available in the following vignettes in directory %s:",
                 sQuote(file.path(x$path, "doc")))
    headers <- sprintf("\n%s\n",
                       c(gettext("Description:"),
                         gettext("Index:"),
                         paste(strwrap(vignetteMsg), collapse = "\n")))
    formatDocEntry <- function(entry) {
        if(is.list(entry) || is.matrix(entry))
            formatDL(entry, style = "list")
        else
            entry
    }
    c(gettextf("\n\t\tInformation on package %s", sQuote(x$name)),
      unlist(lapply(which(!vapply(x$info, is.null, NA)),
                    function(i)
                        c(headers[i], formatDocEntry(x$info[[i]])))))

}

print.packageInfo <-
function(x, ...)
{
    outFile <- tempfile("RpackageInfo")
    writeLines(format(x), outFile)
    file.show(outFile, delete.file = TRUE,
              title =
              gettextf("Documentation for package %s", sQuote(x$name)))
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
### FIXME: utils::packageVersion() should be pushed up here instead
    .findVersion <- function(pkg, lib.loc = NULL) {
        pfile <- system.file("Meta", "package.rds",
                             package = pkg, lib.loc = lib.loc)
        if (nzchar(pfile))
            as.numeric_version(readRDS(pfile)$DESCRIPTION["Version"])
        else
            NULL
    }
    .findAllVersions <- function(pkg, lib.loc = NULL) {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        do.call(c, Filter(Negate(is.null),
                          lapply(lib.loc, .findVersion, pkg=pkg)))
    }
    pkgs <- unique(names(pkgInfo$Depends))
    pkgname <- pkgInfo$DESCRIPTION["Package"]
    for(pkg in setdiff(pkgs, "base")) {
        ## allow for multiple occurrences
        depends <- pkgInfo$Depends[names(pkgInfo$Depends) == pkg]
        attached <- paste("package", pkg, sep = ":") %in% search()
        current <- .findVersion(pkg, lib.loc)
        if(is.null(current))
            stop(gettextf("package %s required by %s could not be found",
                          sQuote(pkg), sQuote(pkgname)),
                 call. = FALSE, domain = NA)
        have_vers <- lengths(depends) > 1L
        for(dep in depends[have_vers]) {
            target <- as.numeric_version(dep$version)
            sufficient <- do.call(dep$op, list(current, target))
            if (!sufficient) {
                if (is.null(lib.loc))
                    lib.loc <- .libPaths()
                versions <- .findAllVersions(pkg, lib.loc)
                sufficient <- vapply(versions, dep$op, logical(1L), target)
                if (any(sufficient)) {
                    warning(gettextf("version %s of %s masked by %s in %s",
                                     versions[which(sufficient)[1L]],
                                     sQuote(pkg),
                                     current,
                                     lib.loc[which(sufficient)[1L]-1L]),
                            call. = FALSE, domain = NA)
                }
                if (attached)
                    msg <- "package %s %s is loaded, but %s %s is required by %s"
                else
                    msg <- "package %s %s was found, but %s %s is required by %s"
                stop(gettextf(msg, sQuote(pkg), current, dep$op,
                              target, sQuote(pkgname)),
                     call. = FALSE, domain = NA)
            }
        }

        if (!attached) {
            if (!quietly)
                packageStartupMessage(gettextf("Loading required package: %s",
                                               pkg), domain = NA)
            library(pkg, character.only = TRUE, logical.return = TRUE,
                    lib.loc = lib.loc, quietly = quietly) ||
                stop(gettextf("package %s could not be loaded", sQuote(pkg)),
                     call. = FALSE, domain = NA)
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
        gsub(paste0("(^|[^%])(%%)*%", spec),
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
