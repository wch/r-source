#  File src/library/utils/R/packages2.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

if (.Platform$OS.type == "windows")
    .install.macbinary <- function(...) NULL	# globalVariables isn't available, so use this to suppress the warning

getDependencies <-
    function(pkgs, dependencies = NA, available = NULL, lib = .libPaths()[1L],
             binary = FALSE)
{
    if (is.null(dependencies)) return(unique(pkgs))
    oneLib <- length(lib) == 1L
    dep2 <- NULL
    if(is.logical(dependencies) && is.na(dependencies))
        dependencies <- c("Depends", "Imports", "LinkingTo")
    depends <-
        is.character(dependencies) || (is.logical(dependencies) && dependencies)
    if(depends && is.logical(dependencies)) {
        if(binary) {
            dependencies <-  c("Depends", "Imports", "Suggests")
            dep2 <- c("Depends", "Imports")
        } else {
            dependencies <-  c("Depends", "Imports", "LinkingTo", "Suggests")
            dep2 <- c("Depends", "Imports", "LinkingTo")
        }
    }
    if(depends && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\nskipping dependencies")
        depends <- FALSE
    }
    p0 <- unique(pkgs)
    miss <-  !p0 %in% row.names(available)
    if(sum(miss)) {
	warning(sprintf(ngettext(sum(miss),
				 "package %s is not available (for %s)",
				 "packages %s are not available (for %s)"),
			paste(sQuote(p0[miss]), collapse=", "),
			sub(" *\\(.*","", R.version.string)),
                domain = NA, call. = FALSE)
        if (sum(miss) == 1L &&
            !is.na(w <- match(tolower(p0[miss]),
                              tolower(row.names(available))))) {
            warning(sprintf("Perhaps you meant %s ?",
                            sQuote( row.names(available)[w])),
                    call. = FALSE, domain = NA)
        }
        flush.console()
    }
    p0 <- p0[!miss]

    if(depends) { # check for dependencies, recursively
        p1 <- p0 # this is ok, as 1 lib only
        ## INSTALL prepends 'lib' to the libpath
        ## Here we are slightly more conservative
        libpath <- .libPaths()
        if(!lib %in% libpath) libpath <- c(lib, libpath)
        installed <- installed.packages(lib.loc = libpath,
                                        fields = c("Package", "Version"))
        not_avail <- character()
	repeat {
	    deps <- apply(available[p1, dependencies, drop = FALSE],
                          1L, function(x) paste(x[!is.na(x)], collapse=", "))
	    res <- .clean_up_dependencies2(deps, installed, available)
            not_avail <- c(not_avail, res[[2L]])
            deps <- unique(res[[1L]])
            ## R should not get to here, but be safe
            deps <- deps[!deps %in% c("R", pkgs)]
	    if(!length(deps)) break
	    pkgs <- c(deps, pkgs)
	    p1 <- deps
            if(!is.null(dep2)) { dependencies <- dep2; dep2 <- NULL }
	}
        if(length(not_avail)) {
            not_avail <- unique(not_avail)
            warning(sprintf(ngettext(length(not_avail),
                                     "dependency %s is not available",
                                     "dependencies %s are not available"),
                            paste(sQuote(not_avail), collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
            flush.console()
        }

        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            message(sprintf(ngettext(length(added),
                                     "also installing the dependency %s",
                                     "also installing the dependencies %s"),
                            paste(sQuote(added), collapse=", ")),
                    "\n", domain = NA)
            flush.console()
        }
        p0 <- pkgs
    }
    p0
}

install.packages <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             method, available = NULL, destdir = NULL, dependencies = NA,
             type = getOption("pkgType"),
             configure.args = getOption("configure.args"),
             configure.vars = getOption("configure.vars"),
             clean = FALSE, Ncpus = getOption("Ncpus", 1L),
	     verbose = getOption("verbose"),
             libs_only = FALSE, INSTALL_opts, quiet = FALSE,
             keep_outputs = FALSE,
             ...)
{
    if (is.logical(clean) && clean)
        clean <- "--clean"
    if(is.logical(dependencies) && is.na(dependencies))
        dependencies <- if(!missing(lib) && length(lib) > 1L) FALSE
        else c("Depends", "Imports", "LinkingTo")

    ## Compute the configuration arguments for a given package.
    ## If configure.args is an unnamed character vector, use that.
    ## If it is named, match the pkg name to the names of the character
    ## vector and if we get a match, use that element.
    ## Similarly, configure.args is a list(), match pkg to the names pkg
    ## and use that element, collapsing it into a single string.

    get_package_name <- function(pkg) {
        ## Since the pkg argument can be the name of a file rather than
        ## a regular package name, we have to clean that up.
        gsub("_\\.(zip|tar\\.gz)", "",
             gsub(.standard_regexps()$valid_package_version, "",
                  basename(pkg)))
    }

    getConfigureArgs <- function(pkg)
    {
        if(.Platform$OS.type == "windows") return(character())

        pkg <- get_package_name(pkg)

        if(length(pkgs) == 1L && length(configure.args) &&
           length(names(configure.args)) == 0L)
            return(paste0("--configure-args=",
                          shQuote(paste(configure.args, collapse = " "))))

        if (length(configure.args) && length(names(configure.args))
              && pkg %in% names(configure.args))
            config <- paste0("--configure-args=",
                             shQuote(paste(configure.args[[ pkg ]], collapse = " ")))
        else
            config <- character()

        config
    }

    getConfigureVars <- function(pkg)
    {
        if(.Platform$OS.type == "windows") return(character())

        pkg <- get_package_name(pkg)

        if(length(pkgs) == 1L && length(configure.vars) &&
           length(names(configure.vars)) == 0L)
            return(paste0("--configure-vars=",
                          shQuote(paste(configure.vars, collapse = " "))))

        if (length(configure.vars) && length(names(configure.vars))
              && pkg %in% names(configure.vars))
            config <- paste0("--configure-vars=",
                             shQuote(paste(configure.vars[[ pkg ]], collapse = " ")))
        else
            config <- character()

        config
    }

    get_install_opts <- function(pkg) {
        if(!length(INSTALL_opts))
            character()
        else
            paste(INSTALL_opts[[get_package_name(pkg)]], collapse = " ")
    }

    if(missing(pkgs) || !length(pkgs)) {
        if(!interactive()) stop("no packages were specified")
        ## if no packages were specified, use a menu
	if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA"
           || (capabilities("tcltk")
               && capabilities("X11") && suppressWarnings(tcltk:::.TkUp)) ) {
            ## this is the condition for a graphical select.list()
	} else
	    stop("no packages were specified")

        ## This will only offer the specified type.
	if(is.null(available))
	    available <- available.packages(contriburl = contriburl,
					    method = method)
	if(NROW(available)) {
            ## avoid duplicate entries in menus, since the latest available
            ## will be picked up
            ## sort in the locale, as R <= 2.10.1 did so
	    pkgs <- select.list(sort(unique(rownames(available))),
                                multiple = TRUE,
                                title = "Packages", graphics = TRUE)
	}
	if(!length(pkgs)) stop("no packages were specified")
    }

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
	if(!quiet && length(.libPaths()) > 1L)
	    message(sprintf(ngettext(length(pkgs),
                                     "Installing package into %s\n(as %s is unspecified)",
                                     "Installing packages into %s\n(as %s is unspecified)"),
                            sQuote(lib), sQuote("lib")), domain = NA)
    }

    ## check for writability by user
    ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
    if(length(lib) > 1 && any(!ok))
        stop(sprintf(ngettext(sum(!ok),
                              "'lib' element %s is not a writable directory",
                              "'lib' elements %s are not writable directories"),
                     paste(sQuote(lib[!ok]), collapse=", ")), domain = NA)
    if(length(lib) == 1L && .Platform$OS.type == "windows") {
        ## file.access is unreliable on Windows, especially >= Vista.
        ## the only known reliable way is to try it
        ok <- dir.exists(lib) # dir might not exist, PR#14311
        if(ok) {
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), sep = "_"))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if(inherits(res, "try-error") || !res) ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if(length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib),
                domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                                   .Platform$path.sep))[1L]
	if(interactive()) {
	    ask.yes.no <- function(msg) {
                ##' returns "no" for "no",  otherwise 'ans', a string
		msg <- gettext(msg)
		if(.Platform$OS.type == "windows") {
                    flush.console() # so warning is seen
		    ans <- winDialog("yesno", sprintf(msg, sQuote(userdir)))
		    if(ans != "YES") "no" else ans
		} else {
		    ans <- readline(paste(sprintf(msg, userdir), " (y/n) "))
		    if(substr(ans, 1L, 1L) == "n") "no" else ans
		}
	    }
	    ans <- ask.yes.no("Would you like to use a personal library instead?")
	    if(identical(ans, "no")) stop("unable to install packages")

	    lib <- userdir
	    if(!file.exists(userdir)) {
		ans <- ask.yes.no("Would you like to create a personal library\n%s\nto install packages into?")
		if(identical(ans, "no")) stop("unable to install packages")
		if(!dir.create(userdir, recursive = TRUE))
                    stop(gettextf("unable to create %s", sQuote(userdir)),
                         domain = NA)
		.libPaths(c(userdir, .libPaths()))
	    }
	} else stop("unable to install packages")
    }

    lib <- normalizePath(lib)

    ## check if we should infer repos=NULL
    if(length(pkgs) == 1L && missing(repos) && missing(contriburl)) {
        if((type == "source" && length(grep("\\.tar.gz$", pkgs))) ||
           (type %in% "win.binary" && length(grep("\\.zip$", pkgs))) ||
           (substr(type, 1L, 10L) == "mac.binary"
            && length(grep("\\.tgz$", pkgs)))) {
            repos <- NULL
            message("inferring 'repos = NULL' from the file name")
        }
    }

# for testing .Platform$pkgType <- "mac.binary.leopard"
    ## Look at type == "both"
    if (type == "both") {
        ## NB it is only safe to use binary packages with a Mac OS X
        ## build that uses the same R foundation layout as CRAN since
        ## paths in DSOs are hard-coded.
        type2 <- .Platform$pkgType
        if (type2 == "source")
            stop("type == \"both\" can only be used on Windows or a CRAN build for Mac OS X")
        if(!missing(contriburl) || !is.null(available))
            stop("type == \"both\" cannot be used if 'available' or 'contriburl' is specified")
        if(is.null(repos))
            stop("type == \"both\" cannot be used with 'repos = NULL'")
        type <- "source"
        contriburl <- contrib.url(repos, "source")
        # The line above may have changed the repos option, so..
        if (missing(repos)) repos <- getOption("repos")
        available <-
            available.packages(contriburl = contriburl, method = method,
                               fields = "NeedsCompilation")
        pkgs <- getDependencies(pkgs, dependencies, available, lib)
        ## Now see what we can get as binary packages.
        av2 <- available.packages(contriburl = contrib.url(repos, type2),
                                  method = method)
        bins <- row.names(av2)
        bins <- pkgs[pkgs %in% bins]
        srcOnly <- pkgs[! pkgs %in% bins]
        binvers <- av2[bins, "Version"]
        hasSrc <-  !is.na(av2[bins, "Archs"])

        srcvers <- available[bins, "Version"]
        later <- as.numeric_version(binvers) < srcvers
        if(any(later)) {
            msg <- ngettext(sum(later),
                            "There is a binary version available but the source version is later",
                            "There are binary versions available but the source versions are later")
            cat("\n",
                paste(strwrap(msg, indent = 2, exdent = 2), collapse = "\n"),
                ":\n", sep = "")
            out <- data.frame(`binary` = binvers, `source` = srcvers,
                              `needs_compilation` =  hasSrc,
                              row.names = bins,
                              check.names = FALSE)[later, ]
            print(out)
            cat("\n")
            if(interactive() && any(later & hasSrc)) {
                msg <-
                    ngettext(sum(later & hasSrc),
                             "Do you want to install from sources the package which need compilation?",
                             "Do you want to install from sources the packages which need compilation?")
                message(msg, domain = NA)
                res <- readline("y/n: ")
                if(res != "y") later <- later & !hasSrc
            }
        }
        bins <- bins[!later]

        if(interactive() && length(srcOnly)) {
            nc <- !( available[srcOnly, "NeedsCompilation"] %in% "no" )
            s2 <- srcOnly[nc]
            if(length(s2)) {
                msg <-
                    ngettext(length(s2),
                             "Package which are only available in source form, and may need compilation of C/C++/Fortran",
                             "Packages which are only available in source form, and may need compilation of C/C++/Fortran")
                msg <- c(paste0(msg, ": "), sQuote(s2))
                msg <- strwrap(paste(msg, collapse = " "), exdent = 2)
                message(paste(msg, collapse = "\n"), domain = NA)
                message("Do you want to attempt to install these from sources?")
                res <- readline("y/n: ")
                if(res != "y") pkgs <- setdiff(pkgs, s2)
            }
        }

        if(length(bins)) {
            if(type2 == "win.binary")
                .install.winbinary(pkgs = bins, lib = lib,
                                   contriburl = contrib.url(repos, type2),
                                   method = method, available = av2,
                                   destdir = destdir,
                                   dependencies = NULL,
                                   libs_only = libs_only, ...)
            else
                .install.macbinary(pkgs = bins, lib = lib,
                                   contriburl = contrib.url(repos, type2),
                                   method = method, available = av2,
                                   destdir = destdir,
                                   dependencies = NULL, ...)
        }
        pkgs <- setdiff(pkgs, bins)
        if(!length(pkgs)) return(invisible())
        message(sprintf(ngettext(length(pkgs),
                                     "installing the source package %s",
                                     "installing the source packages %s"),
                        paste(sQuote(pkgs), collapse=", ")),
                "\n", domain = NA)
	flush.console()
    } else if (getOption("install.packages.check.source", "yes") %in% "yes"
               && (type %in% "win.binary" || substr(type, 1L, 10L) == "mac.binary")) {
        if (missing(contriburl) && is.null(available) && !is.null(repos)) {
            contriburl2 <- contrib.url(repos, "source")
	    # The line above may have changed the repos option, so..
            if (missing(repos)) repos <- getOption("repos")
	    av1 <- tryCatch(suppressWarnings(
			available.packages(contriburl = contriburl2, method = method)),
			    error = function(e)e)
	    if(inherits(av1, "error")) {
                message("source repository is unavailable to check versions")
                available <-
                    available.packages(contriburl = contrib.url(repos, type), method = method)
            } else {
                srcpkgs <- pkgs[pkgs %in% row.names(av1)]
                ## Now see what we can get as binary packages.
                available <-
                    available.packages(contriburl = contrib.url(repos, type), method = method)
                bins <- pkgs[pkgs %in% row.names(available)]
                ## so a package might only be available as source,
                ## or it might be later in source.
                ## FIXME: might only want to check on the same repository,
                ## allowing for CRANextras.
                na <- srcpkgs[!srcpkgs %in% bins]
                if (length(na)) {
                    msg <-
                        sprintf(ngettext(length(na),
                                         "package %s is available as a source package but not as a binary",
                                         "packages %s are available as source packages but not as binaries"),
                                paste(sQuote(na), collapse = ", "))
                    cat("\n   ", msg, "\n\n", sep = "")
                }
                binvers <- available[bins, "Version"]
                srcvers <- binvers
                OK <- bins %in% srcpkgs
                srcvers[OK] <- av1[bins[OK], "Version"]
                later <- as.numeric_version(binvers) < srcvers
                if(any(later)) {
                    msg <- ngettext(sum(later),
                                    "There is a binary version available (and will be installed) but the source version is later",
                                    "There are binary versions available (and will be installed) but the source versions are later")
                    cat("\n",
                        paste(strwrap(msg, indent = 2, exdent = 2), collapse = "\n"),
                        ":\n", sep = "")
                    print(data.frame(`binary` = binvers, `source` = srcvers,
                                     row.names = bins,
                                     check.names = FALSE)[later, ])
                    cat("\n")
                }
            }
        }
    }

    if(.Platform$OS.type == "windows") {
        if(substr(type, 1L, 10L) == "mac.binary")
            stop("cannot install MacOS X binary packages on Windows")

        if(type %in% "win.binary") {
            ## include local .zip files
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               dependencies = dependencies,
                               libs_only = libs_only, quiet = quiet,  ...)
            return(invisible())
        }
        ## Avoid problems with spaces in pathnames.
        have_spaces <- grep(" ", pkgs)
        if(length(have_spaces)) {
            ## we want the short name for the directory,
            ## but not for a .tar.gz, and package names never contain spaces.
            p <- pkgs[have_spaces]
            dirs <- shortPathName(dirname(p))
            pkgs[have_spaces] <- file.path(dirs, basename(p))
        }
        ## Avoid problems with backslashes
        ## -- will mess up UNC names, but they don't work
        pkgs <- gsub("\\\\", "/", pkgs)
    } else {
        if(substr(type, 1L, 10L) == "mac.binary") {
            if(!length(grep("darwin", R.version$platform)))
                stop("cannot install MacOS X binary packages on this platform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               dependencies = dependencies, quiet = quiet, ...)
            return(invisible())
        }

        if(type %in% "win.binary")
            stop("cannot install Windows binary packages on this platform")

        if(!file.exists(file.path(R.home("bin"), "INSTALL")))
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }

    ## we need to ensure that R CMD INSTALL runs with the same
    ## library trees as this session.
    ## FIXME: At least on Windows, either run sub-R directly (to avoid sh)
    ## or run the install in the current process.
    libpath <- .libPaths()
    libpath <- libpath[! libpath %in% .Library]
    if(length(libpath))
        libpath <- paste(libpath, collapse = .Platform$path.sep)

    cmd0 <- file.path(R.home("bin"), "R")
    args0 <- c("CMD", "INSTALL")

    output <- if(quiet) FALSE else ""
    env <- character()

    outdir <- getwd()
    if(is.logical(keep_outputs)) {
        if(is.na(keep_outputs))
            keep_outputs <- FALSE
    } else if(is.character(keep_outputs) &&
              (length(keep_outputs) == 1L)) {
        if(!dir.exists(keep_outputs) &&
           !dir.create(keep_outputs, recursive = TRUE))
            stop(gettextf("unable to create %s", sQuote(keep_outputs)),
                 domain = NA)
        outdir <- normalizePath(keep_outputs)
        keep_outputs <- TRUE
    } else
        stop(gettextf("invalid %s argument", sQuote("keep_outputs")),
             domain = NA)

    if(length(libpath)) {
        ## <NOTE>
        ## For the foreseeable future, the 'env' argument to system2()
        ## on Windows is limited to calls to make and rterm (but not R
        ## CMD): hence need to set the R_LIBS env var here.
        if(.Platform$OS.type == "windows") {
            ## We don't have a way to set an environment variable for
            ## a single command, as we do not spawn a shell.
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        } else
            env <- paste("R_LIBS", shQuote(libpath), sep = "=")
        ## </NOTE>
    }

    if (is.character(clean))
        args0 <- c(args0, clean)
    if (libs_only)
        args0 <- c(args0, "--libs-only")
    if (!missing(INSTALL_opts)) {
        if(!is.list(INSTALL_opts)) {
            args0 <- c(args0, paste(INSTALL_opts, collapse = " "))
            INSTALL_opts <- list()
        }
    } else {
        INSTALL_opts <- list()
    }

    if(verbose)
        message(gettextf("system (cmd0): %s",
                         paste(c(cmd0, args0), collapse = " ")),
                domain = NA)

    if(is.null(repos) & missing(contriburl)) {
        ## install from local source tarball(s)
        update <- cbind(path.expand(pkgs), lib) # for side-effect of recycling to same length

        for(i in seq_len(nrow(update))) {
            args <- c(args0,
                      get_install_opts(update[i, 1L]),
                      "-l", shQuote(update[i, 2L]),
                      getConfigureArgs(update[i, 1L]),
                      getConfigureVars(update[i, 1L]),
                      shQuote(update[i, 1L]))
            status <- system2(cmd0, args, env = env,
                              stdout = output, stderr = output)
            if(status > 0L)
                warning(gettextf("installation of package %s had non-zero exit status",
                                 sQuote(update[i, 1L])),
                        domain = NA)
            else if(verbose) {
                cmd <- paste(c(cmd0, args), collapse = " ")
                message(sprintf("%d): succeeded '%s'", i, cmd),
                        domain = NA)
            }
        }
        return(invisible())
    }

    tmpd <- destdir
    nonlocalrepos <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalrepos) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory %s",
                          sQuote(tmpd)),
                 domain = NA)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib)

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "source", quiet = quiet, ...)

    ## at this point 'pkgs' may contain duplicates,
    ## the same pkg in different libs
    if(length(foundpkgs)) {
	if(verbose) message(gettextf("foundpkgs: %s",
                                     paste(foundpkgs, collapse=", ")),
                            domain = NA)
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1L]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1L]), 2L]
	if(verbose) message(gettextf("files: %s",
                                     paste(files, collapse=", \n\t")),
                            domain = NA)
        update <- cbind(update[found, , drop=FALSE], file = files)
        if(nrow(update) > 1L) {
            upkgs <- unique(pkgs <- update[, 1L])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            ## can't use update[p0, ] due to possible multiple matches
            update <- update[sort.list(match(pkgs, p0)), ]
        }

        if (Ncpus > 1L && nrow(update) > 1L) {
            ## if --no-lock or --lock was specified in INSTALL_opts
            ## that will override this.
            args0 <- c(args0, "--pkglock")
            tmpd <- file.path(tempdir(), "make_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd))
                stop(gettextf("unable to create temporary directory %s",
                              sQuote(tmpd)),
                     domain = NA)
            mfile <- file.path(tmpd, "Makefile")
            conn <- file(mfile, "wt")
            deps <- paste(paste0(update[, 1L], ".ts"), collapse=" ")
            deps <- strwrap(deps, width = 75, exdent = 2)
            deps <- paste(deps, collapse=" \\\n")
            cat("all: ", deps, "\n", sep = "", file = conn)
            aDL <- .make_dependency_list(upkgs, available, recursive = TRUE)
            for(i in seq_len(nrow(update))) {
                pkg <- update[i, 1L]
                args <- c(args0,
                          get_install_opts(update[i, 3L]),
                          "-l", shQuote(update[i, 2L]),
                          getConfigureArgs(update[i, 3L]),
                          getConfigureVars(update[i, 3L]),
                          shQuote(update[i, 3L]),
                          ">", paste0(pkg, ".out"),
                          "2>&1")
                ## <NOTE>
                ## We currently only use env on Unix for R_LIBS.
                ## Windows we do Sys.setenv(R_LIBS = libpath),
                ## since system2() has limited support for 'env'
                ## Should we use env on Windows as well?
                ## If so, would we need
                ##   cmd <- paste(c(shQuote(command), env, args),
                ##                collapse = " ")
                ## on Windows?
                cmd <- paste(c(shQuote(cmd0), args), collapse = " ")
                ## </NOTE>
                deps <- aDL[[pkg]]
                deps <- deps[deps %in% upkgs]
                ## very unlikely to be too long
                deps <- if(length(deps))
                    paste(paste0(deps, ".ts"), collapse = " ") else ""
                cat(paste0(pkg, ".ts: ", deps),
                    paste("\t@echo begin installing package", sQuote(pkg)),
                    paste0("\t@", cmd, " && touch ", pkg, ".ts"),
                    paste0("\t@cat ", pkg, ".out"),
                    "", sep = "\n", file = conn)
            }
            close(conn)
            cwd <- setwd(tmpd)
            on.exit(setwd(cwd))
            ## MAKE will be set by sourcing Renviron
            status <- system2(Sys.getenv("MAKE", "make"),
                              c("-k -j", Ncpus),
                              stdout = output, stderr = output,
                              env = env)
            if(status > 0L) {
                ## Try to figure out which
                pkgs <- update[, 1L]
                tss <- sub("\\.ts$", "", dir(".", pattern = "\\.ts$"))
                failed <- pkgs[!pkgs %in% tss]
		for (pkg in failed) system(paste0("cat ", pkg, ".out"))
                warning(gettextf("installation of one or more packages failed,\n  probably %s",
                                 paste(sQuote(failed), collapse = ", ")),
                        domain = NA)
            }
            if(keep_outputs)
                file.copy(paste0(update[, 1L], ".out"), outdir)
            setwd(cwd); on.exit()
            unlink(tmpd, recursive = TRUE)
        } else {
            for(i in seq_len(nrow(update))) {
                outfile <- if(keep_outputs) {
                    paste0(update[i, 1L], ".out")
                } else output
                args <- c(args0,
                          get_install_opts(update[i, 3L]),
                          "-l", shQuote(update[i, 2L]),
                          getConfigureArgs(update[i, 3L]),
                          getConfigureVars(update[i, 3L]),
                          update[i, 3L])
                status <- system2(cmd0, args, env = env,
                                  stdout = outfile, stderr = outfile)
                if(!quiet && keep_outputs)
                    writeLines(readLines(outfile))
                if(status > 0L)
                    warning(gettextf("installation of package %s had non-zero exit status",
                                     sQuote(update[i, 1L])),
                            domain = NA)
		else if(verbose) {
                    cmd <- paste(c(cmd0, args), collapse = " ")
                    message(sprintf("%d): succeeded '%s'", i, cmd),
                            domain = NA)
                }
            }
            if(keep_outputs && (outdir != getwd()))
                file.copy(paste0(update[, 1L], ".out"), outdir)
        }
        if(!quiet && nonlocalrepos && !is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded source packages are in\n\t%s",
                               sQuote(normalizePath(tmpd, mustWork = FALSE))),
                "\n", sep = "")
        ## update packages.html on Unix only if .Library was installed into
        libs_used <- unique(update[, 2L])
        if(.Platform$OS.type == "unix" && .Library %in% libs_used) {
            message("Updating HTML index of packages in '.Library'")
            make.packages.html(.Library)
        }
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}

## treat variables as global in a package, for codetools & check
globalVariables <- function(names, package, add = TRUE)
    registerNames(names, package, ".__global__", add)

## suppress foreign function checks, for check
suppressForeignCheck <- function(names, package, add = TRUE)
    registerNames(names, package, ".__suppressForeign__", add)

registerNames <- function(names, package, .listFile, add = TRUE) {
    .simplePackageName <- function(env) {
        if(exists(".packageName", envir = env, inherits = FALSE))
           get(".packageName", envir = env)
        else
            "(unknown package)"
    }
    if(missing(package)) {
        env <- topenv(parent.frame(2L)) # We cannot be called directly!
        package <- .simplePackageName(env)
    }
    else if(is.environment(package)) {
        env <- package
        package <- .simplePackageName(env)
    }
    else
        env <- asNamespace(package)
    if(exists(.listFile, envir = env, inherits = FALSE))
        current <- get(.listFile, envir = env)
    else
        current <- character()
    if(! missing(names)) {
        if(environmentIsLocked(env))
            stop(gettextf("The namespace for package \"%s\" is locked; no changes in the global variables list may be made.",
                          package))
        if(add)
            current <- unique(c(current, names))
        else
            current <- names
        assign(.listFile, current, envir = env)
    }
    current
}

packageName <- function(env = parent.frame()) {
    if (!is.environment(env)) stop("'env' must be an environment")
    env <- topenv(env)
    if (exists(".packageName", envir = env, inherits = FALSE))
	get(".packageName", envir = env, inherits = FALSE)
    else if (identical(env, .BaseNamespaceEnv))
	"base"
    else
	NULL
}
