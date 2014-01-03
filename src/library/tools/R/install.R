#  File src/library/tools/R/install.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
# NB: also copyright dates in Usages.
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

#### R based engine for  R CMD INSTALL SHLIB Rprof
####

##' @param args

## R developers can use this to debug the function by running it
## directly as tools:::.install_packages(args), where the args should
## be what commandArgs(TRUE) would return, that is a character vector
## of (space-delimited) terms that would be passed to R CMD INSTALL.  E.g.
##
## tools:::.install_packages(c("--preclean", "--no-multiarch", "tree"))

##' @return ...
.install_packages <- function(args = NULL)
{
    ## calls system() on Windows for
    ## sh (configure.win/cleanup.win) make zip

    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    ## global variables
    curPkg <- character() # list of packages in current pkg
    lockdir <- ""
    is_first_package <- TRUE
    stars <- "*"

    ## Need these here in case of an early error, e.g. missing etc/Makeconf
    tmpdir <- ""
    clean_on_error <- TRUE
    do_exit_on_error <- function()
    {
        ## If we are not yet processing a package, we will not have
        ## set curPkg
        if(clean_on_error && length(curPkg)) {
            pkgdir <- file.path(lib, curPkg)
            if (nzchar(pkgdir) && dir.exists(pkgdir) &&
                is_subdir(pkgdir, lib)) {
                starsmsg(stars, "removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }

            if (nzchar(lockdir) &&
                dir.exists(lp <- file.path(lockdir, curPkg)) &&
                is_subdir(lp, lockdir)) {
                starsmsg(stars, "restoring previous ", sQuote(pkgdir))
                if (WINDOWS) {
                    file.copy(lp, dirname(pkgdir), recursive = TRUE,
                              copy.date = TRUE)
                    unlink(lp, recursive = TRUE)
                } else {
                    ## some shells require that they be run in a known dir
                    setwd(startdir)
                    system(paste("mv", shQuote(lp), shQuote(pkgdir)))
                }
            }
        }

        do_cleanup()
        q("no", status = 1, runLast = FALSE)
    }

    do_cleanup <- function()
    {
        if(nzchar(tmpdir)) do_cleanup_tmpdir()
        if (!is_first_package) {
            ## Only need to do this in case we successfully installed
            ## at least one package
            if (lib == .Library && "html" %in% build_help_types)
                utils::make.packages.html(.Library, docdir = R.home("doc"))
        }
        if (nzchar(lockdir)) unlink(lockdir, recursive = TRUE)
    }

    do_cleanup_tmpdir <- function()
    {
        ## Solaris will not remove any directory in the current path
        setwd(startdir)
        if (dir.exists(tmpdir)) unlink(tmpdir, recursive=TRUE)
    }

    on.exit(do_exit_on_error())
    WINDOWS <- .Platform$OS.type == "windows"

    MAKE <- Sys.getenv("MAKE") # FIXME shQuote, default?
    rarch <- Sys.getenv("R_ARCH") # unix only
    if (WINDOWS && nzchar(.Platform$r_arch))
        rarch <- paste0("/", .Platform$r_arch)
    test_archs <- rarch

    SHLIB_EXT <- if (WINDOWS) ".dll" else {
        ## can we do better?
        mconf <- file.path(R.home(), paste0("etc", rarch), "Makeconf")
        ## PCRE needed for Debian arm* platforms
        sub(".*= ", "", grep("^SHLIB_EXT", readLines(mconf), value = TRUE,
                             perl = TRUE))
    }

    options(warn = 1)
    invisible(Sys.setlocale("LC_COLLATE", "C")) # discard output

    if (WINDOWS) {
        rhome <- chartr("\\", "/", R.home())
        ## These might be needed for configure.win and Make{file,vars}.win
        ## Some people have *assumed* that R_HOME uses /
        Sys.setenv(R_HOME = rhome)
        if (nzchar(rarch)) Sys.setenv(R_ARCH = rarch, R_ARCH_BIN = rarch)
    }

    Usage <- function() {
        cat("Usage: R CMD INSTALL [options] pkgs",
            "",
            "Install the add-on packages specified by pkgs.  The elements of pkgs can",
            "be relative or absolute paths to directories with the package",
            "sources, or to gzipped package 'tar' archives.  The library tree",
            "to install to can be specified via '--library'.  By default, packages are",
            "installed in the library tree rooted at the first directory in",
            ".libPaths() for an R session run in the current environment",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print INSTALL version info and exit",
            "  -c, --clean		remove files created during installation",
            "      --preclean	remove files created during a previous run",
	    "  -d, --debug		turn on debugging messages",
            if(WINDOWS) "			and build a debug DLL",
            "  -l, --library=LIB	install packages to library tree LIB",
            "      --no-configure    do not use the package's configure script",
            "      --no-docs		do not install HTML, LaTeX or examples help",
            "      --html		build HTML help",
            "      --no-html		do not build HTML help",
            "      --latex      	install LaTeX help",
            "      --example		install R code for help examples",
            "      --fake		do minimal install for testing purposes",
            "      --no-lock		install on top of any existing installation",
            "			without using a lock directory",
            "      --lock		use a per-library lock directory (default)",
            "      --pkglock		use a per-package lock directory",
            "      			(default for a single package)",
            "      --build    	build binaries of the installed package(s)",
            "      --install-tests	install package-specific tests (if any)",
            "      --no-R, --no-libs, --no-data, --no-help, --no-demo, --no-exec,",
            "      --no-inst",
            "			suppress installation of the specified part of the",
            "			package for testing or other special purposes",
            "      --no-multiarch	build only the main architecture",
            "      --libs-only	only install the libs directory",
            "      --data-compress=	none, gzip (default), bzip2 or xz compression",
            "			to be used for lazy-loading of data",
            "      --resave-data	re-save data files as compactly as possible",
            "      --compact-docs	re-compress PDF files under inst/doc",
            "      --with-keep.source",
            "      --without-keep.source",
            "			use (or not) 'keep.source' for R code",
            "      --byte-compile	byte-compile R code",
            "      --no-byte-compile	do not byte-compile R code",
            "      --no-test-load	skip test of loading installed package",
            "      --no-clean-on-error	do not remove installed package on error",
            "      --merge-multiarch	multi-arch by merging (from a single tarball only)",
	    ## "      --group-writable	set file permissions to group-writable,",
	    ## "			such that group members can update.packages()",
           "\nfor Unix",
            "      --configure-args=ARGS",
            "			set arguments for the configure scripts (if any)",
            "      --configure-vars=VARS",
            "			set variables for the configure scripts (if any)",
            "      --dsym            (OS X only) generate dSYM directory",
            "\nand on Windows only",
            "      --force-biarch	attempt to build both architectures",
            "			even if there is a non-empty configure.win",
            "      --compile-both	compile both architectures on 32-bit Windows",
            "",
            "Which of --html or --no-html is the default depends on the build of R:",
            paste0("for this one it is ",
		   if(static_html) "--html" else "--no-html", "."),
            "",
            "Report bugs at bugs.r-project.org .", sep = "\n")
    }


    # Check whether dir is a subdirectory of parent,
    # to protect against malicious package names like ".." below
    # Assumes that both directories exist

    is_subdir <- function(dir, parent)
        normalizePath(parent) == normalizePath(file.path(dir, ".."))

    fullpath <- function(dir)
    {
        owd <- setwd(dir)
        full <- getwd()
        setwd(owd)
        full
    }

    ## used for LazyData, KeepSource, ByteCompile, Biarch
    parse_description_field <- function(desc, field, default = TRUE)
    {
        tmp <- desc[field]
        if (is.na(tmp)) default
        else switch(tmp,
                    "yes"=, "Yes" =, "true" =, "True" =, "TRUE" = TRUE,
                    "no" =, "No" =, "false" =, "False" =, "FALSE" = FALSE,
                    ## default
                    errmsg("invalid value of ", field, " field in DESCRIPTION")
                    )
    }

    starsmsg <- function(stars, ...)
        message(stars, " ", ..., domain = NA)

    errmsg <- function(...)
    {
        message("ERROR: ", ..., domain = NA)
        do_exit_on_error()
    }

    pkgerrmsg <- function(msg, pkg)
    {
        message("ERROR: ", msg, " for package ", sQuote(pkg), domain = NA)
        do_exit_on_error()
    }

    ## 'pkg' is the absolute path to package sources.
    do_install <- function(pkg)
    {
        if (WINDOWS && grepl("\\.zip$", pkg)) {
            pkg_name <- basename(pkg)
            pkg_name <- sub("\\.zip$", "", pkg_name)
            pkg_name <- sub("_[0-9.-]+$", "", pkg_name)
            utils:::unpackPkgZip(pkg, pkg_name, lib, libs_only)
            return()
        }

        setwd(pkg)
        ## We checked this exists, but not that it is readable
        desc <- tryCatch(read.dcf(fd <- file.path(pkg, "DESCRIPTION")),
                         error = identity)
        if(inherits(desc, "error") || !length(desc))
            stop(gettextf("error reading file '%s'", fd),
                 domain = NA, call. = FALSE)
        desc <- desc[1L,]
        ## Let's see if we have a bundle
        if (!is.na(desc["Bundle"])) {
            stop("this seems to be a bundle -- and they are defunct")
        } else {
            pkg_name <- desc["Package"]
            if (is.na(pkg_name)) errmsg("no 'Package' field in 'DESCRIPTION'")
            curPkg <<- pkg_name
        }

        instdir <- file.path(lib, pkg_name)
        Sys.setenv(R_PACKAGE_NAME = pkg_name, R_PACKAGE_DIR = instdir)
        status <- .Rtest_package_depends_R_version()
        if (status) do_exit_on_error()

        dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        if (!dir.exists(instdir)) {
            message("ERROR: unable to create ", sQuote(instdir), domain = NA)
            do_exit_on_error()
        }

        if (!is_subdir(instdir, lib)) {
            message("ERROR: ", sQuote(pkg_name), " is not a legal package name",
                    domain = NA)
            do_exit_on_error()
        }

        ## Make sure we do not attempt installing to srcdir.
        owd <- setwd(instdir)
        if (owd == getwd()) pkgerrmsg("cannot install to srcdir", pkg_name)
        setwd(owd)

        ## Figure out whether this is a source or binary package.
        is_source_package <- is.na(desc["Built"])

        if (!is_first_package) cat("\n")

        if (is_source_package)
            do_install_source(pkg_name, instdir, pkg, desc)
        else
            do_install_binary(pkg_name, instdir, desc)

        ## Add read permission to all, write permission to owner
        ## If group-write permissions were requested, set them
        .Call(dirchmod, instdir, group.writable)
        is_first_package <<- FALSE

        if (tar_up) { # Unix only
            starsmsg(stars, "creating tarball")
            version <- desc["Version"]
            filename <- if (!grepl("darwin", R.version$os)) {
                paste0(pkg_name, "_", version, "_R_",
                       Sys.getenv("R_PLATFORM"), ".tar.gz")
            } else {
                paste0(pkg_name, "_", version,".tgz")
            }
            filepath <- file.path(startdir, filename)
            owd <- setwd(lib)
            res <- utils::tar(filepath, curPkg, compression = "gzip",
                              compression_level = 9L,
                              tar = Sys.getenv("R_INSTALL_TAR"))
            if (res)
                errmsg(sprintf("packaging into %s failed", sQuote(filename)))
            message("packaged installation of ",
                    sQuote(pkg_name), " as ",
                    sQuote(paste0(filename, ".gz")),
                    domain = NA)
            setwd(owd)
        }

        if (zip_up) { # Windows only
            starsmsg(stars, "MD5 sums")
            .installMD5sums(instdir)
            ## we could use utils::zip() here.
            ZIP <- "zip"                # Windows only
            version <- desc["Version"]
            filename <- paste0(pkg_name, "_", version, ".zip")
            filepath <- shQuote(file.path(startdir, filename))
            ## system(paste("rm -f", filepath))
            unlink(filepath)
            owd <- setwd(lib)
            res <- system(paste(shQuote(ZIP), "-r9Xq", filepath,
                                paste(curPkg, collapse = " ")))
            setwd(owd)
            if (res)
                message("running 'zip' failed", domain = NA)
            else
                message("packaged installation of ",
                        sQuote(pkg_name), " as ", filename, domain = NA)
        }
        if (Sys.getenv("_R_INSTALL_NO_DONE_") != "yes") {
            ## message("", domain = NA)  # ensure next starts on a new line, for R CMD check
            starsmsg(stars, "DONE (", pkg_name, ")")
        }

        curPkg <<- character()
    }


    ## Unix only
    do_install_binary <- function(pkg, instdir, desc)
    {
        starsmsg(stars, "installing *binary* package ", sQuote(pkg), " ...")

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            if (nzchar(lockdir))
                system(paste("mv", shQuote(instdir),
                             shQuote(file.path(lockdir, pkg))))
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }
        TAR <- Sys.getenv("TAR", 'tar')
        res <- system(paste("cp -R .", shQuote(instdir),
                            "|| (", TAR, "cd - .| (cd", shQuote(instdir), "&&", TAR, "-xf -))"
                            ))
        if (res) errmsg("installing binary package failed")

        if (tar_up) {
            starsmsg(stars, sQuote(pkg),
                     " was already a binary package and will not be rebuilt")
            tar_up <- FALSE
        }
    }

    ## to be run from package source directory
    run_clean <- function()
    {
        if (dir.exists("src") && length(dir("src", all.files = TRUE) > 2L)) {
            if (WINDOWS) archs <- c("i386", "x64")
            else {
                wd2 <- setwd(file.path(R.home("bin"), "exec"))
                archs <- Sys.glob("*")
                setwd(wd2)
            }
            if(length(archs))
                for(arch in archs) {
                    ss <- paste("src", arch, sep = "-")
                    ## it seems fixing permissions is sometimes needed
                    .Call(dirchmod, ss, group.writable)
                    unlink(ss, recursive = TRUE)
                }

            owd <- setwd("src")
            if (WINDOWS) {
                if (file.exists("Makefile.win"))
                    system(paste(MAKE, "-f Makefile.win clean"))
                else
                    unlink(c("Makedeps",
                             Sys.glob("*_res.rc"),
                             Sys.glob("*.[do]")))
                    # system("rm -f *_res.rc *.o *.d Makedeps")
            } else {
                if (file.exists("Makefile")) system(paste(MAKE, "clean"))
                else ## we will be using SHLIB --preclean
                    unlink(Sys.glob(paste0("*", SHLIB_EXT)))
            }
            setwd(owd)
        }
        if (WINDOWS) {
            if (file.exists("cleanup.win")) system("sh ./cleanup.win")
        } else if (file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Administration Manual'", call. = FALSE)

    }

    do_install_source <- function(pkg_name, instdir, pkg_dir, desc)
    {
        Sys.setenv("R_INSTALL_PKG" = pkg_name)
        on.exit(Sys.unsetenv("R_INSTALL_PKG"))
        shlib_install <- function(instdir, arch)
        {
            ## install.libs.R allows customization of the libs installation process
            if (file.exists("install.libs.R")) {
                message("installing via 'install.libs.R' to ", instdir,
                        domain = NA)
                ## the following variables are defined to be available,
                ## and to prevent abuse we don't expose anything else
                local.env <- local({ SHLIB_EXT <- SHLIB_EXT
                                     R_PACKAGE_DIR <- instdir
                                     R_PACKAGE_NAME <- pkg_name
                                     R_PACKAGE_SOURCE <- pkg_dir
                                     R_ARCH <- arch
                                     WINDOWS <- WINDOWS
                                     environment()})
                parent.env(local.env) <- .GlobalEnv
                source("install.libs.R", local = local.env)
                return(TRUE)
            }
            ## otherwise proceed with the default which is to just copy *${SHLIB_EXT}
            files <- Sys.glob(paste0("*", SHLIB_EXT))
            if (length(files)) {
                libarch <- if (nzchar(arch)) paste0("libs", arch) else "libs"
                dest <- file.path(instdir, libarch)
                message('installing to ', dest, domain = NA)
                dir.create(dest, recursive = TRUE, showWarnings = FALSE)
                file.copy(files, dest, overwrite = TRUE)
                ## not clear if this is still necessary, but sh version did so
		if (!WINDOWS)
		    Sys.chmod(file.path(dest, files),
			      if(group.writable) "775" else "755")
		## OS X does not keep debugging symbols in binaries
		## anymore so optionally we can create dSYMs. This is
		## important since we will blow away .o files so there
		## is no way to create it later.

		if (dsym && length(grep("^darwin", R.version$os)) ) {
		    message(gettextf("generating debug symbols (%s)",
                                     "dSYM"),
                            domain = NA)
		    dylib <- Sys.glob(paste0(dest, "/*", SHLIB_EXT))
                    for (file in dylib) system(paste0("dsymutil ", file))
		}

                if(config_val_to_logical(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_",
                                                    "TRUE"))
                   && file_test("-f", "symbols.rds")) {
                    file.copy("symbols.rds", dest)
                }
            }
        }

        ## This is only called for Makevars[.win], so assume it
        ## does create a shlib: not so reliably reported on Windows
        ## Note though that it may not create pkg_name.dll, and
        ## graph does not.
        run_shlib <- function(pkg_name, srcs, instdir, arch)
        {
            args <- c(shargs, "-o", paste0(pkg_name, SHLIB_EXT), srcs)
            if (WINDOWS && debug) args <- c(args, "--debug")
            if (debug) message("about to run ",
                               "R CMD SHLIB ", paste(args, collapse = " "),
                               domain = NA)
            if (.shlib_internal(args) == 0L) {
                if(WINDOWS) {
                    files <- Sys.glob(paste0("*", SHLIB_EXT))
                    if(!length(files)) return(TRUE)
                }
                shlib_install(instdir, arch)
                return(FALSE)
            } else return(TRUE)
        }

        ## Make the destination directories available to the developer's
        ## installation scripts (e.g. configure)
        Sys.setenv(R_LIBRARY_DIR = lib)

        if (nzchar(lib0)) {
            ## FIXME: is this needed?
            ## set R_LIBS to include the current installation directory
            rlibs <- Sys.getenv("R_LIBS")
            rlibs <- if (nzchar(rlibs)) paste(lib, rlibs, sep = .Platform$path.sep) else lib
            Sys.setenv(R_LIBS = rlibs)
            ## This is needed
            .libPaths(c(lib, .libPaths()))
        }

        Type <- desc["Type"]
        if (!is.na(Type) && Type == "Frontend") {
            if (WINDOWS) errmsg("'Frontend' packages are Unix-only")
            starsmsg(stars, "installing *Frontend* package ", sQuote(pkg_name), " ...")
            if (preclean) system(paste(MAKE, "clean"))
            if (use_configure) {
                if (file_test("-x", "configure")) {
                    res <- system(paste(paste(configure_vars, collapse = " "),
                                        "./configure",
                                        paste(configure_args, collapse = " ")))
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                } else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Administration Manual'")
            }
            if (file.exists("Makefile"))
                if (system(MAKE)) pkgerrmsg("make failed", pkg_name)
            if (clean) system(paste(MAKE, "clean"))
            return()
        }

        if (!is.na(Type) && Type == "Translation")
            errmsg("'Translation' packages are defunct")

        OS_type <- desc["OS_type"]
        if (WINDOWS) {
            if ((!is.na(OS_type) && OS_type == "unix") && !fake)
                errmsg(" Unix-only package")
        } else {
            if ((!is.na(OS_type) && OS_type == "windows") && !fake)
                errmsg(" Windows-only package")
        }

	if(group.writable) { ## group-write modes if requested:
	    fmode <- "664"
	    dmode <- "775"
	} else {
	    fmode <- "644"
	    dmode <- "755"
	}

        ## At this point we check that we have the dependencies we need.
        ## We cannot use installed.packages() as other installs might be
        ## going on in parallel

        pkgInfo <- .split_description(.read_description("DESCRIPTION"))
        pkgs <- unique(c(names(pkgInfo$Depends), names(pkgInfo$Imports),
                         names(pkgInfo$LinkingTo)))
        if (length(pkgs)) {
            miss <- character()
            for (pkg in pkgs) {
                if(!length(find.package(pkg, quiet = TRUE)))
                    miss <- c(miss, pkg)
            }
            if (length(miss) > 1)
                 pkgerrmsg(sprintf("dependencies %s are not available",
                                   paste(sQuote(miss), collapse = ", ")),
                           pkg_name)
            else if (length(miss))
                pkgerrmsg(sprintf("dependency %s is not available",
                                  sQuote(miss)), pkg_name)
         }

        starsmsg(stars, "installing *source* package ",
                 sQuote(pkg_name), " ...")

        stars <- "**"

        res <- checkMD5sums(pkg_name, getwd())
        if(!is.na(res) && res) {
            starsmsg(stars,
                     gettextf("package %s successfully unpacked and MD5 sums checked",
                              sQuote(pkg_name)))
        }

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            ## Back up a previous version
            if (nzchar(lockdir)) {
                if (debug) starsmsg(stars, "backing up earlier installation")
                if(WINDOWS) {
                    file.copy(instdir, lockdir, recursive = TRUE,
                              copy.date = TRUE)
                    if (more_than_libs) unlink(instdir, recursive = TRUE)
                } else if (more_than_libs)
                    system(paste("mv", shQuote(instdir),
                                 shQuote(file.path(lockdir, pkg_name))))
                else
                    file.copy(instdir, lockdir, recursive = TRUE,
                              copy.date = TRUE)
            } else if (more_than_libs) unlink(instdir, recursive = TRUE)
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }

        if (preclean) run_clean()

        if (use_configure) {
            if (WINDOWS) {
                if (file.exists("configure.win")) {
                    res <- system("sh ./configure.win")
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                } else if (file.exists("configure"))
                    message("\n",
                            "   **********************************************\n",
                            "   WARNING: this package has a configure script\n",
                            "         It probably needs manual configuration\n",
                            "   **********************************************\n\n", domain = NA)
            } else {
                ## FIXME: should these be quoted?
                if (file_test("-x", "configure")) {
                    cmd <- paste(paste(configure_vars, collapse = " "),
                                 "./configure",
                                 paste(configure_args, collapse = " "))
                    if (debug) message("configure command: ", sQuote(cmd),
                                       domain = NA)
                    ## in case the configure script calls SHLIB (some do)
                    cmd <- paste("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_=false",
                                 cmd)
                    res <- system(cmd)
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                }  else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Administration Manual'")
            }
        }


        if (more_than_libs) {
            for (f in c("NAMESPACE", "LICENSE", "LICENCE", "NEWS"))
                if (file.exists(f)) {
                    file.copy(f, instdir, TRUE)
		    Sys.chmod(file.path(instdir, f), fmode)
                }

            res <- try(.install_package_description('.', instdir))
            if (inherits(res, "try-error"))
                pkgerrmsg("installing package DESCRIPTION failed", pkg_name)
            if (!file.exists(namespace <- file.path(instdir, "NAMESPACE")) ) {
                if(dir.exists("R"))
                    errmsg("a 'NAMESPACE' file is required")
                else writeLines("## package without R code", namespace)
            }
        }

        if (install_libs && dir.exists("src") &&
            length(dir("src", all.files = TRUE) > 2L)) {
            starsmsg(stars, "libs")
            if (!file.exists(file.path(R.home("include"), "R.h")))
                ## maybe even an error?  But installing Fortran-based packages should work
                warning("R include directory is empty -- perhaps need to install R-devel.rpm or similar", call. = FALSE)
            has_error <- FALSE
            linkTo <- pkgInfo$LinkingTo
            if (!is.null(linkTo)) {
                lpkgs <- sapply(linkTo, function(x) x[[1L]])
                ## we checked that these were all available earlier,
                ## but be cautious in case this changed.
                paths <- find.package(lpkgs, quiet = TRUE)
                bpaths <- basename(paths)
                if (length(paths)) {
                    ## check any version requirements
                    have_vers <-
                        (vapply(linkTo, length, 1L) > 1L) & lpkgs %in% bpaths
                    for (z in linkTo[have_vers]) {
                        p <- z[[1L]]
                        path <- paths[bpaths %in% p]
                        current <- readRDS(file.path(path, "Meta", "package.rds"))$DESCRIPTION["Version"]
                        target <- as.numeric_version(z$version)
                        if (!do.call(z$op, list(as.numeric_version(current), target)))
                            stop(gettextf("package %s %s was found, but %s %s is required by %s",
                                          sQuote(p), current, z$op,
                                          target, sQuote(pkgname)),
                                 call. = FALSE, domain = NA)
                    }
                    clink_cppflags <- paste(paste0('-I"', paths, '/include"'),
                                            collapse = " ")
                    Sys.setenv(CLINK_CPPFLAGS = clink_cppflags)
                }
            } else clink_cppflags <- ""
            libdir <- file.path(instdir, paste0("libs", rarch))
            dir.create(libdir, showWarnings = FALSE)
            if (WINDOWS) {
                owd <- setwd("src")
                makefiles <- character()
                if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA))) {
                    if (file.exists(f))  makefiles <- f
                } else if (file.exists(f <- path.expand("~/.R/Makevars.win")))
                    makefiles <- f
                else if (file.exists(f <- path.expand("~/.R/Makevars")))
                    makefiles <- f
                if (file.exists("Makefile.win")) {
                    makefiles <- c("Makefile.win", makefiles)
                    message("  running 'src/Makefile.win' ...", domain = NA)
                    res <- system(paste("make --no-print-directory",
                                        paste("-f", shQuote(makefiles), collapse = " ")))
                    if (res == 0) shlib_install(instdir, rarch)
                    else has_error <- TRUE
                } else { ## no src/Makefile.win
                    srcs <- dir(pattern = "\\.([cfmM]|cc|cpp|f90|f95|mm)$",
                                all.files = TRUE)
                    archs <- if (!force_both && !grepl(" x64 ", win.version()))
                        "i386"
                    else {
                        ## see what is installed
                        ## NB, not R.home("bin")
                        f  <- dir(file.path(R.home(), "bin"))
                        f[f %in% c("i386", "x64")]
                    }
                    one_only <- !multiarch
                    if(!one_only && file.exists("../configure.win")) {
                        ## for now, hardcode some exceptions
                        ## These are packages which have arch-independent
                        ## code in configure.win
                        if(!pkg_name %in% c("AnalyzeFMRI", "CORElearn",
                                            "PearsonDS", "PKI", "RGtk2",
                                            "RNetCDF", "RODBC", "RSclient",
                                            "Rcpp", "Runuran", "SQLiteMap",
                                            "XML", "arulesSequences",
                                            "cairoDevice", "diversitree",
                                            "foreign", "fastICA", "glmnet",
                                            "gstat", "igraph", "jpeg", "png",
                                            "proj4", "randtoolbox", "rgdal",
                                            "rngWELL", "rphast", "rtfbs",
                                            "sparsenet", "tcltk2", "tiff",
                                            "udunits2"))
                            one_only <- sum(nchar(readLines("../configure.win", warn = FALSE), "bytes")) > 0
                        if(one_only && !force_biarch) {
                            if(parse_description_field(desc, "Biarch", FALSE))
                                force_biarch <- TRUE
                            else
                                warning("this package has a non-empty 'configure.win' file,\nso building only the main architecture\n", call. = FALSE, domain = NA)
                        }
                    }
                    if(force_biarch) one_only <- FALSE
                    if(one_only || length(archs) < 2L)
                        has_error <- run_shlib(pkg_name, srcs, instdir, rarch)
                    else {
                        setwd(owd)
                        test_archs <- archs
                        for(arch in archs) {
                            message("", domain = NA) # a blank line
                            starsmsg("***", "arch - ", arch)
                            ss <- paste("src", arch, sep = "-")
                            dir.create(ss, showWarnings = FALSE)
                            file.copy(Sys.glob("src/*"), ss, recursive = TRUE)
                            ## avoid read-only files/dir such as nested .svn
			    .Call(dirchmod, ss, group.writable)
                            setwd(ss)

                            ra <- paste0("/", arch)
                            Sys.setenv(R_ARCH = ra, R_ARCH_BIN = ra)
                            has_error <- run_shlib(pkg_name, srcs, instdir, ra)
                            setwd(owd)
                            if (has_error) break
                        }
                    }
                }
                setwd(owd)
            } else { # not WINDOWS
                if (file.exists("src/Makefile")) {
                    arch <- substr(rarch, 2, 1000)
                    starsmsg(stars, "arch - ", arch)
                    owd <- setwd("src")
                    system_makefile <-
                        file.path(R.home(), paste0("etc", rarch), "Makeconf")
                    site <- Sys.getenv("R_MAKEVARS_SITE", NA)
                    if (is.na(site)) site <- file.path(paste0(R.home("etc"), rarch), "Makevars.site")
                    makefiles <- c(system_makefile,
                                   if(file.exists(site)) site,
                                   "Makefile")
                    if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA))) {
                        if (file.exists(f))  makefiles <- c(makefiles, f)
                    } else if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                                                  Sys.getenv("R_PLATFORM"), sep = "-"))))
                        makefiles <- c(makefiles, f)
                    else if (file.exists(f <- path.expand("~/.R/Makevars")))
                        makefiles <- c(makefiles, f)
                    res <- system(paste(MAKE,
                                        paste("-f", shQuote(makefiles), collapse = " ")))
                    if (res == 0) shlib_install(instdir, rarch)
                    else has_error <- TRUE
                    setwd(owd)
                } else { ## no src/Makefile
                    owd <- setwd("src")
                    srcs <- dir(pattern = "\\.([cfmM]|cc|cpp|f90|f95|mm)$",
                                all.files = TRUE)
                    ## This allows Makevars to set OBJECTS or its own targets.
                    allfiles <- if (file.exists("Makevars")) c("Makevars", srcs) else srcs
                    wd2 <- setwd(file.path(R.home("bin"), "exec"))
                    archs <- Sys.glob("*")
                    setwd(wd2)
                    if (length(allfiles)) {
                        ## if there is a configure script we install only the main
                        ## sub-architecture
                        if (!multiarch || length(archs) <= 1 ||
                            file_test("-x", "../configure")) {
                            if (nzchar(rarch))
                                starsmsg("***", "arch - ",
                                         substr(rarch, 2, 1000))
                            has_error <- run_shlib(pkg_name, srcs, instdir, rarch)
                        } else {
                            setwd(owd)
                            test_archs <- archs
                            for(arch in archs) {
                                if (arch == "R") {
                                    ## top-level, so one arch without subdirs
                                    has_error <- run_shlib(pkg_name, srcs, instdir, "")
                                } else {
                                    starsmsg("***", "arch - ", arch)
                                    ss <- paste("src", arch, sep = "-")
                                    dir.create(ss, showWarnings = FALSE)
                                    file.copy(Sys.glob("src/*"), ss, recursive = TRUE)
                                    setwd(ss)
                                    ra <- paste0("/", arch)
                                    ## FIXME: do this lower down
                                    Sys.setenv(R_ARCH = ra)
                                    has_error <- run_shlib(pkg_name, srcs, instdir, ra)
                                    Sys.setenv(R_ARCH = rarch)
                                    setwd(owd)
                                    if (has_error) break
                                }
                            }
                        }
                    } else warning("no source files found", call. = FALSE)
                }
                setwd(owd)
            }
            if (has_error)
                pkgerrmsg("compilation failed", pkg_name)

            ## if we have subarchs, update DESCRIPTION
            fi <- file.info(Sys.glob(file.path(instdir, "libs", "*")))
            dirs <- basename(row.names(fi[fi$isdir %in% TRUE, ]))
            ## avoid DLLs installed by rogue packages
            if(WINDOWS) dirs <- dirs[dirs %in% c("i386", "x64")]
            if (length(dirs)) {
                descfile <- file.path(instdir, "DESCRIPTION")
                olddesc <- readLines(descfile, warn = FALSE)
                olddesc <- grep("^Archs:", olddesc,
                                invert = TRUE, value = TRUE, useBytes = TRUE)
                newdesc <- c(olddesc,
                             paste("Archs:", paste(dirs, collapse = ", "))
                             )
                writeLines(newdesc, descfile, useBytes = TRUE)
            }
        } else if (multiarch) {   # end of src dir
            if (WINDOWS) {
                wd2 <- setwd(file.path(R.home(), "bin")) # not R.home("bin")
                archs <- Sys.glob("*")
                setwd(wd2)
                test_archs <- archs[archs %in% c("i386", "x64")]
            } else {
                wd2 <- setwd(file.path(R.home("bin"), "exec"))
                test_archs <- Sys.glob("*")
                setwd(wd2)
            }
        }
        if (WINDOWS && "x64" %in% test_archs) {
            ## we cannot actually test x64 unless this is 64-bit
            ## Windows, even if it is installed.
            if (!grepl(" x64 ", win.version())) test_archs <- "i386"
        }


        ## R files must start with a letter
	if (install_R && dir.exists("R") && length(dir("R"))) {
	    starsmsg(stars, "R")
	    dir.create(file.path(instdir, "R"), recursive = TRUE,
		       showWarnings = FALSE)
	    ## This cannot be done in a C locale
	    res <- try(.install_package_code_files(".", instdir))
	    if (inherits(res, "try-error"))
		pkgerrmsg("unable to collate and parse R files", pkg_name)

	    if (file.exists(f <- file.path("R", "sysdata.rda"))) {
                comp <- TRUE
                ## (We set .libPaths)
                if(!is.na(lazycompress <- desc["SysDataCompression"])) {
                    comp <- switch(lazycompress,
                                   "none" = FALSE,
                                   "gzip" = TRUE,
                                   "bzip2" = 2L,
                                   "xz" = 3L,
                                   TRUE)  # default to gzip
                } else if(file.info(f)$size > 1e6) comp <- 3L # "xz"
		res <- try(sysdata2LazyLoadDB(f, file.path(instdir, "R"),
                                              compress = comp))
		if (inherits(res, "try-error"))
		    pkgerrmsg("unable to build sysdata DB", pkg_name)
	    }
	    if (fake) {
		## Fix up hook functions so they do not attempt to
		## (un)load missing compiled code, initialize ...
		## This does stop them being tested at all.
		if (file.exists("NAMESPACE")) {
		    cat("",
			'.onLoad <- .onAttach <- function(lib, pkg) NULL',
			'.onUnload <- function(libpaths) NULL',
			sep = "\n",
			file = file.path(instdir, "R", pkg_name), append = TRUE)
		    ## <NOTE>
		    ## Tweak fake installation to provide an 'empty'
		    ## useDynLib() for the time being.  Completely
		    ## removing the directive results in checkFF()
		    ## being too aggresive in the case where the
		    ## presence of the directive enables unambiguous
		    ## symbol resolution w/out 'PACKAGE' arguments.
		    ## However, empty directives are not really meant
		    ## to work ...

		    ## encoding issues ... so need useBytes = TRUE
		    ## FIXME: some packages have useDynLib()
		    ## spread over several lines.
		    writeLines(sub("useDynLib.*", 'useDynLib("")',
				   readLines("NAMESPACE", warn = FALSE),
				   perl = TRUE, useBytes = TRUE),
			       file.path(instdir, "NAMESPACE"))
		    ## </NOTE>
		} else {
		    cat("",
                        '.onLoad <- function (libname, pkgname) NULL',
                        '.onAttach <- function (libname, pkgname) NULL',
			'.onDetach <- function(libpath) NULL',
			'.onUnload <- function(libpath) NULL',
			'.Last.lib <- function(libpath) NULL',
			sep = "\n",
			file = file.path(instdir, "R", pkg_name), append = TRUE)
		}
	    }
	}                           # end of R

        ## data files must not be hidden: data() may ignore them
	if (install_data && dir.exists("data") && length(dir("data"))) {
	    starsmsg(stars, "data")
	    files <- Sys.glob(file.path("data", "*")) # ignores dotfiles
	    if (length(files)) {
		is <- file.path(instdir, "data")
		dir.create(is, recursive = TRUE, showWarnings = FALSE)
		file.remove(Sys.glob(file.path(instdir, "data", "*")))
		file.copy(files, is, TRUE)
		thislazy <- parse_description_field(desc, "LazyData",
						    default = lazy_data)
		if (!thislazy && resave_data) {
		    paths <- Sys.glob(c(file.path(is, "*.rda"),
					file.path(is, "*.RData")))
		    if (pkg_name == "cyclones")
			paths <-
			    c(paths, Sys.glob(file.path(is, "*.Rdata")))
		    if (length(paths)) {
			starsmsg(paste0(stars, "*"), "resaving rda files")
			resaveRdaFiles(paths, compress = "auto")
		    }
		}
		Sys.chmod(Sys.glob(file.path(instdir, "data", "*")), fmode)
		if (thislazy) {
		    starsmsg(paste0(stars, "*"),
                             "moving datasets to lazyload DB")
		    ## 'it is possible that data in a package will
		    ## make use of the code in the package, so ensure
		    ## the package we have just installed is on the
		    ## library path.'
		    ## (We set .libPaths)
                    lazycompress <- desc["LazyDataCompression"]
                    if(!is.na(lazycompress))
                        data_compress <- switch(lazycompress,
                                                "none" = FALSE,
                                                "gzip" = TRUE,
                                                "bzip2" = 2L,
                                                "xz" = 3L,
                                                TRUE)  # default to gzip
		    res <- try(data2LazyLoadDB(pkg_name, lib,
					       compress = data_compress))
		    if (inherits(res, "try-error"))
			pkgerrmsg("lazydata failed", pkg_name)
		}
	    } else warning("empty 'data' directory", call. = FALSE)
        }

        ## demos must start with a letter
	if (install_demo && dir.exists("demo") && length(dir("demo"))) {
	    starsmsg(stars, "demo")
	    dir.create(file.path(instdir, "demo"), recursive = TRUE,
		       showWarnings = FALSE)
	    file.remove(Sys.glob(file.path(instdir, "demo", "*")))
	    res <- try(.install_package_demos(".", instdir))
	    if (inherits(res, "try-error"))
		pkgerrmsg("ERROR: installing demos failed")
	    Sys.chmod(Sys.glob(file.path(instdir, "demo", "*")), fmode)
	}

        ## dotnames are ignored.
	if (install_exec && dir.exists("exec") && length(dir("exec"))) {
	    starsmsg(stars, "exec")
	    dir.create(file.path(instdir, "exec"), recursive = TRUE,
		       showWarnings = FALSE)
	    file.remove(Sys.glob(file.path(instdir, "exec", "*")))
	    files <- Sys.glob(file.path("exec", "*"))
	    if (length(files)) {
		file.copy(files, file.path(instdir, "exec"), TRUE)
                if (!WINDOWS)
		    Sys.chmod(Sys.glob(file.path(instdir, "exec", "*")), dmode)
	    }
	}

	if (install_inst && dir.exists("inst") &&
            length(dir("inst", all.files = TRUE)) > 2L) {
	    starsmsg(stars, "inst")
            i_dirs <- list.dirs("inst")[-1L] # not inst itself
            i_dirs <- grep(.vc_dir_names_re, i_dirs,
                           invert = TRUE, value = TRUE)
            ## This ignores any restrictive permissions in the source
            ## tree, since the later .Call(dirchmod) call will
            ## fix the permissions.

            ## handle .Rinstignore:
            ignore_file <- ".Rinstignore"
            ignore <- if (file.exists(ignore_file)) {
                ignore <- readLines(ignore_file, warn = FALSE)
                ignore[nzchar(ignore)]
            } else character()
            for(e in ignore)
                i_dirs <- grep(e, i_dirs, perl = TRUE, invert = TRUE,
                               value = TRUE, ignore.case = TRUE)
            lapply(gsub("^inst", instdir, i_dirs),
                   function(p) dir.create(p, FALSE, TRUE)) # be paranoid
            i_files <- list.files("inst", all.files = TRUE,
                                  full.names = TRUE, recursive = TRUE)
            i_files <- grep(.vc_dir_names_re, i_files,
                            invert = TRUE, value = TRUE)
            for(e in ignore)
                i_files <- grep(e, i_files, perl = TRUE, invert = TRUE,
                                value = TRUE, ignore.case = TRUE)
            i_files <- i_files[!i_files %in%
                               c("inst/doc/Rplots.pdf", "inst/doc/Rplots.ps")]
            i_files <- grep("inst/doc/.*[.](log|aux|bbl|blg|dvi)$",
                            i_files, perl = TRUE, invert = TRUE,
                            value = TRUE, ignore.case = TRUE)
            ## Temporary kludge
            if (!dir.exists("vignettes") && ! pkgname %in% c("RCurl"))
                i_files <- grep("inst/doc/.*[.](png|jpg|jpeg|gif|ps|eps)$",
                                i_files, perl = TRUE, invert = TRUE,
                                value = TRUE, ignore.case = TRUE)
            i_files <- i_files[! i_files %in% "Makefile"]
            i2_files <- gsub("^inst", instdir, i_files)
            file.copy(i_files, i2_files)
            if (!WINDOWS) {
                ## make executable if the source file was (for owner)
                modes <- file.info(i_files)$mode
                execs <- as.logical(modes & as.octmode("100"))
		Sys.chmod(i2_files[execs], dmode)
            }
            if (compact_docs) {
                pdfs <- dir(file.path(instdir, "doc"), pattern="\\.pdf",
                            recursive = TRUE, full.names = TRUE,
                            all.files = TRUE)
                res <- compactPDF(pdfs, gs_quality = "none")
                ## print selectively
                print(res[res$old > 1e5, ])
            }
	}

	if (install_tests && dir.exists("tests") &&
            length(dir("tests", all.files = TRUE) > 2L)) {
	    starsmsg(stars, "tests")
	    file.copy("tests", instdir, recursive = TRUE)
	}

	## LazyLoading/Compiling
	if (install_R && dir.exists("R") && length(dir("R"))) {
            BC <- if (!is.na(byte_compile)) byte_compile
            else
                parse_description_field(desc, "ByteCompile", default = FALSE)
            rcp <- as.numeric(Sys.getenv("R_COMPILE_PKGS"))
            BC <- BC || (!is.na(rcp) && rcp > 0)
            if (BC) {
                starsmsg(stars,
                         "byte-compile and prepare package for lazy loading")
                ## need to disable JIT
                Sys.setenv(R_ENABLE_JIT = 0L)
                compiler::compilePKGS(1L)
                compiler::setCompilerOptions(suppressUndefined = TRUE)
            } else
                starsmsg(stars, "preparing package for lazy loading")
            keep.source <-
                parse_description_field(desc, "KeepSource",
                                        default = keep.source)
	    ## Something above, e.g. lazydata,  might have loaded the namespace
	    if (pkg_name %in% loadedNamespaces())
		unloadNamespace(pkg_name)
            deps_only <-
                config_val_to_logical(Sys.getenv("_R_CHECK_INSTALL_DEPENDS_", "FALSE"))
            if(deps_only) {
                env <- setRlibs(LinkingTo = TRUE)
                libs0 <- .libPaths()
		env <- sub("^.*=", "", env[1L])
                .libPaths(c(lib0, env))
            } else libs0 <- NULL
	    res <- try({
                suppressPackageStartupMessages(.getRequiredPackages(quietly = TRUE))
                makeLazyLoading(pkg_name, lib, keep.source = keep.source)
            })
            if (BC) compiler::compilePKGS(0L)
	    if (inherits(res, "try-error"))
		pkgerrmsg("lazy loading failed", pkg_name)
            if (!is.null(libs0)) .libPaths(libs0)
	}

	if (install_help) {
	    starsmsg(stars, "help")
	    if (!dir.exists("man") ||
	       !length(list_files_with_type("man", "docs")))
		cat("No man pages found in package ", sQuote(pkg_name), "\n")
	    encoding <- desc["Encoding"]
	    if (is.na(encoding)) encoding <- "unknown"
	    res <- try(.install_package_Rd_objects(".", instdir, encoding))
	    if (inherits(res, "try-error"))
		pkgerrmsg("installing Rd objects failed", pkg_name)


	    starsmsg(paste0(stars, "*"), "installing help indices")
	    ## always want HTML package index
	    .writePkgIndices(pkg_dir, instdir)
	    if (build_help) {
		## This is used as the default outputEncoding for latex
		outenc <- desc["Encoding"]
		if (is.na(outenc)) outenc <- "latin1" # or ASCII
		.convertRdfiles(pkg_dir, instdir,
				types = build_help_types,
				outenc = outenc)
	    }
	    if (file_test("-d", figdir <- file.path(pkg_dir, "man", "figures"))) {
		starsmsg(paste0(stars, "*"), "copying figures")
		dir.create(destdir <- file.path(instdir, "help", "figures"))
		file.copy(Sys.glob(c(file.path(figdir, "*.png"),
		                     file.path(figdir, "*.jpg"),
		                     file.path(figdir, "*.jpeg"),
				     file.path(figdir, "*.svg"),
				     file.path(figdir, "*.pdf"))), destdir)
	    }
        }

	## pkg indices: this also tangles the vignettes (if installed)
	if (install_inst || install_demo || install_help) {
	    starsmsg(stars, "building package indices")
	    res <- try(.install_package_indices(".", instdir))
	    if (inherits(res, "try-error"))
		errmsg("installing package indices failed")
            if(file_test("-d", "vignettes")) {
                starsmsg(stars, "installing vignettes")
                enc <- desc["Encoding"]
                if (is.na(enc)) enc <- ""
		if (!fake &&
                    file_test("-f", file.path("build", "vignette.rds")))
		    installer <- .install_package_vignettes3
		## FIXME:  this handles pre-3.0.2 tarballs.  In the long run, delete the alternative.
		else
		    installer <- .install_package_vignettes2
                res <- try(installer(".", instdir, enc))
	    if (inherits(res, "try-error"))
		errmsg("installing vignettes failed")
            }
	}

	## Install a dump of the parsed NAMESPACE file
	if (install_R && file.exists("NAMESPACE") && !fake) {
	    res <- try(.install_package_namespace_info(".", instdir))
	    if (inherits(res, "try-error"))
		errmsg("installing namespace metadata failed")
	}

        if (clean) run_clean()

        if (test_load) {
            ## Do this in a separate R process, in case it crashes R.
	    starsmsg(stars, "testing if installed package can be loaded")
            ## FIXME: maybe the quoting as 'lib' is not quite good enough
            ## On a Unix-alike this calls system(input=)
            ## and that uses a temporary file and redirection.
            cmd <- paste0("tools:::.test_load_package('", pkg_name, "', '", lib, "')")
            ## R_LIBS was set already.  R_runR is in check.R
            deps_only <-
                config_val_to_logical(Sys.getenv("_R_CHECK_INSTALL_DEPENDS_", "FALSE"))
            env <- if (deps_only) setRlibs(lib0, self = TRUE, quote = TRUE) else ""
            if (length(test_archs) > 1L) {
                msgs <- character()
                opts <- "--no-save --slave"
                for (arch in test_archs) {
                    starsmsg("***", "arch - ", arch)
                    out <- R_runR(cmd, opts, env = env, arch = arch)
                    if(length(attr(out, "status")))
                        msgs <- c(msgs, arch)
                    if(length(out))
                        cat(paste(c(out, ""), collapse = "\n"))
                }
                if (length(msgs)) {
                    msg <- paste("loading failed for",
                                 paste(sQuote(msgs), collapse = ", "))
                    errmsg(msg) # does not return
                }
            } else {
                opts <- if (deps_only) "--vanilla --slave"
                else "--no-save --slave"
                out <- R_runR(cmd, opts, env = env)
                if(length(out))
                    cat(paste(c(out, ""), collapse = "\n"))
                if(length(attr(out, "status")))
                    errmsg("loading failed") # does not return
            }
        }
    }

    options(showErrorCalls=FALSE)
    pkgs <- character()
    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse = " ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }
    args0 <- args

    startdir <- getwd()
    if (is.null(startdir))
        stop("current working directory cannot be ascertained")
    lib <- lib0 <- ""
    clean <- FALSE
    preclean <- FALSE
    debug <- FALSE
    static_html <- nzchar(system.file("html", "mean.html", package="base"))
    build_html <- static_html
    build_latex <- FALSE
    build_example <- FALSE
    use_configure <- TRUE
    auto_zip <- FALSE
    configure_args <- character()
    configure_vars <- character()
    fake <- FALSE
    lazy <- TRUE
    lazy_data <- FALSE
    byte_compile <- NA # means take from DESCRIPTION file.
    ## Next is not very useful unless R CMD INSTALL reads a startup file
    lock <- getOption("install.lock", NA) # set for overall or per-package
    pkglock <- FALSE  # set for per-package locking
    libs_only <- FALSE
    tar_up <- zip_up <- FALSE
    shargs <- character()
    multiarch <- TRUE
    force_biarch <- FALSE
    force_both <- FALSE
    test_load <- TRUE
    merge <- FALSE
    dsym <- nzchar(Sys.getenv("PKG_MAKE_DSYM"))
    get_user_libPaths <- FALSE
    data_compress <- TRUE # FALSE (none), TRUE (gzip), 2 (bzip2), 3 (xz)
    resave_data <- FALSE
    compact_docs <- FALSE
    keep.source <- getOption("keep.source.pkgs")

    install_libs <- TRUE
    install_R <- TRUE
    install_data <- TRUE
    install_demo <- TRUE
    install_exec <- TRUE
    install_inst <- TRUE
    install_help <- TRUE
    install_tests <- FALSE

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package installer: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2013 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            q("no", runLast = FALSE)
        } else if (a %in% c("-c", "--clean")) {
            clean <- TRUE
            shargs <- c(shargs, "--clean")
        } else if (a == "--preclean") {
            preclean <- TRUE
            shargs <- c(shargs, "--preclean")
        } else if (a %in% c("-d", "--debug")) {
            debug <- TRUE
        } else if (a == "--no-configure") {
            use_configure <- FALSE
        } else if (a == "--no-docs") {
            build_html <- build_latex <- build_example <- FALSE
        } else if (a == "--no-html") {
            build_html <- FALSE
        } else if (a == "--html") {
            build_html <- TRUE
        } else if (a == "--latex") {
            build_latex <- TRUE
        } else if (a == "--example") {
            build_example <- TRUE
        } else if (a == "--use-zip-data") {
            warning("use of '--use-zip-data' is defunct",
                    call. = FALSE, domain = NA)
            warning("use of '--use-zip-data' is deprecated",
                    call. = FALSE, domain = NA)
        } else if (a == "--auto-zip") {
            warning("'--auto-zip' is defunct",
                           call. = FALSE, domain = NA)
        } else if (a == "-l") {
            if (length(args) >= 2L) {lib <- args[2L]; args <- args[-1L]}
            else stop("-l option without value", call. = FALSE)
        } else if (substr(a, 1, 10) == "--library=") {
            lib <- substr(a, 11, 1000)
        } else if (substr(a, 1, 17) == "--configure-args=") {
            configure_args <- c(configure_args, substr(a, 18, 1000))
        } else if (substr(a, 1, 17) == "--configure-vars=") {
            configure_vars <- c(configure_vars, substr(a, 18, 1000))
        } else if (a == "--fake") {
            fake <- TRUE
        } else if (a == "--no-lock") {
            lock <- pkglock <- FALSE
        } else if (a == "--lock") {
            lock <- TRUE; pkglock <- FALSE
        } else if (a == "--pkglock") {
            lock <- pkglock <- TRUE
        } else if (a == "--libs-only") {
            libs_only <- TRUE
        } else if (a == "--no-multiarch") {
            multiarch <- FALSE
        } else if (a == "--force-biarch") {
            force_biarch <- TRUE
        } else if (a == "--compile-both") {
            force_both <- TRUE
        } else if (a == "--maybe-get-user-libPaths") {
            get_user_libPaths <- TRUE
        } else if (a == "--build") {
            if (WINDOWS) zip_up <- TRUE else tar_up <- TRUE
        } else if (substr(a, 1, 16) == "--data-compress=") {
            dc <- substr(a, 17, 1000)
            dc <- match.arg(dc, c("none", "gzip", "bzip2", "xz"))
            data_compress <- switch(dc,
                                    "none" = FALSE,
                                    "gzip" = TRUE,
                                    "bzip2" = 2,
                                    "xz" = 3)
        } else if (a == "--resave-data") {
            resave_data <- TRUE
        } else if (a == "--install-tests") {
            install_tests <- TRUE
        } else if (a == "--no-inst") {
            install_inst <- FALSE
        } else if (a == "--no-R") {
            install_R <- FALSE
        } else if (a == "--no-libs") {
            install_libs <- FALSE
        } else if (a == "--no-data") {
            install_data <- FALSE
        } else if (a == "--no-demo") {
            install_demo <- FALSE
        } else if (a == "--no-exec") {
            install_exec <- FALSE
        } else if (a == "--no-help") {
            install_help <- FALSE
        } else if (a == "--no-test-load") {
            test_load <- FALSE
        } else if (a == "--no-clean-on-error") {
            clean_on_error  <- FALSE
        } else if (a == "--merge-multiarch") {
            merge <- TRUE
        } else if (a == "--compact-docs") {
            compact_docs <- TRUE
        } else if (a == "--with-keep.source") {
            keep.source <- TRUE
        } else if (a == "--without-keep.source") {
            keep.source <- FALSE
        } else if (a == "--byte-compile") {
            byte_compile <- TRUE
        } else if (a == "--no-byte-compile") {
            byte_compile <- FALSE
        } else if (a == "--dsym") {
            dsym <- TRUE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a), domain = NA)
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }

    tmpdir <- tempfile("R.INSTALL")
    if (!dir.create(tmpdir))
        stop("cannot create temporary directory")

    if (merge) {
        if (length(pkgs) != 1L || !file_test("-f", pkgs))
            stop("ERROR: '--merge-multiarch' applies only to a single tarball",
                 call. = FALSE)
        if (WINDOWS) {
            f  <- dir(file.path(R.home(), "bin"))
            archs <- f[f %in% c("i386", "x64")]
            if (length(archs) > 1L) {
                args <- args0[! args0 %in% c("--merge-multiarch", "--build")]
                ## this will report '* DONE (foo)' if it works, which
                ## R CMD check treats as an indication of success.
                ## so use a backdoor to suppress it.
                Sys.setenv("_R_INSTALL_NO_DONE_" = "yes")
                for (arch in archs) {
                    cmd <- c(file.path(R.home(), "bin", arch, "Rcmd.exe"),
                             "INSTALL", args, "--no-multiarch")
                    if (arch == "x64") {
                        cmd <- c(cmd, "--libs-only", if(zip_up) "--build")
                        Sys.unsetenv("_R_INSTALL_NO_DONE_")
                    }
                    cmd <- paste(cmd, collapse = " ")
                    if (debug) message("about to run ", cmd, domain = NA)
                    message("\n", "install for ", arch, "\n", domain = NA)
                    res <- system(cmd)
                    if(res) break
                }
            }
        } else {
            archs  <- dir(file.path(R.home("bin"), "exec"))
            if (length(archs) > 1L) {
                args <- args0[! args0 %in% c("--merge-multiarch", "--build")]
                ## this will report '* DONE (foo)' if it works, which
                ## R CMD check treats as an indication of success.
                ## so use a backdoor to suppress it.
                Sys.setenv("_R_INSTALL_NO_DONE_" = "yes")
                last <- archs[length(archs)]
                for (arch in archs) {
                    cmd <- c(file.path(R.home("bin"), "R"),
                             "--arch", arch, "CMD",
                             "INSTALL", args, "--no-multiarch")
                    if (arch != archs[1L]) cmd <- c(cmd, "--libs-only")
                    if (arch == last) {
                        Sys.unsetenv("_R_INSTALL_NO_DONE_")
                        if(tar_up) cmd <- c(cmd, "--build")
                    }
                    cmd <- paste(cmd, collapse = " ")
                    if (debug) message("about to run ", cmd, domain = NA)
                    message("\n", "install for ", arch, "\n", domain = NA)
                    res <- system(cmd)
                    if(res) break
                }
            }
        }
        if (length(archs) > 1L) {
            if (res) do_exit_on_error()
            do_cleanup()
            on.exit()
            return(invisible())
        }
        message("only one architecture so ignoring '--merge-multiarch'",
                domain = NA)
    }

    ## now unpack tarballs and do some basic checks
    allpkgs <- character()
    for(pkg in pkgs) {
        if (debug) message("processing ", sQuote(pkg), domain = NA)
        if (file_test("-f", pkg)) {
            if (WINDOWS && grepl("\\.zip$", pkg)) {
                if (debug) message("a zip file", domain = NA)
                pkgname <- basename(pkg)
                pkgname <- sub("\\.zip$", "", pkgname)
                pkgname <- sub("_[0-9.-]+$", "", pkgname)
                allpkgs <- c(allpkgs, pkg)
                next
            }
            if (debug) message("a file", domain = NA)
            of <- dir(tmpdir, full.names = TRUE)
            ## force the use of internal untar unless over-ridden
            ## so e.g. .tar.xz works everywhere
            if (utils::untar(pkg, exdir = tmpdir,
                             tar =  Sys.getenv("R_INSTALL_TAR", "internal")))
                errmsg("error unpacking tarball")
            ## Now see what we got
            nf <- dir(tmpdir, full.names = TRUE)
            new <- nf[!nf %in% of]
            if (!length(new))
                errmsg("cannot extract package from ", sQuote(pkg))
            if (length(new) > 1L)
                errmsg("extracted multiple files from ", sQuote(pkg))
            if (file.info(new)$isdir) pkgname <- basename(new)
            else errmsg("cannot extract package from ", sQuote(pkg))

            ## If we have a binary bundle distribution, there should
            ## be a DESCRIPTION file at top level. These are defunct
            if (file.exists(ff <- file.path(tmpdir, "DESCRIPTION"))) {
                con <- read.dcf(ff, "Contains")
                if (!is.na(con))
                    message("looks like a binary bundle", domain = NA)
                else
                    message("unknown package layout", domain = NA)
                do_cleanup_tmpdir()
                q("no", status = 1, runLast = FALSE)
            } else if (file.exists(file.path(tmpdir, pkgname, "DESCRIPTION"))) {
                allpkgs <- c(allpkgs, file.path(tmpdir, pkgname))
            } else errmsg("cannot extract package from ", sQuote(pkg))
        } else if (file.exists(file.path(pkg, "DESCRIPTION"))) {
            if (debug) message("a directory", domain = NA)
            pkgname <- basename(pkg)
            allpkgs <- c(allpkgs, fullpath(pkg))
        } else {
            warning("invalid package ", sQuote(pkg), call. = FALSE)
            next
        }
    }

    if (!length(allpkgs))
        stop("ERROR: no packages specified", call.=FALSE)


    if (!nzchar(lib)) {
        lib <- if (get_user_libPaths) { ## need .libPaths()[1L] *after* the site- and user-initialization
	    system(paste(file.path(R.home("bin"), "Rscript"),
                         "-e 'cat(.libPaths()[1L])'"),
                   intern = TRUE)
        }
        else .libPaths()[1L]
        starsmsg(stars, "installing to library ", sQuote(lib))
    } else {
        lib0 <- lib <- path.expand(lib)
        ## lib is allowed to be a relative path.
        ## should be OK below, but be sure.
        cwd <- tryCatch(setwd(lib), error = function(e)
                        stop(gettextf("ERROR: cannot cd to directory %s", sQuote(lib)),
                             call. = FALSE, domain = NA))
        lib <- getwd()
        setwd(cwd)
    }
    ok <- dir.exists(lib)
    if (ok) {
        if (WINDOWS) {
            ## file.access is unreliable on Windows
            ## the only known reliable way is to try it
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), sep = "_"))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) ok <- FALSE
            else unlink(fn, recursive = TRUE)
        } else ok <- file.access(lib, 2L) == 0
    }
    if (!ok)
        stop("ERROR: no permission to install to directory ",
             sQuote(lib), call. = FALSE)

    group.writable <- if(WINDOWS) FALSE else {
	## install package group-writable  iff  in group-writable lib
	m <- file.info(lib)$mode
	(m & "020") == as.octmode("020") ## TRUE  iff  g-bit is "w"
    }

    if (libs_only) {
	install_R <- FALSE
	install_data <- FALSE
	install_demo <- FALSE
	install_exec <- FALSE
	install_inst <- FALSE
	install_help <- FALSE
    }
    more_than_libs <- !libs_only
    ## if(!WINDOWS && !more_than_libs) test_load <- FALSE


    mk_lockdir <- function(lockdir)
    {
        if (file.exists(lockdir)) {
            message("ERROR: failed to lock directory ", sQuote(lib),
                    " for modifying\nTry removing ", sQuote(lockdir),
                    domain = NA)
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        dir.create(lockdir, recursive = TRUE)
        if (!dir.exists(lockdir)) {
            message("ERROR: failed to create lock directory ", sQuote(lockdir),
                    domain = NA)
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        if (debug) starsmsg(stars, "created lock directory ", sQuote(lockdir))
    }

    if (is.na(lock)) {
        lock <- TRUE
        pkglock <- length(allpkgs) == 1L
    }
    if (lock && !pkglock) {
        lockdir <- file.path(lib, "00LOCK")
        mk_lockdir(lockdir)
    }

    if  ((tar_up || zip_up) && fake)
        stop("building a fake installation is disallowed")

    if (fake) {
        use_configure <- FALSE
        build_html <- FALSE
        build_latex <- FALSE
        build_example <- FALSE
	install_libs <- FALSE
	install_demo <- FALSE
	install_exec <- FALSE
#	install_inst <- FALSE
    }

    build_help_types <- character()
    if (build_html) build_help_types <- c(build_help_types, "html")
    if (build_latex) build_help_types <- c(build_help_types, "latex")
    if (build_example) build_help_types <- c(build_help_types, "example")
    build_help <- length(build_help_types) > 0L

    if (debug)
        starsmsg(stars, "build_help_types=",
                 paste(build_help_types, collapse = " "))

    if (debug)
        starsmsg(stars, "DBG: 'R CMD INSTALL' now doing do_install()")

    for(pkg in allpkgs) {
        if (pkglock) {
            lockdir <- file.path(lib, paste("00LOCK", basename(pkg), sep = "-"))
            mk_lockdir(lockdir)
        }
        do_install(pkg)
    }
    do_cleanup()
    on.exit()
    invisible()
} ## .install_packages()

## for R CMD SHLIB on all platforms
.SHLIB <- function()
{
    status <- .shlib_internal(commandArgs(TRUE))
    q("no", status = (status != 0), runLast=FALSE)
}

## for .SHLIB and R CMD INSTALL on all platforms
.shlib_internal <- function(args)
{
    Usage <- function()
        cat("Usage: R CMD SHLIB [options] files | linker options",
            "",
            "Build a shared object for dynamic loading from the specified source or",
            "object files (which are automagically made from their sources) or",
            "linker options.  If not given via '--output', the name for the shared",
            "object is determined from the first source or object file.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -o, --output=LIB	use LIB as (full) name for the built library",
            "  -c, --clean		remove files created during compilation",
            "  --preclean		remove files created during a previous run",
            "  -n, --dry-run		dry run, showing commands that would be used",
            "",
            "Windows only:",
            "  -d, --debug		build a debug DLL",
            "",
            "Report bugs at bugs@r-project.org .",
            sep = "\n")

    ## FIXME shQuote here?
    p1 <- function(...) paste(..., collapse = " ")

    WINDOWS <- .Platform$OS.type == "windows"
    if (!WINDOWS) {
        mconf <- readLines(file.path(R.home(),
                                     paste0("etc", Sys.getenv("R_ARCH")),
                                     "Makeconf"))
        SHLIB_EXT <- sub(".*= ", "", grep("^SHLIB_EXT", mconf, value = TRUE,
                                          perl = TRUE))
        SHLIB_LIBADD <- sub(".*= ", "", grep("^SHLIB_LIBADD", mconf,
                                             value = TRUE, perl = TRUE))
        MAKE <- Sys.getenv("MAKE")
        rarch <- Sys.getenv("R_ARCH")
    } else {
        rhome <- chartr("\\", "/", R.home())
        Sys.setenv(R_HOME = rhome)
        SHLIB_EXT <- ".dll"
        SHLIB_LIBADD <- ""
        MAKE <- "make"
        ## Formerly for winshlib.mk to pick up Makeconf
        rarch <- Sys.getenv("R_ARCH", NA)
        if(is.na(rarch)) {
            if (nzchar(.Platform$r_arch)) {
                rarch <- paste0("/", .Platform$r_arch)
                Sys.setenv(R_ARCH = rarch)
            } else rarch <- ""
        }
    }

    OBJ_EXT <- ".o" # all currrent compilers, but not some on Windows

    objs <- character()
    shlib <- ""
    site <- Sys.getenv("R_MAKEVARS_SITE", NA)
    if (is.na(site))
        site <- file.path(paste0(R.home("etc"), rarch), "Makevars.site")
    makefiles <-
        c(file.path(paste0(R.home("etc"), rarch), "Makeconf"),
          if(file.exists(site)) site,
          file.path(R.home("share"), "make",
                    if (WINDOWS) "winshlib.mk" else "shlib.mk"))
    shlib_libadd <- if (nzchar(SHLIB_LIBADD)) SHLIB_LIBADD else character()
    with_cxx <- FALSE
    with_f77 <- FALSE
    with_f9x <- FALSE
    with_objc <- FALSE
    use_cxx1x <- FALSE
    pkg_libs <- character()
    clean <- FALSE
    preclean <- FALSE
    dry_run <- FALSE
    debug <- FALSE

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            return(0L)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R shared object builder: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2013 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            return(0L)
        } else if (a %in% c("-n", "--dry-run")) {
            dry_run <- TRUE
        } else if (a %in% c("-d", "--debug")) {
            debug <- TRUE
        } else if (a %in% c("-c", "--clean")) {
            clean <- TRUE
        } else if (a == "--preclean") {
            preclean <- TRUE
        } else if (a == "-o") {
            if (length(args) >= 2L) {shlib <- args[2L]; args <- args[-1L]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            shlib <- substr(a, 10, 1000)
        } else {
            ## a source file or something like -Ldir -lfoo
            base <- sub("\\.[[:alnum:]]*$", "", a)
            ext <- sub(paste0(base, "."),  "", a, fixed = TRUE)
            nobj <- ""
            if (nzchar(ext)) {
                if (ext %in% c("cc", "cpp")) {
                    with_cxx <- TRUE
                    nobj <- base
                } else if (ext == "m") {
                    with_objc <- TRUE
                    nobj <- base
                } else if (ext %in% c("mm", "M")) {
                    ## ObjC++ implies ObjC because we need ObjC runtime
                    ## ObjC++ implies C++ because we use C++ linker
                    with_objc <- with_cxx <- TRUE
                    nobj <- base
                } else if (ext == "f") {
                    with_f77 <- TRUE
                    nobj <- base
                } else if (ext %in% c("f90", "f95")) {
                    with_f9x <- TRUE
                    nobj <- base
                } else if (ext == "c") {
                    nobj <- base
                } else if (ext == "o") {
                    nobj <- base
                }
                if (nzchar(nobj) && !nzchar(shlib))
                    shlib <- paste0(nobj, SHLIB_EXT)
            }
            if (nzchar(nobj)) objs <- c(objs, nobj)
            else pkg_libs <- c(pkg_libs, a)
        }
        args <- args[-1L]
    }

    if (length(objs)) objs <- paste0(objs, OBJ_EXT, collapse = " ")

    if (WINDOWS) {
        if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA))) {
            if (file.exists(f))  makefiles <- c(makefiles, f)
        } else if (rarch == "/x64" &&
                   file.exists(f <- path.expand("~/.R/Makevars.win64")))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars.win")))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    } else {
        if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA))) {
            if (file.exists(f))  makefiles <- c(makefiles, f)
        } else if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                               Sys.getenv("R_PLATFORM"),
                                               sep = "-"))))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    }

    makeobjs <- paste0("OBJECTS=", shQuote(objs))
    if (WINDOWS && file.exists("Makevars.win")) {
        makefiles <- c("Makevars.win", makefiles)
        lines <- readLines("Makevars.win", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl=TRUE, useBytes = TRUE)))
            makeobjs <- ""
        if (length(ll <- grep("^USE_CXX1X *=", lines, perl = TRUE,
                              value = TRUE, useBytes = TRUE))) {
            use_cxx1x <- TRUE
            cxx1xstd <- sub("^USE_CXX1X *= *", "", ll)
            if(!nzchar(cxx1xstd)) cxx1xstd <- "$(CXX1XSTD)"
        }
    } else if (file.exists("Makevars")) {
        makefiles <- c("Makevars", makefiles)
        lines <- readLines("Makevars", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl = TRUE, useBytes = TRUE)))
            makeobjs <- ""
        if (length(ll <- grep("^USE_CXX1X *=", lines, perl = TRUE,
                              value = TRUE, useBytes = TRUE))) {
            use_cxx1x <- TRUE
            cxx1xstd <- sub("^USE_CXX1X *= *", "", ll)
            if(!nzchar(cxx1xstd)) cxx1xstd <- "$(CXX1XSTD)"
        }
    }

    makeargs <- paste0("SHLIB=", shQuote(shlib))
    if (with_f9x) {
        makeargs <- c("SHLIB_LDFLAGS='$(SHLIB_FCLDFLAGS)'",
                      "SHLIB_LD='$(SHLIB_FCLD)'", makeargs)
    } else if (with_cxx) {
        makeargs <- if (use_cxx1x)
            c(sprintf("CXX='$(CXX1X) %s'", cxx1xstd),
              "CXXFLAGS='$(CXX1XFLAGS)'",
              "CXXPICFLAGS='$(CXX1XPICFLAGS)'",
              "SHLIB_LDFLAGS='$(SHLIB_CXX1XLDFLAGS)'",
              "SHLIB_LD='$(SHLIB_CXX1XLD)'", makeargs)
        else
            c("SHLIB_LDFLAGS='$(SHLIB_CXXLDFLAGS)'",
              "SHLIB_LD='$(SHLIB_CXXLD)'", makeargs)
    }
    if (with_objc) shlib_libadd <- c(shlib_libadd, "$(OBJC_LIBS)")
    if (with_f77) shlib_libadd <- c(shlib_libadd, "$(FLIBS)")
    if (with_f9x) shlib_libadd <- c(shlib_libadd, "$(FCLIBS)")

    if (length(pkg_libs))
        makeargs <- c(makeargs,
                      paste0("PKG_LIBS='", p1(pkg_libs), "'"))
    if (length(shlib_libadd))
        makeargs <- c(makeargs,
                      paste0("SHLIB_LIBADD='", p1(shlib_libadd), "'"))

    if (WINDOWS && debug) makeargs <- c(makeargs, "DEBUG=T")
    ## TCLBIN is needed for tkrplot and tcltk2
    if (WINDOWS && rarch == "/x64") makeargs <- c(makeargs, "WIN=64 TCLBIN=64")

    build_objects_symbol_tables <-
        config_val_to_logical(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_",
                                         "FALSE"))

    cmd <- paste(MAKE, p1(paste("-f", shQuote(makefiles))), p1(makeargs),
                 p1(makeobjs))
    if (dry_run) {
        cat("make cmd is\n  ", cmd, "\n\nmake would use\n", sep = "")
        system(paste(cmd, "-n"))
        res <- 0
    } else {
        if (preclean) system(paste(cmd, "shlib-clean"))
        res <- system(cmd)
        if(build_objects_symbol_tables) {
            ## Should only do this if the previous one went ok.
            system(paste(cmd, "symbols.rds"))
        }
        if (clean) system(paste(cmd, "shlib-clean"))
    }
    res # probably a multiple of 256
}


## called for base packages from src/Makefile[.win] and from
## .install.packages in this file.  Really *help* indices.
.writePkgIndices <-
    function(dir, outDir, OS = .Platform$OS.type, html = TRUE)
{
    re <- function(x)
    {
        ## sort order for topics, a little tricky
        ## FALSE sorts before TRUE
        xx <- rep(TRUE, length(x))
        xx[grep("-package", x, fixed = TRUE)] <- FALSE
        order(xx, toupper(x), x)
    }

    html_header <- function(pkg, title, version, conn)
    {
        cat(paste(HTMLheader(title, Rhome="../../..",
                             up="../../../doc/html/packages.html",
                             css = "R.css"),
                  collapse = "\n"),
           '<h2>Documentation for package &lsquo;', pkg, '&rsquo; version ',
            version, '</h2>\n\n', sep = "", file = conn)

	cat('<ul><li><a href="../DESCRIPTION">DESCRIPTION file</a>.</li>\n', file=conn)
	if (file.exists(file.path(outDir, "doc")))
	    cat('<li><a href="../doc/index.html">User guides, package vignettes and other documentation.</a></li>\n', file=conn)
	if (file.exists(file.path(outDir, "demo")))
	    cat('<li><a href="../demo">Code demos</a>.  Use <a href="../../utils/help/demo">demo()</a> to run them.</li>\n',
		 sep = "", file=conn)
	if (any(file.exists(c(file.path(outDir, "NEWS"), file.path(outDir, "NEWS.Rd")))))
	    cat('<li><a href="../NEWS">Package NEWS</a>.</li>\n',
		 sep = "", file=conn)

        cat('</ul>\n\n<h2>Help Pages</h2>\n\n\n',
            sep ="", file = conn)
    }

    firstLetterCategory <- function(x)
    {
        x[grep("-package$", x)] <- " "
        x <- toupper(substr(x, 1, 1))
        x[x > "Z"] <- "misc"
        x[x < "A" & x != " "] <- "misc"
        x
    }

    ## This may well already have been done:
    Rd <- if (file.exists(f <- file.path(outDir, "Meta", "Rd.rds")))
        readRDS(f)
    else {
        ## Keep this in sync with .install_package_Rd_indices().
        ## Rd objects should already have been installed.
        db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                       error = function(e) NULL)
        ## If not, we build the Rd db from the sources:
        if (is.null(db)) db <- Rd_db(dir = dir)
        Rd <- Rd_contents(db)
        saveRDS(Rd, file.path(outDir, "Meta", "Rd.rds"))
        Rd
    }

    topics <- Rd$Aliases
    M <- if (!length(topics)) {
        data.frame(Topic = character(),
                   File = character(),
                   Title = character(),
                   Internal = character(),
                   stringsAsFactors = FALSE)
    } else {
        lens <- sapply(topics, length)
        files <- sub("\\.[Rr]d$", "", Rd$File)
        internal <- sapply(Rd$Keywords, function(x) "internal" %in% x)
        data.frame(Topic = unlist(topics),
                   File = rep.int(files, lens),
                   Title = rep.int(Rd$Title, lens),
                   Internal = rep.int(internal, lens),
                   stringsAsFactors = FALSE)
    }
    ## FIXME duplicated aliases warning
    outman <- file.path(outDir, "help")
    dir.create(outman, showWarnings = FALSE)
    MM <- M[re(M[, 1L]), 1:2]
    write.table(MM, file.path(outman, "AnIndex"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    a <- structure(MM[, 2L], names=MM[, 1L])
    saveRDS(a, file.path(outman, "aliases.rds"))

    ## have HTML index even if no help pages
    outman <- file.path(outDir, "html")
    dir.create(outman, showWarnings = FALSE)
    outcon <- file(file.path(outman, "00Index.html"), "wt")
    on.exit(close(outcon))
    ## we know we have a valid file by now.
    desc <- read.dcf(file.path(outDir, "DESCRIPTION"))[1L, ]
    ## re-encode if necessary
    if(!is.na(enc <- desc["Encoding"])) {
        ## should be valid in UTF-8, might be invalid in declared encoding
        desc <- iconv(desc, enc, "UTF-8", sub = "byte")
    }
    ## drop internal entries
    M <- M[!M[, 4L], ]
    if (desc["Package"] %in% c("base", "graphics", "stats", "utils")) {
        for(pass in 1:2) {
            ## we skip method aliases
            gen <- gsub("\\.data\\.frame", ".data_frame", M$Topic)
            gen <- sub("\\.model\\.matrix$", ".modelmatrix", gen)
            gen <- sub("^(all|as|is|file|Sys|row|na|model)\\.", "\\1_", gen)
            gen <- sub("^(.*)\\.test", "\\1_test", gen)
            gen <- sub("([-[:alnum:]]+)\\.[^.]+$", "\\1", gen)
            last <- nrow(M)
            nongen <- gen %in% c("ar", "bw", "contr", "dyn", "lm", "qr", "ts", "which", ".Call", ".External", ".Library", ".First", ".Last")
            nc <- nchar(gen)
            asg <- (nc > 3) & substr(gen, nc-1, nc) == "<-"
            skip <- (gen == c("", gen[-last])) & (M$File == c("", M$File[-last])) & !nongen
            skip <- skip | asg
            ##N <- cbind(M$Topic, gen, c("", gen[-last]), skip)
            M <- M[!skip, ]
        }
    }

    # Collapse method links into unique (generic, file) pairs
    M$Topic <- sub("^([^,]*),.*-method$", "\\1-method", M$Topic)
    M <- M[!duplicated(M[, c("Topic", "File")]),]
    M <- M[re(M[, 1L]), ]

    ## encode some entries.
    htmlize <- function(x, backtick)
    {
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        if (backtick) {
            x <- gsub("---", "-", x, fixed = TRUE)
            x <- gsub("--", "-", x, fixed = TRUE)
            ## these have been changed in the Rd parser
            #x <- gsub("``", "&ldquo;", x, fixed = TRUE)
            #x <- gsub("''", "&rdquo;", x, fixed = TRUE)
            #x <- gsub("\\`([^']+)'", "&lsquo;\\1&rsquo;", x)
            #x <- gsub("`", "'", x, fixed = TRUE)
        }
        x
    }
    M$HTopic <- htmlize(M$Topic, FALSE)
    M$Title <- htmlize(M$Title, TRUE)

    ## No need to handle encodings: everything is in UTF-8

    html_header(desc["Package"], desc["Title"], desc["Version"], outcon)

    use_alpha <- (nrow(M) > 100)
    if (use_alpha) {
        first <- firstLetterCategory(M$Topic)
        nm <- sort(names(table(first)))
        m <- match(" ", nm, 0L) # -package
        if (m) nm <- c(" ", nm[-m])
        m <- match("misc", nm, 0L) # force last in all locales.
        if (m) nm <- c(nm[-m], "misc")
	writeLines(c("<p align=\"center\">",
		     paste0("<a href=\"#", nm, "\">", nm, "</a>"),
		     "</p>\n"), outcon)
        for (f in nm) {
            MM <- M[first == f, ]
            if (f != " ")
                cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
                    sep = "", file = outcon)
	    writeLines(c('<table width="100%">',
			 paste0('<tr><td width="25%"><a href="', MM[, 2L], '.html">',
				MM$HTopic, '</a></td>\n<td>', MM[, 3L],'</td></tr>'),
			 "</table>"), outcon)
       }
    } else if (nrow(M)) {
	writeLines(c('<table width="100%">',
		     paste0('<tr><td width="25%"><a href="', M[, 2L], '.html">',
			    M$HTopic, '</a></td>\n<td>', M[, 3L],'</td></tr>'),
		     "</table>"), outcon)
    } else { # no rows
         writeLines("There are no help pages in this package", outcon)
    }
    writeLines('</body></html>', outcon)
    file.copy(file.path(R.home("doc"), "html", "R.css"), outman)
    invisible(NULL)
}

### * .convertRdfiles

## possible types are "html", "latex", "example"
## outenc is used as the default output encoding for latex conversion
.convertRdfiles <-
    function(dir, outDir, types = "html", silent = FALSE, outenc = "UTF-8")
{
    showtype <- function(type) {
    	if (!shown) {
            nc <- nchar(bf)
            if (nc < 38L)
                cat("    ", bf, rep(" ", 40L - nc), sep = "")
            else
                cat("    ", bf, "\n", rep(" ", 44L), sep = "")
            shown <<- TRUE
        }
        ## 'example' is always last, so 5+space
        cat(type, rep(" ", max(0L, 6L - nchar(type))), sep = "")
    }

    dirname <- c("html", "latex", "R-ex")
    ext <- c(".html", ".tex", ".R", ".html")
    names(dirname) <- names(ext) <- c("html", "latex", "example")
    mandir <- file.path(dir, "man")
    if (!file_test("-d", mandir)) return()
    desc <- readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION
    pkg <- desc["Package"]
    ver <- desc["Version"]

    for(type in types)
        dir.create(file.path(outDir, dirname[type]), showWarnings = FALSE)

    cat("  converting help for package ", sQuote(pkg), "\n", sep = "")

    ## FIXME: add this lib to lib.loc?
    if ("html" %in% types) {
        ## may be slow, so add a message
        if (!silent) message("    finding HTML links ...", appendLF = FALSE, domain = NA)
        Links <- findHTMLlinks(outDir, level = 0:1)
        if (!silent) message(" done")
        .Links2 <- function() {
            message("\n    finding level-2 HTML links ...", appendLF = FALSE, domain = NA)
            Links2 <- findHTMLlinks(level = 2)
            message(" done", domain = NA)
            Links2
        }
        delayedAssign("Links2", .Links2())
    }

    ## Rd objects may already have been installed.
    db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                   error = function(e) NULL)
    ## If not, we build the Rd db from the sources:
    if (is.null(db)) db <- Rd_db(dir = dir)

    if (!length(db)) return()

    files <- names(db)

    .whandler <-  function(e) {
        .messages <<- c(.messages,
                        paste("Rd warning:", conditionMessage(e)))
        invokeRestart("muffleWarning")
    }
    .ehandler <- function(e) {
        message("", domain = NA) # force newline
        unlink(ff)
        stop(conditionMessage(e), domain = NA, call. = FALSE)
    }
    .convert <- function(expr)
        withCallingHandlers(tryCatch(expr, error = .ehandler),
                            warning = .whandler)

    for(f in files) {
        .messages <- character()
        Rd <- db[[f]]
        attr(Rd, "source") <- NULL
        bf <- sub("\\.[Rr]d$", "", basename(f))

        shown <- FALSE

        if ("html" %in% types) {
            type <- "html"
            ff <- file.path(outDir, dirname[type],
                            paste0(bf, ext[type]))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                showtype(type)
                ## assume prepare_Rd was run when dumping the .rds
                ## so use defines = NULL for speed
                .convert(Rd2HTML(Rd, ff, package = c(pkg, ver),
                                 defines = NULL,
                                 Links = Links, Links2 = Links2))
            }
        }
        if ("latex" %in% types) {
            type <- "latex"
            ff <- file.path(outDir, dirname[type],
                            paste0(bf, ext[type]))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                showtype(type)
                .convert(Rd2latex(Rd, ff, defines = NULL,
                                  outputEncoding = outenc))
            }
        }
        if ("example" %in% types) {
            type <- "example"
            ff <- file.path(outDir, dirname[type],
                            paste0(bf, ext[type]))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                .convert(Rd2ex(Rd, ff, defines = NULL))
                if (file_test("-f", ff)) showtype(type)
            }
        }
        if (shown) {
            cat("\n")
            if (length(.messages)) writeLines(unique(.messages))
        }
    }

    ## Now check for files to remove.
    ## These start with a letter.
    bfs <- sub("\\.[Rr]d$", "", basename(files)) # those to keep
    if ("html" %in% types) {
        type <- "html"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.html", "", basename(have))
        drop <- have[! have2 %in% c(bfs, "00Index", "R.css")]
        unlink(file.path(outDir, dirname[type], drop))
    }
    if ("latex" %in% types) {
        type <- "latex"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.tex", "", basename(have))
        drop <- have[! have2 %in% bfs]
        unlink(file.path(outDir, dirname[type], drop))
    }
    if ("example" %in% types) {
        type <- "example"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.R", "", basename(have))
        drop <- have[! have2 %in% bfs]
        unlink(file.path(outDir, dirname[type], drop))
    }

}

### * .makeDllRes

.makeDllRes <-
function(name="", version = "0.0")
{
    if (file.exists(f <- "../DESCRIPTION") ||
        file.exists(f <- "../../DESCRIPTION")) {
        desc <- read.dcf(f)[[1L]]
        if (!is.na(f <- desc["Package"])) name <- f
        if (!is.na(f <- desc["Version"])) version <- f
    }
    writeLines(c('#include <windows.h>',
                 '#include "Rversion.h"',
                 '',
                 'VS_VERSION_INFO VERSIONINFO',
                 'FILEVERSION R_FILEVERSION',
                 'PRODUCTVERSION 3,0,0,0',
                 'FILEFLAGSMASK 0x3L',
                 'FILEOS VOS__WINDOWS32',
                 'FILETYPE VFT_APP',
                 'BEGIN',
                 '    BLOCK "StringFileInfo"',
                 '    BEGIN',
                 '        BLOCK "040904E4"',
                 '        BEGIN'))
    cat("            VALUE \"FileDescription\", \"DLL for R package `", name,"'\\0\"\n",
        "            VALUE \"FileVersion\", \"", version, "\\0\"\n", sep = "")
    writeLines(c(
                 '            VALUE "Compiled under R Version", R_MAJOR "." R_MINOR " (" R_YEAR "-" R_MONTH "-" R_DAY ")\\0"',
                 '            VALUE "Project info", "http://www.r-project.org\\0"',
                 '        END',
                 '    END',
                 '    BLOCK "VarFileInfo"',
                 '    BEGIN',
                 '        VALUE "Translation", 0x409, 1252',
                 '    END',
                 'END'))
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
