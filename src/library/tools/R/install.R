#  File src/library/tools/R/install.R
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

#### R based engine for  R CMD INSTALL SHLIB Rdconv Rd2dvi
####

##' @param args

##' @return ...
.install_packages <- function(args = NULL)
{
    ## calls system() on Windows for
    ## sh (configure.win/cleanup.win) make zip

    ## we don't want to load utils just for this
    .file_test <- function(op, x)
        switch(op,
               "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
               "-x" = (file.access(x, 1L) == 0L),
               stop(sprintf("test '%s' is not available", op), domain = NA))
    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    ## global variables
    bundle_pkgs <- character() # list of packages in current pkg/bundle
    lockdir <- ""
    is_first_package <- TRUE
    stars <- "*"

    on.exit(do_exit_on_error())
    WINDOWS <- .Platform$OS.type == "windows"

    paste0 <- function(...) paste(..., sep="")

    MAKE <- Sys.getenv("MAKE")
    TAR <- shQuote(Sys.getenv("TAR"))
    GZIP <- Sys.getenv("R_GZIPCMD")
    if (!nzchar(GZIP)) GZIP <- "gzip"
    if (WINDOWS) zip <- "zip"
    rarch <- Sys.getenv("R_ARCH")

    SHLIB_EXT <- if (WINDOWS) ".dll" else {
        ## can we do better?
        mconf <- file.path(R.home(), paste0("etc", rarch), "Makeconf")
        sub(".*= ", "", grep("^SHLIB_EXT", readLines(mconf), value = TRUE))
    }

    options(warn = 1)
    invisible(Sys.setlocale("LC_COLLATE", "C")) # discard output

    if (WINDOWS) {
        rhome <- chartr("\\", "/", R.home())
        ## These might be needed for configure.win and Make{file,vars}.win
        ## Some people have *assumed* that R_HOME uses /
        Sys.setenv(R_HOME = rhome)
        ## and others have assumed that RHOME is set:
        Sys.setenv(RHOME = rhome)
    }

    Usage <- function() {
        cat("Usage: R CMD INSTALL [options] pkgs",
            "",
            "Install the add-on packages specified by pkgs.  The elements of pkgs can",
            "be relative or absolute paths to directories with the package (bundle)",
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
            "  -d, --debug		turn on script and build-help debugging",
            "  -l, --library=LIB	install packages to library tree LIB",
            "      --no-configure    do not use the package's configure script",
            "      --no-docs		do not install HTML, LaTeX or examples help",
            "      --html		build HTML help",
            "      --no-html		do not build HTML help",
            "      --latex      	install LaTeX help",
            "      --example		install R code for help examples",
            "      --use-zip-data	collect data files in zip archive",
            "      --install-tests	install package-specific tests (if any)",
            "      --fake		do minimal install for testing purposes",
            "      --no-lock, --unsafe",
            "			install on top of any existing installation",
            "			without using a lock directory",
            "      --pkglock		use a per-package lock directory",
            "      --build    	build binaries of the installed package(s)",
            "      --data-compress=	none, gzip (default), bzip2 or xz compression",
            "			to be used for lazy-loading of data",
            "      --resave-data	re-save data files as compactly as possible",
           "\nfor Unix",
            "      --configure-args=ARGS",
            "			set arguments for the configure scripts (if any)",
            "      --configure-vars=VARS",
            "			set variables for the configure scripts (if any)",
            "      --libs-only	only install the libs directory",
            "      --no-multiarch	build only the main architecture",
            "\nand on Windows only",
            "      --auto-zip	select whether to zip data automatically",
            "",
            "Which of --html or --no-html is the default depends on the build of R:",
            paste("for this one it is ",
                  ifelse(static_html, "--html", "--no-html"), ".", sep = ""),
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep="\n")
    }

    do_cleanup <- function()
    {
        do_cleanup_tmpdir()
        if (!is_first_package) {
            ## Only need to do this in case we successfully installed at least
            ## *one* package ... well not so sure for bundles.
            file.copy(file.path(R.home("doc"), "html", "R.css"), lib)
            if (lib == .Library) {
                if (build_help)
                    unix.packages.html(.Library, docdir = R.home("doc"))
            }
        }
        if (lock && nzchar(lockdir)) unlink(lockdir, recursive = TRUE)
    }

    do_cleanup_tmpdir <- function()
    {
        ## Solaris will not remove any directory in the current path
        setwd(startdir)
        if (dir.exists(tmpdir)) unlink(tmpdir, recursive=TRUE)
    }

    do_exit_on_error <- function()
    {
        # message("*** do_exit_on_error ***")
        ## If we are not yet processing a package, we will not have
        ## set bundle_pkgs
        for(p in bundle_pkgs) {
            if (is.na(p) || !nzchar(p)) next
            pkgdir <- file.path(lib, p)
            if (nzchar(pkgdir) && dir.exists(pkgdir)) {
                starsmsg(stars, "removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }
            if (lock && nzchar(lockdir) &&
                dir.exists(lp <- file.path(lockdir, p))) {
                starsmsg(stars, "restoring previous ", sQuote(pkgdir))
                if (WINDOWS) {
                    file.copy(lp, dirname(pkgdir), recursive = TRUE)
                    unlink(lp, recursive = TRUE)
                } else system(paste("mv", lp, pkgdir))
            }
        }

        do_cleanup()
        q("no", status = 1, runLast = FALSE)
    }

    fullpath <- function(dir)
    {
        owd <- setwd(dir)
        full <- getwd()
        setwd(owd)
        full
    }


    parse_description_field <- function(desc, field, default=TRUE)
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
        message(stars, " ", ..., domain=NA)

    errmsg <- function(...)
    {
        message("ERROR: ", ...)
        do_exit_on_error()
    }

    pkgerrmsg <- function(msg, pkg)
    {
        message("ERROR: ", msg, " for package ", sQuote(pkg), domain = NA)
        do_exit_on_error()
    }

    ## 'pkg' is the absolute path to package/bundle sources.
    do_install <- function(pkg)
    {
        setwd(pkg)
        desc <- read.dcf(file.path(pkg, "DESCRIPTION"))[1, ]
        ## Let's see if we have a bundle
        bundle_name <- desc["Bundle"]
        is_bundle <- !is.na(bundle_name)
        if (is_bundle) {
            contains <- .get_contains_from_package_db(desc)
            for(p in contains) {
                if (dir.exists(file.path(pkg, p))) {
                    pkgs <- c(pkgs, p)
                } else {
                    warning("incorrect Contains metadata for bundle ",
                            sQuote(bundle_name),
                            ": there is no package '", sQuote(p),
                            call. = FALSE, domain = NA)
                    warning("skipping installation of bundle ",
                            sQuote(bundle_name), call. = FALSE, domain = NA)
                    contains <- character()
                    break
                }
            }
            ## binary bundles are special.  Like source bundles they
            ## have a top-level DESCRIPTION file, but they have no
            ## 'Built' field in it, and no */DESCRIPTION.in
            if (length(contains) && length(Sys.glob("*/DESCRIPTION.in"))) {
                ## Create the package level DESCRIPTION files from the bundle
                ## level DESCRIPTION and the package level DESCRIPTION.in ones.
                res <- try(.vcreate_bundle_package_descriptions(pkg, paste(contains, collapse=" ")))
                if (inherits(res, "try-error"))
                    warning("problem installing per-package DESCRIPTION files",
                            call. = FALSE, domain = NA)
            }
            ## This cannot create a binary bundle, no top-level DESCRIPTION
            if (tar_up)
                errmsg("cannot create a binary bundle: use 'R CMD build --binary' to do so")
            bundle_pkgs <<- contains
        } else {
            bundle_name <- desc["Package"]
            if (is.na(bundle_name)) errmsg("no 'Package' field in 'DESCRIPTION'")
            bundle_pkgs <<- bundle_name
        }

        for(p in bundle_pkgs) {
            if (is_bundle) {
                pkg_dir <- file.path(pkg, p)
                setwd(pkg_dir)
                desc <- read.dcf("DESCRIPTION")[1, ]
            } else pkg_dir <- pkg
            pkg_name <- desc["Package"]
            Sys.setenv(R_PACKAGE_NAME = pkg_name)
            instdir <- file.path(lib, pkg_name)
            Sys.setenv(R_PACKAGE_DIR = instdir) ## installation dir
            ## if (WINDOWS) Sys.setenv(DPKG = instdir) ## assumed by some

            ## FIXME: do this at bundle level?
            ## Could different packages have different version requirements?
            status <- .Rtest_package_depends_R_version()
            if (status) do_exit_on_error()

            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
            if (!dir.exists(instdir)) {
                message("ERROR: unable to create ", sQuote(instdir),
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
                do_install_source(pkg_name, instdir, pkg_dir, desc)
            else
                do_install_binary(pkg_name, instdir, desc)

            ## Add read permission to all, write permission to ownwer
            .Internal(dirchmod(instdir))
            ##    system(paste("find", shQuote(instdir),  "-exec chmod a+r \\{\\} \\;"))
            if (is_bundle)
                starsmsg(stars, "DONE (", pkg_name, ")")
            is_first_package <<- FALSE
        }

        if (tar_up) {
            version <- desc["Version"]
            filename <- paste0(bundle_name, "_", version, "_R_",
                               Sys.getenv("R_PLATFORM"), ".tar")
            filepath <- shQuote(file.path(startdir, filename))
            owd <- setwd(lib)
            system(paste(TAR, "-chf", filepath,
                         paste(bundle_pkgs, collapse = " ")))
            system(paste(GZIP, "-9f", filepath))
            message("packaged installation of ",
                    sQuote(bundle_name), " as ", filename, ".gz",
                    domain = NA)
            setwd(owd)
        }

        if (zip_up) {
            ZIP <- "zip"                # Windows only
            version <- desc["Version"]
            filename <- paste0(bundle_name, "_", version, ".zip")
            filepath <- shQuote(file.path(startdir, filename))
            ## system(paste("rm -f", filepath))
            unlink(filepath)
            owd <- setwd(lib)
            system(paste(ZIP, "-r9Xq", filepath,
                         paste(bundle_pkgs, collapse = " ")))
	    if (is_bundle) {
                ## need to add top-level DESCRIPTION file
                setwd(pkg)
                system(paste(ZIP, "-9Xq", filepath, "DESCRIPTION"))
	    }
            setwd(owd)
            message("packaged installation of ",
                    sQuote(bundle_name), " as ", filename)
        }

        starsmsg(stars, "DONE (", bundle_name, ")")

        bundle_pkgs <<- character()
    }


    ## Unix only
    do_install_binary <- function(pkg, instdir, desc)
    {
        starsmsg(stars, "installing *binary* package ", sQuote(pkg), " ...")

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            if (lock) system(paste("mv", instdir, file.path(lockdir, pkg)))
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }
        res <- system(paste("cp -r .", shQuote(instdir),
                            "|| (", TAR, "cd - .| (cd", shQuote(instdir), "&&", TAR, "xf-))"
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
        if (dir.exists("src")) {
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
        } else if (.file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Adminstration Manual'", call. = FALSE)

    }

    do_install_source <- function(pkg_name, instdir, pkg_dir, desc)
    {
        cp_r <- function(from, to)
        {
            ## used for inst/
            if (WINDOWS) {
                file.copy(Sys.glob(file.path(from, "*")), to, recursive = TRUE)
                # system(paste0("cp -r ", shQuote(from), "/* ", shQuote(to)))
            } else {
                from <- shQuote(from)
                to <- shQuote(to)
                system(paste0("cp -r ", from, "/* ", to,
                              " || (cd ", from, " && ", TAR, " cf - . | (cd '",
                              to, "' && ", TAR, "xf - ))"))
            }
        }

        shlib_install <- function(instdir, arch)
        {
            files <- Sys.glob(paste0("*", SHLIB_EXT))
            if (length(files)) {
                libarch <- if (nzchar(arch)) paste0("libs", arch) else "libs"
                dest <- file.path(instdir, libarch)
                dir.create(dest, recursive = TRUE, showWarnings = FALSE)
                file.copy(files, dest, overwrite = TRUE)
                if (!WINDOWS)
                    Sys.chmod(Sys.glob(file.path(dest, "*")), "755")
            }
        }

        run_shlib <- function(pkg_name, srcs, instdir, arch)
        {
            args <- c(shargs, "-o", paste0(pkg_name, SHLIB_EXT), srcs)
            if (debug) message("about to run ",
                               "R CMD SHLIB ", paste(args, collapse= " "),
                               domain = NA)
            if (.shlib_internal(args) == 0L) {
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
            rlibs <- if (nzchar(rlibs)) paste(lib, rlibs, sep=.Platform$path.sep) else lib
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
                if (.file_test("-x", "configure")) {
                    res <- system(paste(paste(configure_vars, collapse = " "),
                                        "./configure",
                                        paste(configure_args, collapse = " ")))
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                } else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
            }
            if (file.exists("Makefile"))
                if (system(MAKE)) pkgerrmsg("make failed", pkg_name)
            if (clean) system(paste(MAKE, "clean"))
            return()
        }

        if (!is.na(Type) && Type == "Translation") {
            starsmsg(stars, "installing *Translation* package ", sQuote(pkg_name), " ...")
            if (dir.exists("share")) {
                files <- Sys.glob("share/*")
                if (length(files)) file.copy(files, R.home("share"), TRUE)
            }
            if (dir.exists("library")) {
                ## FIXME use file.copy
                system(paste("cp -r ./library", R.home()))
            }
            return()
        }

        OS_type <- desc["OS_type"]
        if (WINDOWS) {
            if ((!is.na(OS_type) && OS_type == "unix") && !fake)
                errmsg(" Unix-only package")
        } else {
            if ((!is.na(OS_type) && OS_type == "windows") && !fake)
                errmsg(" Windows-only package")
        }

        starsmsg(stars, "installing *source* package ", sQuote(pkg_name), " ...")

        stars <- "**"

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            ## Back up a previous version
            if (lock) {
                if (debug) starsmsg(stars, "backing up earlier installation")
                if(WINDOWS) {
                    file.copy(instdir, lockdir, recursive = TRUE)
                    unlink(instdir, recursive = TRUE)
                } else
                    system(paste("mv", instdir, file.path(lockdir, pkg_name)))
            } else if (more_than_libs) unlink(instdir, recursive = TRUE)
            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
        }

        if (preclean) run_clean()

        if (auto_zip || zip_up) { ## --build implies --auto-zip
            thislazy <- parse_description_field(desc, "LazyData",
                                                default = lazy_data)
            thiszip <- parse_description_field(desc, "ZipData",
                                               default = TRUE)
            if (!thislazy && thiszip && dir.exists("data")) {
                fi <- file.info(dir("data", full.names=TRUE))
                if (sum(fi$size) > 100000) {
                    this <- sub("\\.[a-zA-Z]+$", "", row.names(fi))
                    if (!anyDuplicated(this)) use_zip_data <- TRUE
                }
                if (use_zip_data)
                     message("\n  Using auto-selected zip option ",
                             sQuote("--use-zip-data"), "\n", domain = NA)
            }
        }

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
                if (.file_test("-x", "configure")) {
                    cmd <- paste(paste(configure_vars, collapse = " "),
                                 "./configure",
                                 paste(configure_args, collapse = " "))
                    if (debug) message("configure command: ", sQuote(cmd),
                                       domain = NA)
                    res <- system(cmd)
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                }  else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
            }
        }


        if (more_than_libs) {
            for (f in c("NAMESPACE", "LICENSE", "LICENCE", "COPYING", "NEWS"))
                if (file.exists(f)) {
                    file.copy(f, instdir, TRUE)
                    Sys.chmod(file.path(instdir, f), "644")
                }

            ## This cannot be done in a MBCS: write.dcf fails
            ctype <- Sys.getlocale("LC_CTYPE")
            Sys.setlocale("LC_CTYPE", "C")
            res <- try(.install_package_description(".", instdir))
            Sys.setlocale("LC_CTYPE", ctype)
            if (inherits(res, "try-error"))
                pkgerrmsg("installing package DESCRIPTION failed", pkg_name)
        }

        if (dir.exists("src") && !fake) {
            system_makefile <- file.path(R.home(), paste0("etc", rarch),
                                         "Makeconf")
            starsmsg(stars, "libs")
            if (!file.exists(file.path(R.home("include"), "R.h")))
                ## maybe even an error?  But installing Fortran-based packages should work
                warning("R include directory is empty -- perhaps need to install R-devel.rpm or similar", call. = FALSE)
            has_error <- FALSE
            linkTo <- desc["LinkingTo"]
            if (!is.na(linkTo)) {
                lpkgs <- strsplit(linkTo, ",[[:blank:]]*")[[1L]]
                paths <- .find.package(lpkgs, quiet=TRUE)
                if (length(paths)) {
                    clink_cppflags <- paste(paste0('-I"', paths, '/include"'),
                                            collapse=" ")
                    Sys.setenv(CLINK_CPPFLAGS = clink_cppflags)
                }
            } else clink_cppflags <- ""
            libdir <- file.path(instdir, paste0("libs", rarch))
            dir.create(libdir, showWarnings = FALSE)
            if (WINDOWS) {
                owd <- setwd("src")
                makefiles <- character()
                if (file.exists(f <- path.expand("~/.R/Makevars.win")))
                    makefiles <- f
                else if (file.exists(f <- path.expand("~/.R/Makevars")))
                    makefiles <- f
                if (file.exists("Makefile.win")) {
                    makefiles <- c("Makefile.wIn", makefiles)
                    message("  running src/Makefile.win ...")
                    res <- system(paste("make --no-print-directory",
                                        paste("-f", shQuote(makefiles), collapse = " ")))
                    if (res == 0) shlib_install(instdir, "")
                    else has_error <- TRUE
                } else {
                    message("  making DLL ...")
                    srcs <- dir(pattern = "\\.([cfmCM]|cc|cpp|f90|f95|mm)$")
                    has_error <- run_shlib(pkg_name, srcs, instdir, "")
                    message("  ... done")
                }
                setwd(owd)
            } else { # not WINDOWS
                if (file.exists("src/Makefile")) {
                    arch <- substr(rarch, 2, 1000)
                    starsmsg(stars, "arch - ", arch)
                    owd <- setwd("src")
                    makefiles <- c(system_makefile, "Makefile")
                    if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                                           Sys.getenv("R_PLATFORM"), sep="-"))))
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
                    srcs <- dir(pattern = "\\.([cfmCM]|cc|cpp|f90|f95|mm)$")
                    ## This allows Makevars to set OBJECTS or its own targets.
                    allfiles <- if (file.exists("Makevars")) c("Makevars", srcs) else srcs
                    wd2 <- setwd(file.path(R.home(), "bin", "exec"))
                    archs <- Sys.glob("*")
                    setwd(wd2)
                    if (length(allfiles)) {
                        ## if there is a configure script we install only the main
                        ## sub-architecture
                        if (!multiarch ||
                            .file_test("-x", "../configure")) {
                            if (nzchar(rarch))
                                starsmsg(stars, "arch - ", substr(rarch, 2, 1000))
                            has_error <- run_shlib(pkg_name, srcs, instdir, rarch)
                        } else {
                            for(arch in archs) {
                                system("rm -f *.o *.so *.sl *.dylib")
                                if (arch == "R") {
                                    ## top-level, so one arch without subdirs
                                    has_error <- run_shlib(pkg_name, srcs, instdir, "")
                                } else if (arch == "Rgnome") {
                                    ## ignore
                                } else {
                                    starsmsg(stars, "arch - ", arch)
                                    ra <- paste0("/", arch)
                                    ## FIXME: do this lower down
                                    Sys.setenv(R_ARCH = ra)
                                    has_error <- run_shlib(pkg_name, srcs, instdir, ra)
                                    if (has_error) break
                                    Sys.setenv(R_ARCH = rarch)
                                }
                            }
                        }
                    } else warning("no source files found", call. = FALSE)
                }
                setwd(owd)
            }
            if (has_error)
                pkgerrmsg("compilation failed", pkg_name)
        }                               # end of src dir

        if (more_than_libs) {
            if (dir.exists("R")) {
                starsmsg(stars, "R")
                dir.create(file.path(instdir, "R"), recursive = TRUE,
                           showWarnings = FALSE)
                ## This cannot be done in a C locale
                res <- try(.install_package_code_files(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("unable to collate files", pkg_name)

                if (file.exists(file.path("R", "sysdata.rda"))) {
                    res <- try(sysdata2LazyLoadDB("R/sysdata.rda",
                                                          file.path(instdir, "R")))
                    if (inherits(res, "try-error"))
                        pkgerrmsg("unable to build sysdata DB", pkg_name)
                }
                if (fake) {
                    if (file.exists("NAMESPACE")) {
                        cat("",
                            '.onLoad <- .onAttach <- function(lib, pkg) NULL',
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
                        ## FIXME: some packages have useDynlib()
                        ## spread over several lines.
                        writeLines(sub("useDynLib.*", 'useDynLib("")',
                                       readLines("NAMESPACE", warn = FALSE),
                                       perl = TRUE, useBytes = TRUE),
                                   file.path(instdir, "NAMESPACE"))
                        ## </NOTE>
                    } else {
                        cat("",
                            '.First.lib <- function(lib, pkg) NULL',
                            sep = "\n",
                            file = file.path(instdir, "R", pkg_name), append = TRUE)
                    }
                }
            }                           # end of R

            if (dir.exists("data")) {
                starsmsg(stars, "data")
                files <- Sys.glob(file.path("data", "*"))
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
                    Sys.chmod(Sys.glob(file.path(instdir, "data", "*")), "644")
                    if (thislazy) {
                        ## This also had an extra space in the sh version
                        starsmsg(stars, " moving datasets to lazyload DB")
                        ## 'it is possible that data in a package will
                        ## make use of the code in the package, so ensure
                        ## the package we have just installed is on the
                        ## library path.'
                        ## (We set .libPaths)
                        res <- try(data2LazyLoadDB(pkg_name, lib,
                                                   compress = data_compress))
                        if (inherits(res, "try-error"))
                            pkgerrmsg("lazydata failed", pkg_name)
                    } else if (use_zip_data &&
                               (WINDOWS ||
                               (nzchar(Sys.getenv("R_UNZIPCMD")) &&
                               nzchar(zip <- Sys.getenv("R_ZIPCMD"))) )) {
                        owd <- setwd(file.path(instdir, "data"))
                        writeLines(dir(), "filelist")
                        system(paste(zip, "-q -m Rdata * -x filelist 00Index"))
                        setwd(owd)
                    }
                } else warning("empty 'data' directory", call. = FALSE)
            }

            if (dir.exists("demo") && !fake) {
                starsmsg(stars, "demo")
                dir.create(file.path(instdir, "demo"), recursive = TRUE,
                           showWarnings = FALSE)
                file.remove(Sys.glob(file.path(instdir, "demo", "*")))
                res <- try(.install_package_demos(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("ERROR: installing demos failed")
                Sys.chmod(Sys.glob(file.path(instdir, "demo", "*")), "644")
            }

            if (dir.exists("exec") && !fake) {
                starsmsg(stars, "exec")
                dir.create(file.path(instdir, "exec"), recursive = TRUE,
                           showWarnings = FALSE)
                file.remove(Sys.glob(file.path(instdir, "exec", "*")))
                files <- Sys.glob(file.path("exec", "*"))
                if (length(files)) {
                    file.copy(files, file.path(instdir, "exec"), TRUE)
                    Sys.chmod(Sys.glob(file.path(instdir, "exec", "*")), "755")
                }
            }

            if (dir.exists("inst") && !fake) {
                starsmsg(stars, "inst")
                ## FIXME avoid installing .svn etc?
                cp_r("inst", instdir)
                ## file.copy("inst", "instdir", recursive = TRUE)
            }

            if (install_tests && dir.exists("tests") && !fake) {
                starsmsg(stars, "tests")
                file.copy("tests", instdir, recursive = TRUE)
            }

            ## Defunct:
            ## FIXME: remove these at some point
            if (file.exists("install.R"))
                warning("use of file 'install.R' is no longer supported",
                        call. = FALSE, domain = NA)
            if (file.exists("R_PROFILE.R"))
                warning("use of file 'R_PROFILE.R' is no longer supported",
                        call. = FALSE, domain = NA)
            value <- parse_description_field(desc, "SaveImage", default = NA)
            if (!is.na(value))
                warning("field 'SaveImage' is defunct: please remove it",
                        call. = FALSE, domain = NA)


            ## LazyLoading
            value <- parse_description_field(desc, "LazyLoad", default = lazy)
            if (dir.exists("R") && value) {
                starsmsg(stars, "preparing package for lazy loading")
                ## Something above, e.g. lazydata,  might have loaded the namespace
                if (pkg_name %in% loadedNamespaces())
                    unloadNamespace(pkg_name)
                ## suppress second round of parse warnings
                options(warnEscapes = FALSE)
                res <- try({.getRequiredPackages(quietly = TRUE)
                            makeLazyLoading(pkg_name, lib)})
                options(warnEscapes = TRUE)
                if (inherits(res, "try-error"))
                    pkgerrmsg("lazy loading failed", pkg_name)
                ## FIXME: still needed?  If so needs a pretest
                ## file.remove(file.path(instdir, "R", "all.rda"))
            }

            ## as from 2.10.0 do this always
            {
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
            }

            ## pkg indices
            starsmsg(stars, "building package indices ...")
            res <- try(.install_package_indices(".", instdir))
            if (inherits(res, "try-error"))
                errmsg("installing package indices failed")

            ## Install a dump of the parsed NAMESPACE file
            if (file.exists("NAMESPACE") && !fake) {
                res <- try(.install_package_namespace_info(".", instdir))
                if (inherits(res, "try-error"))
                    errmsg("installing namespace metadata failed")
            }

        }                               # more_than_libs

        ## <NOTE>
        ## Remove stuff we should not have installed in the first place.
        ## When installing from a source directory under version control, we
        ## should really exclude the subdirs CVS, .svn (Subversion) and
        ## .arch-ids (arch).
        for(d in c("CVS", ".svn", ".arch-ids", ".git")) {
            ## FIXME
            if (!WINDOWS)
                system(paste("find",  shQuote(instdir), "-name", d,
                             "-type d -prune -exe rm \\{\\} \\;"),
                       ignore.stderr = TRUE)
        }

        if (clean) run_clean()

        if (WINDOWS) { ## Add MD5 sums: only for --build?
            starsmsg(stars, "MD5 sums")
            .installMD5sums(instdir)
        }

    }

    options(showErrorCalls=FALSE)
    pkgs <- character(0)
    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1]][-1]
    }

    startdir <- getwd()

    lib <- lib0 <- ""
    clean <- FALSE
    preclean <- FALSE
    debug <- FALSE
    static_html <- nzchar(system.file("html", "mean.html", package="base"))
    build_html <- static_html
    build_latex <- FALSE
    build_example <- FALSE
    use_configure <- TRUE
    use_zip_data <- FALSE
    auto_zip <- FALSE
    configure_args <- character(0)
    configure_vars <- character(0)
    fake <- FALSE
    lazy <- TRUE
    lazy_data <- FALSE
    lock <- TRUE
    pkglock <- FALSE
    pkglockname <- ""
    libs_only <- FALSE
    tar_up <- zip_up <- FALSE
    shargs <- character(0)
    multiarch <- TRUE
    install_tests <- FALSE
    get_user_libPaths <- FALSE
    data_compress <- TRUE # FALSE (none), TRUE (gzip), 2 (bzip2), 3 (xz)
    resave_data <- FALSE

    while(length(args)) {
        a <- args[1]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package installer: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
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
        } else if (a == "--use-zipdata") {
            use_zip_data <- TRUE
        } else if (a == "--auto-zip") {
            if (WINDOWS) auto_zip <- TRUE
            else warning("'--auto-zip' is for Windows only", call. = FALSE)
        } else if (a == "-l") {
            if (length(args) >= 2) {lib <- args[2]; args <- args[-1]}
            else stop("-l option without value", call. = FALSE)
        } else if (substr(a, 1, 10) == "--library=") {
            lib <- substr(a, 11, 1000)
        } else if (substr(a, 1, 17) == "--configure-args=") {
            configure_args <- c(configure_args, substr(a, 18, 1000))
        } else if (substr(a, 1, 17) == "--configure-vars=") {
            configure_vars <- c(configure_vars, substr(a, 18, 1000))
        } else if (a == "--fake") {
            fake <- TRUE
        } else if (a %in% c("--no-lock", "--unsafe")) {
            lock <- FALSE
        } else if (a == "--pkglock") {
            pkglock <- TRUE
        } else if (a == "--libs-only") {
            libs_only <- TRUE
        } else if (a == "--no-multiarch") {
            multiarch <- FALSE
        } else if (a == "--install-tests") {
            install_tests <- TRUE
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
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1]
    }

    tmpdir <- tempfile("R.INSTALL")
    if (!dir.create(tmpdir))
        stop("cannot create temporary directory")

    ## now unpack tarballs and do some basic checks
    allpkgs <- character()
    for(pkg in pkgs) {
        if (debug) message("processing ", sQuote(pkg), domain = NA)
        if (.file_test("-f", pkg)) {
            if (debug) message("a file", domain = NA)
            pkgname <- basename(pkg) # or bundle name
            ## Also allow for 'package.tgz' ...
            pkgname <- sub("\\.(tgz|tar\\.gz|tar\\.bz2)$", "", pkgname)
            pkgname <- sub("_.*", "", pkgname)
            res <- if (WINDOWS) {
                utils::untar(pkg, exdir = chartr("\\", "/", tmpdir))
            } else {
                utils::untar(pkg, exdir = tmpdir)
            }
            if (res) errmsg("error unpacking tarball")
            ## If we have a binary bundle distribution, there should be
            ## a DESCRIPTION file at top level.
            if (file.exists(ff <- file.path(tmpdir, "DESCRIPTION"))) {
                con <- read.dcf(ff, "Contains")
                if (!is.na(con)) {
                    starsmsg(stars, "looks like a binary bundle")
                    allpkgs <- c(allpkgs, tmpdir)
                } else {
                    message("unknown package layout", domain = NA)
                    do_cleanup_tmpdir()
                    q("no", status = 1, runLast = FALSE)
                }
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
        if (pkglock) {
            if (nzchar(pkglockname)) {
                warning("--pkglock applies only to a single bundle/package",
                        call. = FALSE)
                pkglock <- FALSE
            } else pkglockname <- pkgname
        }
    }

    if (!length(allpkgs))
        stop("ERROR: no packages specified", call.=FALSE)

    if (!nzchar(lib)) {
        lib <- if (get_user_libPaths) { ## need .libPaths()[1] *after* the site- and user-initialization
	    system(paste(file.path(R.home("bin"), "Rscript"),
                         "-e 'cat(.libPaths()[1])'"),
                   intern = TRUE)
        }
        else .libPaths()[1]
        starsmsg(stars, "installing to library ", sQuote(lib))
    } else {
        lib0 <- lib <- path.expand(lib)
        ## lib is allowed to be a relative path.
        ## should be OK below, but be sure.
        cwd <- tryCatch(setwd(lib), error = function(e)
                        stop("ERROR: cannot cd to directory ", sQuote(lib), call. = FALSE))
        lib <- getwd()
        setwd(cwd)
    }
    ok <- dir.exists(lib)
    if (ok) {
        if (WINDOWS) {
            ## file.access is unreliable on Windows
            ## the only known reliable way is to try it
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), sep="_"))
            unlink(fn, recursive = TRUE) # precaution
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) ok <- FALSE
            else unlink(fn, recursive = TRUE)
        } else ok <- file.access(lib, 2L) == 0
    }
    if (!ok)
        stop("ERROR: no permission to install to directory ",
             sQuote(lib), call. = FALSE)

    if (libs_only) {
        lock <- FALSE
        tar_up <- FALSE
    }
    more_than_libs <- !libs_only


    if (lock) {
        lockdir <- if (pkglock) file.path(lib, paste("00LOCK", pkglockname, sep="-"))
        else file.path(lib, "00LOCK")
        if (file.exists(lockdir)) {
            message("ERROR: failed to lock directory ", sQuote(lib),
                    " for modifying\nTry removing ", sQuote(lockdir))
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        dir.create(lockdir, recursive = TRUE)
        if (!dir.exists(lockdir)) {
            message("ERROR: failed to create lock directory ", sQuote(lockdir))
            do_cleanup_tmpdir()
            q("no", status = 3, runLast = FALSE)
        }
        if (debug) starsmsg(stars, "created lock directory ", sQuote(lockdir))
    }

    if  ((tar_up || zip_up) && fake)
        stop("building a fake installation is disallowed")

    if (fake) {
        use_configure <- FALSE
        build_html <- FALSE
        build_latex <- FALSE
        build_example <- FALSE
    }

    build_help_types <- character(0)
    if (build_html) build_help_types <- c(build_help_types, "html")
    if (build_latex) build_help_types <- c(build_help_types, "latex")
    if (build_example) build_help_types <- c(build_help_types, "example")
    build_help <- length(build_help_types) > 0L

    if (debug)
        starsmsg(stars, "build_help_types=",
                 paste(build_help_types, collapse=" "))

    if (debug)
        starsmsg(stars, "DBG: R CMD INSTALL' now doing do_install")

    for(pkg in allpkgs) do_install(pkg)
    do_cleanup()
    on.exit()
    invisible()
}

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
            "Build a shared library for dynamic loading from the specified source or",
            "object files (which are automagically made from their sources) or",
            "linker options.  If not given via '--output', the name for the shared",
            "library is determined from the first source or object file.",
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
            "Report bugs to <r-bugs@r-project.org>.",
            sep="\n")

    p0 <- function(...) paste(..., sep="")
    ## FIXME shQuote here?
    p1 <- function(...) paste(..., collapse=" ")

    WINDOWS <- .Platform$OS.type == "windows"
    if (!WINDOWS) {
        mconf <- readLines(file.path(R.home(),
                                     p0("etc", Sys.getenv("R_ARCH")),
                                     "Makeconf"))
        SHLIB_EXT <- sub(".*= ", "", grep("^SHLIB_EXT", mconf, value = TRUE))
        SHLIB_LIBADD <- sub(".*= ", "", grep("^SHLIB_LIBADD", mconf, value = TRUE))
        MAKE <- Sys.getenv("MAKE")
    } else {
        rhome <- chartr("\\", "/", R.home())
        Sys.setenv(R_HOME = rhome)
        SHLIB_EXT <- ".dll"
        SHLIB_LIBADD <- ""
        MAKE <- "make"
    }

    OBJ_EXT <- ".o" # all currrent compilers, but not some on Windows

    objs <- character()
    shlib <- ""
    makefiles <-
        file.path(R.home("share"), "make",
                  if (WINDOWS) "winshlib.mk" else "shlib.mk")
    shlib_libadd <- if (nzchar(SHLIB_LIBADD)) SHLIB_LIBADD else character()
    with_cxx <- FALSE
    with_f77 <- FALSE
    with_f9x <- FALSE
    with_objc <- FALSE
    pkg_libs <- character()
    clean <- FALSE
    preclean <- FALSE
    dry_run <- FALSE
    debug <- FALSE

    while(length(args)) {
        a <- args[1]
        if (a %in% c("-h", "--help")) {
            Usage()
            return(0L)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R shared library builder: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2000-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
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
            if (length(args) >= 2) {shlib <- args[2]; args <- args[-1]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            shlib <- substr(a, 10, 1000)
        } else {
            ## a source file or something like -Ldir -lfoo
            base <- sub("\\.[[:alnum:]]*$", "", a)
            ext <- sub(p0(base, "."),  "", a)
            nobj <- ""
            if (nzchar(ext)) {
                if (ext %in% c("cc", "cpp", "C")) {
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
                    shlib <- p0(nobj, SHLIB_EXT)
            }
            if (nzchar(nobj)) objs <- c(objs, nobj)
            else pkg_libs <- c(pkg_libs, a)
        }
        args <- args[-1]
    }

    if (length(objs)) objs <- p0(objs, OBJ_EXT, collapse=" ")

    if (WINDOWS) {
        if (file.exists(f <- path.expand("~/.R/Makevars.win")))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    } else {
        if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                               Sys.getenv("R_PLATFORM"), sep="-"))))
            makefiles <- c(makefiles, f)
        else if (file.exists(f <- path.expand("~/.R/Makevars")))
            makefiles <- c(makefiles, f)
    }

    makeobjs <- p0("OBJECTS=", shQuote(objs))
    if (WINDOWS && file.exists("Makevars.win")) {
        makefiles <- c("Makevars.win", makefiles)
        lines <- readLines("Makevars.win", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl=TRUE, useBytes=TRUE)))
            makeobjs <- ""
    } else if (file.exists("Makevars")) {
        makefiles <- c("Makevars", makefiles)
        lines <- readLines("Makevars", warn = FALSE)
        if (length(grep("^OBJECTS *=", lines, perl=TRUE, useBytes=TRUE)))
            makeobjs <- ""
    }

    makeargs <- p0("SHLIB=", shQuote(shlib))
    if (with_f9x) {
        makeargs <- c("SHLIB_LDFLAGS='$(SHLIB_FCLDFLAGS)'",
                      "SHLIB_LD='$(SHLIB_FCLD)'", makeargs)
    } else if (with_cxx) {
        makeargs <- c("SHLIB_LDFLAGS='$(SHLIB_CXXLDFLAGS)'",
                      "SHLIB_LD='$(SHLIB_CXXLD)'", makeargs)
    }
    if (with_objc) shlib_libadd <- c(shlib_libadd, "$(OBJC_LIBS)")
    if (with_f77) shlib_libadd <- c(shlib_libadd, "$(FLIBS)")

    if (length(pkg_libs))
        makeargs <- c(makeargs,
                      p0("PKG_LIBS='", p1(pkg_libs), "'"))
    if (length(shlib_libadd))
        makeargs <- c(makeargs,
                      p0("SHLIB_LIBADD='", p1(shlib_libadd), "'"))

    ## removed in 2.10.0
    ## if (WINDOWS) makeargs <- c(makeargs, "all")
    if (WINDOWS && debug) makeargs <- c(makeargs, "DEBUG=T")

    cmd <- paste(MAKE, p1(paste("-f", makefiles)), p1(makeargs), p1(makeobjs))
    if (dry_run) {
        cat("make cmd is\n  ", cmd, "\n\nmake would use\n", sep = "")
        system(paste(cmd, "-n"))
        res <- 0
    } else {
        if (preclean) system(paste(cmd, "shlib-clean"))
        res <- system(cmd)
        if (clean) system(paste(cmd, "shlib-clean"))
    }
    res # probably a multiple of 256
}

## base packages do not have versions and this is called on
## DESCRIPTION.in
## encodings are tricky: this may be done in a foreign encoding
## (e.g., Latin-1 in UTF-8)
.DESCRIPTION_to_latex <- function(descfile, outfile, version = "Unknown")
{
    desc <- read.dcf(descfile)[1, ]
    if (is.character(outfile)) {
        out <- file(outfile, "a")
        on.exit(close(out))
    } else out <- outfile
    cat("\\begin{description}", "\\raggedright{}", sep="\n", file=out)
    fields <- names(desc)
    fields <- fields[! fields %in% c("Bundle", "Package", "Packaged", "Built")]
    if ("Encoding" %in% fields)
        cat("\\inputencoding{", latex_canonical_encoding(desc["Encoding"]),
            "}\n", sep = "", file = out)
    for (f in fields) {
        text <- desc[f]
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Rd2dvi, then added to.
        ## KH: the LaTeX special characters are
        ##   # $ % & _ ^ ~ { } \
        ## \Rd@AsIs@dospecials in Rd.sty handles the first seven, so
        ## braces and backslashes need explicit handling.
        text <- gsub('"([^"]*)"', "\\`\\`\\1''", text, useBytes = TRUE)
        text <- gsub("\\", "\\textbackslash{}", text,
                     fixed = TRUE, useBytes = TRUE)
        text <- gsub("([{}$#_])", "\\\\\\1", text, useBytes = TRUE)
        text <- gsub("@VERSION@", version, text, fixed = TRUE, useBytes = TRUE)
        ## text can have paras, and digest/DESCRIPTION does.
        ## \AsIs is per-para.
        text <- strsplit(text, "\n\n", fixed = TRUE, useBytes = TRUE)[[1]]
        Encoding(text) <- "unknown"
        wrap <- paste("\\AsIs{", text, "}", sep = "")
        if(f %in% c("Author", "Maintainer"))
            wrap <- gsub("<([^@]+)@([^>]+)>", "\\\\email{\\1@\\2}",
                         wrap, useBytes = TRUE)
        if(f == "URL")
            wrap <- gsub("(http://|ftp://)([^[:space:]]+)",
                         "\\\\url{\\1\\2}", wrap, useBytes = TRUE)
        ## Not entirely safe: in theory, tags could contain \ ~ ^.
        cat("\\item[", gsub("([#$%&_{}])", "\\\\\\1", f),
            "]", paste(wrap, collapse = "\n\n"),  "\n", sep = "", file=out)
    }
    cat("\\end{description}\n", file = out)
}

## workhorse of .Rd2dvi
.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", outputEncoding = "latin1",
             append = FALSE, extraDirs = NULL, internals = FALSE,
             silent = FALSE)
{
    if (file_test("-d", files))
        .pkg2tex(files, outfile, encoding = encoding, append = append,
                 asChapter = FALSE, extraDirs = extraDirs,
                 internals = internals, silent = silent)
    else {
        files <- strsplit(files, "[[:space:]]+")[[1]]
        latexdir <- tempfile("ltx")
        dir.create(latexdir)
        if (!silent) message("Converting Rd files to LaTeX ...")
        if (is.character(outfile)) {
            outfile <- file(outfile, if (append) "at" else "wt")
            on.exit(close(outfile))
        }
        latexEncodings <- character()
        for(f in files) {
            cat("  ", basename(f), "\n", sep="")
            if (!internals) {
                lines <- readLines(f)
                if (any(grepl("\\\\keyword\\{\\s*internal\\s*\\}",
                         lines, perl = TRUE))) next
            }
            out <-  file.path(latexdir, sub("\\.[Rr]d$", ".tex", basename(f)))
            ## people have file names with quotes in them.
            latexEncodings <- c(latexEncodings,
                                attr(Rd2latex(f, out, encoding=encoding,
                                              outputEncoding=outputEncoding),
                                     "latexEncoding"))
            writeLines(readLines(out), outfile)
        }
        unique(latexEncodings[!is.na(latexEncodings)])
    }
}

## used for the refman (from doc/manual/Makefile*)
## and for directories from .Rdfiles2tex  (with asChapter = FALSE)
.pkg2tex <-
    function(pkgdir, outfile, internals = FALSE, asChapter = TRUE,
             encoding = "unknown", outputEncoding = "latin1",
             extraDirs = NULL, append = FALSE, silent = FALSE)
{
    ## sort order for topics, a little tricky
    re <- function(x) x[order(toupper(x), x)]

    ## given an installed package with a latex dir or a source package
    ## with a man dir, make a single file for use in the refman.

    options(warn = 1)
    if (missing(outfile))
        outfile <- paste(basename(pkgdir), "-pkg.tex", sep="")

    latexEncodings <- character() # Record any encodings used in the output

    ## First check for a latex dir.
    ## Second guess is this is a >= 2.10.0 package with stored .rds files.
    ## If it does not exist, guess this is a source package.
    latexdir <- file.path(pkgdir, "latex")
    if (!file_test("-d", latexdir)) {
        if (file_test("-d", file.path(pkgdir, "help"))) {
            ## So convert it
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            if (!silent) message("Converting parsed Rd's to LaTeX ",
                                 appendLF = FALSE, domain = NA)
            Rd <- Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
            if (!length(Rd)) {
                if (is.character(outfile))
                    close(file(outfile, if (append) "at" else "wt"))
                return(invisible(character()))
            }
            cnt <- 0L
            for(f in names(Rd)) {
                bf <- basename(f)
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("[Rr]d$", "tex", basename(f))
                latexEncodings <- c(latexEncodings,
                                    attr(Rd2latex(Rd[[f]],
                                                  file.path(latexdir, out),
                                                  encoding = encoding,
                                                  outputEncoding = outputEncoding,
                                                  defines = NULL),
                                         "latexEncoding"))
            }
            if (!silent) message(domain = NA)
        } else {
            files <- c(Sys.glob(file.path(pkgdir, "*.Rd")),
                       Sys.glob(file.path(pkgdir, "*.rd")))
            if (!length(files)) {
                ## is this a source package?  That has man/*.Rd files.
                files <- c(Sys.glob(file.path(pkgdir, "man", "*.Rd")),
                           Sys.glob(file.path(pkgdir, "man", "*.rd")))
                if (!length(files))
                    stop("this package does not have either a ", sQuote("latex"),
                         " or a (source) ", sQuote("man"), " directory",
                         domain = NA)
                if (is.null(extraDirs)) extraDirs <- .Platform$OS.type
                for(e in extraDirs)
                    files <- c(files,
                               Sys.glob(file.path(pkgdir, "man", e, "*.Rd")),
                               Sys.glob(file.path(pkgdir, "man", e, "*.rd")))
            }
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            message("Converting Rd files to LaTeX ...")
            for(f in files) {
                cat("  ", basename(f), "\n", sep="")
                out <-  sub("\\.[Rr]d$", ".tex", basename(f))
                latexEncodings <-
                    c(latexEncodings,
                      attr(Rd2latex(f, file.path(latexdir, out),
                                    encoding = encoding,
                                    outputEncoding = outputEncoding),
                           "latexEncoding"))
            }
        }
    }
    ## they might be zipped up
    if (file.exists(f <- file.path(latexdir, "Rhelp.zip"))) {
        dir.create(newdir <- tempfile("latex"))
        unzip(f, exdir = newdir)
        ## res <- system(paste("unzip -q", f, "-d", newdir))
        ## if (res) stop("unzipping latex files failed")
        latexdir <- newdir
    }
    ## There are some restrictions, but the former "[[:alnum:]]+\\.tex$" was
    ## too strict.
    files <- dir(latexdir, pattern = "\\.tex$", full.names = TRUE)
    if (!length(files))
        stop("no validly-named files in the ", sQuote("latex"), " directory",
             domain = NA)

    if (is.character(outfile)) {
        outcon <- file(outfile, if (append) "at" else "wt")
        on.exit(close(outcon))
    } else outcon <- outfile

    if (asChapter)
        cat("\n\\chapter{The \\texttt{", basename(pkgdir), "} package}\n",
            sep = "", file = outcon)
    topics <- rep.int("", length(files)); names(topics) <- files
    scanForEncoding <- !length(latexEncodings)
    for (f in files) {
        lines <- readLines(f)  # This reads as "unknown", no re-encoding done
        hd <- grep("^\\\\HeaderA", lines, value = TRUE,
                   perl = TRUE, useBytes = TRUE)
        if (!length(hd)) {
            warning("file ", sQuote(f), " lacks a header: skipping",
                    domain = NA)
            next
        }
        this <- sub("\\\\HeaderA\\{\\s*([^}]*)\\}.*", "\\1", hd[1], perl = TRUE)
        if (!internals &&
           any(grepl("\\\\keyword\\{\\s*internal\\s*\\}", lines, perl = TRUE)))
            next
        if (scanForEncoding) {
	    enc <- lines[grepl('^\\\\inputencoding', lines, perl = TRUE)]
	    latexEncodings <- c(latexEncodings,
	                        sub("^\\\\inputencoding\\{(.*)\\}", "\\1", enc))
	}
        topics[f] <- this
    }

    topics <- topics[nzchar(topics)]
    summ <- grep("-package$", topics, perl = TRUE)
    topics <- if (length(summ)) c(topics[summ], re(topics[-summ])) else re(topics)
    for (f in names(topics)) writeLines(readLines(f), outcon)

    if (asChapter)
        cat("\\clearpage\n", file = outcon)

    invisible(latexEncodings)
}

## replacement for tools/Rdnewer.pl
.Rdnewer <- function(dir, file)
    q("no", status = ..Rdnewer(dir, file), runLast = FALSE)

..Rdnewer <- function(dir, file, OS = .Platform$OS.type)
{
    ## Test whether any Rd file in the 'man' and 'man/$OS'
    ## subdirectories of directory DIR is newer than a given FILE.
    ## Return 0 if such a file is found (i.e., in the case of
    ## 'success'), and 1 otherwise, so that the return value can be used
    ## for shell 'if' tests.

    ## <NOTE>
    ## For now only used for the R sources (/doc/manual/Makefile.in)
    ## hence no need to also look for Rd files with '.rd' extension.
    ## </NOTE>

    if (!file.exists(file)) return(0L)
    age <- file.info(file)$mtime

    if (any(file.info(c(Sys.glob(file.path(dir, "man", "*.Rd")),
                        Sys.glob(file.path(dir, "man", "*.rd")))
                      )$mtime > age))
        return(0L)

    if (isTRUE(file.info(file.path(dir, OS))$isdir)) {
        if (any(file.info(c(Sys.glob(file.path(dir, "man", OS, "*.Rd")),
                            Sys.glob(file.path(dir, "man", OS, "*.rd")))
                          )$mtime > age))
            return(0L)
    }

    1L
}

## called for base packages from src/Makefile[.win] and from
## .install.packages in this file.
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

    html_header <- function(pkg, title, version, encoding, conn)
    {
        cat('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
            '<html><head><title>R: ', title, '</title>\n',
            '<meta http-equiv="Content-Type" content="text/html; charset=',
            encoding, '">',
            '<link rel="stylesheet" type="text/css" href="../../R.css">\n',
            '</head><body>\n',
            '<h1>', title, ' <img class="toplogo" src="../../../doc/html/logo.jpg" alt="[R logo]"></h1>\n\n<hr>\n\n',
            '<div align="center">\n<a href="../../../doc/html/packages.html"><img src="../../../doc/html/left.jpg"\n',
            'alt="[Package List]" width="30" height="30" border="0"></a>\n',
            '<a href="../../../doc/html/index.html"><img src="../../../doc/html/up.jpg"\n',
            'alt="[Top]" width="30" height="30" border="0"></a>\n</div>\n\n',
            '<h2>Documentation for package &lsquo;', pkg, '&rsquo; version ',
            version, '</h2>\n\n', sep ='', file = conn)

        if (file.exists(file.path(outDir, "doc")))
		    cat('<h2>User Guides and Package Vignettes</h2>\n',
		        'Read <a href="../doc/index.html">overview</a> or ',
		        'browse <a href="../doc">directory</a>.\n\n',
	        sep = '', file=conn)

        cat('<h2>Help Pages</h2>\n\n\n',
            sep ='', file = conn)
    }

    firstLetterCategory <- function(x)
    {
        x[grep("-package$", x)] <- " "
        x <- toupper(substr(x, 1, 1))
        x[x > "Z"] <- "misc"
        x[x < "A" & x != " "] <- ""
        x
    }

    ## This may well already have been done:
    Rd <- if (file.exists(f <- file.path(outDir, "Meta", "Rd.rds")))
        .readRDS(f)
    else {
        ## Keep this in sync with .install_package_Rd_indices().
        ## Rd objects should already have been installed.
        db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                       error = function(e) NULL)
        ## If not, we build the Rd db from the sources:
        if (is.null(db)) db <- Rd_db(dir = dir)
        Rd <- Rd_contents(db)
        .saveRDS(Rd, file.path(outDir, "Meta", "Rd.rds"))
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
    MM <- M[re(M[, 1]), 1:2]
    write.table(MM, file.path(outman, "AnIndex"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    a <- structure(MM[, 2L], names=MM[, 1L])
    .saveRDS(a, file.path(outman, "aliases.rds"))

    ## no HTML indices if no help pages?
    outman <- file.path(outDir, "html")
    dir.create(outman, showWarnings = FALSE)
    outcon <- file(file.path(outman, "00Index.html"), "wt")
    on.exit(close(outcon))
    desc <- read.dcf(file.path(outDir, "DESCRIPTION"))[1,]
    ## re-encode if necessary
    if(!is.na(enc <- desc["Encoding"])) {
        ## should be valid in UTF-8, might be invalid in declared encoding
        desc <- iconv(desc, enc, "UTF-8", sub = "byte")
    }
    ## drop internal entries
    M <- M[!M[, 4], ]
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
            M <- M[re(M[, 1]), ]
        }
    } else M <- M[re(M[, 1]), ]
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

    html_header(desc["Package"], desc["Title"], desc["Version"], "UTF-8", outcon)

    use_alpha <- (nrow(M) > 100)
    if (use_alpha) {
        first <- firstLetterCategory(M$Topic)
        nm <- sort(names(table(first)))
        m <- match(" ", nm, 0L)
        if (m) nm <- c(" ", nm[-m])
        writeLines("<p align=\"center\">", outcon)
        writeLines(paste("<a href=\"#", nm, "\">", nm, "</a>", sep = ""),
                   outcon)
        writeLines("</p>\n", outcon)

        for (f in nm) {
            MM <- M[first == f, ]
            cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
                sep = "", file = outcon)
            writeLines('<table width="100%">', outcon)
            writeLines(paste('<tr><td width="25%"><a href="', MM[, 2], '.html">',
                             MM$HTopic, '</a></td>\n<td>', MM[, 3],'</td></tr>',
                             sep = ''), outcon)
            writeLines("</table>", outcon)
       }
    } else if (nrow(M)) {
        writeLines('<table width="100%">', outcon)
        writeLines(paste('<tr><td width="25%"><a href="', M[, 2], '.html">',
                         M$HTopic, '</a></td>\n<td>', M[, 3],'</td></tr>',
                         sep = ''), outcon)
        writeLines("</table>", outcon)
    } else { # no rows
         writeLines("There are no help pages in this package", outcon)
    }
    writeLines('</body></html>', outcon)
}

### * .convertRdfiles

## possible types are "html", "latex", "example"
## outenc is used as the default output encoding for latex conversion
.convertRdfiles <-
    function(dir, outDir, types = "html", silent = FALSE, outenc = "latin1")
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
        cat(type, rep(" ", max(0L, 6L - nchar(type))), sep="")
    }

    dirname <- c("html", "latex", "R-ex")
    ext <- c(".html", ".tex", ".R", ".html")
    names(dirname) <- names(ext) <- c("html", "latex", "example")
    mandir <- file.path(dir, "man")
    if (!file_test("-d", mandir)) return()
    desc <- .readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION
    pkg <- desc["Package"]
    ver <- desc["Version"]

    for(type in types)
        dir.create(file.path(outDir, dirname[type]), showWarnings = FALSE)

    cat("  converting help for package ", sQuote(pkg), "\n", sep="")

    ## FIXME: add this lib to lib.loc?
    if ("html" %in% types) {
        ## may be slow, so add a message
        if (!silent) message("    finding HTML links ...", appendLF = FALSE)
        Links <- findHTMLlinks(outDir, level = 0:1)
        if (!silent) message(" done")
        .Links2 <- function() {
            message("\n    finding level-2 HTML links ...", appendLF = FALSE)
            Links2 <- findHTMLlinks(level = 2)
            message(" done")
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
        message("") # force newline
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
                            paste(bf, ext[type], sep = ""))
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
                            paste(bf, ext[type], sep = ""))
            if (!file_test("-f", ff) || file_test("-nt", f, ff)) {
                showtype(type)
                .convert(Rd2latex(Rd, ff, defines = NULL,
                                  outputEncoding = outenc))
            }
        }
        if ("example" %in% types) {
            type <- "example"
            ff <- file.path(outDir, dirname[type],
                            paste(bf, ext[type], sep = ""))
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
    bfs <- sub("\\.[Rr]d$", "", basename(files)) # those to keep
    if ("html" %in% types) {
        type <- "html"
        have <- list.files(file.path(outDir, dirname[type]))
        have2 <- sub("\\.html", "", basename(have))
        drop <- have[! have2 %in% c(bfs, "00Index")]
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
        desc <- read.dcf(f)[[1]]
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
        "            VALUE \"FileVersion\", \"", version, "\\0\"\n", sep="")
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

### * .Rdconv

## replacement R code for Perl-based R CMD Rdconv

.Rdconv <- function(args = NULL)
{
    Usage <- function() {
        cat("Usage: R CMD Rdconv [options] FILE",
            "",
            "Convert R documentation in FILE to other formats such as plain text,",
            "HTML or LaTeX.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -t, --type=TYPE	convert to format TYPE",
            "  --encoding=enc        use 'enc' as the output encoding",
            "  --package=pkg         use 'pkg' as the package name",
            "  -o, --output=OUT	use 'OUT' as the output file",
            "      --os=NAME		assume OS 'NAME' (unix or windows)",
            "      --OS=NAME		the same as '--os'",
            "",
            "Possible format specifications are 'txt' (plain text), 'html', 'latex',",
            "and 'example' (extract R code in the examples).",
            "",
            "The default is to send output to stdout, which is also given by '-o -'.",
            "Using '-o \"\"' will choose an output filename by removing a '.Rd'",
            "extension from FILE and adding a suitable extension.",
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep = "\n")
    }

    options(showErrorCalls = FALSE, warn = 1)
    files <- character(0L)
    type <- "unknown"
    enc <- ""
    pkg <- ""
    out <- NULL
    os <- ""

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("Rdconv: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 1997-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "-t") {
            if (length(args) >= 2) {type <- args[2]; args <- args[-1]}
            else stop("-t option without value", call. = FALSE)
        } else if (substr(a, 1, 7) == "--type=") {
            type <- substr(a, 8, 1000)
        } else if (substr(a, 1, 10) == "--encoding=") {
            enc <- substr(a, 11, 1000)
        } else if (substr(a, 1, 10) == "--package=") {
            pkg <- substr(a, 11, 1000)
        } else if (a == "-o") {
            if (length(args) >= 2) {out <- args[2]; args <- args[-1]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            out <- substr(a, 10, 1000)
        } else if (substr(a, 1, 5) %in% c("--os=", "--OS=")) {
            os <- substr(a, 6, 1000)
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else files <- c(files, a)
        args <- args[-1L]
    }
    if (length(files) != 1L)
        stop("exactly one Rd file must be specified", call. = FALSE)
    if (is.character(out) && !nzchar(out)) {
        ## choose 'out' from filename
        bf <- sub("\\.[Rr]d$", "", file)
        exts <- c(txt=".txt", html=".html", latex=".tex", exmaple=".R")
        out <- paste(bf,  exts[type], sep = "")
    } else if (is.null(out)) out <- ""
    if (!nzchar(os)) os <- .Platform$OS.type
    switch(type,
           "txt" = {
               Rd2txt(files, out, package=pkg, defines=os,
                      outputEncoding = enc)
           },
           "html" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2HTML(files, out, package = pkg, defines = os,
                       outputEncoding = enc, no_links = TRUE)
           },
           "latex" = {
               if (!nzchar(enc)) enc <- "latin1"
               Rd2latex(files, out, defines = os,
                        outputEncoding = enc)
           },
           "example" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2ex(files, out, defines = os, outputEncoding = enc)
           },
           "unknown" = stop("no 'type' specified", call. = FALSE),
           stop("'type' must be one of 'txt', 'html', 'latex' or 'example'",
                call. = FALSE)
           )
    invisible()
}

### * .Rd2dvi

.Rd2dvi <-
function(pkgdir, outfile, is_bundle, title, batch = FALSE,
         description = TRUE, only_meta = FALSE,
         enc = "unknown", outputEncoding = "latin1", files_or_dir, OSdir,
         internals = "no", index = "true")
{
    # print(match.call())

    ## %in% and others cause problems for some page layouts.
    if (basename(pkgdir) == "base") index <- "false"
    # Write directly to the final location.  Encodings may mean we need
    # to make edits, but for most files one pass should be enough.
    out <- file(outfile, "wt")
    if (!nzchar(enc)) enc <- "unknown"
    description <- description == "true"
    only_meta <- only_meta == "true"
    internals <- internals != "no"
    index <- index != "false"

    desc <- NULL
    if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
        desc <- read.dcf(f)[1,]
        if (enc == "unknown") {
            pkg_enc <- desc["Encoding"]
            if (!is.na(pkg_enc)) {
            	enc <- pkg_enc
            	outputEncoding <- pkg_enc
            }
        }
    }

    ## Rd2.tex part 1: header
    if (batch == "true") writeLines("\\nonstopmode{}", out)
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4DVI", "ae"), "]{Rd}\n",
        sep = "", file = out)
    if (index) writeLines("\\usepackage{makeidx}", out)
    ## this needs to be canonical, e.g. 'utf8'
    setEncoding <- paste("\\usepackage[", latex_canonical_encoding(outputEncoding), "]{inputenc} % @SET ENCODING@", sep="")
    writeLines(c(setEncoding,
                 "\\makeindex{}",
                 "\\begin{document}"), out)
    if (is_bundle == "no") {
        if (!nzchar(title)) {
            if (is.character(desc))
                title <- paste("Package `", desc["Package"], "'", sep = "")
            else if (file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
                desc <- read.dcf(f)[1,]
                title <- paste("Package `", desc["Package"], "'", sep = "")
            } else {
                if (file_test("-d", pkgdir)) {
                    subj <- paste("all in \\file{", pkgdir, "}", sep ="")
                } else {
                    files <- strsplit(files_or_dir, "[[:space:]]+")[[1]]
                    subj1 <- if (length(files) > 1) " etc." else ""
                    subj <- paste("\\file{", pkgdir, "}", subj1, sep = "")
                }
                subj <- gsub("[_$]", "\\\\1", subj)
                title <- paste("\\R{} documentation}} \\par\\bigskip{{\\Large of", subj)
            }
        }
        cat("\\chapter*{}\n",
            "\\begin{center}\n",
            "{\\textbf{\\huge ", title, "}}\n",
            "\\par\\bigskip{\\large \\today}\n",
            "\\end{center}\n", sep = "", file = out)
        if (description && file.exists(f <- file.path(pkgdir, "DESCRIPTION")))
            .DESCRIPTION_to_latex(f, out)
        ## running on the sources of a base package will have DESCRIPTION.in,
        ## only.
        if (description &&
           file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            version <- readLines(file.path(pkgdir, "../../../VERSION"))
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION.in"),
                                  out, version)
        }
    } else { ## bundle case
        if (!nzchar(title) && is.character(desc))
            title <- paste("Bundle `", desc["Bundle"], "'", sep = "")
        cat("\\pagenumbering{Roman}\n",
            "\\begin{titlepage}\n",
            "\\strut\\vfill\n",
            "\\begin{center}\n",
            "{\\textbf{\\Huge ", title, "}}\n",
            "\\par\\bigskip{\\large \\today}\n",
            "\\end{center}\n",
            "\\par\\bigskip\n", sep = "", file = out)
        if (description)
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION"), out)
        writeLines("\\vfill\\vfill\n\\end{titlepage}", out)
    }

    ## Rd2.tex part 2: body
    toc <- if (file_test("-d", files_or_dir)) {
        "\\Rdcontents{\\R{} topics documented:}"
    } else ""

    latexEncodings <- character(0)
    if (is_bundle == "no") {
        ## if this looks like a package with no man pages, skip body
        if (file.exists(file.path(pkgdir, "DESCRIPTION")) &&
           !(file_test("-d", file.path(pkgdir, "man")) ||
             file_test("-d", file.path(pkgdir, "help")) ||
             file_test("-d", file.path(pkgdir, "latex")))) only_meta <- TRUE
        if (!only_meta) {
            if (nzchar(toc)) writeLines(toc, out)
            latexEncodings <-
                .Rdfiles2tex(files_or_dir, out, encoding = enc, append = TRUE,
                             extraDirs = OSdir, internals = internals,
                             silent = (batch == "true"))
        }
    } else {
        writeLines(c("\\setcounter{secnumdepth}{-1}",
                     "\\pagenumbering{roman}",
                     "\\tableofcontents{}",
                     "\\cleardoublepage{}",
                     "\\pagenumbering{arabic}"), out)
        desc <- read.dcf(file.path(pkgdir, "DESCRIPTION"))[1,]
        bundle_pkgs <- .get_contains_from_package_db(desc)
        for (p in bundle_pkgs) {
            message("Bundle package: ", p)
            cat("\\chapter{Package `", p, "'}\n", sep = "", file = out)
            if (description &&
                file.exists(f <- file.path(pkgdir, p, "DESCRIPTION.in")))
                .DESCRIPTION_to_latex(f, out)
            if (!only_meta)
                latexEncodings <- c(latexEncodings, .pkg2tex(file.path(pkgdir, p), out, encoding = enc,
                         append = TRUE, asChapter = FALSE,
                         internals = internals))
            writeLines("\\clearpage{}", out)
        }
        writeLines("\\cleardoublepage{}", out)
    }

    ## Rd2.tex part 3: footer
    if (index) writeLines("\\printindex{}", out)
    writeLines("\\end{document}", out)
    close(out)

    ## Fix up encodings
    ## FIXME cyrillic probably only works with times, not ae.
    latexEncodings <- unique(latexEncodings)
    latexEncodings <- latexEncodings[!is.na(latexEncodings)]
    cyrillic <- if (nzchar(Sys.getenv("_R_CYRILLIC_TEX_"))) "utf8" %in% latexEncodings else FALSE
    latex_outputEncoding <- latex_canonical_encoding(outputEncoding)
    encs <- latexEncodings[latexEncodings != latex_outputEncoding]
    if (length(encs) || cyrillic) {
        lines <- readLines(outfile)
	encs <- paste(encs, latex_outputEncoding, collapse=",", sep=",")

	if (!cyrillic) {
	    lines[lines == setEncoding] <-
		paste("\\usepackage[", encs, "]{inputenc}", sep = "")
	} else {
	    lines[lines == setEncoding] <-
		paste(
"\\usepackage[", encs, "]{inputenc}
\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}", sep = "")
	}
	writeLines(lines, outfile)
    }

    invisible(NULL)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
