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

## calls sytem() on Windows for sh mv make zip perl hhc

.install_packages <- function(args = NULL)
{
    .file_test <- function(op, x, y)
        ## we don't want to load utils just for this
        switch(op,
               "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
               "-d" = !is.na(isdir <- file.info(x)$isdir) & isdir,
               "-x" = (file.access(x, 1L) == 0L),
               stop(sprintf("test '%s' is not available", op), domain = NA))

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
            "sources, or to gzipped package 'tar' archives (Unix only).  The library tree",
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
            "      --no-docs		do not build and install documentation",
            "      --no-text		do not build text help",
            "      --no-html		do not build HTML help",
            "      --no-latex      	do not build LaTeX help",
            "      --no-example   	do not install R code for help examples",
            "      --use-zip-data	collect data files in zip archive",
            "      --use-zip-help	collect help and examples into zip archives",
            "      --use-zip		combine '--use-zip-data' and '--use-zip-help'",
            "      --install-tests	install package-specific tests (if any)",
            "      --fake		do minimal install for testing purposes",
            "      --no-lock, --unsafe",
            "			install on top of any existing installation",
            "			without using a lock directory",
            "      --pkglock		use a per-package lock directory",
            "      --build    	build binaries of the installed package(s)",
            "\nfor Unix",
            "      --configure-args=ARGS",
            "			set arguments for the configure scripts (if any)",
            "      --configure-vars=VARS",
            "			set variables for the configure scripts (if any)",
            "      --libs-only	only install the libs directory",
            "      --no-multiarch	build only the main architecture",
            "\nand on Windows only",
            "      --auto-zip	select whether to zip automatically",
            "      --no-chm		do not build CHM help [disabled pro lem]",
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
                .file_append_ensuring_LFs(file.path(R.home("doc"), "html", "search", "index.txt"),
                                                  Sys.glob(file.path(R.home(), "library", "*", "CONTENTS")))
                if (build_help && identical(Sys.getenv("NO_PERL5"), "false"))
                    tools::unix.packages.html(.Library, docdir = R.home("doc"))

            }
        }
        if (lock && nzchar(lockdir)) unlink(lockdir, recursive = TRUE)
    }

    do_cleanup_tmpdir <- function()
    {
        ## Solaris will not remove any directory in the current path
        setwd(startdir)
        if (.file_test("-d", tmpdir)) unlink(tmpdir, recursive=TRUE)
    }

    do_exit_on_error <- function()
    {
        # message("*** do_exit_on_error ***")
        ## If we are not yet processing a package, we will not have
        ## set bundle_pkgs
        for(p in bundle_pkgs) {
            if (is.na(p) || !nzchar(p)) next
            pkgdir <- file.path(lib, p)
            if (nzchar(pkgdir) && .file_test("-d", pkgdir)) {
                starsmsg(stars, "Removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }
            if (lock && nzchar(lockdir) &&
                .file_test("-d", lp <- file.path(lockdir, p))) {
                starsmsg(stars, "Restoring previous ", sQuote(pkgdir))
                ## FIXME: on Windows use file.copy(recursive = TRUE)
                system(paste("mv", lp, pkgdir))
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
        message("ERROR: ", msg, " for package ", sQuote(pkg))
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
            contains <- strsplit(desc["Contains"], " ")[[1]]
            for(p in contains) {
                if (.file_test("-d", file.path(pkg, p))) {
                    pkgs <- c(pkgs, p)
                } else {
                    warning("incorrect Contains metadata for bundle ",
                            sQuote(bundle_name),
                            ": there is no package '", sQuote(p), call. = FALSE)
                    warning("skipping installation of bundle ",
                            sQuote(bundle_name), call. = FALSE)
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
                            call. = FALSE)
            }
            ##if (tar_up)
            ##    errmsg("cannot create a binary bundle: use 'R CMD build --binary' to do so")
            bundle_pkgs <<- contains
        } else {
            bundle_name <- desc["Package"]
            if (is.na(bundle_name)) errmsg("no 'Package' field in 'DESCRIPTION'")
            bundle_pkgs <<- bundle_name
        }

        for(p in bundle_pkgs) {
            if(is_bundle) {
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
            if (!.file_test("-d", instdir)) {
                message("ERROR unable to create ", sQuote(instdir))
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

            ## Add read permission to all, write permission to ownwe
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
                    sQuote(bundle_name), " as ", filename, ".gz")
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
            message("packaged installation of ",
                    sQuote(bundle_name), " as ", filename)
            setwd(owd)
        }

        starsmsg(stars, "DONE (", bundle_name, ")")

        bundle_pkgs <<- character()
    }


    do_install_binary <- function(pkg, instdir, desc)
    {
        starsmsg(stars, "Installing *binary* package ", sQuote(pkg), " ...")

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
        if (.file_test("-d", "src")) {
            owd <- setwd("src")
            if(WINDOWS) {
                if (file.exists("Makefile.win"))
                    system(paste(MAKE, "-f Makefile.win clean"))
                else
                    system("rm -f *_res.rc *.o *.d Makedeps")
                ## FIXME copied from MakePkg: reconsider?
                system("rm -rf ../chm ../check ../tests/*.Rout")
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
        paste0 <- function(...) paste(..., sep="")

        cp_r <- function(from, to)
        {
            ## unused now
            if (WINDOWS) {
                system(paste0("cp -r ", shQuote(from), "/* ", shQuote(to)))
            } else {
                from <- shQuote(from)
                to <- shQuote(to)
                system(paste("cp -r ", from, "/* ", to,
                             " || (cd ", from, " && ", TAR, " cf - . | (cd '", to, "' && ", TAR, "xf - ))", sep = ""))
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
                if(!WINDOWS)
                    Sys.chmod(Sys.glob(file.path(dest, "*")), "755")
            }
        }

        run_shlib <- function(pkg_name, srcs, instdir, arch)
        {
            args <- c(shargs, "-o", paste0(pkg_name, SHLIB_EXT), srcs)
            if (debug) message("about to run ",
                               "R CMD SHLIB ", paste(args, collapse= " "))
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
            starsmsg(stars, "Installing *Frontend* package ", sQuote(pkg_name), " ...")
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
            starsmsg(stars, "Installing *Translation* package ", sQuote(pkg_name), " ...")
            if (.file_test("-d", "share")) {
                files <- Sys.glob("share/*")
                if (length(files)) file.copy(files, R.home("share"), TRUE)
            }
            if (.file_test("-d", "library")) {
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

        starsmsg(stars, "Installing *source* package ", sQuote(pkg_name), " ...")

        stars <- "**"

        if (file.exists(file.path(instdir, "DESCRIPTION"))) {
            ## Back up a previous version
            if (lock) {
                if (debug) starsmsg(stars, "backing up earlier installation")
                ## FIXME use file.copy + unlink
                system(paste("mv", instdir, file.path(lockdir, pkg_name)))
            } else if (more_than_libs && !WINDOWS)
                unlink(instdir, recursive = TRUE)
            dir.create(instdir, recursive=TRUE, showWarnings = FALSE)

            ## Preserve man pages to speed up installation?  Only makes sense
            ## if we install from a non-temporary directory.
            if (!WINDOWS && lock && is.na(pmatch(tmpdir, getwd()))) {
                system(paste("(cd", shQuote(file.path(lockdir, pkg_name)),
                             "&&", TAR,
                             "cf  - R-ex help html latex 2>/dev/null) | (cd",
                             shQuote(instdir), "&&", TAR, "xf -)"))
            }
        }

        if (preclean) run_clean()

        if (auto_zip || zip_up) { ## --build implies --auto-zip
            thislazy <- parse_description_field(desc, "LazyData",
                                                default = lazy_data)
            thiszip <- parse_description_field(desc, "ZipData",
                                               default = TRUE)
            if (!thislazy && thiszip && .file_test("-d", "data")) {
                fi <- file.info(dir("data", full.names=TRUE))
                if (sum(fi$size) > 100000) {
                    this <- sub("\\.[a-zA-Z]+$", "", row.names(fi))
                    if(!any(duplicated(this))) use_zip_data <- TRUE
                }
            }
            if (.file_test("-d", "man") &&
                length(Sys.glob("man/*.Rd")) > 20) use_zip_help <- TRUE
            message("\n  Using auto-selected zip options '",
                    if (use_zip_data) "--use-zip-data ",
                    if (use_zip_help) "--use-zip-help",
                    "'\n")
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
                            "   **********************************************\n\n")
            } else {
                ## FIXME: should these be quoted?
                if (.file_test("-x", "configure")) {
                    cmd <- paste(paste(configure_vars, collapse = " "),
                                 "./configure",
                                 paste(configure_args, collapse = " "))
                    if (debug) message("configure command: ", sQuote(cmd))
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

        if (.file_test("-d", "src") && !fake) {
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
            if (.file_test("-d", "R")) {
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

            if (.file_test("-d", "data")) {
                starsmsg(stars, "data")
                files <- Sys.glob(file.path("data", "*"))
                if (length(files)) {
                    dir.create(file.path(instdir, "data"), recursive = TRUE,
                               showWarnings = FALSE)
                    file.remove(Sys.glob(file.path(instdir, "data", "*")))
                    file.copy(files, file.path(instdir, "data"), TRUE)
                    Sys.chmod(Sys.glob(file.path(instdir, "data", "*")), "644")
                    thislazy <- parse_description_field(desc, "LazyData",
                                                        default = lazy_data)
                    if (thislazy) {
                        ## This also had an extra space in the sh version
                        starsmsg(stars, " moving datasets to lazyload DB")
                        ## 'it is possible that data in a package will
                        ## make use of the code in the package, so ensure
                        ## the package we have just installed is on the
                        ## library path.'
                        ## (We set .libPaths)
                        res <- try(data2LazyLoadDB(pkg_name, lib))
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

            if (.file_test("-d", "demo") && !fake) {
                starsmsg(stars, "demo")
                dir.create(file.path(instdir, "demo"), recursive = TRUE,
                           showWarnings = FALSE)
                file.remove(Sys.glob(file.path(instdir, "demo", "*")))
                res <- try(.install_package_demos(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("ERROR: installing demos failed")
                Sys.chmod(Sys.glob(file.path(instdir, "demo", "*")), "644")
            }

            if (.file_test("-d", "exec") && !fake) {
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

            if (.file_test("-d", "inst") && !fake) {
                starsmsg(stars, "inst")
                ## FIXME avoid installing .svn etc?
                cp_r("inst", instdir)
                ## file.copy("inst", "instdir", recursive = TRUE)
            }

            if (install_tests && .file_test("-d", "tests") && !fake) {
                starsmsg(stars, "tests")
                ## system(paste0("cp -r tests " , shQuote(instdir)))
                file.copy("tests", instdir, recursive = TRUE)
            }

            ## Defunct:
            if (file.exists("install.R"))
                warning("use of file 'install.R' is no longer supported",
                        call. = FALSE)
            if (file.exists("R_PROFILE.R"))
                warning("use of file 'R_PROFILE.R' is no longer supported",
                        call. = FALSE)
            value <- parse_description_field(desc, "SaveImage", default = NA)
            if (!is.na(value))
                warning("field 'SaveImage' is defunct: please remove it",
                        call. = FALSE)


            ## LazyLoading
            value <- parse_description_field(desc, "LazyLoad", default = lazy)
            if (.file_test("-d", "R") && value) {
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

            if (.file_test("-d", "man")) {
                starsmsg(stars, "help")
                res <- try(.install_package_man_sources(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("installing man sources failed", pkg_name)
                Sys.chmod(file.path(instdir, "man",
                                    paste0(pkg_name, ".Rd.gz")), "644")
                ## 'Maybe build preformatted help pages ...'
                if (build_help) {
                    starsmsg(paste0(stars, "*"),
                             "installing help indices")
                    .writePkgIndices(pkg_dir, instdir, CHM = build_chm)
                    cmd <- paste("perl",
                                 shQuote(file.path(R.home("share"), "perl",
                                                   "build-help.pl")),
                                 paste(build_help_opts, collapse=" "),
                                 shQuote(pkg_dir),
                                 shQuote(lib),
                                 shQuote(instdir),
                                 pkg_name)
                    if (debug) message("about to run ", sQuote(cmd))
                    res <- system(cmd)
                    if (res)
                        pkgerrmsg("building help failed", pkg_name)
                    if (use_zip_help &&
                        (WINDOWS ||
                         (nzchar(Sys.getenv("R_UNZIPCMD")) &&
                          nzchar(zip <- Sys.getenv("R_ZIPCMD")) ))) {
                        owd <- setwd(instdir)
                        if (.file_test("-d", "R-ex")) {
                            wd2 <- setwd("R-ex")
                            system(paste(zip, " -q -m Rex *.R"))
                            setwd(wd2)
                        }
                        if (.file_test("-d", "help")) {
                            wd2 <- setwd("help")
                            system(paste(zip, " -q -m Rhelp * -x AnIndex"))
                            setwd(wd2)
                        }
                        if (.file_test("-d", "latex")) {
                            wd2 <- setwd("latex")
                            system(paste(zip, " -q -m Rhelp *.tex"))
                            setwd(wd2)
                        }
                        setwd(owd)
                    }
                    if (build_chm) {
                        if (.file_test("-d", "chm")) {
                            owd <- setwd("chm")
                            file.copy(file.path(R.home(), "src/gnuwin32/help/Rchm.css"), ".")
                            file.copy(file.path(R.home("doc"), "html/logo.jpg"), ".")
                            system(paste0("hhc ", pkg_name, ".hhp"))
                            ## always gives an error code
                            chm_file <- paste0(pkg_name, ".chm")
                            if (file.exists(chm_file)) {
                                dest <- file.path(instdir, "chtml")
                                ## parent must exist by now
                                dir.create(dest, showWarnings = FALSE)
                                file.copy(chm_file, dest, TRUE)
                            }
                            setwd(owd)
                        }
                    }
                }
            } else                      # yes, to stdout
            cat("No man pages found in package ", sQuote(pkg_name), "\n")

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
    lib <- ""
    if(is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1]][-1]
    }

    startdir <- getwd()

    ## FIXME: move down to where needed
    tmpdir <- tempfile("R.INSTALL")
    if (!dir.create(tmpdir))
        stop("cannot create temporary directory")

    lib <- lib0 <- ""
    clean <- FALSE
    preclean <- FALSE
    debug <- FALSE
    build_text <- TRUE
    build_html <- TRUE
    build_latex <- TRUE
    build_example <- TRUE
    build_chm <- WINDOWS
    use_configure <- TRUE
    use_zip_data <- FALSE
    use_zip_help <- FALSE
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
        } else if (a == "--with-package-versions") {
            stop("Use of --with-package-versions is defunct", call. = FALSE)
        } else if (a == "--no-configure") {
            use_configure = FALSE;
        } else if (a == "--no-docs") {
            build_text <- build_html <- build_latex <- build_example <- build_chm <- FALSE
        } else if (a == "--no-text") {
            build_text <- FALSE
        } else if (a == "--no-html") {
            build_html <- FALSE
        } else if (a == "--no-example") {
            build_example <- FALSE
        } else if (a == "--no-chm") {
            build_chm <- FALSE
        } else if (a == "--use-zip") {
            use_zip_data <- use_zip_help <- TRUE
        } else if (a == "--use-zipdata") {
            use_zip_data <- TRUE
        } else if (a == "--use-ziphelp") {
            use_zip_help <- TRUE
        } else if (a == "--auto-zip") {
            if (WINDOWS) auto_zip <- TRUE
            else warning("--auto-zip' is for Windows only", call. = FALSE)
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
        } else if (a == "--build") {
            if (WINDOWS) zip_up <- TRUE else tar_up <- TRUE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1]
    }

    ## now unpack tarballs and do some basic checks
    allpkgs <- character(0)
    for(pkg in pkgs) {
        if (debug) message("processing ", sQuote(pkg))
        if (.file_test("-f", pkg)) {
            if (debug) message("a file")
            pkgname <- basename(pkg) # or bundle name
            ## Also allow for 'package.tgz' ...
            pkgname <- sub("\\.tgz$", "", pkgname)
            pkgname <- sub("_.*", "", pkgname)
            res <- if (WINDOWS) {
                ## FIXME: may need to play with paths: Perl version had
                ## $pkg =~ s+^([A-Za-x]):+/cygdrive/\1+;
                td <- chartr("\\", "/", tmpdir)
                system(paste("tar -zxf", shQuote(pkg), "-C", shQuote(td)))
            } else {
                ## Note that we use '-m' so that modification dates are *not*
                ## preserved when untarring the sources.  This is necessary to
                ## ensure that the preformatted help pages are always rebuilt.
                ## Otherwise, the build date for an older version may be newer
                ## than the modification date for the new sources as recorded in
                ## the tarball ...

                ## We cannot assume GNU tar, but we can assume system uses
                ## a shell which understands subshells and pipes
                system(paste(GZIP, "-dc", shQuote(pkg),
                             "| (cd ", shQuote(tmpdir), "&&", TAR, "-mxf -)"))
            }
            if (res) errmsg("error unpacking tarball")
            ## If we have a binary bundle distribution, there should be
            ## a DESCRIPTION file at top level.
            if (file.exists(ff <- file.path(tmpdir, "DESCRIPTION"))) {
                con <- read.dcf(ff, "Contains")
                if (!is.na(con)) {
                    starsmsg(stars, "Looks like a binary bundle")
                    allpkgs <- c(allpkgs, tmpdir)
                } else {
                    message("unknown package layout")
                    do_cleanup_tmpdir()
                    q("no", status = 1, runLast = FALSE)
                }
            } else if (file.exists(file.path(tmpdir, pkgname, "DESCRIPTION"))) {
                allpkgs <- c(allpkgs, file.path(tmpdir, pkgname))
            } else errmsg("cannot extract package from ", sQuote(pkg))
        } else if (file.exists(file.path(pkg, "DESCRIPTION"))) {
            if (debug) message("a directory")
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
        lib <- .libPaths()[1]
        starsmsg(stars, "Installing to library ", sQuote(lib))
    } else {
        lib0 <- lib <- path.expand(lib)
        ## lib is allowed to be a relative path.
        ## should be OK below, but be sure.
        cwd <- try(setwd(lib), silent = TRUE)
        if (inherits(cwd, "try-error"))
            stop("ERROR: cannot cd to directory ", sQuote(lib), call. = FALSE)
        lib <- getwd()
        setwd(cwd)
    }
    if (!.file_test("-d", lib) || file.access(lib, 2L))
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
            q("no", status=3, runLast = FALSE)
        }
        dir.create(lockdir, recursive = TRUE)
        if (!.file_test("-d", lockdir)) {
            message("ERROR: failed to create lock directory ", sQuote(lockdir))
            do_cleanup_tmpdir()
            q("no", status=3, runLast = FALSE)
        }
        if (debug) starsmsg(stars, "created lock directory ", sQuote(lockdir))
    }

    if  (tar_up && fake)
        stop("building a fake installation is disallowed")

    if (fake) {
        use_configure <- FALSE
        build_text <- TRUE
        build_html <- FALSE
        build_latex <- TRUE
        build_example <- FALSE
        build_chm <- FALSE
    }

    if (build_chm) {
        res <- try(system("hhc.exe", intern=TRUE), silent=TRUE)
        if (inherits(res, "try-error")) {
            build_chm <- FALSE
            message("\n*** 'hhc.exe' not found: not building CHM help\n")
        }
    }

    build_help_opts <- character(0)
    if (build_text) build_help_opts <- c(build_help_opts, "--txt")
    if (build_html) build_help_opts <- c(build_help_opts, "--html")
    if (build_latex) build_help_opts <- c(build_help_opts, "--latex")
    if (build_example) build_help_opts <- c(build_help_opts, "--example")
    if (build_chm) build_help_opts <- c(build_help_opts, "--chm")
    build_help <- length(build_help_opts) > 0L
    if (build_help && debug) build_help_opts <- c("--debug", build_help_opts)
    if (build_help && WINDOWS)
        build_help_opts <- c("--os=windows", build_help_opts)

    if (debug)
        starsmsg(stars, "build_help_opts=", paste(build_help_opts, collapse=" "))

    if (build_help) {
        perllib <- Sys.getenv("PERL5LIB")
        if (nzchar(perllib)) {
            Sys.setenv(PERL5LIB = paste(file.path(R.home("share"), "perl"),
                       perllib, sep = .Platform$path.sep))
        } else {
            perllib <- Sys.getenv("PERLLIB")
            Sys.setenv(PERLLIB = paste(file.path(R.home("share"), "perl"),
                       perllib, sep = .Platform$path.sep))
        }
    }

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
        SHLIB_EXT <-sub(".*= ", "", grep("^SHLIB_EXT", mconf, value = TRUE))
        SHLIB_LIBADD <-sub(".*= ", "", grep("^SHLIB_LIBADD", mconf, value = TRUE))
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
                  if(WINDOWS) "winshlib.mk" else "shlib.mk")
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

    if(WINDOWS) makeargs <- c(makeargs, "all")
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
    if("Encoding" %in% fields)
        cat("\\inputencoding{", latex_canonical_encoding(desc["Encoding"]),
            "}\n", sep = "", file = out)
    for (f in fields) {
        text <- desc[f]
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Rd2dvi, then added to.
        text <- gsub('"([^"]*)"', "\\`\\`\\1''", text)
        text <- gsub("\\", "\\textbackslash{}", text, fixed = TRUE)
        text <- gsub("([{}$#_])", "\\\\\\1", text)
        text <- gsub("@VERSION@", version, text, fixed = TRUE)
        ## test can have paras, and digest/DESCRIPTION does.
        ## \AsIs is per-para.
        text <- strsplit(text, "\n\n")[[1]]
        Encoding(text) <- "unknown"
        wrap <- paste("\\AsIs{", text, "}", sep = "")
        cat("\\item[", gsub("([_#])", "\\\\\\1", f),
            "]", paste(wrap, collapse = "\n\n"),  "\n", sep = "", file=out)
    }
    cat("\\end{description}\n", file = out)
}

.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", append = FALSE,
             extraDirs = NULL, internals = FALSE)
{
    if(file_test("-d", files))
        .pkg2tex(files, outfile, encoding = encoding, append = append,
                 asChapter = FALSE, extraDirs = extraDirs,
                 internals = internals)
    else {
        files <- strsplit(files, "[[:space:]]+")[[1]]
        latexdir <- tempfile("ltx")
        dir.create(latexdir)
         message("Converting Rd files to LaTeX ...")
        cmd <- paste(R.home(), "/bin/R CMD Rdconv -t latex --encoding=",
                     encoding, sep="")
        if (is.character(outfile)) {
            outfile <- file(outfile, if(append) "at" else "wt")
            on.exit(close(outfile))
        }
        for(f in files) {
            cat("  ", basename(f), "\n", sep="")
            if(!internals) {
                lines <- readLines(f)
                if(any(grepl("\\\\keyword\\{\\s*internal\\s*\\}",
                         lines, perl = TRUE))) next
            }
            out <-  file.path(latexdir, sub("\\.[Rr]d",".tex", basename(f)))
            ## people have file names with quotes in them.
            system(paste(cmd,"-o", shQuote(out), shQuote(f)))
            writeLines(readLines(out), outfile)
        }
    }
}

## replacement for tools/pkg2tex.pl, and more
.pkg2tex <-
    function(pkgdir, outfile, internals = FALSE, asChapter = TRUE,
             encoding = "unknown", extraDirs = NULL, append = FALSE)
{
    re <- function(x)
    {
        ## sort order for topics, a little tricky
        x[order(toupper(x), x)]
    }

    ## given an installed package with a latex dir, make a single file
    ## for use in the refman.

    options(warn=1)
    if (missing(outfile))
        outfile <- paste(basename(pkgdir), "-pkg.tex", sep="")

    ## First check for a latex dir.
    ## If it does not exist, guess this is a source package.
    latexdir <- file.path(pkgdir, "latex")
    if (!file_test("-d", latexdir)) {
        files <- Sys.glob(file.path(pkgdir, "*.[Rr]d"))
        if (!length(files)) {
            ## is this a source package?  That has man/*.Rd files.
            files <- Sys.glob(file.path(pkgdir, "man", "*.[Rr]d"))
            if (!length(files))
                stop("this package does not have either a ", sQuote("latex"),
                 " or a ", sQuote("man"), " directory")
            if (is.null(extraDirs)) extraDirs <- .Platform$OS.type
            for(e in extraDirs)
                files <- c(files,
                           Sys.glob(file.path(pkgdir, "man", e, "*.[Rr]d")))
        }
        latexdir <- tempfile("ltx")
        dir.create(latexdir)
        message("Converting Rd files to LaTeX ...")
        cmd <- paste(R.home(), "/bin/R CMD Rdconv -t latex --encoding=",
                     encoding, sep="")
        for(f in files) {
            cat("  ", basename(f), "\n", sep="")
            out <-  sub("\\.[Rr]d",".tex", basename(f))
            system(paste(cmd,"-o", shQuote(file.path(latexdir, out)),
                         shQuote(f)))
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
        stop("no validly-named files in the ", sQuote("latex"), " directory")

    if(is.character(outfile)) {
        outcon <- file(outfile, if(append) "at" else "wt")
        on.exit(close(outcon))
    } else outcon <- outfile

    if(asChapter)
        cat("\n\\chapter{The \\texttt{", basename(pkgdir), "} package}\n",
            sep = "", file = outcon)
    topics <- rep.int("", length(files)); names(topics) <- files
    for (f in files) {
        lines <- readLines(f)
        hd <- grep("^\\\\HeaderA", lines, value = TRUE)
        if (!length(hd)) {
            warning("file ", sQuote(f), " lacks a header: skipping")
            next
        }
        this <- sub("\\\\HeaderA\\{\\s*([^}]*)\\}.*", "\\1", hd[1], perl = TRUE)
        if(!internals &&
           any(grepl("\\\\keyword\\{\\s*internal\\s*\\}", lines, perl = TRUE)))
            next
        topics[f] <- this
    }

    topics <- topics[nzchar(topics)]
    summ <- grep("-package$", topics)
    topics <- if(length(summ)) c(topics[summ], re(topics[-summ])) else re(topics)
    for (f in names(topics)) writeLines(readLines(f), outcon)

    if(asChapter)
        cat("\\clearpage\n", file = outcon)
}

## replacement for tools/Rdnewer.pl
.Rdnewer <- function(dir, file)
    q("no", status = ..Rdnewer(dir, file), runLast = FALSE)

..Rdnewer <- function(dir, file, OS = .Platform$OS.type)
{
    ## Test whether any Rd file in the 'man' and 'man/$OS' subdirectories of
    ## directory DIR is newer than a given FILE.  Return 0 if such a file is
    ## found (i.e., in the case of 'success'), and 1 otherwise, so that the
    ## return value can be used for shell 'if' tests.

    if (!file.exists(file)) return(0L)
    age <- file.info(file)$mtime

    if(any(file.info(Sys.glob(file.path(dir, "man", "*.Rd")))$mtime > age))
        return(0L)

    if(isTRUE(file.info(file.path(dir, OS))$isdir)) {
        if(any(file.info(Sys.glob(file.path(dir, "man", OS, "*.Rd")))$mtime > age))
            return(0L)
    }

    1L
}

## given a source package in 'dir', write outDirc/help/AnIndex
## This is a two-column tab-separated file of topic and file basename,
## conventionally sorted on topic (but with foo-package first)
## NB: ASCII sort, C locale
.writeAnIndex <- function(dir, outDir, OS = .Platform$OS.type)
{
    re <- function(x)
    {
        ## sort order for topics, a little tricky
        ## FALSE sorts before TRUE
        xx <- rep(TRUE, length(x))
        xx[grep("-package", x, fixed = TRUE)] <- FALSE
        order(xx, toupper(x), x)
    }

    mandir <- file.path(dir, "man")
    if(!file_test("-d", mandir))
        stop("there are no help pages in this package")
    files <- Sys.glob(file.path(mandir, "*.[Rr]d"))
    if(file_test("-d", f <- file.path(mandir, OS)))
        files <- c(files, Sys.glob(file.path(f, "*.[Rr]d")))
    ## Should only process files starting with [A-Za-z0-9] and with
    ## suffix .Rd or .rd, according to 'Writing R Extensions'.
    OK <- grep("^[A-Za-z0-9]", basename(files))
    files <- files[OK]
    topics <- ff <- character()
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        ## some \alias entries have trailing comments including a }
        aliases <- grep("^\\s*\\\\alias\\{\\s*([^}]+)\\}", lines,
                        perl = TRUE, value = TRUE)
        aliases <- sub("\\s*\\\\alias\\{\\s*([^}]+)\\}.*", "\\1",
                       aliases, perl = TRUE)
        ## unescape % and {
        aliases <- gsub("\\\\([%{])", "\\1", aliases)
        dups <- aliases %in% topics
        if (any(dups))
            warning("skipping repeated alias(es) ",
                    paste(sQuote(aliases[dups]), collapse = ", "),
                    " in file ", basename(f))
        aliases <- aliases[!dups]
        topics <- c(topics, aliases)
        fff <- sub("\\.[Rr]d",  "", basename(f))
        ff <- c(ff, rep(fff, length(aliases)))
    }
    outman <- file.path(outDir, "help")
    dir.create(outman, showWarnings = FALSE)
    write.table(cbind(topics, ff)[re(topics),], file.path(outman, "AnIndex"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
}

.writePkgIndices <-
    function(dir, outDir, OS = .Platform$OS.type, CHM = FALSE)
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

    chm_header <- function(pkg, title, version, conn)
    {
        cat("<html><head><title>", title, "</title>\n",
            "<link rel=\"stylesheet\" type=\"text/css\" href=\"Rchm.css\">\n" ,
            "</head><body>\n",
            "<h1>", title, "\n",
            "<img class=\"toplogo\" src=\"logo.jpg\" alt=\"[R logo]\"></h1>\n\n",
            "<hr>\n\n",
            "<object type=\"application/x-oleobject\" classid=\"clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e\">\n",
            "<param name=\"keyword\" value=\".. contents\">\n",
            "</object>\n\n",
            "<h2>Help pages for package &lsquo;", pkg, "&rsquo;",
            if(nzchar(version)) paste(" version", version),
            "</h2>\n\n",
            sep = "", file = conn)
    }

    chm_toc<- function(dir, pkg, M)
    {
        conn <- file(file.path(dir, "chm", paste(pkg, ".toc", sep = "")), "wt")
        on.exit(close(conn))
        if(length(M$File)) M$File <- paste(M$File, ".html", sep = "")
        cat("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n",
            "<HEAD></HEAD><HTML><BODY>\n<UL>\n",
            "<LI> <OBJECT type=\"text/sitemap\">\n",
            "<param name=\"Name\" value=\"Package ", pkg, ":  Contents\">\n",
            "<param name=\"Local\" value=\"00Index.html\">\n",
            "</OBJECT>\n",
            "<LI> <OBJECT type=\"text/sitemap\">\n",
            "<param name=\"Name\" value=\"Package ", pkg, ":  R objects\">\n",
            "</OBJECT>\n", "<UL>\n",
            sep = "", file = conn)
        writeLines(paste("<LI> <OBJECT type=\"text/sitemap\">\n",
                         "<param name=\"Name\" value=\"", M$Topic, "\">\n",
                         "<param name=\"Local\" value=\"", M$File, "\">\n",
                         "</OBJECT>", sep= ""), conn)
        cat("</UL>\n",
            "<LI> <OBJECT type=\"text/sitemap\">\n",
            "<param name=\"Name\" value=\"Package ", pkg, ":  Titles\">\n",
            "</OBJECT>\n",
            "<UL>\n",
            sep = "", file = conn)
        M <- M[!duplicated(M$Title), ]
        o <- order(tolower(M$Title), M$Title)
        writeLines(paste("<LI> <OBJECT type=\"text/sitemap\">\n",
                         "<param name=\"Name\" value=\"", M$Title[o], "\">\n",
                         "<param name=\"Local\" value=\"", M$File[o], "\">\n",
                         "</OBJECT>", sep = ""), conn)
        cat("</UL>\n",
            "</UL>\n</BODY></HTML>\n", sep="", file = conn)
    }

    firstLetterCategory <- function(x)
    {
        x[grep("-package$", x)] <- " "
        x <- toupper(substr(x, 1, 1))
        x[x > "Z"] <- "misc"
        x[x < "A" & x != " "] <- ""
        x
    }

    mandir <- file.path(dir, "man")
    if(!file_test("-d", mandir))
        stop("there are no help pages in this package")

    ## This may well already have been done:
    Rd <- if (file.exists(f <- file.path(outDir, "Meta", "Rd.rds"))) .readRDS(f)
    else {
        ## or use list_files_with_type
        files <- Sys.glob(file.path(mandir, "*.[Rr]d"))
        if(file_test("-d", f <- file.path(mandir, OS)))
            files <- c(files, Sys.glob(file.path(f, "*.[Rr]d")))
        ## Should only process files starting with [A-Za-z0-9] and with
        ## suffix .Rd or .rd, according to 'Writing R Extensions'.
        OK <- grep("^[A-Za-z0-9]", basename(files))
        files <- files[OK]
        Rdcontents(files)
    }

    topics <- Rd$Aliases
    lens <- sapply(topics, length)
    files <- sub("\\.[Rr]d$", "", Rd$File)
    internal <- sapply(Rd$Keywords, function(x) "internal" %in% x)
    M <- data.frame(Topic = unlist(topics),
                    File = rep.int(files, lens),
                    Title = rep.int(Rd$Title, lens),
                    Internal = rep.int(internal, lens),
                    stringsAsFactors = FALSE)
    ## FIXME duplicated aliases warning
    outman <- file.path(outDir, "help")
    dir.create(outman, showWarnings = FALSE)
    MM <- M[re(M[, 1]), 1:2]
    write.table(MM, file.path(outman, "AnIndex"),
                quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

    outman <- file.path(outDir, "html")
    dir.create(outman, showWarnings = FALSE)
    outcon <- file(file.path(outman, "00Index.html"), "wt")
    on.exit(close(outcon))
    if(CHM) {
        chmdir <- file.path(dir, "chm")
        dir.create(chmdir, showWarnings = FALSE)
        chmcon <- file(file.path(chmdir, "00Index.html"), "wt")
        on.exit(close(chmcon), add = TRUE)
    }
    desc <- read.dcf(file.path(outDir, "DESCRIPTION"))[1,]
    ## drop internal entries
    M <- M[!M[, 4], ]
    if(desc["Package"] %in% c("base", "graphics", "stats", "utils")) {
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
        if(backtick) {
            x <- gsub("---", "-", x, fixed = TRUE)
            x <- gsub("--", "-", x, fixed = TRUE)
            ## these hve been changed in the Rd parser
            #x <- gsub("``", "&ldquo;", x, fixed = TRUE)
            #x <- gsub("''", "&rdquo;", x, fixed = TRUE)
            #x <- gsub("\\`([^']+)'", "&lsquo;\\1&rsquo;", x)
            #x <- gsub("`", "'", x, fixed = TRUE)
        }
        x
    }
    M$HTopic <- htmlize(M$Topic, FALSE)
    M$Title <- htmlize(M$Title, TRUE)

    ## handle encodings
    def <- desc["Encoding"]
    def <- if(is.na(def)) "" else mime_canonical_encoding(def)
    encodings <- mime_canonical_encoding(Rd$Encoding)
    enc <- if(any(nzchar(encodings))) {
        encs <- unique(c(def, encodings))
        ## FIXME: we could reencode individual files
        encs[nzchar(encs)][1]
    } else def
    html_header(desc["Package"], desc["Title"], desc["Version"],
                if(nzchar(enc)) enc else "iso-8859-1", outcon)
    if(CHM)
        chm_header(desc["Package"], desc["Title"], desc["Version"], chmcon)

    use_alpha <- (nrow(M) > 100)
    if (use_alpha) {
        first <- firstLetterCategory(M$Topic)
        nm <- sort(names(table(first)))
        m <- match(" ", nm, 0L)
        if(m) nm <- c(" ", nm[-m])
        writeLines("<p align=\"center\">", outcon)
        writeLines(paste("<a href=\"#", nm, "\">", nm, "</a>", sep = ""),
                   outcon)
        writeLines("</p>\n", outcon)
        if (CHM) {
            writeLines("<p align=\"center\">", chmcon)
            writeLines(paste("<a href=\"#", nm, "\">", nm, "</a>", sep = ""),
                       chmcon)
            writeLines("</p>\n", chmcon)
       }
        for (f in nm) {
            cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
                sep = "", file = outcon)
            MM <- M[first == f, ]
            ## cat("writing", nrow(MM), "lines for", sQuote(f), "\n")
            writeLines('<table width="100%">', outcon)
            writeLines(paste('<tr><td width="25%"><a href="', MM[, 2], '.html">',
                             MM$HTopic, '</a></td>\n<td>', MM[, 3],'</td></tr>',
                             sep = ''), outcon)
            writeLines("</table>", outcon)
            if(CHM) {
                cat("\n<h2><a name=\"", f, "\">-- ", f, " --</a></h2>\n\n",
                    sep = "", file = chmcon)
                writeLines('<table width="100%">', chmcon)
                writeLines(paste('<tr><td width="25%"><a href="', MM[, 2], '.html">',
                                 MM$HTopic, '</a></td>\n<td>', MM[, 3],'</td></tr>',
                                 sep = ''), chmcon)
                writeLines("</table>", chmcon)
            }
       }
    } else {
        writeLines('<table width="100%">', outcon)
        writeLines(paste('<tr><td width="25%"><a href="', M[, 2], '.html">',
                         M$HTopic, '</a></td>\n<td>', M[, 3],'</td></tr>',
                         sep = ''), outcon)
        writeLines("</table>", outcon)
        if (CHM) {
            writeLines('<table width="100%">', chmcon)
            writeLines(paste('<tr><td width="25%"><a href="', M[, 2], '.html">',
                             M$HTopic, '</a></td>\n<td>', M[, 3],'</td></tr>',
                             sep = ''), chmcon)
            writeLines("</table>", chmcon)
        }
    }
    writeLines('</body></html>', outcon)
    if(CHM) writeLines('</body></html>', chmcon)
    if(CHM) {
        chm_toc(dir, desc["Package"], M)
        .write_CHM_hhp(dir, desc["Package"])
    }
}

## dir is the package top-level directory
.write_CHM_hhp <- function(dir, pkg)
{
    if(missing(pkg)) pkg <- basename(dir)
    d <- file.path(dir, "chm")
    dir.create(d, showWarnings = FALSE)
    con <- file(file.path(d, paste(pkg, ".hhp", sep = "")), "wt")
    on.exit(close(con))
    writeLines(paste("[OPTIONS]\nAuto Index=Yes\n",
                     "Contents file=", pkg, ".toc\n",
                     "Compatibility=1.1 or later\n",
                     "Compiled file=", pkg, ".chm\n",
                     "Default topic=00Index.html\n",
                     "Display compile progress=No\n",
                     "Full-text search=Yes\n",
                     "Full text search stop list file=..\\..\\..\\gnuwin32\\help\\R.stp\n",
                     "Title=R Help for package ", pkg, "\n",
                     "\n\n[FILES]", sep = ""), con)
    writeLines(dir(d, pattern = "\\.html$"), con)
}

.convertRdfiles <-
    function(dir, outDir, OS = .Platform$OS.type,
             types = c("txt", "html", "latex", "example"))
{
    dirname <- c("help", "html", "latex", "R-ex")
    ext <- c("", ".html", ".tex", ".R")
    names(dirname) <- names(ext) <- c("txt", "html", "latex", "example")
    mandir <- file.path(dir, "man")
    if(!file_test("-d", mandir))
        stop("there are no help pages in this package")
    desc <- .readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION
    pkg <- desc["Package"]
    ver <- desc["Version"]
    enc <- desc["encoding"]
    if(is.na(enc)) enc <- "unknown"

    ## or use list_files_with_type
    files <- Sys.glob(file.path(mandir, "*.[Rr]d"))
    if(file_test("-d", f <- file.path(mandir, OS)))
        files <- c(files, Sys.glob(file.path(f, "*.[Rr]d")))
    ## Should only process files starting with [A-Za-z0-9] and with
    ## suffix .Rd or .rd, according to 'Writing R Extensions'.
    OK <- grep("^[A-Za-z0-9]", basename(files))
    files <- files[OK]

    for(type in types)
        dir.create(file.path(outDir, dirname[type]), showWarnings = FALSE)
    cmd <- paste(R.home(), "/bin/Rdconv -t ", sep = "")

    ## FIXME: perl version cleans up non-matching converted files
    types <- "txt"
    cat("\n   converting help for package ", sQuote(pkg), "\n", sep="")
    if(TRUE) {
        ## FIXME: add this lib to lib.loc?
        Links <- if ("html" %in% types) findHTMLlinks(outDir) else ""
        for (f in files) {
            bf <-  sub("\\.[Rr]d","", basename(f))
            Rd <- tools::parse_Rd(f)
            if ("txt" %in% types) {
                type <- "txt"
                ff <- file.path(outDir, dirname[type],
                                paste(bf, ext[type], sep = ""))
                if(!file.exists(ff) || file_test("-nt", f, ff)) {
                    cat("    ", bf, rep(" ", max(0, 30-nchar(bf))), "txt\n",
                        sep = "")
                    res <- try(Rd2txt(Rd, ff, package = pkg))
                    if(inherits(res, "try-error")) unlink(ff)
                }
            }
            if("html" %in% types) {
                type <- "html"
                ff <- file.path(outDir, dirname[type],
                                paste(bf, ext[type], sep = ""))
                if(!file.exists(ff) || file_test("-nt", f, ff)) {
                    cat("    ", bf, rep(" ", max(0, 30-nchar(bf))), "html\n",
                        sep = "")
                    res <- try(Rd2HTML(Rd, ff, package = pkg, Links = Links))
                    if(inherits(res, "try-error")) unlink(ff)
                }
            }
            if ("latex" %in% types) {
                type <- "latex"
                ff <- file.path(outDir, dirname[type],
                                paste(bf, ext[type], sep = ""))
                if(!file.exists(ff) || file_test("-nt", f, ff)) {
                    cat("    ", bf, rep(" ", max(0, 30-nchar(bf))), "latex\n",
                        sep = "")
                    res <- try(Rd2latex(Rd, ff))
                    if(inherits(res, "try-error")) unlink(ff)
                }
            }
            if ("example" %in% types) {
                type <- "example"
                ff <- file.path(outDir, dirname[type],
                                paste(bf, ext[type], sep = ""))
                if(!file.exists(ff) || file_test("-nt", f, ff)) {
                    Rd2ex(Rd, ff)
                    if (file.exists(ff))
                        cat("    ", bf, rep(" ", max(0, 30-nchar(bf))), "example\n",
                            sep = "")
                }
            }
        }
    }
    else {
        perllib <- Sys.getenv("PERL5LIB")
        if (nzchar(perllib)) {
            Sys.setenv(PERL5LIB = paste(file.path(R.home("share"), "perl"),
                       perllib, sep = .Platform$path.sep))
        } else {
            perllib <- Sys.getenv("PERLLIB")
            Sys.setenv(PERLLIB = paste(file.path(R.home("share"), "perl"),
                       perllib, sep = .Platform$path.sep))
        }

        for (f in files) {
            Links <- findHTMLlinks(outDir)
            bf <-  sub("\\.[Rr]d","", basename(f))
            need <- character()
            for(type in types) {
                ff <- file.path(outDir, dirname[type],
                                paste(bf, ext[type], sep = ""))
                if(!file.exists(ff) || file_test("-nt", f, ff)) {
                    this <- paste(cmd, type, " -o ", ff, " --package=", pkg,
                                  " --version=", ver,
                                  " --encoding=", enc,
                                  " ", f, sep="")
                    ##print(this)
                    res <- system(this)
                    if(res) {
                        stop("problem in converting ", bf)
                    }
                    if(file.exists(ff)) need <- c(need, type)
                }
            }
            if(length(need)) {
                cat("    ", bf, rep(" ", max(0, 30-nchar(bf))),
                    paste(need, collapse=" \t"),
                    "\n",sep="")
            }
        }
    }
}

.makeDllRes <- function(name="", version = "0.0")
{
    if (file.exists(f <- "../DESCRIPTION") ||
        file.exists(f <- "../../DESCRIPTION")) {
        desc <- read.dcf(f)[[1]]
        if(!is.na(f <- desc["Package"])) name <- f
        if(!is.na(f <- desc["Version"])) version <- f
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

.Rd2dvi <- function(pkgdir, outfile, is_bundle, title, batch = FALSE,
                    description = TRUE, only_meta = FALSE,
                    enc = "unknown", files_or_dir, OSdir,
                    internals = "no", index = "true")
{
    # print(match.call())

    ## %in% and others cause problems for some page layouts.
    if (basename(pkgdir) == "base") index <- "false"
    out <- file(of <- tempfile(), "wt")
    if (!nzchar(enc)) enc <- "unknown"
    description <- description == "true"
    only_meta <- only_meta == "true"
    internals <- internals != "no"
    index <- index != "false"

    desc <- NULL
    if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
        desc <- read.dcf(f)[1,]
        if(enc == "unknown") {
            pkg_enc <- desc["Encoding"]
            if (!is.na(pkg_enc)) enc <- pkg_enc
        }
    }

    ## Rd2.tex part 1: header
    if(batch == "true") writeLines("\\nonstopmode{}", out)
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4DVI", "ae"), "]{Rd}\n",
        sep = "", file = out)
    if(index) writeLines("\\usepackage{makeidx}", out)
    writeLines(c("\\usepackage[@ENC@]{inputenc}",
                 "@CYRILLIC_SUPPORT@",
                 "\\makeindex{}",
                 "\\begin{document}"), out)
    if(is_bundle == "no") {
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
                subJ <- gsub("[_$]", "\\\\1", subj)
                title <- paste("\\R{} documentation}} \\par\\bigskip{{\\Large of", subj)
            }
        }
        cat("\\chapter*{}\n",
            "\\begin{center}\n",
            "{\\textbf{\\huge ", title, "}}\n",
            "\\par\\bigskip{\\large \\today}\n",
            "\\end{center}\n", sep = "", file = out)
        if(description && file.exists(f <- file.path(pkgdir, "DESCRIPTION")))
            .DESCRIPTION_to_latex(f, out)
        ## running on the sources of a base package will have DESCRIPTION.in,
        ## only.
        if(description &&
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
    toc <- if(file_test("-d", files_or_dir)) {
        "\\Rdcontents{\\R{} topics documented:}"
    } else ""

    if (is_bundle == "no") {
        ## if this looks like a package with no man pages, skip body
        if(file.exists(file.path(pkgdir, "DESCRIPTION")) &&
           !(file_test("-d", file.path(pkgdir, "man")) ||
             file_test("-d", file.path(pkgdir, "latex")))) only_meta <- TRUE
        if(!only_meta) {
            if(nzchar(toc)) writeLines(toc, out)
            .Rdfiles2tex(files_or_dir, out, encoding = enc, append = TRUE,
                         extraDirs = OSdir, internals = internals)
        }
    } else {
        writeLines(c("\\setcounter{secnumdepth}{-1}",
                     "\\pagenumbering{roman}",
                     "\\tableofcontents{}",
                     "\\cleardoublepage{}",
                     "\\pagenumbering{arabic}"), out)
        desc <- read.dcf(file.path(pkgdir, "DESCRIPTION"))[1,]
        bundle_pkgs <- strsplit(desc["Contains"], "[[:blank:]]+")[[1]]
        for (p in bundle_pkgs) {
            message("Bundle package: ", p)
            cat("\\chapter{Package `", p, "'}\n", sep = "", file = out)
            if (description &&
                file.exists(f <- file.path(pkgdir, p, "DESCRIPTION.in")))
                .DESCRIPTION_to_latex(f, out)
            if(!only_meta)
                .pkg2tex(file.path(pkgdir, p), out, encoding = enc,
                         append = TRUE, asChapter = FALSE,
                         internals = internals)
            writeLines("\\clearpage{}", out)
        }
        writeLines("\\cleardoublepage{}", out)
    }

    ## Rd2.tex part 3: footer
    if(index) writeLines("\\printindex{}", out)
    writeLines("\\end{document}", out)
    close(out)

    ## Look for encodings
    ## FIXME cyrillic probably only works with times, not ae.
    lines <- readLines(of)
    encs <- lines[grepl('^\\\\inputencoding', lines)]
    encs <- unique(sub("^\\\\inputencoding\\{(.*)\\}", "\\1", encs))
    encs <- paste(encs, collapse=",")
    utf8 <- if(nzchar(Sys.getenv("_R_CYRILLIC_TEX_"))) "utf8" %in% encs else FALSE
    if (!nzchar(encs)) {
        lines <- lines[! lines %in%
                       c("\\usepackage[@ENC@]{inputenc}",
                         "@CYRILLIC_SUPPORT@")]
    } else if (!utf8) {
        lines[lines == "\\usepackage[@ENC@]{inputenc}"] <-
            paste("\\usepackage[", encs, "]{inputenc}", sep = "")
        lines <- lines[lines != "@CYRILLIC_SUPPORT@"]
    } else {
        lines[lines == "\\usepackage[@ENC@]{inputenc}"] <-
            paste("\\usepackage[", encs, "]{inputenc}", sep = "")
        lines[lines == "@CYRILLIC_SUPPORT@"] <-
            "\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}"
    }

    if(is.character(outfile)) {
        out <- file(outfile, "at")
        on.exit(close(out))
    } else out <- outfile
    writeLines(lines, out)

    invisible(NULL)
}
