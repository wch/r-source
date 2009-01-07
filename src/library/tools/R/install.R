#  File share/R/INSTALL.R
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

## issues:
## --fake is little tested

.install_packages <- function()
{
    ## global variables
    bundle_pkgs <- character() # list of packages in current pkg/bundle
    lockdir <- ""
    is_first_package <- TRUE
    stars <- "*"

    on.exit(do_exit_on_error())
    WINDOWS <- .Platform$OS.type == "windows"
    paste0 <- function(...) paste(..., sep="")

    TAR <- shQuote(Sys.getenv("TAR"))
    GZIP <- Sys.getenv("R_GZIPCMD")
    if (!nzchar(GZIP)) GZIP <- "gzip"
    if (WINDOWS) zip <- "zip"
    rarch <- Sys.getenv("R_ARCH")

    SHLIB_EXT <- if (WINDOWS) ".dll" else {
        ## can we do better?
        mconf <- file.path(R.home(), paste0("etc", rarch), "Makeconf")
        sub(".*= ", "", grep("SHLIB_EXT", readLines(mconf), value = TRUE))
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
            "      --no-chm		do not build CHM help",
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
                tools:::.file_append_ensuring_LFs(file.path(R.home("doc"), "html", "search", "index.txt"),
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
        if (utils::file_test("-d", tmpdir)) unlink(tmpdir, recursive=TRUE)
    }

    do_exit_on_error <- function()
    {
        # message("*** do_exit_on_error ***")
        ## If we are not yet processing a package, we will not have
        ## set bundle_pkgs
        for(p in bundle_pkgs) {
            if (is.na(p) || !nzchar(p)) next
            pkgdir <- file.path(lib, p)
            if (nzchar(pkgdir) && utils::file_test("-d", pkgdir)) {
                starsmsg(stars, "Removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }
            if (lock && nzchar(lockdir) &&
                utils::file_test("-d", lp <- file.path(lockdir, p))) {
                starsmsg(stars, "Restoring previous ", sQuote(pkgdir))
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
                if (utils::file_test("-d", file.path(pkg, p))) {
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
                res <- try(tools:::.vcreate_bundle_package_descriptions(pkg, paste(contains, collapse=" ")))
                if (inherits(res, "try-error"))
                    warning("problem installing per-package DESCRIPTION files",
                            call. = FALSE)
            }
            ## This cannot create a binary bundle
            if (tar_up)
                errmsg("cannot create a binary bundle: use 'R CMD build --binary' to do so")
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

            ## FIXME: do this at bundle level?
            ## Could different packages have different version requirements?
            status <- tools:::.Rtest_package_depends_R_version()
            if (status) do_exit_on_error()

            dir.create(instdir, recursive = TRUE, showWarnings = FALSE)
            if (!utils::file_test("-d", instdir)) {
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

            ## FIXME do this in R?
            if (!WINDOWS)
                system(paste("find", shQuote(instdir),  "-exec chmod a+r \\{\\} \\;"))
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
            ## zip appends to existing archives
            system(paste("rm -f", filepath))
            owd <- setwd(lib)
            system(paste(ZIP, "-r9Xq", filepath, bundle_pkgs))
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
        if (utils::file_test("-d", "src")) {
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
        } else if (utils::file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Adminstration Manual'", call. = FALSE)

    }

    do_install_source <- function(pkg_name, instdir, pkg_dir, desc)
    {
        MAKE <- Sys.getenv("MAKE")
        paste0 <- function(...) paste(..., sep="")

        cp_r <- function(from, to)
        {
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
                file.copy(files, dest)
                Sys.chmod(Sys.glob(file.path(dest, "*")), "755")
            }
        }

        run_shlib <- function(pkg_name, srcs, instdir, arch)
        {
            cmd <- paste("sh",
                         shQuote(file.path(R.home(), "bin", "SHLIB")),
                         shargs,
                         "-o",
                         paste0(pkg_name, SHLIB_EXT),
                         paste(srcs, collapse=" "))
            if (debug) message("about to run ", sQuote(cmd))
            if (system(cmd) == 0L) {
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
                if (utils::file_test("-x", "configure")) {
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
            if (utils::file_test("-d", "share")) {
                files <- Sys.glob("share/*")
                if (length(files)) file.copy(files, R.home("share"))
            }
            if (utils::file_test("-d", "library")) {
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
                system(paste("mv", instdir, file.path(lockdir, pkg_name)))
            } else if (more_than_libs)
                ## this is only used for recommended packages installed from .tgz
                unlink(instdir, recursive = TRUE)
            dir.create(instdir, recursive=TRUE)

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
            if (!thislazy && utils::file_test("-d", "data")) {
                sizes <- system("ls.exe -s1 data", intern = TRUE)
                out <- 0; nodups <- TRUE; prev <- ""
                for(line in sizes) {
                    if (length(grep("total", line))) next
                    this <- sub("([ 0-9]*)(.*)", "\\1", line)
                    out <- out + as.numeric(this)
                    this <- sub("[[:space:]]*[0-9]+\\ ", "", line)
                    this <- sub("\\.[a-zA-Z]+$", "", this)
                    if (this == prev) nodups <- FALSE
                    prev <- this
                }
                if(nodups && out > 100) use_zip_data <- TRUE
            }
            if (utils::file_test("-d", "man") &&
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
                if (utils::file_test("-x", "configure")) {
                    res <- system(paste(paste(configure_vars, collapse = " "),
                                        "./configure",
                                        paste(configure_args, collapse = " ")))
                    if (res) pkgerrmsg("configuration failed", pkg_name)
                }  else if (file.exists("configure"))
                    errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
            }
        }


        if (more_than_libs) {
            for (f in c("NAMESPACE", "LICENSE", "LICENCE", "COPYING", "NEWS"))
                if (file.exists(f)) {
                    file.copy(f, instdir)
                    Sys.chmod(file.path(instdir, f), "644")
                }

            ## This cannot be done in a MBCS: write.dcf fails
            ctype <- Sys.getlocale("LC_CTYPE")
            Sys.setlocale("LC_CTYPE", "C")
            res <- try(tools:::.install_package_description(".", instdir))
            Sys.setlocale("LC_CTYPE", ctype)
            if (inherits(res, "try-error"))
                pkgerrmsg("installing package DESCRIPTION failed", pkg_name)
        }

        if (utils::file_test("-d", "src") && !fake) {
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
                makefiles <- character()
                if (file.exists(f <- path.expand("~/.R/Makevars.win")))
                    makefiles <- f
                else if (file.exists(f <- path.expand("~/.R/Makevars")))
                    makefiles <- f
                if (file.exists("src/Makefile.win")) {
                    makefiles <- c("Makefile.wIn", makefiles)
                    message("  running src/Makefile.win ...")
                } else {
                    makefiles <- c(file.path(rhome, "src/gnuwin32/MakeDll"),
                                   makefiles)
                    message("  making DLL ...")
                }
                cmd <- paste0("make --no-print-directory -C src RHOME=", shQuote(rhome),
                              " DLLNAME=", pkg_name,
                              " CLINK_CPPFLAGS=", shQuote(clink_cppflags),
                              " ",
                              paste("-f", shQuote(makefiles), collapse = " "))
                if(debug) cat("  running make cmd\n\t", cmd, "\n", sep="")
                res <- system(cmd)
                message("  ... done")
                if (res) has_error <- TRUE

                dllfile <- file.path("src", paste0(pkg_name, ".dll"))
                if (!has_error &&file.exists(dllfile)) {
                    message("  installing DLL ...")
                    file.copy(dllfile, libdir)
                }
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
                            utils::file_test("-x", "../configure")) {
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
            if (utils::file_test("-d", "R")) {
                starsmsg(stars, "R")
                dir.create(file.path(instdir, "R"), recursive = TRUE)
                ## This cannot be done in a C locale
                res <- try(tools:::.install_package_code_files(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("unable to collate files", pkg_name)

                if (file.exists(file.path("R", "sysdata.rda"))) {
                    res <- try(tools:::sysdata2LazyLoadDB("R/sysdata.rda",
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
                                       readLines("NAMESPACE"),
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

            if (utils::file_test("-d", "data")) {
                starsmsg(stars, "data")
                files <- Sys.glob(file.path("data", "*"))
                if (length(files)) {
                    dir.create(file.path(instdir, "data"), recursive = TRUE)
                    file.remove(Sys.glob(file.path(instdir, "data", "*")))
                    file.copy(files, file.path(instdir, "data"))
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
                        res <- try(tools:::data2LazyLoadDB(pkg_name, lib))
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

            if (utils::file_test("-d", "demo") && !fake) {
                starsmsg(stars, "demo")
                dir.create(file.path(instdir, "demo"), recursive = TRUE)
                file.remove(Sys.glob(file.path(instdir, "demo", "*")))
                res <- try(tools:::.install_package_demos(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("ERROR: installing demos failed")
                Sys.chmod(Sys.glob(file.path(instdir, "demo", "*")), "644")
            }

            if (utils::file_test("-d", "exec") && !fake) {
                starsmsg(stars, "exec")
                dir.create(file.path(instdir, "exec"), recursive = TRUE)
                file.remove(Sys.glob(file.path(instdir, "exec", "*")))
                files <- Sys.glob(file.path("exec", "*"))
                if (length(files)) {
                    file.copy(files, file.path(instdir, "exec"))
                    Sys.chmod(Sys.glob(file.path(instdir, "exec", "*")), "755")
                }
            }

            if (utils::file_test("-d", "inst") && !fake) {
                starsmsg(stars, "inst")
                ## FIXME avoid installing .svn etc?
                cp_r("inst", instdir)
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
            if (utils::file_test("-d", "R") && value) {
                starsmsg(stars, "preparing package for lazy loading")
                ## Something above, e.g. lazydata,  might have loaded the namespace
                if (pkg_name %in% loadedNamespaces())
                    unloadNamespace(pkg_name)
                ## suppress second round of parse warnings
                options(warnEscapes = FALSE)
                res <- try({.getRequiredPackages(quietly = TRUE)
                            tools:::makeLazyLoading(pkg_name, lib)})
                options(warnEscapes = TRUE)
                if (inherits(res, "try-error"))
                    pkgerrmsg("lazy loading failed", pkg_name)
                ## FIXME: still needed?  If so needs a pretest
                ## file.remove(file.path(instdir, "R", "all.rda"))
            }

            if (utils::file_test("-d", "man")) {
                starsmsg(stars, "help")
                res <- try(tools:::.install_package_man_sources(".", instdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("installing man sources failed", pkg_name)
                Sys.chmod(file.path(instdir, "man",
                                    paste0(pkg_name, ".Rd.gz")), "644")
                ## 'Maybe build preformatted help pages ...'
                if (build_help) {
                    cmd <- paste("perl",
                                 shQuote(file.path(R.home("share"), "perl",
                                                   if (WINDOWS) "build-help-windows.pl" else  "build-help.pl")),
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
                        if (utils::file_test("-d", "R-ex")) {
                            wd2 <- setwd("R-ex")
                            system(paste(zip, " -q -m Rex *.R"))
                            setwd(wd2)
                        }
                        if (utils::file_test("-d", "help")) {
                            wd2 <- setwd("help")
                            system(paste(zip, " -q -m Rhelp * -x AnIndex"))
                            setwd(wd2)
                        }
                        if (utils::file_test("-d", "latex")) {
                            wd2 <- setwd("latex")
                            system(paste(zip, " -q -m Rhelp *.tex"))
                            setwd(wd2)
                        }
                        setwd(owd)
                    }
                    if (build_chm) {
                        if (utils::file_test("-d", "chm")) {
                            owd <- setwd("chm")
                            file.copy(file.path(R.home(), "src/gnuwin32/help/Rchm.css"), ".")
                            file.copy(file.path(R.home(), "doc/html/logo.jpg"), ".")
                            system(paste0("hhc ", pkg_name, ".hhp"))
                            ## always gives an error code
                            chm_file <- paste0(pkg_name, ".chm")
                            if (file.exists(chm_file)) {
                                dest <- file.path(instdir, "chtml")
                                ## parent must exist by now
                                dir.create(dest, showWarnings = FALSE)
                                file.copy(chm_file, dest)
                            }
                            setwd(owd)
                        }
                    }
                }
            } else                      # yes, to stdout
            cat("No man pages found in package ", sQuote(pkg_name), "\n")

            ## pkg indices
            starsmsg(stars, "building package indices ...")
            res <- try(tools:::.install_package_indices(".", instdir))
            if (inherits(res, "try-error"))
                errmsg("installing package indices failed")

            ## Install a dump of the parsed NAMESPACE file
            if (file.exists("NAMESPACE") && !fake) {
                res <- try(tools:::.install_package_namespace_info(".", instdir))
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
            tools:::.installMD5sums(instdir)
        }

    }

    options(showErrorCalls=FALSE)
    pkgs <- character(0)
    lib <- ""
    args <- commandArgs(TRUE)

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

    while(length(args)) {
        a <- args[1]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package installer r",
                R.version[["svn rev"]], "\n", sep = "")
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
        } else if (a == "--multiarch") {
            multiarch <- FALSE
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
        if (utils::file_test("-f", pkg)) {
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
    if (!utils::file_test("-d", lib) || file.access(lib, 2L))
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
        if (!utils::file_test("-d", lockdir)) {
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

    build_help_opts <- character(0)
    if (build_text) build_help_opts <- c(build_help_opts, "--txt")
    if (build_html) build_help_opts <- c(build_help_opts, "--html")
    if (build_latex) build_help_opts <- c(build_help_opts, "--latex")
    if (build_example) build_help_opts <- c(build_help_opts, "--example")
    if (build_chm) build_help_opts <- c(build_help_opts, "--chm")
    build_help <- length(build_help_opts) > 0L
    if (build_help && debug) build_help_opts <- c("--debug", build_help_opts)
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
