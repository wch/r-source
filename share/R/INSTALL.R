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

## FIXME: get this from etc/Makeconf
SHLIB_EXT <- ".so"

MAKE <- Sys.getenv("MAKE")
TAR <- shQuote(Sys.getenv("TAR"))
## FIXME
GZIP <- "gzip"
arch <- Sys.getenv("R_ARCH")
etcpath <- paste("etc", arch, sep="")
libpath <- paste("libs", arch, sep="")
options(warn = 1)
foo <- Sys.setlocale("LC_COLLATE", "C")
ctype <- Sys.getlocale("LC_CTYPE")


Usage <- function() {
    cat("Usage: R CMD INSTALL [options] pkgs",
        "",
        "Install the add-on packages specified by pkgs.  The elements of pkgs can",
        "be relative or absolute paths to directories with the package (bundle)",
        "sources, or to gzipped package 'tar' archives.  The library tree to",
        "install to can be specified via '--library'.  By default, packages are",
        "installed in the library tree rooted at the first directory in",
        ".libPaths() for an R session run in the current environment",
        "",
        "Options:",
        "  -h, --help		print short help message and exit",
        "  -v, --version		print INSTALL version info and exit",
        "      --configure-args=ARGS",
        "			set arguments for the package's configure script",
        "			(if any)",
        "      --configure-vars=VARS",
        "			set variables for the configure script (if any)",
        "  -c, --clean		remove files created during installation",
        "      --preclean	remove files created during a previous run",
        "  -d, --debug		turn on shell and build-help debugging",
        "  -l, --library=LIB	install packages to library tree LIB",
        "      --no-configure    do not use the package's configure script",
        "      --no-docs		do not build and install documentation",
        "      --use-zip-data	collect data files in zip archive",
        "      --use-zip-help	collect help and examples into zip archives",
        "      --use-zip		combine '--use-zip-data' and '--use-zip-help'",
        "      --auto-zip       select whether to zip automatically",
        "      --fake		do minimal install for testing purposes",
        "      --no-lock		install on top of any existing installation",
        "			without using a lock directory",
        "      --unsafe		ditto",
        "      --pkglock		use a per-package lock directory",
        "      --libs-only	only install the libs directory",
        "      --build    	build binary tarball(s) of the installed package(s)",
        "",
        "Report bugs to <r-bugs@r-project.org>.", sep="\n")
}

paste0 <- function(...) paste(..., sep="")

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
    if (lock) unlink(lockdir, recursive = TRUE)
}

do_cleanup_tmpdir <- function()
{
    ## Solaris will not remove any directory in the current path
    setwd(startdir)
    if (utils::file_test("-d", tmpdir)) unlink(tmpdir, recursive=TRUE)
}

do_exit_on_error <- function(rm = TRUE)
{
    ## If we are not yet processing a package, we will not have
    ## set pkg_dir.  It's the first thing in do_install.
    if (nzchar(pkg_dir)) {
        ## Find out if this is a bundle.
        bundlepkg <- read.dcf(file.path(pkg_dir, "DESCRIPTION"), "Contains")
        bundlepkg <- if (is.na(bundlepkg)) pkg_name else strsplit(bundlepkg, " ")[[1]]
        for(p in bundlepkg) {
            if (is.na(p) || !nzchar(p)) next
            pkgdir <- file.path(lib, p)
            if (rm) {
                starsmsg(stars, "Removing ", sQuote(pkgdir))
                unlink(pkgdir, recursive = TRUE)
            }
            if (lock && utils::file_test("-d", lp <- file.path(lockdir, p))) {
                starsmsg(stars, "Restoring previous ", sQuote(pkgdir))
                system(paste("mv", lp, pkgdir))
            }
        }
    }

    do_cleanup()
    q("no", status = 1)
}

get_packages <- function(dir)
{
    ## get the full path names to all packages contained in $1.
    desc <- read.dcf(file.path(dir, "DESCRIPTION"))[1, ]
    contains <- desc["Contains"]
    if (!is.na(contains)) {
        ## Maybe a bundle

        ## Be careful whether package listed in the Contains field really
        ## "exist".  Refrain from installing the bundle in case of an entry
        ## without corresponding package subdirectory, but be nice in case
        ## of a subdirectorues without DESCRIPTION.in files (handled by the
        ## R code).
        bundle_name <- desc["Bundle"]
        contains <- strsplit(contains, " ")[[1]]
        pkgs <- character(0)
        for(p in contains) {
            if (utils::file_test("-d", file.path(dir, p))) {
                owd <- setwd(file.path(dir, p))
                pkgs <- c(pkgs, getwd())
                setwd(owd)
            } else {
                warning("incorrect Contains metadata for bundle ",
                        sQuote(bundle_name),
                        ": there is no package '", sQuote(p))
                warning("skipping installation of bundle ",
                        sQuote(bundle_name))
                pkgs <- character(0)
                break
            }
        }
        if (length(pkgs)) {
            ## Create the package level DESCRIPTION files from the bundle
            ## level DESCRIPTION and the package level DESCRIPTION.in ones.
            res <- try(tools:::.vcreate_bundle_package_descriptions(dir, paste(contains, collapse=" ")))
            if (inherits(res, "try-error"))
                warning("problem installing per-package DESCRIPTION files")
         }
    } else {
        ## Not a bundle
        owd <- setwd(dir)
        pkgs <- getwd()
        setwd(owd)
    }
    pkgs
}

cp_r <- function(from, to)
{
    from <- shQuote(from)
    to <- shQuote(to)
    system(paste0("cp -r ", from, "/* ", to,
                  " || (cd ", from, " && ", TAR, " cf - . | (cd '", to, "' && ", TAR, "xf - ))"))
}

shlib_install <- function(rpkgdir, arch)
{
    files <- Sys.glob(paste0("*", SHLIB_EXT))
    if (length(files)) {
        libarch <- if (nzchar(arch)) paste0("libs", arch) else "libs"
        dest <- file.path(rpkgdir, libarch)
        file.copy(files, dest)
        Sys.chmod(Sys.glob(file.path(dest, "*")), "755")
    }
}

run_shlib <- function(pkg_name, srcs, rpkgdir, arch)
{
    cmd <- paste("sh",
                 shQuote(file.path(R.home(), "bin", "SHLIB")),
                 shargs,
                 "-o",
                 paste0(pkg_name, SHLIB_EXT),
                 paste(srcs, collapse=" "))
    if (debug) message("about to run ", sQuote(cmd))
    if (system(cmd) == 0L) {
        shlib_install(rpkgdir, arch)
        return(FALSE)
    } else return(TRUE)
}

parse_description_field <- function(field, default=TRUE)
{
    tmp <- desc[field]
    if (is.na(tmp))
        default
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

do_install <- function(pkg)
{
    setwd(pkg)
    pkg_dir <<- pkg
    desc <<- read.dcf(file.path(pkg, "DESCRIPTION"))[1, ]
    pkg_name <<- desc["Package"]
    if (is.nan(pkg_name)) errmsg("no 'Package' field in 'DESCRIPTION'")

    Sys.setenv(R_PACKAGE_NAME = pkg_name)
    rpkgdir <- file.path(lib, pkg_name)
    Sys.setenv(R_PACKAGE_DIR = rpkgdir)
    status <- tools:::.Rtest_package_depends_R_version()
    if (status) do_exit_on_error()

    dir.create(rpkgdir, recursive = TRUE, showWarnings = FALSE)
    if (!utils::file_test("-d", rpkgdir)) {
        message("ERROR unable to create ", sQuote(rpkgdir))
        do_exit_on_error(FALSE)
    }

    ## Make sure we do not attempt installing to srcdir.
    owd <- setwd(rpkgdir)
    if (owd == getwd()) errmsg("cannot install to srcdir")
    setwd(owd)

    ## Figure out whether this is a source or binary package.
    is_source_package <- is.na(desc["Built"])

    if (!is_first_package) cat("\n")

    if (is_source_package)
        do_install_source(pkg_name, rpkgdir, pkg_dir)
    else
        do_install_binary(pkg_name, rpkgdir)

    ## FIXME
    if (.Platform$OS.type != "windows")
        system(paste("find", shQuote(rpkgdir),  "-exec chmod a+r \\{\\} \\;"))

    starsmsg(stars, "DONE (", pkg_name, ")")
    is_first_package <<- FALSE
}


do_install_binary <- function(pkg, rpkgdir)
{
    stars(stars, "Installing *binary* package ", sQuote(pkg), " ...")

    if (file.exists(file.path(rpkgdir, "DESCRIPTION"))) {
        if (lock) system(paste("mv", rpkgdir, file.path(lockdir, pkg)))
        dir.create(rpkgdir, recursive=TRUE)
    }
    res <- system(paste("cp -r .", shQuote(rpkgdir),
                        "|| (", TAR, "cd - .| (cd", shQuote(rpkgdir), "&&", TAR, "xf-))"
                        ))
    if (res) errmsg("installing binary package failed")

    if (tar_up)
        starsmsg(stars, sQuote(pkg),
                " was already a binary package and will not be rebuilt")
}

do_install_source <- function(pkg_name, rpkgdir, pkg_dir)
{
    ## Make the destination directories available to the developer's
    ## installation scripts (e.g. configure, etc.)
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
        starsmsg(stars, "Installing *Frontend* package ", sQuote(pkg_name), " ...")
        if (preclean) system(paste(MAKE, "clean"))
        if (use_configure) {
            if (utils::file_test("-x", "configure"))
                system("./configure")
            else if (file.exists("configure"))
                errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
        }
        if (file.exists("Makefile"))
            if (system(MAKE)) pkgerrmsg("make failed")
        if (clean) system(paste(MAKE, "clean"))
        return()
    }
    if (!is.na(Type) && Type == "Translation") {
        starsmsg(stars, "Installing *Translation* package ", sQuote(pkg_name), " ...")
         if(utils::file_test("-d", "share")) {
             files <- Sys.glob("share/*")
             if(length(files)) file.copy(files, R.home("share"))
         }
         if(utils::file_test("-d", "library")) {
             system(paste("cp -r ./library", R.home()))
         }
        return()
    }
    OS_type <- desc["OS_type"]
    if ((!is.na(OS_type) && OS_type == "windows") && !fake)
        errmsg(" windows-only package")

    starsmsg(stars, "Installing *source* package ", sQuote(pkg_name), " ...")

    stars <- "**"

    if (file.exists(file.path(rpkgdir, "DESCRIPTION"))) {
        ## Back up a previous version
        if (lock) {
            if (debug) starsmsg(stars, "backing up earlier installation")
            system(paste("mv", rpkgdir, file.path(lockdir, pkg_name)))
	## this is only used for recommended packages installed from .tgz
        } else if (more_than_libs) unlink(rpkgdir, recursive = TRUE)
        dir.create(rpkgdir, recursive=TRUE)
        ## FIXME preserve man pages
    }

    if (preclean) {
        if (utils::file_test("-d", "src")) {
            owd <- setwd("src")
            if (file.exists("Makefile")) system(paste(MAKE, "clean"))
            else unlink(Sys.glob(paste0("*", SHLIB_EXT)))
            setwd(owd)
        }
        if (utils::file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
    }

    if (use_configure) {
        if (utils::file_test("-x", "configure"))
            system("./configure")
        else if (file.exists("configure"))
            errmsg("'configure' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
    }

    if (more_than_libs) {
        for (f in c("NAMESPACE", "LICENSE", "LICENCE", "COPYING", "NEWS"))
            if (file.exists(f)) {
                file.copy(f, rpkgdir)
                Sys.chmod(file.path(rpkgdir, f), "644")
            }

        ## This cannot be done in a MBCS: write.dcf fails
        Sys.setlocale("LC_CTYPE", "C")
        res <- try(tools:::.install_package_description(".", rpkgdir))
        Sys.setlocale("LC_CTYPE", ctype)
        if (inherits(res, "try-error"))
            errmsg("installing package DESCRIPTION failed")
    }

    if (utils::file_test("-d", "src") && !fake) {
        system_makefile <- file.path(R.home(), etcpath, "Makeconf")
        starsmsg(stars, "libs")
        if (!file.exists(file.path(R.home("include"), "R.h")))
            warning(stars, " ",
                    "R include directory is empty -- perhaps need to install R-devel.rpm or similar")
        has_error <- FALSE
        linkTo <- desc["LinkingTo"]
        if(!is.na(linkTo)) {
            lpkgs <- strsplit(linkTo, ",[[:blank:]]*")[[1L]]
            paths <- .find.package(lpkgs, quiet=TRUE)
            if(length(paths)) {
                clink_cppflags <- paste(paste0('-I"', paths, '/include"'),
                                        collapse=" ")
                Sys.setenv(CLINK_CPPFLAGS = clink_cppflags)
           }
    }
        dir.create(file.path(rpkgdir, libpath))
        if (file.exists("src/Makefile")) {
            starsmsg(stars, "arch - ", arch)
            owd <- setwd("src")
            makefiles <- c(system_makefile, "Makefile")
            if (file.exists(f <- path.expand(paste("~/.R/Makevars",
                                                   Sys.getenv("R_PLATFORM"), sep="-"))))
                makefiles <- c(makefiles, f)
            else if (file.exists(f <- path.expand(paste("~/.R/Makevars"))))
                makefiles <- c(makefiles, f)
            res <- system(paste(MAKE,
                                paste("-f", shQuote(makefiles), collapse = " ")))
            if (res == 0) shlib_install(rpkgdir, arch)
            else has_error <- TRUE
            setwd(owd)
        } else { ## no src/Makefile
            owd <- setwd("src")
            srcs <- dir(pattern = "\\.([cfmCM]|cc|cpp|f90|f95|mm)$")
            allfiles <- if (file.exists("Makevars")) c("Makevars", srcs) else srcs
            wd2 <- setwd(file.path(R.home(), "bin", "exec"))
            archs <- Sys.glob("*")
            setwd(wd2)
            arch_keep <- arch
            if (length(allfiles)) {
                ## if there is a configure script we install only the main
                ## sub-architecture
                if (utils::file_test("-x", "configure")) {
                    if (nzchar(arch))
                        starsmsg(stars, "arch - ", substr(arch, 2, 1000))
                    has_error <- run_shlib(pkg_name, srcs, rpkgdir, arch)
                } else {
                    for(arch in archs) {
                        system("rm -f *.o *.so *.sl *.dylib")
                        if (arch == "R") {
                            has_error <- run_shlib(pkg_name, srcs, rpkgdir, "")
                        } else if (arch == "Rgnome") {
                            arch <- arch_keep
                        } else {
                            starsmsg(stars, "arch - ", arch)
                            Sys.setenv(R_ARCH = arch)
                            libarch <- paste("libs", arch, sep="/")
                            dir.create(file.path(rpkgdir, libarch),
                                       recursive = TRUE)
                            has_error <- run_shlib(pkg_name, srcs, rpkgdir, arch)
                        }
                    }
                    arch <- arch_keep
                }
            } else warning("no source files found")
        }
        if (has_error)
            pkgerrmsg("compilation failed", pkg_name)
        setwd(owd)
    } # end of src dir

    if (more_than_libs) {
        if (utils::file_test("-d", "R")) {
            starsmsg(stars, "R")
            dir.create(file.path(rpkgdir, "R"), recursive = TRUE)
            ## This cannot be done in C
            res <- try(tools:::.install_package_code_files(".", rpkgdir))
            if (inherits(res, "try-error"))
                pkgerrmsg("unable to collate files", pkg_name)

            if (file.exists(file.path("R", "sysdata.R"))) {
                res <- try(tools:::sysdata2LazyLoadDB("R/sysdata.rda", rpkgdir))
                if (inherits(res, "try-error"))
                    pkgerrmsg("unable to build sysdata DB", pkg_name)
            }
            if (fake) {
                if (file.exists("NAMESPACE")) {
                    cat("",
                        '.onLoad <- .onAttach <- function(lib, pkg) NULL',
                        sep = "\n",
                        file = file.path(rpkgdir, "R", pkg_name), append = TRUE)
                    ## <NOTE>
                    ## Tweak fake installation to provide an 'empty' useDynLib() for
                    ## the time being.  Completely removing the directive results in
                    ## checkFF() being too aggresive in the case where the presence
                    ## of the directive enables unambiguous symbol resolution w/out
                    ## 'PACKAGE' arguments.  However, empty directives are not
                    ## really meant to work ...
                    ## ${SED} -e 's/useDynLib.*/useDynLib("")/' NAMESPACE > \
                    ## "${R_PACKAGE_DIR}/NAMESPACE"
                    ## </NOTE>
                } else {
                    cat("",
                        '.First.lib <- function(lib, pkg) NULL',
                        sep = "\n",
                        file = file.path(rpkgdir, "R", pkg_name), append = TRUE)
                }
            }
        } # end of R

        if (utils::file_test("-d", "data")) {
            starsmsg(stars, "data")
            files <- Sys.glob(file.path("data", "*"))
            if (length(files)) {
                dir.create(file.path(rpkgdir, "data"), recursive = TRUE)
                file.remove(Sys.glob(file.path(rpkgdir, "data", "*")))
                file.copy(files, file.path(rpkgdir, "data"))
                Sys.chmod(Sys.glob(file.path(rpkgdir, "data", "*")), "644")
                value <- parse_description_field("LazyData", default = lazy_data)
                if (value) {
                    ## This also had an extra space in the sh version
                    starsmsg(stars, " moving datasets to lazyload DB")
                    ## it is possible that data in a package will make use of the
                    ## code in the package, so ensure the package we have just
                    ## installed is on the library path.
                    ## (We set .libPaths)
                    res <- try(tools:::data2LazyLoadDB(pkg_name, lib))
                    if (inherits(res, "try-error"))
                        pkgerrmsg("lazydata failed", pkg_name)
                } else if (use_zip_data &&
                           nzchar(Sys.getenv("R_UNZIPCMD")) &&
                           nzchar(zip <- Sys.getenv("R_ZIPCMD"))) {
                    owd <- setwd(file.path(rpkgdir, "data"))
                    system("find . -type f -print > filelist")
                    system(paste(zip, "-q -m Rdata * -x filelist 00Index"))
                    setwd(owd)
                }
            } else warning("empty 'data' directory")
        }

        if (utils::file_test("-d", "demo") && !fake) {
            starsmsg(stars, "demo")
            dir.create(file.path(rpkgdir, "demo"), recursive = TRUE)
            file.remove(Sys.glob(file.path(rpkgdir, "demo", "*")))
            res <- try(tools:::.install_package_demos(".", rpkgdir))
            if (inherits(res, "try-error"))
                pkgerrmsg("ERROR: installing demos failed")
        }

        if (utils::file_test("-d", "exec") && !fake) {
            starsmsg(stars, "exec")
            dir.create(file.path(rpkgdir, "exec"), recursive = TRUE)
            file.remove(Sys.glob(file.path(rpkgdir, "exec", "*")))
            files <- Sys.glob(file.path("exec", "*"))
            if (length(files)) {
                file.copy(files, file.path(rpkgdir, "exec"))
                Sys.chmod(Sys.glob(file.path(rpkgdir, "exec", "*")), "755")
            }
        }

        if (utils::file_test("-d", "inst") && !fake) {
            starsmsg(stars, "inst")
            cp_r("inst", rpkgdir)
        }

        ## LazyLoading
        value <- parse_description_field("LazyLoad", default = lazy)
        if (!utils::file_test("-d", "R")) value <- FALSE
        if (value) {
            starsmsg(stars, "preparing package for lazy loading")
            ## Something above, e.g. lazydata,  might have loaded the namespace
            if(pkg_name %in% loadedNamespaces())
                unloadNamespace(pkg_name)
            res <- try({.getRequiredPackages(quietly = TRUE)
                        tools:::makeLazyLoading(pkg_name, lib)})
            if (inherits(res, "try-error"))
                pkgerrmsg("lazy loading failed", pkg_name)
            ## FIXME: still needed?  If so needs a pretest
            ## file.remove(file.path(rpkgdir, "R", "all.rda"))
        }

        if (utils::file_test("-d", "man")) {
            starsmsg(stars, "help")
            res <- try(tools:::.install_package_man_sources(".", rpkgdir))
            if (inherits(res, "try-error"))
                pkgerrmsg("installing man sources failed", pkg_name)
            Sys.chmod(file.path(rpkgdir, "man",
                                paste0(pkg_name, ".Rd.gz")), "644")
            if (build_help) {
                cmd <- paste("perl",
                             shQuote(file.path(R.home("share"), "perl", "build-help.pl")),
                             paste(build_help_opts, collapse=" "),
                             shQuote(pkg_dir),
                             shQuote(lib),
                             shQuote(rpkgdir),
                             pkg_name)
                if (debug) message("about to run ", sQuote(cmd))
                res <- system(cmd)
                if (res)
                    pkgerrmsg("building help failed", pkg_name)
                if (use_zip_help &&
                   nzchar(Sys.getenv("R_UNZIPCMD")) &&
                   nzchar(zip <- Sys.getenv("R_ZIPCMD"))) {
                    owd <- setwd(rpkgdir)
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
            }
        } # else

        ## pkg indices
        starsmsg(stars, "building package indices ...")
        res <- try(tools:::.install_package_indices(".", rpkgdir))
        if (inherits(res, "try-error"))
            errmsg("installing package indices failed")

        ## Install a dump of the parsed NAMESPACE file
        if (file.exists("NAMESPACE") && !fake) {
            res <- try(tools:::.install_package_namespace_info(".", rpkgdir))
            if (inherits(res, "try-error"))
                errmsg("installing namespace metadata failed")
        }

    } # more_than_libs
    ## <NOTE>
    ## Remove stuff we should not have installed in the first place.
    ## When installing from a source directory under version control, we
    ## should really exclude the subdirs CVS, .svn (Subversion) and
    ## .arch-ids (arch).
    for(d in c("CVS", ".svn", ".arch-ids", ".git")) {
        ## FIXME
        system(paste("find",  shQuote(rpkgdir), "-name", d,
                     "-type d -prune -exe rm \\{\\} \\;"),
               ignore.stderr = TRUE)
    }

    if (clean) {
        if (utils::file_test("-d", "src")) {
            owd <- setwd("src")
            if (file.exists("Makefile"))
                system(paste(MAKE, "clean"))
            else {
                ## we used SHLIB --clean
                system(paste0("rm -f *", SHLIB_EXT))
            }
            setwd(owd)
        }
        if (utils::file_test("-x", "cleanup")) system("./cleanup")
        else if (file.exists("cleanup"))
            warning("'cleanup' exists but is not executable -- see the 'R Installation and Adminstration Manual'")
    }

    if (tar_up) {
        version <- desc["version"]
        filename <- paste0(pkg_name, "_", version, "_R_",
                           Sys.getenv("R_PLATFORM"), ".tar")
        filepath <- shQuote(file.path(startdir, filename))
        system(paste(TAR, "-chf", filepath, "-C", rlibdir, pkg_name))
        system(paste(GZIP, "-9f", filepath))
        message("packaged installation of ",
                sQuote(pkg_name), " as ", filename, ".gz")
    }
    stars <- "*"
}

options(showErrorCalls=FALSE)
pkgs <- character(0)
lib <- ""
args <- commandArgs(TRUE)

startdir <- getwd()

tmpdir <- tempfile("R.INSTALL")
if (!dir.create(tmpdir))
    stop("cannot create temporary directory")

R_LIBS0 <- Sys.getenv("R_LIBS")
stars <- "*"

pkgs <- character(0)
lib <- lib0 <- ""
clean <- FALSE
preclean <- FALSE
debug <- FALSE
build_text <- TRUE
build_html <- TRUE
build_latex <- TRUE
build_example <- TRUE
build_help_opts <- character(0)
use_configure <- TRUE
use_zip_data <- FALSE
use_zip_help <- FALSE
## FIXME: make these character vectors
configure_args <- ""
configure_vars <- ""
fake <- FALSE
lazy <- TRUE
lazy_data <- FALSE
lock <- TRUE
pkglock <- FALSE
pkglockname <- ""
libs_only <- FALSE
tar_up <- FALSE
shargs <- character(0)

while(length(args)) {
    a <- args[1]
    if (a %in% c("-h", "--help")) {
        Usage()
        return(invisible())
    }
    else if (a %in% c("-v", "--version")) {
        cat("R add-on package installer ", R.version[["svn rev"]], "\n", sep = "")
        cat("",
            "Copyright (C) 2000-2009 The R Core Development Team.",
            "This is free software; see the GNU General Public License version 2",
            "or later for copying conditions.  There is NO warranty.",
            sep="\n")
        q("no")
    } else if (a %in% c("-c", "--clean")) {
        clean <- TRUE
        shargs <- c(shargs, "--clean")
    } else if (a == "--preclean") {
        preclean <- TRUE
        shargs <- c(shargs, "--preclean")
    } else if (a %in% c("-d", "--debug")) {
        debug <- TRUE
    } else if (a == "--with-package-versions") {
        warning("Use of --with-package-versions is defunct")
    } else if (a == "--no-configure") {
        use_configure = FALSE;
    } else if (a == "--no-docs") {
        build_text <- build_html <- build_latex <- build_example <- FALSE
    } else if (a == "--no-text") {
        build_text <- FALSE
    } else if (a == "--no-html") {
        build_html <- FALSE
    } else if (a == "--no-examples") {
        build_examples <- FALSE
    } else if (a == "--use-zip") {
        use_zip_data <- use_ziphelp <- TRUE
    } else if (a == "--use-zipdata") {
        use_zip_data <- TRUE
    } else if (a == "--use-ziphelp") {
        use_zip_help <- TRUE
    } else if (a == "--auto-zip") {
        ## FIXME: implement this
        auto_zip <- TRUE
    } else if (a == "-l") {
        if (length(args) >= 2) {lib <- args[2]; args <- args[-1]}
        else stop("-l option without value", call. = FALSE)
    } else if (substr(a, 1, 10) == "--library=") {
        lib <- substr(a, 11, 1000)
    } else if (substr(a, 1, 17) == "--configure-args=") {
        configure_args <- substr(a, 18, 1000)
    } else if (substr(a, 1, 17) == "--configure-vars=") {
        configure_vars <- substr(a, 18, 1000)
    } else if (a == "--fake") {
        fake <- TRUE
    } else if (a %in% c("--no-lock", "--unsafe")) {
        lock <- FALSE
    } else if (a == "--pkglock") {
        pkglock <- TRUE
    } else if (a == "--libs-only") {
        libs_only <- TRUE
    } else if (a == "--build") {
        tar_up <- TRUE
    } else pkgs <- c(pkgs, a)
    args <- args[-1]
}

## now unpack unbundle etc
allpkgs <- character(0)
for(pkg in pkgs) {
    if (debug) message("processing ", sQuote(pkg))
    if (utils::file_test("-f", pkg)) {
        if (debug) message("a file")
        pkgname <- basename(pkg)
	## Also allow for 'package.tgz' ...
        pkgname <- sub("\\.tgz$", "", pkgname)
        pkgname <- sub("_.*", "", pkgname)
	## Note that we use '-m' so that modification dates are *not*
	## preserved when untarring the sources.  This is necessary to
	## ensure that the preformatted help pages are always rebuilt.
	## Otherwise, the build date for an older version may be newer
	## than the modification date for the new sources as recorded in
	## the tarball ...
        system(paste(GZIP, "-dc", shQuote(pkg),
                     "| (cd ", shQuote(tmpdir), "&&", TAR, "-mxf -)"))
	## If we have a binary bundle distribution, the DESCRIPTION file
	## is at top level.
        if (file.exists(ff <- file.path(tmpdir, "DESCRIPTION"))) {
            con <- read.dcf(ff, "Contains")
            if (!is.na(con)) {
                starmsg(stars, "Looks like a binary bundle")
                allpkgs <- c(allpkgs, get_packages(tmpdir))
            } else {
                message("unknown package layout")
                do_cleanup_tmpdir()
                q("no", status = 1)
            }
        } else if (file.exists(file.path(tmpdir, pkgname, "DESCRIPTION"))) {
            allpkgs <- c(allpkgs, get_packages(file.path(tmpdir, pkgname)))
        } else
            errmsg("cannot extract package from ", sQuote(pkg))
    } else if (file.exists(file.path(pkg, "DESCRIPTION"))) {
        if (debug) message("a directory")
        allpkgs <- c(allpkgs, get_packages(pkg))
    } else {
        warning("invalid package ", sQuote(pkg))
    }
    if (pkglock) {
        if (nzchar(pkglockname)) {
            warning("--pkglock applies only to a single bundle/package")
            pkglock <- FALSE
        } else pkglockname <- pkg
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
    stop("ERROR: no permission install to directory ",
         sQuote(lib), call. = FALSE)

if (lock) {
    lockdir <- if (pkglock) file.path(lib, paste("00LOCK", pkg_name, sep="-"))
    else file.path(lib, "00LOCK")
    if (file.exists(lockdir)) {
        message("ERROR: failed to lock directory ", sQuote(lib),
                " for modifying\nTry removing ", sQuote(lockdir))
        do_cleanup_tmpdir()
        q("no", status=3)
    }
    dir.create(lockdir, recursive = TRUE)
    if (!utils::file_test("-d", lockdir)) {
        message("ERROR: failed to create lock directory ", sQuote(lockdir))
        do_cleanup_tmpdir()
        q("no", status=3)
     }
    if (debug) starsmsg(stars, "created lock directory ", sQuote(lockdir))
}

if  (tar_up && fake)
    stop("building a fake installation is disallowed")

if (libs_only) {
    lock <- FALSE
    tar_up <- FALSE
}
more_than_libs <- !libs_only

if (fake) {
  use_configure <- FALSE
  build_text <- TRUE
  build_html <- FALSE
  build_latex <- TRUE
  build_example <- FALSE
}

if (build_text) build_help_opts <- c(build_help_opts, "--txt")
if (build_html) build_help_opts <- c(build_help_opts, "--html")
if (build_latex) build_help_opts <- c(build_help_opts, "--latex")
if (build_example) build_help_opts <- c(build_help_opts, "--example")
build_help <- length(build_help_opts) > 0L
if (build_help && debug) build_help_opts <- c("--debug", build_help_opts)
if (debug)
    starsmsg(stars, "build_help_opts=", paste(build_help_opts, collapse=" "))

if (build_help) {
    perllib <- Sys.getenv("PERL5LIB")
    if(nzchar(perllib)) {
        Sys.setenv(PERL5LIB = paste(file.path(R.home("share"), "perl"),
                   perllib, sep = ":"))
    } else {
        perllib <- Sys.getenv("PERLLIB")
        Sys.setenv(PERLLIB = paste(file.path(R.home("share"), "perl"),
                   perllib, sep = ":"))
    }
}

is_first_package <- TRUE

if (debug)
    starsmsg(stars, "DBG: R CMD INSTALL' now doing do_install")

for(pkg in allpkgs) do_install(pkg)
do_cleanup()

# q("no")
