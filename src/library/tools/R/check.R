#  File src/library/tools/R/check.R
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

#### R based engine for R CMD check

## Perl version uses R_system and R_runR extensively.

R_system <- function(cmd, env = "")
{
    WINDOWS <- .Platform$OS.type == "windows"
    if(WINDOWS) shell(paste(env, cmd), shell = "sh.exe")
    else system(paste(env, cmd))
}

R_runR <- function(cmd, Ropts="", env = "")
{
    Rin <- tempfile("Rin")
    Rout <- tempfile("Rout")
    writeLines(cmd, Rin)
    R_system(paste(shQuote(file.path(R.home("bin"),
                                     ifelse(WINDOWS("Rterm.exe", "R")))),
                   Ropts, "<", Rin, ">", Rout, "2>&1"), env)
    readLines(Rout, warn = FALSE)
}

R_run_R <- function(cmd, Ropts, env)
{
    WINDOWS <- .Platform$OS.type == "windows"
    Rin <- tempfile("Rin")
    Rout <- tempfile("Rout")
    writeLines(cmd, Rin)
    status <- R_system(paste(shQuote(file.path(R.home("bin"),
                                               ifelse(WINDOWS("Rterm.exe", "R")))),
                             Ropts, "<", Rin, ">", Rout, "2>&1"), env)
    list(status = status, out = readLines(Rout, warn = FALSE))
}

.check_packages <- function(args = NULL)
{
    WINDOWS <- .Platform$OS.type == "windows"

    if(!WINDOWS) R_EXE <- file.path(R.home("bin"), "R")

    ## This version merges stdout and stderr
    shell_with_capture <- function (command) {
        outfile <- tempfile("xshell")
        on.exit(unlink(outfile))
        status <- if (.Platform$OS.type == "windows")
            shell(sprintf("%s > %s 2>&1", command, outfile), shell = "cmd.exe")
        else system(sprintf("%s > %s 2>&1", command, shQuote(outfile)))
        list(status = status, stdout = readLines(outfile, warn = FALSE))
    }

    .file_test <- function(op, x)
        switch(op,
               "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
               "-x" = (file.access(x, 1L) == 0L),
               stop(sprintf("test '%s' is not available", op), domain = NA))

    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    do_exit <- function(status = 1L) q("no", status = status, runLast = FALSE)

    env_path <- function(...) {
        paths <- c(...)
        paste(paths[nzchar(paths)], collapse = .Platform$path.sep)
    }

    config_val_to_logical <- function(val) {
        v <- tolower(val)
        if (v %in% c("1", "yes", "true")) TRUE
        else if (v %in% c("0", "no", "false")) FALSE
        else {
            warning("cannot coerce ", sQuote(val), " to logical")
            NA
        }
    }

    Usage <- function() {
        cat("Usage: R CMD check [options] pkgs",
            "",
            " Check R packages from package sources, which can be directories or",
            "package 'tar' archives with extension '.tar.gz', '.tar.bz2' or '.tgz'.",
            "",
            "A variety of diagnostic checks on directory structure, index and",
            "control files are performed.  The package is installed into the log",
            "directory (which includes the translation of all Rd files into several",
            "formats), and the Rd files are tested by LaTeX (if available).  All",
            "examples and tests provided by the package are tested to see if they",
            "run successfully.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -l, --library=LIB     library directory used for test installation",
            "			of packages (default is outdir)",
            "  -o, --outdir=DIR      directory used for logfiles, R output, etc.",
            "			(default is 'pkg.Rcheck' in current directory,",
            "			where 'pkg' is the name of the package checked)",
            "      --no-clean        do not clean outdir before using it",
            "      --no-codoc        do not check for code/documentation mismatches",
            "      --no-examples     do not run the examples in the Rd files",
            "      --no-install      skip installation and associated tests",
            "      --no-tests        do not run code in tests subdirectory",
            "      --no-vignettes    do not check vignettes in Sweave format",
            "      --no-latex        do not run LaTeX on help files",
            "      --use-gct         use 'gctorture(TRUE)' when running examples/tests",
            "      --use-valgrind    use 'valgrind' when running examples/tests/vignettes",
            "      --timings         record timings for examples",
            "      --install-args=	command-line args to be passed to INSTALL",
            "      --check-subdirs=default|yes|no",
            "			run checks on the package subdirectories",
            "			(default is yes for a tarball, no otherwise)",
            "      --rcfile=FILE     read configuration values from FILE",
            "      --extra-arch      do only runtime tests needed for an additional",
            "                        sub-architecture.",
            "",
            "By default, all test sections are turned on.",
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep="\n")
    }

    options(showErrorCalls=FALSE, warn = 1)

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    clean <- TRUE
    do_codoc <- TRUE
    do_examples <- TRUE
    do_install <- TRUE; install <- ""
    do_tests <- TRUE
    do_vignettes <- TRUE
    do_latex <- TRUE
    use_gct <- FALSE
    use_valgrind <- FALSE
    do_timings <- FALSE
    install_args <- NULL
    check_subdirs <- ""           # defaults to R_check_subdirs_strict
    home <- Sys.getenv("HOME", NA)
    rcfile <- if(!is.na(home)) file.path(home, ".R", "check.conf") else character()
    extra_arch <- FALSE
    spec_install <- FALSE
    do_ff_calls <- TRUE

    libdir <- ""
    outdir <- ""
    pkgs <- character()
    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            do_exit(0L)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package check: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 1997-2010 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            do_exit(0L)
        } else if (a == "-o") {
            if (length(args) >= 2L) {outdir <- args[2L]; args <- args[-1L]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            outdir <- substr(a, 10, 1000)
        } else if (a == "-l") {
            if (length(args) >= 2L) {libdir <- args[2L]; args <- args[-1L]}
            else stop("-l option without value", call. = FALSE)
        } else if (substr(a, 1, 10) == "--library=") {
            libdir <- substr(a, 11, 1000)
        } else if (a == "--no-clean") {
            clean  <- FALSE
        } else if (a == "--no-codoc") {
            do_codoc  <- FALSE
        } else if (a == "--no-examples") {
            do_examples  <- FALSE
        } else if (a == "--no-install") {
            do_install  <- FALSE
        } else if (substr(a, 1, 10) == "--install=") {
            install <- substr(a, 11, 1000)
        } else if (a == "--no-tests") {
            do_tests  <- FALSE
        } else if (a == "--no-vignettes") {
            do_vignettes  <- FALSE
        } else if (a == "--no-latex") {
            do_latex  <- FALSE
        } else if (a == "--use-gct") {
            use_gct  <- TRUE
        } else if (a == "--use-valgrind") {
            use_valgrind  <- TRUE
        } else if (a == "--timings") {
            do_timings  <- TRUE
        } else if (substr(a, 1, 15) == "--install-args=") {
            install_args <- substr(a, 16, 1000)
        } else if (substr(a, 1, 16) == "--check_subdirs=") {
            check_subdirs <- substr(a, 17, 1000)
        } else if (substr(a, 1, 9) == "--rcfile=") {
            rcfile <- c(rcfile, substr(a, 10, 1000))
        } else if (a == "--extra-arch") {
            extra_args  <- TRUE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }

    ## record some of the options used.
    opts <- character()
    if(install == "fake") opts <- c(opts, "--install=fake")
    if(!do_install) opts <- c(opts, "--no-install")
    if(install == "no") {
        opts <- c(opts, "--install=no")
        do_install <- FALSE
    }

    if(install == "fake") {
        ## If we fake installation, then we cannot *run* any code.
        do_examples = do_tests = do_vignettes = 0;
        spec_install = TRUE;
    }

    ## The neverending story ...
    ## For the time being, allow to turn this off by setting the environment
    ## variable _R_CHECK_FF_CALLS_ to a Perl 'null' value.
    do_ff_calls <- nzchar(Sys.getenv("_R_CHECK_FF_CALLS_", "true"))

    ## Use system default unless explicitly specified otherwise.
    Sys.setenv(R_DEFAULT_PACKAGES="")

    ## Configurable variables
    R_check_use_install_log <-
        Sys.getenv("_R_CHECK_USE_INSTALL_LOG_", "TRUE")
    R_check_subdirs_nocase <-
        Sys.getenv("_R_CHECK_SUBDIRS_NOCASE_", "FALSE")
    R_check_all_non_ISO_C <-
        Sys.getenv("_R_CHECK_ALL_NON_ISO_C_", "FALSE")
    R_check_weave_vignettes <-
        Sys.getenv("_R_CHECK_WEAVE_VIGNETTES_", "TRUE")

    ## This needs texi2dvi.  MiKTeX has a binary texi2dvi, but other
    ## Windows' LaTeX distributions do not.  We check later.
    R_check_latex_vignettes <-
        Sys.getenv("_R_CHECK_LATEX_VIGNETTES_", "TRUE")

    R_check_subdirs_strict <-
        Sys.getenv("_R_CHECK_SUBDIRS_STRICT_", "default")
    R_check_Rd_xrefs <-
        Sys.getenv("_R_CHECK_RD_XREFS_", "TRUE")
    R_check_use_codetools <-
        Sys.getenv("_R_CHECK_USE_CODETOOLS_", "TRUE")
    R_check_force_suggests <-
        Sys.getenv("_R_CHECK_FORCE_SUGGESTS_", "TRUE")
    R_check_Rd_style <-
        Sys.getenv("_R_CHECK_RD_STYLE_", "TRUE")
    R_check_executables <-
        Sys.getenv("_R_CHECK_EXECUTABLES_", "TRUE")
    R_check_executables_exclusions <-
        Sys.getenv("_R_CHECK_EXECUTABLES_EXCLUSIONS_", "TRUE")
    ## This check needs codetools
    R_check_dot_internal <-
        Sys.getenv("_R_CHECK_DOT_INTERNAL_", "FALSE")
    R_check_Rd_contents <-
        Sys.getenv("_R_CHECK_RD_CONTENTS_", "TRUE")
    ## Only relevant when the package is loaded, thus installed.
    R_check_suppress_RandR_message <-
        do_install && config_val_to_logical(Sys.getenv("_R_CHECK_SUPPRESS_RANDR_MESSAGE_", "TRUE"))

    for(f in rcfile) {
        if(!file.exists(rcfile)) next
        lines <- readLines(rcfile, warn = FALSE)
        ## FIXME do something here: source it?
    }

    R_check_use_install_log <-
        config_val_to_logical(R_check_use_install_log)
    R_check_subdirs_nocase <-
        config_val_to_logical(R_check_subdirs_nocase)
    R_check_all_non_ISO_C <-
        config_val_to_logical(R_check_all_non_ISO_C)
    R_check_weave_vignettes <-
        config_val_to_logical(R_check_weave_vignettes)
    R_check_latex_vignettes <-
        config_val_to_logical(R_check_latex_vignettes)
    R_check_Rd_xrefs <-
        config_val_to_logical(R_check_Rd_xrefs)
    R_check_use_codetools <-
        config_val_to_logical(R_check_use_codetools)
    R_check_Rd_style <-
        config_val_to_logical(R_check_Rd_style)
    R_check_executables <-
        config_val_to_logical(R_check_executables)
    R_check_executables_exclusions <-
        config_val_to_logical(R_check_executables_exclusions)
    R_check_dot_internal <-
        config_val_to_logical(R_check_dot_internal)
    ## <NOTE>
    ## This looks a bit strange, but tools:::.check_package_depends()
    ## currently gets the information about forcing suggests via an
    ## environment variable rather than an explicit argument.
    R_check_force_suggests <-
        config_val_to_logical(R_check_force_suggests)
    ## And in fact, it gets even stranger ...
    ## <FIXME>
    ## Compatibility code for old-style interface: sanitize eventually.
    R_check_force_suggests <- ifelse(R_check_force_suggests, "true", "false")
    Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = R_check_force_suggests)
    ## </FIXME>
    ## </NOTE>
    R_check_Rd_contents <-
        config_val_to_logical(R_check_Rd_contents)
    R_check_suppress_RandR_message <-
        config_val_to_logical(R_check_suppress_RandR_message)

    if(!nzchar(check_subdirs)) check_subdirs <- R_check_subdirs_strict

    if(extra_arch)
        do_latex <- R_check_Rd_contents <- R_check_all_non_ISO_C <-
            R_check_Rd_xrefs <- R_check_use_codetools <- R_check_Rd_style <-
                R_check_executables <- R_check_dot_internal <- FALSE

    startdir <- getwd()
    if(!nzchar(outdir)) outdir <- startdir
    try(setwd(outdir)) # check it
    outdir <- getwd()
    setwd(startdir)

    R_LIBS <- Sys.getenv("R_LIBS")
    if(nzchar(libdir)) {
        try(setwd(libdir)) # test it
        libdir <- getwd()
        Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
        setwd(startdir)
    }

    TAR <- Sys.getenv("TAR", "tar")
    R_opts <- "--vanilla"

    msg_DESCRIPTION <- c("See the information on DESCRIPTION files",
                         " in the chapter 'Creating R packages'",
                         " of the 'Writing R Extensions' manual.\n")

    if(!length(pkgs)) {
        message("Error: no packages were specified")
        do_exit(1L)
    }

    ## This is the main loop over all packages to be checked.
    for (pkg in pkgs) {
        ## pkg should be the path to the package root source
        ## directory, either absolute or relative to startdir.
        ## As from 2.1.0 it can also be a tarball

        ## $pkgdir is the corresponding absolute path.
        ## pkgname is the name of the package.
        setwd(startdir)
        pkg <- sub("/$", "", pkg) # strip any trailing '/'
        opkgname <- pkgname <- basename(pkg)
        is_ascii <- FALSE

        thispkg_subdirs <- check_subdirs
        ## is this a tar archive?
        if(dir.exists(pkg)) {
            istar <- FALSE
            if(thispkg_subdirs == "default") thispkg_subdirs <- "no"
       } else {
            istar <- TRUE
            if(thispkg_subdirs == "default") thispkg_subdirs <- "yes-maybe"
            pkgname <- sub("\\.(tar\\.gz|tgz|tar\\.bz2)$", "", pkgname)
            pkgname <- sub("_[0-9.-]*$", "", pkgname)
        }
        pkgoutdir <- file.path(outdir, paste(pkgname, "Rcheck", sep = "."))
        if(clean && dir.exists(pkgoutdir))
            unlink(pkgoutdir, recursive = TRUE)
        dir.create(pkgoutdir, mode = "0755") # FIXME check
        if(istar) {
            dir <- file.path(pkgoutdir, "00_pkg_src")
            dir.create(dir, mode = "0755")  # FIXME check
            untar(pkg, exdir = dir)
            pkg <- file.path(dir, pkgname)
        }
        if(!dir.exists(pkg))
            stop("package dir ", sQuote(pkg), " does not exist")
        setwd(pkg)
        pkgdir <- getwd()
        thispkg_src_subdirs <- thispkg_subdirs
        if(thispkg_src_subdirs == "yes-maybe") {
            ## now see if there is a 'configure' file
            ## configure files are only used if executable, but
            ## -x is always false on Windows.
            if(WINDOWS) {
                if(.file_test("-f", "configure")) thispkg_src_subdirs <- "no"
            } else {
                if(.file_test("-x", "configure")) thispkg_src_subdirs <- "no"
            }
        }
        setwd(startdir)

        Log <- newLog(file.path(pkgoutdir, "00check.log"));
        messageLog(Log, "using log directory ", sQuote(pkgoutdir))
        messageLog(Log, "using ", R.version.string)
        .find_charset <- function () {
            l10n <- l10n_info()
            if (l10n[["UTF-8"]]) "UTF-8" else utils::localeToCharset()
        }
        charset <- .find_charset()
        messageLog(Log, "using session charset: ", charset)
        is_ascii <- charset == "ASCII"

        ## report options used
        if(!do_codoc) opts <- c(opts, "--no-codoc")
        if(!do_examples && !spec_install) opts <- c(opts, "--no-examples")
        if(!do_tests && !spec_install) opts <- c(opts, "--no-tests")
        if(!do_vignettes && !spec_install) opts <- c(opts, "--no-vignettes")
        if(use_gct) opts <- c(opts, "--use-gct")
        if(use_valgrind) opts <- c(opts, "--use-valgrind")
        if(length(opts) > 1L)
            messageLog(Log, "using options ", sQuote(paste(opts, collapse=" ")))
        else if (length(opts) == 1L)
            messageLog(Log, "using option ", sQuote(opts))

        if(!nzchar(libdir)) {
            libdir <- pkgoutdir
            Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
        }
        if(WINDOWS && grepl(" ", libdir)) # need to avoid spaces in libdir
            libdir <- shortPathName(libdir)

        is_base_pkg <- FALSE

        ## Package sources from the R distribution are special.  They
        ## have a 'DESCRIPTION.in' file (instead of 'DESCRIPTION'),
        ## with Version field containing '@VERSION@' for substitution
        ## by configure.  We test for such packages by looking for
        ## 'DESCRIPTION.in' (and 'Makefile.in') with Priority 'base',
        ## and skip the installation test for such packages.
        ## We have to check for Makefile.in in addition to
        ## DESCRIPTION.in so that we can allow packages to have a
        ## DESCRIPTION.in.  Such DESCRIPTION.in files will have their
        ## content completed via the package's configuration which we
        ## have not yet run.  The DESCRIPTION.in file may be malformed
        ## (according to R::Dcf()), e.g if it has a line @SYSTEM@
        ## which is either empty or "System: ...." depending on the
        ## configuration script.

        if(file.exists(file.path(pkgdir, "DESCRIPTION.in")) &&
           file.exists(file.path(pkgdir, "Makefile.in"))) {
            ## FIXME check
            desc <- read.dcf(file.path(pkgdir, "DESCRIPTION.in"))[1L, ]
            if(desc["Priority"] == "base") {
                messageLog(Log, "looks like ", sQuote(pkgname),
                           " is a base package")
                messageLog(Log, "skipping installation test")
                is_base_pkg <- TRUE
            }
        }
        if(!is_base_pkg) {
            checkingLog(Log, "for file ",
                        sQuote(file.path(pkgname, "DESCRIPTION")))
            if(file.exists(file.path(pkgdir, "DESCRIPTION"))) {
                ## FIXME check
                desc <- read.dcf(file.path(pkgdir, "DESCRIPTION"))[1L, ]
                resultLog(Log, "OK")
                encoding <- desc["Encoding"]
            } else {
                resultLog(Log, "NO")
                do_exit(1L)
            }
            if(!is.na(desc["Type"])) { # standard packages do not have this
                checkingLog(Log, "extension type")
                resultLog(Log, desc["Type"])
                if(desc["Type"] != "Package") {
                    printLog(Log,
                             "Only Type = Package extensions can be checked.\n")
                    do_exit(0L)
                }
            }
            if(!is.na(desc["Bundle"])) {
                messageLog(Log, "looks like ", sQuote(pkgname),
                           " is a package bundle -- they are defunct")
                errorLog(Log, "")
                do_exit(1L)
            }

            package_name <- desc["Package"]
            messageLog(Log, "this is package ", sQuote(package_name),
                       " version ", sQuote(desc["Version"]))

            if(!is.na(encoding)) messageLog(Log, "package encoding: ", encoding)

            ## Check if we have any hope of installing

            OS_type <- desc["OS_type"]
            if (do_install && !is.na(OS_type)) {
                if (WINDOWS && OS_type != "windows") {
                    messageLog(Log, "will not attempt to install this package on Windows")
                    do_install <- FALSE
                }
                if (!WINDOWS && OS_type == "windows" && do_install) {
                    messageLog(Log, "this is a Windows-only package, skipping installation")
                    do_install <- FALSE
                }
            }

            ## Check CRAN incoming feasibility.
            if(config_val_to_logical(Sys.getenv("_R_CHECK_CRAN_INCOMING_", "FALSE"))) {
                checkingLog(Log, "CRAN incoming feasibility")
                out <- .check_package_CRAN_incoming(pkgdir )
                print(out)
                ## FIXME: do some analysis here
            }

            ## Check package dependencies.

            ## <NOTE>
            ## We want to check for dependencies early, since missing
            ## dependencies may make installation fail, and in any case we
            ## give up if they are missing.  But we don't check them if
            ## we are not going to install and hence not run any code.
            ## </NOTE>

            if(do_install) {
                ## Try figuring out whether the package dependencies can be
                ## resolved at run time.  Ideally, the installation
                ## mechanism would do this, and we also do not check
                ## versions ... also see whether vignette and namespace
                ## package dependencies are recorded in DESCRIPTION.

                ## <NOTE>
                ## We are not checking base packages here, so all packages do
                ## have a description file.
                ## </NOTE>

                ## <NOTE>
                ## If a package has a namespace, checking dependencies will
                ## try making use of it without the NAMESPACE file ever
                ## being validated.
                ## Uncaught errors can lead to messages like
                ##   * checking package dependencies ... ERROR
                ##   Error in e[[1]] : object is not subsettable
                ##   Execution halted
                ## which are not too helpful :-(
                ## Hence, we try to intercept this here.

                if(!extra_arch &&
                   file.exists(file.path(pkgdir, "NAMESPACE"))) {
                    checkingLog(Log, "package name space information")
                    msg_NAMESPACE <-
                        c("See section 'Package name spaces'",
                          " of the 'Writing R Extensions' manual.\n")
                    tryCatch(parseNamespaceFile(basename(pkgdir), dirname(pkgdir)),
                             error = function(e) {
                                 printLog(Log,
                                          "Invalid NAMESPACE file, parsing gives:", "\n",
                                          as.character(e), "\n")
                                 printLog(Log, strwrap(msg_NAMESPACE), "\n")
                                 do_exit(1L)
                             })
                    resultLog(Log, "OK")
                }

                checkingLog(Log, "package dependencies")
                ## Everything listed in Depends or Suggests or Imports
                ## should be available for successfully running R CMD check.
                ## \VignetteDepends{} entries not "required" by the package code
                ## must be in Suggests.  Note also that some of us think that a
                ## package vignette must require its own package, which OTOH is
                ## not required in the package DESCRIPTION file.
                ## Namespace imports must really be in Depends.
                res <- .check_package_depends(pkgdir)
                if(any(sapply(res, length) > 0)) {
                    errorLog(Log)
                    res <- capture.ouptut(print(res))
                    printLog(Log, paste(res, collapse="\n"), "\n")
                    printLog(Log, strwrap(msg_NAMESPACE), "\n")
                    do_exit(1L)
                } else resultLog(Log, "OK")

                ## <NOTE>
                ## This check should be adequate, but would not catch a manually
                ## installed package, nor one installed prior to 1.4.0.
                ## </NOTE>
                checkingLog(Log, "if this is a source package")
                if (!is.na(desc["Built"])) {
                    errorLog(Log)
                    printLog(Log, "Only *source* packages can be checked.\n")
                    do_exit(1L)
                } else if (!grepl("^check", install)) {
                    ## Check for package 'src' subdirectories with object
                    ## files (but not if installation was already performed).
                    pat <- "(a|o|[ls][ao]|sl|obj)"; # Object file extensions.
                    any <- FALSE
                    srcd <- file.path(pkgdir, "src")
                    if(dir.exists(srcd) &&
                       length(list_files_with_exts(srcd, pat))) {
                        if(!any) Log <- warningLog(Log)
                        any <- TRUE
                        printLog(Log, "Subdirectory ",
                                 sQuote(file.path(pkgname, "src")),
                                 " contains object files.\n")
                    }
                    if(thispkg_src_subdirs != "no" && dir.exists(srcd)) {
                        setwd(srcd)
                        if(!file.exists("Makefile") &&
                           !file.exists("Makefile.win") &&
                           !(file.exists("Makefile.in") && spec_install)) {
                            ## Recognized extensions for sources or headers.
                            srcfiles <- dir(".", all.files = TRUE)
                            fi <- file.info(srcfiles)
                            srcfiles <- srcfiles[!fi$isdir]
                            srcfiles <- grep("(\\.([cfmCM]|cc|cpp|f90|f95|mm|h)$|^Makevars|-win\\.def$)",
                                             srcfiles,
                                             invert = TRUE, value = TRUE)
                            if (length(srcfiles)) {
                                if(!any) Log <- warningLog()
                                any <- TRUE
                                text <- c(paste("Subdirectory ", sQuote("src"), "contains:"),
                                          strwrap(paste(srcfiles, collapse = " "),
                                                  indent = 2, exdent = indent),
                                          strwrap("These are unlikely file names for src files."), "")
                                printLog(Log, paste(text, collapse = "\n"))
                            }
                        }
                        setwd(startdir)
                    }
                    if(!any) resultLog(Log, "OK")
                } else resultLog(Log, "OK")

                ## we need to do this before installation
                if (R_check_executables) {
                }

                ## Option '--no-install' turns off installation and the tests
                ## which require the package to be installed.  When testing
                ## recommended packages bundled with R we can skip installation,
                ## and do so if '--install=skip' was given.  If command line
                ## option '--install' is of the form 'check:FILE', it is assumed
                ## that installation was already performed with stdout/stderr to
                ## FILE, the contents of which need to be checked (without
                ## repeating the installation).
                ## <NOTE>
                ## In this case, one also needs to specify *where* the package
                ## was installed to using command line option '--library'.
                ## Perhaps we should check for that, although '--install=check'
                ## is really only meant for repository maintainers.
                ## </NOTE>
                if(install == "skip")
                    messageLog(Log, "skipping installation test")
                else {
                    use_install_log <-
                        (grepl("^check", install) ||
                         R_check_use_install_log) # || some check on stdin/out
                    INSTALL_opts <- install_args
                    ## don't use HTML, checkRd goes over the same ground.
                    INSTALL_opts <- c(INSTALL_opts,  "--no-html")
                    if(WINDOWS)
                        INSTALL_opts <- c(INSTALL_opts,  "--no-multiarch")
                    if(install == "fake")
                        INSTALL_opts <- c(INSTALL_opts,  "--fake")
                    INSTALL_opts <- paste(INSTALL_opts, collapse = " ")
                    cmd <- if(WINDOWS) {
                        paste("Rcmd.exe INSTALL -l",
                              shQuote(libdir),
                              INSTALL_opts,
                              shQuote(shortPathName(pkgdir)))
                    } else {
                        paste(shQuote(R_EXE),
                              "CMD INSTALL -l",
                              shQuote(libdir),
                              INSTALL_opts,
                              shQuote(pkgdir))
                    }
                    if(!use_install_log) {
                        ## Case A: No redirection of stdout/stderr from
                        ## installation.
                        message("")
                        if(R_system(cmd)) {
                            errorLog(Log, "Installation failed.")
                            do_exit(1L)
                        }
                        message("")
                    } else {
                        ## Case B. All output from installation redirected,
                        ## or already available in the log file.
                        checkingLog(Log,
                                    "whether package ",
                                    sQuote(package_name),
				   " can be installed")
                        outfile <- file.path(pkgoutdir, "00install.out")
                        if(grepl("^check", install)) {
                            outfile <- substr(install, 6, 1000)
                            install <- "check"
                            lines <- readLines(outfile, warn = FALSE)
                            ## <NOTE>
                            ## We used to have
                            ## $install_error = ($lines[$#lines] !~ /^\* DONE/);
                            ## but what if there is output from do_cleanup
                            ## in (Unix) R CMD INSTALL?
                            ## </NOTE>
                            install_error <- any(grepl("^\\* DONE", lines))
                        } else {
                            ## record in the log what options were used
                            cat("* install options ", sQuote(INSTALL_opts),
                                "\n\n", sep = "", file = outfile)
                            cmd <- paste(cmd, ">>", shQuote(outfile), "2>&1")
                            install_error <- R_system(cmd)
                            if (WINDOWS) {
                                ## MS Html Help Compiler gives lines terminated
                                ## by CRCRLF, so we clean up the log file.

                                ## Still needed?
                            }
                            lines <- readLines(outfile)
                        }
                        if(install_error) {
                            Log <- warningLog(Log, "Found the following significant warnings:\n")
                            printLog(Log, paste(lines, collapse="\n"), "\n")
                            printLog(Log, "See ", sQuote(outfile),
                                     " for details.\n")
                        } else resultLog(Log, "OK")

                        ## There could still be some important warnings that
                        ## we'd like to report.  For the time being, start
                        ## with compiler warnings about non ISO C code (or
                        ## at least, what looks like it), and also include
                        ## warnings resulting from the const char * CHAR()
                        ## change in R 2.6.0.  (In theory, we should only do
                        ## this when using GCC ...)
                    } # end of case B
                }

            }

        } # end of if(!is_base_pkg)

        setwd(startdir)
        check_pkg(pkgdir, pkgoutdir, startdir, libdir, desc, Log,
                  is_base_pkg, thispkg_subdirs)
        instdir <- file.path(libdir, pkgname)
        if (do_latex) {
            setwd(pkgoutdir)
            if (dir.exists(file.path(instdir, "help")))
                check_pkg_manual(instdir, desc["Package"], Log)
            else
                check_pkg_manual(pkgdir, desc["Package"], Log)
        }

        if(Log$warnings > 0) { message(""); summaryLog(Log) }
        closeLog(Log)
        message("")
    }
}


check_pkg <- function(pkgdir, pkgoutdir, startdir, libdir, desc, Log,
                      is_base_pkg, thispkg_subdirs)
{
}

check_pkg_manual <- function(pkgdir, pkgname, Log)
{
    WINDOWS <- .Platform$OS.type == "windows"
    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
    if(!WINDOWS) R_EXE <- file.path(R.home("bin"), "R")

    ## Run LaTeX on the manual, if there are man pages
    ## If it is installed there is a 'help' dir
    ## and for a source package, there is a 'man' dir
    if(dir.exists(file.path(pkgdir, "help")) ||
       dir.exists(file.path(pkgdir, "man"))) {
        topdir <- pkgdir
        Rd2pdf_opts <- "--batch --no-preview"
        checkingLog(Log, "PDF version of manual")
        build_dir <- tempfile("Rd2pdf")
        cmd0 <- if(WINDOWS) "Rcmd.exe" else paste(shQuote(R_EXE), "CMD")
        cmd <- paste(cmd0, " Rd2pdf ", Rd2pdf_opts,
                     " --build-dir=", build_dir,
                     " --no-clean",
                     " -o ", pkgname, "-manual.pdf ",
                     topdir,
                     " > Rdlatex.log 2>&1 ",
                     sep="")
        res <- R_system(cmd)
        latex_log <- file.path(build_dir, "Rd2.log")
        if(file.exists(latex_log))
            file.copy(latex_log, paste(pkgname, "-manual.log", sep=""))
        if(res == 2816) { ## 11*256
            errorLog(Log, "Rd conversion errors:\n")
            lines <- readLines("Rdlatex.log", warn = FALSE)
            writeLines(lines)
            ## FIXME analyse
            unlink(build_dir, recursive = TRUE)
            q("no", status = 1L, runLast = FALSE)
        } else if (res > 0) {
            latex_file <- file.path(build_dir, "Rd2.tex")
            if(file.exists(latex_file))
                file.copy(latex_file, paste(pkgname, "-manual.tex", sep=""))
            Log <- warningLog(Log)
            printLog(Log, "LaTeX errors when creating PDF version.\n",
                     "This typically indicates Rd problems.\n")
            ## If possible, indicate the problems found.
            ## Note that Rd2pdf works on 'Rd2.tex'.
            if(file.exists(latex_log)) {
                lines <- .get_LaTeX_errors_from_log_file(latex_log)
                printLog(Log, "LaTeX errors found:\n")
                printLog(log, paste(c(lines, ""), sep="\n"))
            }
            unlink(build_dir, recursive = TRUE)
            checkingLog(Log, "PDF version of manual without index")
            cmd <- paste(cmd0, " Rd2pdf ", Rd2pdf_opts,
                         " --build-dir=", build_dir,
                         " --no-clean --no-index",
                         " -o ", pkgname, "-manual.pdf  > Rdlatex.log 2>&1 ",
                         topdir, sep="")
            if(R_system(cmd)) {
                errorLog(Log, "")
                if(file.exists(latex_log))
                    file.copy(latex_log, paste(pkgname, "-manual.log", sep=""))
                else {
                    ## No log file and thus no chance to find out
                    ## what went wrong.  Hence, re-run without
                    ## redirecting stdout/stderr and hope that this
                    ## gives the same problem ...
                    printLog(Log, "LaTeX error when running command:\n")
                    printLog(Log, strwrap(cmd, indent = 2, exdent = 4), "\n")
                    printLog(Log, "Re-running with no redirection of stdout/stderr.\n");
                    cmd <- paste(cmd0, " Rd2pdf ", Rd2pdf_opts,
                                 " --build-dir=", build_dir,
                                 " --no-clean --no-index",
                                 " -o ", pkgname, "-manual.pdf ",
                                 topdir, sep="")
                    R_system(cmd)
                }
                unlink(build_dir, recursive = TRUE)
                q("no", status = 1L, runLast = FALSE)
            } else {
                unlink(build_dir, recursive = TRUE)
                resultLog(Log, "OK")
            }
        } else {
            unlink(build_dir, recursive = TRUE)
            resultLog(Log, "OK")
        }
    }
}
