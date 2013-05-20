#  File src/library/tools/R/check.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

###- R based engine for R CMD check

## R developers can use this to debug the function by running it
## directly as tools:::.check_packages(args), where the args should
## be what commandArgs(TRUE) would return, that is a character vector
## of (space-delimited) terms that would be passed to R CMD checks.

## Used for INSTALL and Rd2pdf
run_Rcmd <- function(args, out = "", env = "")
{
    if(.Platform$OS.type == "windows")
        system2(file.path(R.home("bin"), "Rcmd.exe"), args, out, out)
    else
        system2(file.path(R.home("bin"), "R"), c("CMD", args), out, out,
                env = env)
}

R_runR <- function(cmd = NULL, Ropts = "", env = "",
                   stdout = TRUE, stderr = TRUE, stdin = NULL,
                   arch = "")
{
    if (.Platform$OS.type == "windows") {
        ## workaround Windows problem with input = cmd
        if (!is.null(cmd)) {
            ## In principle this should escape \
           Rin <- tempfile("Rin"); on.exit(unlink(Rin)); writeLines(cmd, Rin)
        } else Rin <- stdin
        suppressWarnings(system2(if(nzchar(arch)) file.path(R.home(), "bin", arch, "Rterm.exe")
                                 else file.path(R.home("bin"), "Rterm.exe"),
                                 c(Ropts, paste("-f", Rin)), stdout, stderr, env = env))
    } else {
        suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                 c(if(nzchar(arch)) paste0("--arch=", arch), Ropts),
                                 stdout, stderr, stdin, input = cmd, env = env))
    }
}

setRlibs <-
    function(lib0 = "", pkgdir = ".", suggests = FALSE, libdir = NULL,
             self = FALSE, self2 = TRUE, quote = FALSE)
{
    WINDOWS <- .Platform$OS.type == "windows"
    useJunctions <- WINDOWS && !nzchar(Sys.getenv("R_WIN_NO_JUNCTIONS"))
    flink <- function(from, to) {
        res <- if(WINDOWS) {
            if(useJunctions) Sys.junction(from, to)
            else file.copy(from, to, recursive = TRUE)
        } else file.symlink(from, to)
        if (!res) stop(gettextf("cannot link from %s", from), domain = NA)
    }

    pi <- .split_description(.read_description(file.path(pkgdir, "DESCRIPTION")))
    thispkg <- unname(pi$DESCRIPTION["Package"])

    ## We need to make some assumptions about layout: this version
    ## assumes .Library contains standard and recommended packages
    ## and nothing else.
    tmplib <- tempfile("RLIBS_")
    dir.create(tmplib)
    ## Since this is under the session directory and only contains
    ## symlinks and dummies (hence will be small) we never clean it up.

    test_recommended <-
        config_val_to_logical(Sys.getenv("_R_CHECK_NO_RECOMMENDED_", "FALSE"))

    if(test_recommended) {
        ## Now add dummies for recommended packages (removed later if declared)
        recommended <-  .get_standard_package_names()$recommended
        ## grDevices has :: to KernSmooth
        ## stats has ::: to Matrix, Matrix depends on lattice
        ## which gives false positives in MASS and Rcpp
        ## codetools is really part of tools
        exceptions <- "codetools"
        if (thispkg %in% c("MASS", "Rcpp"))
            exceptions <- c(exceptions, "Matrix", "lattice")
        if (thispkg %in%
            c("Modalclust", "aroma.core", "iWebPlots",
              "openair", "oce", "pcalg", "tileHMM"))
            exceptions <- c(exceptions, "KernSmooth")
        recommended <- recommended[!recommended %in% exceptions]
        for(pkg in recommended) {
            if(pkg == thispkg) next
            dir.create(pd <- file.path(tmplib, pkg))
            file.copy(file.path(.Library, pkg, "DESCRIPTION"), pd)
            ## to make sure find.package throws an error:
            close(file(file.path(pd, "dummy_for_check"), "w"))
        }
    }

    deps <- unique(c(names(pi$Depends), names(pi$Imports), names(pi$LinkingTo),
                     if(suggests) names(pi$Suggests)))
    if(length(libdir) && self2) flink(file.path(libdir, thispkg), tmplib)
    ## .Library is not necessarily canonical, but the .libPaths version is.
    lp <- .libPaths()
    poss <- c(lp[length(lp)], .Library)
    already <- thispkg
    more <- unique(deps[!deps %in% already]) # should not depend on itself ...
    while(length(more)) {
        m0 <- more; more <- character()
        for (pkg in m0) {
            if (test_recommended) {
                if (pkg %in% recommended) unlink(file.path(tmplib, pkg), TRUE)
                ## hard-code dependencies for now.
                if (pkg == "mgcv")
                    unlink(file.path(tmplib, c("Matrix", "lattice", "nlme")), TRUE)
                if (pkg == "Matrix") unlink(file.path(tmplib, "lattice"), TRUE)
                if (pkg == "class") unlink(file.path(tmplib, "MASS"), TRUE)
                if (pkg == "nlme") unlink(file.path(tmplib, "lattice"), TRUE)
            }
            where <- find.package(pkg, quiet = TRUE)
            if(length(where)) {
                if (!(dirname(where) %in% poss))
                    flink(where, tmplib)
                else if (!test_recommended)
                    # If the package is in the standard library we can
                    # assume dependencies have been met, but we can
                    # only skip the traversal if we aren't testing recommended
                    # packages, because loading will fail if there is
                    # an indirect dependency to one that has been hidden
                    # by a dummy in tmplib.
                    next
                pi <- readRDS(file.path(where, "Meta", "package.rds"))
                more <- c(more, names(pi$Depends), names(pi$Imports),
                          names(pi$LinkingTo))
            }
        }
        already <- c(already, m0)
        more <- unique(more[!more %in% already])
    }
    if (self) flink(normalizePath(pkgdir), tmplib)
    # print(dir(tmplib))
    rlibs <- tmplib
    if (nzchar(lib0)) rlibs <- c(lib0, rlibs)
    rlibs <- paste(rlibs, collapse = .Platform$path.sep)
    if(quote) rlibs <- shQuote(rlibs)
    c(paste("R_LIBS", rlibs, sep = "="),
      if(WINDOWS) " R_ENVIRON_USER='no_such_file'" else "R_ENVIRON_USER=''",
      if(WINDOWS) " R_LIBS_USER='no_such_dir'" else "R_LIBS_USER=''",
      " R_LIBS_SITE='no_such_dir'")
}

###- The main function for "R CMD check"  {currently extends all the way to the end-of-file}
.check_packages <- function(args = NULL)
{
    WINDOWS <- .Platform$OS.type == "windows"
    ## this requires on Windows: file.exe (optional)

    wrapLog <- function(...) {
        text <- paste(..., collapse = " ")
        ## strwrap expects paras separated by blank lines.
        ## Perl's wrap split on \n
        text <- strsplit(text, "\n", useBytes = TRUE)[[1L]]
        printLog(Log, paste(strwrap(text), collapse = "\n"), "\n")
    }

    ## Used for
    ## .check_packages_used
    ## .check_packages_used_in_examples
    ## .check_packages_used_in_tests
    ## .check_packages_used_in_vignettes
    ## checkS3methods
    ## checkReplaceFuns
    ## checkFF
    ## .check_code_usage_in_package (with full set)
    ## .check_T_and_F (with full set)
    ## .check_dotInternal (with full set)
    ## undoc, codoc, codocData, codocClasses
    ## checkDocFiles, checkDocStyle
    ## The default set of packages here are as they are because
    ## .get_S3_generics_as_seen_from_package needs utils,graphics,stats
    ##  Used by checkDocStyle (which needs the generic visible) and checkS3methods.
    R_runR2 <-
        if(WINDOWS) {
            function(cmd,
                     env = "R_DEFAULT_PACKAGES=utils,grDevices,graphics,stats")
                {
                    out <- R_runR(cmd, R_opts2, env)
                    ## pesky gdata ....
                    grep("^(ftype: not found|File type)", out,
                         invert = TRUE, value = TRUE)
                }
        } else
            function(cmd,
                     env = "R_DEFAULT_PACKAGES='utils,grDevices,graphics,stats'")
            {
                out <- R_runR(cmd, R_opts2, env)
                if (R_check_suppress_RandR_message)
                    grep('^Xlib: *extension "RANDR" missing on display', out,
                         invert = TRUE, value = TRUE)
                else out
            }

    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    td0 <- Inf # updated below
    print_time <- function(t1, t2, Log)
    {
        td <- t2 - t1
        if(td[3L] < td0) return()
        td2 <- if (td[3L] > 600) {
            td <- td/60
            if(WINDOWS) sprintf(" [%dm]", round(td[3L]))
            else sprintf(" [%dm/%dm]", round(sum(td[-3L])), round(td[3L]))
        } else {
            if(WINDOWS) sprintf(" [%ds]", round(td[3L]))
            else sprintf(" [%ds/%ds]", round(sum(td[-3L])), round(td[3L]))
        }
        cat(td2)
        if (!is.null(Log) && Log$con > 0L) cat(td2, file = Log$con)
    }

    parse_description_field <- function(desc, field, default=TRUE)
    {
        tmp <- desc[field]
        if (is.na(tmp)) default
        else switch(tmp,
                    "yes"=, "Yes" =, "true" =, "True" =, "TRUE" = TRUE,
                    "no" =, "No" =, "false" =, "False" =, "FALSE" = FALSE,
                    default)
    }

    check_pkg <- function(pkg, pkgname, pkgoutdir, startdir, libdir, desc,
                          is_base_pkg, is_rec_pkg, subdirs, extra_arch)
    {
        ## pkg is the argument we received from the main loop.
        ## pkgdir is the corresponding absolute path,

        checkingLog(Log, "package directory")
        setwd(startdir)
        pkg <- sub("/$", "", pkg)
        if (dir.exists(pkg)) {
            setwd(pkg) ## wrap in try()?
            pkgdir <- getwd()
            resultLog(Log, "OK")
        } else {
            errorLog(Log, "Package directory ", sQuote(pkg), "does not exist.")
            do_exit(1L)
        }

        haveR <- dir.exists("R") && !extra_arch

        if (!extra_arch) {
            check_meta()  # Check DESCRIPTION meta-information.
            check_top_level()
            check_detritus()
            check_indices()
            check_subdirectories(haveR, subdirs)
            ## Check R code for non-ASCII chars which
            ## might be syntax errors in some locales.
            if (!is_base_pkg && haveR && R_check_ascii_code) check_non_ASCII()
        } # end of !extra_arch

        ## Check we can actually load the package: base is always loaded
        if (do_install && pkgname != "base") {
            if (this_multiarch) {
                Log$stars <<-  "**"
                for (arch in inst_archs) {
                    printLog(Log, "* loading checks for arch ", sQuote(arch), "\n")
                    check_loading(arch)
                }
                Log$stars <<-  "*"
            } else {
                check_loading()
            }
        }

        if (haveR) {
            check_R_code() # unstated dependencies, S3 methods, replacement, foreign
            check_R_files(is_rec_pkg) # codetools etc
        }

        check_Rd_files(haveR)

        check_data() # 'data' dir and sysdata.rda

        if (!is_base_pkg && dir.exists("src") && !extra_arch) check_src_dir()

        if(do_install &&
           dir.exists("src") &&
           length(so_symbol_names_table)) # suitable OS
            check_sos()

        miss <- file.path("inst", "doc", c("Rplots.ps", "Rplots.pdf"))
        if (any(f <- file.exists(miss))) {
            checkingLog(Log, "for left-overs from vignette generation")
            warningLog(Log)
            printLog(Log,
                     paste("  file", paste(sQuote(miss[f]), collapse = ", "),
                           "will not be installed: please remove it\n"))
        }
        if (dir.exists("inst/doc")) {
            if (R_check_doc_sizes) check_doc_size()
            else if (as_cran)
                warningLog(Log, "'qpdf' is needed for checks on size reduction of PDFs")
        }
        if (dir.exists("inst/doc") && do_install) check_doc_contents()

        setwd(pkgoutdir)

        ## Run the examples: this will be skipped if installation was
        if (dir.exists(file.path(libdir, pkgname, "help"))) {
            run_examples()
        } else if (dir.exists(file.path(pkgdir, "man"))) {
            checkingLog(Log, "examples")
            resultLog(Log, "SKIPPED")
        }

        ## Run the package-specific tests.
        tests_dir <- file.path(pkgdir, "tests")
        if (dir.exists(tests_dir) && # trackObjs has only *.Rin
            length(dir(tests_dir, pattern = "\\.(R|Rin)$")))
            run_tests()

        ## Check package vignettes.
        setwd(pkgoutdir)
        run_vignettes(desc)

    } ## end{ check_pkg }

    check_file_names <- function()
    {
        ## Check for portable file names.
        checkingLog(Log, "for portable file names")

        ## Build list of exclude patterns.
        ignore <- get_exclude_patterns()
        ignore_file <- ".Rbuildignore"
        if (file.exists(ignore_file))
            ignore <- c(ignore, readLines(ignore_file))

        ## Ensure that the names of the files in the package are valid
        ## for at least the supported OS types.  Under Unix, we
        ## definitely cannot have '/'.  Under Windows, the control
        ## characters as well as " * : < > ? \ | (i.e., ASCII
        ## characters 1 to 31 and 34, 36, 58, 60, 62, 63, 92, and 124)
        ## are or can be invalid.  (In addition, one cannot have
        ## one-character file names consisting of just ' ', '.', or
        ## '~'.)  Based on information by Uwe Ligges, Duncan Murdoch,
        ## and Brian Ripley.

        ## In addition, Windows does not allow the following DOS type
        ## device names (by themselves or with possible extensions),
        ## see e.g.
        ## http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
        ## http://msdn.microsoft.com/en-us/library/aa365247%28VS.85%29.aspx#naming_conventions
        ## and http://en.wikipedia.org/wiki/Filename (which as of
        ## 2007-04-22 is wrong about claiming that COM0 and LPT0 are
        ## disallowed):
        ##
        ## CON: Keyboard and display
        ## PRN: System list device, usually a parallel port
        ## AUX: Auxiliary device, usually a serial port
        ## NUL: Bit-bucket device
        ## CLOCK$: System real-time clock
        ## COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8, COM9:
        ##   Serial communications ports 1-9
        ## LPT1, LPT2, LPT3, LPT4, LPT5, LPT6, LPT7, LPT8, LPT9:
        ##   parallel printer ports 1-9

        ## In addition, the names of help files get converted to HTML
        ## file names and so should be valid in URLs.  We check that
        ## they are ASCII and do not contain %, which is what is known
        ## to cause troubles.

        allfiles <- dir(".", all.files = TRUE,
                        full.names = TRUE, recursive = TRUE)
        allfiles <- c(allfiles, unique(dirname(allfiles)))
        allfiles <- af <- sub("^./", "", allfiles)
        ignore_re <- paste0("(", paste(ignore, collapse = "|"), ")")
        allfiles <- grep(ignore_re, allfiles, invert = TRUE, value = TRUE)

        bad_files <- allfiles[grepl("[[:cntrl:]\"*/:<>?\\|]",
                                    basename(allfiles))]
        is_man <- grepl("man$", dirname(allfiles))
        bad <- sapply(strsplit(basename(allfiles[is_man]), ""),
                      function(x) any(grepl("[^ -~]|%", x)))
        if (length(bad))
            bad_files <- c(bad_files, (allfiles[is_man])[bad])
        bad <- tolower(basename(allfiles))
        ## remove any extension(s) (see 'Writing R Extensions')
        bad <- sub("[.].*", "", bad)
        bad <- grepl("^(con|prn|aux|clock[$]|nul|lpt[1-9]|com[1-9])$", bad)
        bad_files <- c(bad_files, allfiles[bad])
        if (nb <- length(bad_files)) {
            errorLog(Log)
            msg <- ngettext(nb,
                            "Found the following file with a non-portable file name:\n",
                            "Found the following files with non-portable file names:\n",
                            domain = NA)
            wrapLog(msg)
            printLog(Log, .format_lines_with_indent(bad_files), "\n")
            wrapLog("These are not valid file names",
                    "on all R platforms.\n",
                    "Please rename the files and try again.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            do_exit(1L)
        }

        ## Next check for name clashes on case-insensitive file systems
        ## (that is on Windows and (by default) on OS X).

        dups <- unique(allfiles[duplicated(tolower(allfiles))])
        if (nb <- length(dups)) {
            errorLog(Log)
            wrapLog("Found the following files with duplicate lower-cased file names:\n")
            printLog(Log, .format_lines_with_indent(dups), "\n")
            wrapLog("File names must not differ just by case",
                    "to be usable on all R platforms.\n",
                    "Please rename the files and try again.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            do_exit(1L)
        }

        ## NB: the omission of ' ' is deliberate.
        non_ASCII_files <-
            allfiles[grepl("[^-A-Za-z0-9._!#$%&+,;=@^(){}\'[\\]]", #
                           basename(allfiles), perl = TRUE)]
        any <- FALSE
        if (nb <- length(non_ASCII_files)) {
            any <- TRUE
            warningLog(Log)
            msg <- ngettext(nb,
                            "Found the following file with a non-portable file name:\n",
                            "Found the following files with non-portable file names:\n",
                            domain = NA)
            wrapLog(msg)
            printLog(Log, .format_lines_with_indent(non_ASCII_files), "\n")
            wrapLog("These are not fully portable file names.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
        }

        ## now check lengths, as tarballs can only record up to 100 bytes
        ## plus perhaps 155 bytes as a prefix plus /
        af <- file.path(pkgname, af)
        lens <- nchar(af, "b")
        if (any(lens > 100L)) {
            bad_files <- af[lens > 100L]
            OK <- TRUE
            if (any(lens > 256L)) OK <- FALSE
            else { # check if can be splt
                for (f in bad_files) {
                    name <- charToRaw(f)
                    s <- max(which(name[1:155] == charToRaw("/")))
                    if(is.infinite(s) || s+100 < length(name)) {
                        OK <- FALSE; break
                    }
                }
                if (!OK) errorLog(Log)
                else if(!any) {
                    noteLog(Log)
                    any <- TRUE
                }
            }
            msg <- ngettext(length(bad_files),
                            "Found the following non-portable file path:\n",
                            "Found the following non-portable file paths:\n",
                            domain = NA)
            wrapLog(msg)
            printLog(Log, .format_lines_with_indent(bad_files), "\n\n")
            wrapLog("Tarballs are only required to store paths of up to 100",
                    "bytes and cannot store those of more than 256 bytes,",
                    "with restrictions including to 100 bytes for the",
                    "final component.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            if (!OK) do_exit(1L)
        }
        if (!any) resultLog(Log, "OK")

        allfiles
    }

    check_permissions <- function(allfiles)
    {
        checkingLog(Log, "for sufficient/correct file permissions")

        ## This used to be much more 'aggressive', requiring that dirs
        ## and files have mode >= 00755 and 00644, respectively (with
        ## an error if not), and that files know to be 'text' have
        ## mode 00644 (with a warning if not).  We now only require
        ## that dirs and files have mode >= 00700 and 00400,
        ## respectively, and try to fix insufficient permission in the
        ## INSTALL code (Unix only).
        ##
        ## In addition, we check whether files 'configure' and
        ## 'cleanup' exists in the top-level directory but are not
        ## executable, which is most likely not what was intended.

        ## Phase A.  Directories at least 700, files at least 400.
        bad_files <- character()
        ##                 allfiles <- dir(".", all.files = TRUE,
        ##                                 full.names = TRUE, recursive = TRUE)
        ##                 allfiles <- sub("^./", "", allfiles)
        if(length(allfiles)) {
            mode <- file.info(allfiles)$mode
            bad_files <- allfiles[(mode & "400") < as.octmode("400")]
        }
        if(length(alldirs <- unique(dirname(allfiles)))) {
            mode <- file.info(alldirs)$mode
            bad_files <- c(bad_files,
                           alldirs[(mode & "700") < as.octmode("700")])
        }
        if (length(bad_files)) {
            errorLog(Log)
            wrapLog("Found the following files with insufficient permissions:\n")
            printLog(Log, .format_lines_with_indent(bad_files), "\n")
            wrapLog("Permissions should be at least 700 for directories and 400 for files.\nPlease fix permissions and try again.\n")
            do_exit(1L)
        }

        ## Phase B.  Top-level scripts 'configure' and 'cleanup'
        ## should really be mode at least 500, or they will not be
        ## necessarily be used (or should we rather change *that*?)
        bad_files <- character()
        for (f in c("configure", "cleanup")) {
            if (!file.exists(f)) next
            mode <- file.info(f)$mode
            if ((mode & "500") < as.octmode("500"))
                bad_files <- c(bad_files, f)
        }
        if (length(bad_files)) {
            warningLog(Log)
            wrapLog("The following files should most likely be executable (for the owner):\n")
            printLog(Log, .format_lines_with_indent(bad_files), "\n")
            printLog(Log, "Please fix their permissions\n")
        } else resultLog(Log, "OK")
    }

    check_meta <- function()
    {
        ## If we just installed the package (via R CMD INSTALL), we already
        ## validated most of the package DESCRIPTION metadata.  Otherwise,
        ## let us be defensive about this ...

        checkingLog(Log, "DESCRIPTION meta-information")
        dfile <- if (is_base_pkg) "DESCRIPTION.in" else "DESCRIPTION"
        ## FIXME: this does not need to be run in another process
        Rcmd <- sprintf("tools:::.check_package_description(\"%s\")", dfile)
        out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            errorLog(Log)
            printLog(Log, paste(out, collapse = "\n"), "\n")
            do_exit(1L)
        }
        any <- FALSE
        ## Check the encoding.
        Rcmd <- sprintf("tools:::.check_package_description_encoding(\"%s\")", dfile)
        out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            warningLog(Log)
            any <- TRUE
            printLog(Log, paste(out, collapse = "\n"), "\n")
        }

        ## Check the license.
        ## For base packages, the DESCRIPTION.in files have non-canonical
        ##   License: Part of R @VERSION@
        ## entries because these really are a part of R: hence, skip the
        ## check.
        check_license <- if (!is_base_pkg) {
            Check_license <- Sys.getenv("_R_CHECK_LICENSE_", NA)
            if(is.na(Check_license)) {
                ## The check code conditionalizes *output* on _R_CHECK_LICENSE_.
                Sys.setenv('_R_CHECK_LICENSE_' = "TRUE")
                TRUE
            } else config_val_to_logical(Check_license)
        } else FALSE
        if (!identical(check_license, FALSE)) {
            Rcmd <- sprintf("tools:::.check_package_license(\"%s\", \"%s\")",
                            dfile, pkgdir)
            ## FIXME: this does not need to be run in another process
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if (check_license == "maybe") {
                    if (!any) warningLog(Log)
                } else if (any(grepl("^(Standardizable: FALSE|Invalid license file pointers:)",
                                     out))) {
                    if (!any) warningLog(Log)
                } else {
                    if (!any) noteLog(Log)
                }
                any <- TRUE
                printLog(Log, paste(out, collapse = "\n"), "\n")
            }
        }

        ## .check_package_description() only checks Authors@R "if needed",
        ## and does not check for persons with no valid roles.
        db <- .read_description(dfile)
        if(!is.na(aar <- db["Authors@R"])) {
            out <- .check_package_description_authors_at_R_field(aar,
                                                                 strict = TRUE)
            if(length(out)) {
                if(!any) noteLog(Log)
                any <- TRUE
                out <- .format_check_package_description_authors_at_R_field_results(out)
                printLog(Log, paste(out, collapse = "\n"), "\n")
            }
        }

        if (!any) resultLog(Log, "OK")
    }

    check_top_level <- function()
    {
        checkingLog(Log, "top-level files")
        topfiles <- Sys.glob(c("install.R", "R_PROFILE.R"))
        any <- FALSE
        if (length(topfiles)) {
            any <- TRUE
            warningLog(Log)
            printLog(Log, .format_lines_with_indent(topfiles), "\n")
            wrapLog("These files are defunct.",
                    "See manual 'Writing R Extensions'.\n")
        }
        topfiles <- Sys.glob(c("LICENCE", "LICENSE"))
        if (length(topfiles)) {
            ## Are these mentioned in DESCRIPTION?
            lic <- desc["License"]
            if(!is.na(lic)) {
                found <- sapply(topfiles,
                                function(x) grepl(x, lic, fixed = TRUE))
                topfiles <- topfiles[!found]
                if (length(topfiles)) {
                    any <- TRUE
                    noteLog(Log)
                    one <- (length(topfiles) == 1L)
                    msg <- c(if(one) "File" else "Files",
                             "\n",
                             .format_lines_with_indent(topfiles),
                             "\n",
                             if(one) {
                                 "is not mentioned in the DESCRIPTION file.\n"
                             } else {
                                 "are not mentioned in the DESCRIPTION file.\n"
                             })
                    printLog(Log, msg)
                }
            }
        }
        if (!any) resultLog(Log, "OK")
    }

    check_detritus <- function()
    {
        checkingLog(Log, "for left-over files")
        files <- dir(".", full.names = TRUE, recursive = TRUE)
        bad <- grep("svn-commit[.].*tmp$", files, value = TRUE)
        bad <- c(bad, grep("^[.]/[^/]*[.][rR]d$", files, value = TRUE))
        if (length(bad)) {
            bad <- sub("^[.]/", paste0(pkgname, "/"), bad)
            noteLog(Log)
            printLog(Log,
                     "The following files look like leftovers:\n",
                     paste(strwrap(paste(sQuote(bad), collapse = ", "),
                                   indent = 2, exdent = 2), collapse = "\n"),
                     "\nPlease remove them from your package.\n")
        } else resultLog(Log, "OK")
    }


    check_indices <- function()
    {
        ## Check index information.
        checkingLog(Log, "index information")
        any <- FALSE
        if (file.exists("INDEX") &&
            !length(readLines("INDEX", warn = FALSE))) {
            any <- TRUE
            warningLog(Log, "Empty file 'INDEX'.")
        }
        if (dir.exists("demo")) {
            index <- file.path("demo", "00Index")
            if (!file.exists(index) ||
                !length(readLines(index, warn = FALSE))) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         sprintf("Empty or missing file %s.\n",
                                 sQuote(index)))
            } else {
                Rcmd <- "options(warn=1)\ntools:::.check_demo_index(\"demo\")\n"
                ## FIXME: this does not need to be run in another process
                out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
                if(length(out)) {
                    if(!any) warningLog(Log)
                    any <- TRUE
                    printLog(Log, paste(c(out, ""), collapse = "\n"))
                }
            }
        }
        if (dir.exists(file.path("inst", "doc"))) {
            Rcmd <- "options(warn=1)\ntools:::.check_vignette_index(\"inst/doc\")\n"
            ## FIXME: this does not need to be run in another process
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log, paste(c(out, ""), collapse = "\n"))
            }
        }
        if (any)
            wrapLog("See the information on INDEX files and package",
                    "subdirectories in the chapter 'Creating R packages'",
                    "of the 'Writing R Extensions' manual.\n")
        else  resultLog(Log, "OK")
    }

    check_subdirectories <- function(haveR, subdirs)
    {
        checkingLog(Log, "package subdirectories")
        any <- FALSE
        if (haveR && !length(list_files_with_type("R", "code")) &&
            !file.exists(file.path("R", "sysdata.rda"))) {
            haveR <- FALSE
            warningLog(Log, "Found directory 'R' with no source files.")
            any <- TRUE
        }
        if (R_check_subdirs_nocase) {
            ## Argh.  We often get submissions where 'R' comes out as 'r',
            ## or 'man' comes out as 'MAN', and we've just ran into 'DATA'
            ## instead of 'data' (2007-03-31).  Maybe we should warn about
            ## this unconditionally ...
            ## <FIXME>
            ## Actually, what we should really do is check whether there is
            ## any directory with lower-cased name matching a lower-cased
            ## name of a standard directory, while differing in name.
            ## </FIXME>

            ## Watch out for case-insensitive file systems
            if ("./r" %in% list.dirs(recursive = FALSE)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Found subdirectory 'r'.\n",
                         "Most likely, this should be 'R'.\n")
            }
            if ("./MAN" %in% list.dirs(recursive = FALSE)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Found subdirectory 'MAN'.\n",
                         "Most likely, this should be 'man'.\n")
            }
            if ("./DATA" %in% list.dirs(recursive = FALSE)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Found subdirectory 'DATA'.\n",
                         "Most likely, this should be 'data'.\n")
            }
        }

        all_dirs <- list.dirs(".")

        ## several packages have had check dirs in the sources, e.g.
        ## ./languageR/languageR.Rcheck
        ## ./locfdr/man/locfdr.Rcheck
        ## ./clustvarsel/inst/doc/clustvarsel.Rcheck
        ## ./bicreduc/OldFiles/bicreduc.Rcheck
        ## ./waved/man/waved.Rcheck
        ## ./waved/..Rcheck
        ind <- grepl("\\.Rcheck$", all_dirs)
        if(any(ind)) {
            if(!any) warningLog(Log)
            any <- TRUE
            msg <- ngettext(sum(ind),
                            "Found the following directory with the name of a check directory:\n",
                            "Found the following directories with names of check directories:\n", domain = NA)
            printLog(Log, msg,
                     .format_lines_with_indent(all_dirs[ind]),
                     "\n",
                     "Most likely, these were included erroneously.\n")
        }

        ## Several packages had leftover Rd2dvi build directories in
        ## their sources
        ind <- grepl("^\\.Rd2(dvi|pdf)", basename(all_dirs))
        if(any(ind)) {
            if(!any) warningLog(Log)
            any <- TRUE
            msg <- ngettext(sum(ind),
                            "Found the following directory with the name of a Rd2pdf directory:\n",
                            "Found the following directories with names of Rd2pdf directories:\n", domain = NA)
           printLog(Log, msg,
                     .format_lines_with_indent(all_dirs[ind]),
                     "\n",
                     "Most likely, these were included erroneously.\n")
        }


        if(!is_base_pkg && (istar || R_check_vc_dirs)) {
            ## Packages also should not contain version control subdirs
            ## provided that we check a .tar.gz or know we unpacked one.
            ind <- basename(all_dirs) %in% .vc_dir_names
            if(any(ind)) {
                if(!any) warningLog(Log)
                any <- TRUE
            msg <- ngettext(sum(ind),
                            "Found the following directory with the name of a version control directory:\n",
                            "Found the following directories with names of version control directories:\n", domain = NA)
                printLog(Log, msg,
                         .format_lines_with_indent(all_dirs[ind]),
                         "\n",
                         "These should not be in a package tarball.\n")
            }
        }

        if (subdirs != "no") {
            Rcmd <- "tools:::.check_package_subdirs(\".\")\n"
            ## We don't run this in the C locale, as we only require
            ## certain filenames to start with ASCII letters/digits, and not
            ## to be entirely ASCII.
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("Please remove or rename the files.\n",
                        "See section 'Package subdirectories'",
                        "in the 'Writing R Extensions' manual.\n")
            }
        }

        ## Subdirectory 'data' without data sets?
        if (dir.exists("data") &&
            !length(list_files_with_type("data", "data"))) {
            if (!any) warningLog(Log)
            any <- TRUE
            printLog(Log, "Subdirectory 'data' contains no data sets.\n")
       }
        ## Subdirectory 'demo' without demos?

        if (dir.exists("demo")) {
            demos <- list_files_with_type("demo", "demo")
            if(!length(demos)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Subdirectory 'demo' contains no demos.\n")
            } else {
                ## check for non-ASCII code in each demo
                bad <- character()
                for(d in demos) {
                    x <- readLines(d, warn = FALSE)
                    asc <- iconv(x, "latin1", "ASCII")
                    ind <- is.na(asc) | asc != x
                    if (any(ind)) bad <- c(bad, basename(d))
                }
                if (length(bad)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    printLog(Log, "Demos with non-ASCII characters:")
                    if(length(bad) > 1L)
                        printLog(Log, "\n",
                                 .format_lines_with_indent(bad), "\n")
                    else printLog(Log, "  ", bad, "\n")
                    wrapLog("Portable packages must use only ASCII",
                            "characters in their demos.\n",
                            "Use \\uxxxx escapes for other characters.\n")
                    demos <- demos[! basename(demos) %in% bad]
                }
                ## check we can parse each demo.
                bad <- character()
                for(d in demos)
                    tryCatch(parse(file = d),
                             error = function(e) bad <<- c(bad, basename(d)))
                if (length(bad)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    printLog(Log, "Demos which do not contain valid R code:")
                    if(length(bad) > 1L)
                        printLog(Log, "\n",
                                 .format_lines_with_indent(bad), "\n")
                    else printLog(Log, "  ", bad, "\n")
               }
            }
        }

        ## Subdirectory 'exec' without files?
        if (dir.exists("exec") && !length(dir("exec"))) {
            if (!any) warningLog(Log)
            any <- TRUE
            printLog(Log, "Subdirectory 'exec' contains no files.\n")
        }

        ## Subdirectory 'inst' without files?
        if (dir.exists("inst") && !length(dir("inst", recursive = TRUE))) {
            if (!any) warningLog(Log)
            any <- TRUE
            printLog(Log, "Subdirectory 'inst' contains no files.\n")
        }

        ## Subdirectory 'src' without sources?
        if (dir.exists("src")) {
            ## <NOTE>
            ## If there is a Makefile (or a Makefile.win), we cannot assume
            ## that source files have the predefined extensions.
            ## </NOTE>
            if (!any(file.exists(file.path("src",
                                           c("Makefile", "Makefile.win"))))) {
                if (!length(dir("src", pattern = "\\.([cfmM]|cc|cpp|f90|f95|mm)"))) {
                    if (!any) warningLog(Log)
                    printLog(Log, "Subdirectory 'src' contains no source files.\n")
                    any <- TRUE
                }
            }
        }

        ## Do subdirectories of 'inst' interfere with R package system
        ## subdirectories?
        if (dir.exists("inst")) {
            ## These include pre-2.10.0 ones
            R_system_subdirs <-
                c("Meta", "R", "data", "demo", "exec", "libs",
                  "man", "help", "html", "latex", "R-ex")
            allfiles <- dir("inst", full.names = TRUE)
            alldirs <- allfiles[file.info(allfiles)$isdir]
            suspect <- basename(alldirs) %in% R_system_subdirs
            if (any(suspect)) {
                ## check they are non-empty
                suspect <- alldirs[suspect]
                suspect <- suspect[sapply(suspect, function(x) {
                    length(dir(x, all.files = TRUE)) > 2L
                })]
                if (length(suspect)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    wrapLog("Found the following non-empty",
                            "subdirectories of 'inst' also",
                            "used by R:\n")
                    printLog(Log, paste(c(suspect, ""), collapse = "\n"))
                    wrapLog("It is recommended not to interfere",
                            "with package subdirectories used by R.\n")
                }
            }
        }

        ## Valid NEWS.Rd?
        nfile <- file.path("inst", "NEWS.Rd")
        if(file.exists(nfile)) {
            ## Catch all warning and error messages.
            ## We use the same construction in at least another place,
            ## so maybe factor out a common utility function
            ##   .try_catch_all_warnings_and_errors
            ## eventually.
            ## For testing package NEWS.Rd files, we really need a real
            ## QC check function eventually ...
            .warnings <- NULL
            .error <- NULL
            withCallingHandlers(tryCatch(.build_news_db_from_package_NEWS_Rd(nfile),
                                         error = function(e)
                                         .error <<- conditionMessage(e)),
                                warning = function(e) {
                                    .warnings <<- c(.warnings,
                                                    conditionMessage(e))
                                    invokeRestart("muffleWarning")
                                })
            msg <- c(.warnings, .error)
            if(length(msg)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Problems with news in 'inst/NEWS.Rd':\n")
                printLog(Log,
                         paste("  ",
                               unlist(strsplit(msg, "\n", fixed = TRUE)),
                               sep = "", collapse = "\n"),
                         "\n")
            }
        }

        ## Valid CITATION metadata?
        if (file.exists(file.path("inst", "CITATION"))) {
            Rcmd <- "tools:::.check_citation(\"inst/CITATION\")\n"
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=utils")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "Invalid citation information in 'inst/CITATION':\n")
                printLog(Log, .format_lines_with_indent(out), "\n")
            }
        }

        ## CITATION files in non-standard places?
        ## Common problems: rather than inst/CITATION, have
        ##   CITATION
        ##   CITATION.txt
        ##   inst/doc/CITATION
        ## Of course, everything in inst is justifiable, so only give a
        ## note for now.
        files <- dir(".", pattern = "^CITATION.*", recursive = TRUE)
        files <- files[file_path_sans_ext(basename(files)) == "CITATION" &
                       files != file.path("inst", "CITATION")]
        if(length(files)) {
            if(!any) noteLog(Log)
            any <- TRUE
            msg <- ngettext(length(files),
                            "Found the following CITATION file in a non-standard place:\n",
                            "Found the following CITATION files in a non-standard place:\n", domain = NA)
            wrapLog(msg)
            printLog(Log, .format_lines_with_indent(files), "\n")
            wrapLog("Most likely 'inst/CITATION' should be used instead.\n")
        }

        if(!any) resultLog(Log, "OK")
    }

    check_non_ASCII <- function()
    {
        checkingLog(Log, "R files for non-ASCII characters")
        out <- R_runR("tools:::.check_package_ASCII_code('.')",
                      R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            warningLog(Log)
            msg <- ngettext(length(out),
                            "Found the following file with non-ASCII characters:\n",
                            "Found the following files with non-ASCII characters:\n",
                            domain = NA)
            wrapLog(msg)
            printLog(Log, .format_lines_with_indent(out), "\n")
            wrapLog("Portable packages must use only ASCII",
                    "characters in their R code,\n",
                    "except perhaps in comments.\n",
                    "Use \\uxxxx escapes for other characters.\n")
        } else resultLog(Log, "OK")

        checkingLog(Log, "R files for syntax errors")
        Rcmd  <- "options(warn=1);tools:::.check_package_code_syntax(\"R\")"
        out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (any(grepl("^Error", out))) {
            errorLog(Log)
            printLog(Log, paste(c(out, ""), collapse = "\n"))
            do_exit(1L)
        } else if (length(out)) {
            warningLog(Log)
            printLog(Log, paste(c(out, ""), collapse = "\n"))
        } else resultLog(Log, "OK")
    }

    check_R_code <- function()
    {
        if (!is_base_pkg) {
            checkingLog(Log, "for unstated dependencies in R code")
            if (do_install) {
                Rcmd <- paste("options(warn=1, showErrorCalls=FALSE)\n",
                              sprintf("tools:::.check_packages_used(package = \"%s\")\n", pkgname))

                out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
                if (length(out)) {
                    warningLog(Log)
                    printLog(Log, paste(c(out, ""), collapse = "\n"))
                    wrapLog(msg_DESCRIPTION)
                } else resultLog(Log, "OK")
            } else {
                ## this needs to read the package code, and will fail on
                ## syntax errors such as non-ASCII code.
                Rcmd <- paste("options(warn=1, showErrorCalls=FALSE)\n",
                              sprintf("tools:::.check_packages_used(dir = \"%s\")\n", pkgdir))

                out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
                if (length(out)) {
                    warningLog(Log)
                    printLog(Log, paste(c(out, ""), collapse = "\n"))
                    wrapLog(msg_DESCRIPTION)
                } else resultLog(Log, "OK")
            }
        }

        ## Check whether methods have all arguments of the corresponding
        ## generic.
        checkingLog(Log, "S3 generic/method consistency")
        Rcmd <- paste("options(warn=1)\n",
                      "options(expressions=1000)\n",
                      if (do_install)
                      sprintf("tools::checkS3methods(package = \"%s\")\n", pkgname)
                      else
                      sprintf("tools::checkS3methods(dir = \"%s\")\n", pkgdir))
        out <- R_runR2(Rcmd)
        if (length(out)) {
            warningLog(Log)
            printLog(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("See section 'Generic functions and methods'",
                    "of the 'Writing R Extensions' manual.\n")
        } else resultLog(Log, "OK")

        ## Check whether replacement functions have their final argument
        ## named 'value'.
        checkingLog(Log, "replacement functions")
        Rcmd <- paste("options(warn=1)\n",
                      if (do_install)
                      sprintf("tools::checkReplaceFuns(package = \"%s\")\n", pkgname)
                      else
                      sprintf("tools::checkReplaceFuns(dir = \"%s\")\n", pkgdir))
        out <- R_runR2(Rcmd)
        if (length(out)) {
            ## <NOTE>
            ## We really want to stop if we find offending replacement
            ## functions.  But we cannot use error() because output may
            ## contain warnings ...
            warningLog(Log)
            ## </NOTE>
            printLog(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("The argument of a replacement function",
                    "which corresponds to the right hand side",
                    "must be named 'value'.\n")
        } else resultLog(Log, "OK")

        ## Check foreign function calls.
        ## The neverending story ...
        ## For the time being, allow to turn this off by setting the environment
        ## variable _R_CHECK_FF_CALLS_ to an empty value.
        if (nzchar(Sys.getenv("_R_CHECK_FF_CALLS_", "true"))) {
            checkingLog(Log, "foreign function calls")
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools::checkFF(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools::checkFF(dir = \"%s\")\n", pkgdir))
            out <- R_runR2(Rcmd)
            if (length(out)) {
                if(any(grepl("^Foreign function calls? with(out| empty)", out)) ||
                   (!is_base_pkg && any(grepl("in a base package:", out))) ||
                   any(grepl("^Undeclared packages? in", out))
                   ) warningLog(Log)
                else noteLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                if(!is_base_pkg && any(grepl("in a base package:", out)))
                    wrapLog("Packages should not make .C/.Call/.Fortran",
                            "calls to base packages.",
                            "They are not part of the API,",
                            "for use only by R itself",
                            "and subject to change without notice.")
                else
                    wrapLog("See the chapter 'System and foreign language interfaces' of the 'Writing R Extensions' manual.\n")
            } else resultLog(Log, "OK")
        }
    }

    check_R_files <- function(is_rec_pkg)
    {
        checkingLog(Log, "R code for possible problems")
        if (!is_base_pkg) {
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_code_shlib(dir = \"%s\")\n",
                                  pkgdir))
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                errorLog(Log)
                wrapLog("Incorrect (un)loading of package",
                        "shared object.\n")
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("The system-specific extension for",
                        "shared objects must not be added.\n",
                        "See ?library.dynam.\n")
                do_exit(1L)
            }
        }

        Rcmd <- paste("options(warn=1)\n",
                      sprintf("tools:::.check_package_code_startup_functions(dir = \"%s\")\n",
                              pkgdir))
        out1 <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        Rcmd <- paste("options(warn=1)\n",
                      sprintf("tools:::.check_package_code_unload_functions(dir = \"%s\")\n",
                              pkgdir))
        out1a <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        out1 <- if (length(out1) && length(out1a)) c(out1, "", out1a)
                else c(out1, out1a)

        out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- NULL

        if (!is_base_pkg && R_check_unsafe_calls) {
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_code_tampers(dir = \"%s\")\n",
                                  pkgdir))
            out2 <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        }

        if (R_check_use_codetools && do_install) {
            Rcmd <-
                paste("options(warn=1)\n",
                      sprintf("tools:::.check_code_usage_in_package(package = \"%s\")\n", pkgname))
            out3 <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
        }

        if(!is_base_pkg && R_check_use_codetools && R_check_dot_internal) {
            details <- pkgname != "relax" # has .Internal in a 10,000 line fun
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                              sprintf("tools:::.check_dotInternal(package = \"%s\",details=%s)\n", pkgname, details)
                          else
                              sprintf("tools:::.check_dotInternal(dir = \"%s\",details=%s)\n", pkgdir, details))
            out4 <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
            ## Hmisc, gooJSON, quantmod give spurious output
            if (!any(grepl("^Found.* .Internal call", out4))) out4 <- NULL
        }

        if(!is_base_pkg && R_check_code_assign_to_globalenv) {
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_code_assign_to_globalenv(dir = \"%s\")\n",
                                  pkgdir))
            out5 <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }

        if(!is_base_pkg && R_check_code_attach) {
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_code_attach(dir = \"%s\")\n",
                                  pkgdir))
            out6 <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }
        if(!is_base_pkg && R_check_code_data_into_globalenv) {
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_code_data_into_globalenv(dir = \"%s\")\n",
                                  pkgdir))
            out7 <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }


        ## Use of deprecated and defunct?
        if(!is_base_pkg && R_check_use_codetools && R_check_depr_def) {
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                              sprintf("tools:::.check_depdef(package = \"%s\")\n", pkgname)
                          else
                              sprintf("tools:::.check_depdef(dir = \"%s\")\n", pkgdir))
            out8 <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
        }
        if (length(out1) || length(out2) || length(out3) ||
            length(out4) || length(out5) || length(out6) ||
            length(out7) || length(out8)) {
            ini <- character()
            if(length(out4) ||
               length(grep("^Found the defunct/removed function", out8)))
                warningLog(Log) else noteLog(Log)
            if (length(out4)) {
                first <- grep("^Found.* .Internal call", out4)[1L]
                if(first > 1L) out4 <- out4[-seq_len(first-1)]
                printLog0(Log, paste(c(ini, out4, "", ""), collapse = "\n"))
                wrapLog(c("Packages should not call .Internal():",
                          "it is not part of the API,",
                          "for use only by R itself",
                          "and subject to change without notice."))
                ini <- ""
            }
            if (length(out8)) {
                printLog0(Log, paste(c(ini, out8, ""), collapse = "\n"))
                ini <- ""
            }
            ## All remaining checks give notes and not warnings.
            if(length(ini))
                ini <- c("",
                         "In addition to the above warning(s), found the following notes:",
                         "")

            if (length(out1)) {
                printLog0(Log, paste(c(ini, out1, ""), collapse = "\n"))
                ini <- ""
            }
            if (length(out2)) {
                printLog0(Log,
                          paste(c(ini,
                                  "Found the following possibly unsafe calls:",
                                  out2, ""),
                                collapse = "\n"))
                ini <- ""
            }
            if (length(out3)) {
                printLog0(Log, paste(c(ini, out3, ""), collapse = "\n"))
                ini <- ""
            }
            if (length(out5)) {
                printLog0(Log, paste(c(ini, out5, ""), collapse = "\n"))
                ini <- ""
            }
            if (length(out6)) {
                printLog0(Log, paste(c(ini, out6, ""), collapse = "\n"))
                ini <- ""
                wrapLog(gettextf("See section %s in '%s'.",
                                 sQuote("Good practice"), "?attach"))
           }
            if (length(out7)) {
                printLog0(Log, paste(c(ini, out7, ""), collapse = "\n"))
                ini <- ""
                wrapLog(gettextf("See section %s in '%s'.",
                                 sQuote("Good practice"), "?data"))
            }
        } else resultLog(Log, "OK")
    }

    check_Rd_files <- function(haveR)
    {
        msg_writing_Rd <-
            c("See the chapter 'Writing R documentation files'",
              "in the 'Writing R Extensions' manual.\n")

        if (dir.exists("man") && !extra_arch) {
            checkingLog(Log, "Rd files")
            minlevel <- Sys.getenv("_R_CHECK_RD_CHECKRD_MINLEVEL_", "-1")
            Rcmd <- paste("options(warn=1)\n",
                          sprintf("tools:::.check_package_parseRd('.', minlevel=%s)\n", minlevel))
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if(length(grep("^prepare.*Dropping empty section", out,
                               invert = TRUE)))
                    warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")

            checkingLog(Log, "Rd metadata")
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_metadata(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_metadata(dir = \"%s\")\n", pkgdir))
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check cross-references in R documentation files.

        ## <NOTE>
        ## Installing a package warns about missing links (and hence R CMD
        ## check knows about this too provided an install log is used).
        ## However, under Windows the install-time check verifies the links
        ## against what is available in the default library, which might be
        ## considerably more than what can be assumed to be available.
        ##
        ## The formulations in section "Cross-references" of R-exts are not
        ## quite clear about this, but CRAN policy has for a long time
        ## enforced anchoring links to targets (aliases) from non-base
        ## packages.
        ## </NOTE>

        if (dir.exists("man") && R_check_Rd_xrefs) {
            checkingLog(Log, "Rd cross-references")
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_xrefs(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_xrefs(dir = \"%s\")\n", pkgdir))
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if (!all(grepl("Package[s]? unavailable to check", out)))
                    warningLog(Log)
                else noteLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check for missing documentation entries.
        if (!extra_arch && (haveR || dir.exists("data"))) {
            checkingLog(Log, "for missing documentation entries")
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools::undoc(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools::undoc(dir = \"%s\")\n", pkgdir))
            ## This is needed to pick up undocumented S4 classes.
            ## even for packages which only import methods.
            ## But as that check needs to run get() on all the lazy-loaded
            ## promises, avoid if possible.
            ## desc exists in the body of this function.
            use_methods <- if(pkgname == "methods") TRUE else {
                pi <- .split_description(desc)
                "methods" %in% c(names(pi$Depends), names(pi$Imports))
            }
            out <- if (use_methods) {
                env <- if(WINDOWS) "R_DEFAULT_PACKAGES=utils,grDevices,graphics,stats,methods" else "R_DEFAULT_PACKAGES='utils,grDevices,graphics,stats,methods'"
                R_runR2(Rcmd, env = env)
            } else R_runR2(Rcmd)
            ## Grr, get() in undoc can change the search path
            ## Current example is TeachingDemos
            out <- grep("^Loading required package:", out,
                        invert = TRUE, value = TRUE)
            err <- grep("^Error", out)
            if (length(err)) {
                errorLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                do_exit(1L)
            } else if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("All user-level objects",
                        "in a package",
                        if (any(grepl("^Undocumented S4", out)))
                        "(including S4 classes and methods)",
                        "should have documentation entries.\n")
                wrapLog(msg_writing_Rd)
            } else resultLog(Log, "OK")
        }

        ## Check for code/documentation mismatches.
        if (dir.exists("man") && !extra_arch) {
            checkingLog(Log, "for code/documentation mismatches")
            if (!do_codoc) resultLog(Log, "SKIPPED")
            else {
                any <- FALSE
                ## Check for code/documentation mismatches in functions.
                if (haveR) {
                    Rcmd <- paste("options(warn=1)\n",
                                  if (do_install)
                                  sprintf("tools::codoc(package = \"%s\")\n", pkgname)
                                  else
                                  sprintf("tools::codoc(dir = \"%s\")\n", pkgdir))
                    out <- R_runR2(Rcmd)
                    if (length(out)) {
                        any <- TRUE
                        warningLog(Log)
                        printLog0(Log, paste(c(out, ""), collapse = "\n"))
                    }
                }

                ## Check for code/documentation mismatches in data sets.
                if (do_install) {
                    Rcmd <- paste("options(warn=1)\n",
                                  sprintf("tools::codocData(package = \"%s\")\n", pkgname))
                    out <- R_runR2(Rcmd)
                    if (length(out)) {
                        if (!any) warningLog(Log)
                        any <- TRUE
                        printLog0(Log, paste(c(out, ""), collapse = "\n"))
                    }
                }

                ## Check for code/documentation mismatches in S4 classes.
                if (do_install && haveR) {
                    Rcmd <- paste("options(warn=1)\n",
                                  sprintf("tools::codocClasses(package = \"%s\")\n", pkgname))
                    out <- R_runR2(Rcmd)
                    if (length(out)) {
                        if (!any) warningLog(Log)
                        any <- TRUE
                        printLog0(Log, paste(c(out, ""), collapse = "\n"))
                    }
                }

                if (!any) resultLog(Log, "OK")
            }
        }

        ## Check Rd files, for consistency of \usage with \arguments (are
        ## all arguments shown in \usage documented in \arguments?) and
        ## aliases (do all functions shown in \usage have an alias?)
        if (dir.exists("man") && !extra_arch) {
            checkingLog(Log, "Rd \\usage sections")

            msg_doc_files <-
                c("Functions with \\usage entries",
                  "need to have the appropriate \\alias entries,",
                  "and all their arguments documented.\n",
                  "The \\usage entries must correspond to syntactically",
                  "valid R code.\n")
            any <- FALSE
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools::checkDocFiles(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools::checkDocFiles(dir = \"%s\")\n", pkgdir))
            out <- R_runR2(Rcmd)
            if (length(out)) {
                any <- TRUE
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog(msg_doc_files)
                wrapLog(msg_writing_Rd)
            }

            if (R_check_Rd_style && haveR) {
                msg_doc_style <-
                    c("The \\usage entries for S3 methods should use",
                      "the \\method markup and not their full name.\n")

                Rcmd <- paste("options(warn=1)\n",
                              if (do_install)
                              sprintf("tools::checkDocStyle(package = \"%s\")\n", pkgname)
                              else
                              sprintf("tools::checkDocStyle(dir = \"%s\")\n", pkgdir))
                out <- R_runR2(Rcmd)
                if (length(out)) {
                    if (!any) noteLog(Log)
                    any <- TRUE
                    printLog0(Log, paste(c(out, ""), collapse = "\n"))
                    wrapLog(msg_doc_style)
                    wrapLog(msg_writing_Rd)
                }
            }

            if (!any) resultLog(Log, "OK")
        }

        ## Check Rd contents
        if (dir.exists("man") && R_check_Rd_contents && !extra_arch) {
            checkingLog(Log, "Rd contents")
            Rcmd <- paste("options(warn=1)\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_contents(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_contents(dir = \"%s\")\n", pkgdir))
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check undeclared dependencies in examples (if any)
        if (dir.exists("man") && do_install && !extra_arch && !is_base_pkg) {
            checkingLog(Log, "for unstated dependencies in examples")
            Rcmd <- paste("options(warn=1, showErrorCalls=FALSE)\n",
                          sprintf("tools:::.check_packages_used_in_examples(package = \"%s\")\n", pkgname))

            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                # wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")
        } ## FIXME, what if no install?
    }

    check_data <- function()
    {
        ## Check contents of 'data'
        if (!is_base_pkg && dir.exists("data")) {
            checkingLog(Log, "contents of 'data' directory")
            fi <- list.files("data")
            if (!any(grepl("\\.[Rr]$", fi))) { # code files can do anything
                dataFiles <- basename(list_files_with_type("data", "data"))
                odd <- fi[! fi %in% c(dataFiles, "datalist")]
                if (length(odd)) {
                    warningLog(Log)
                    msg <- c("Files not of a type allowed in a 'data' directory:\n",
                             paste0(.pretty_format(odd), "\n"),
                             "Please use e.g. 'inst/extdata' for non-R data files\n")
                    printLog(Log, msg)
                } else resultLog(Log, "OK")
            } else resultLog(Log, "OK")
        }

        ## Check for non-ASCII characters in 'data'
        if (!is_base_pkg && R_check_ascii_data && dir.exists("data")) {
            checkingLog(Log, "data for non-ASCII characters")
            out <- R_runR("tools:::.check_package_datasets('.')", R_opts2)
            out <- grep("Loading required package", out,
                        invert = TRUE, value = TRUE)
            out <- grep("Warning: changing locked binding", out,
                        invert = TRUE, value = TRUE, fixed = TRUE)
           if (length(out)) {
                bad <- grep("^Warning:", out)
                if (length(bad)) warningLog(Log) else noteLog(Log)
                printLog0(Log, .format_lines_with_indent(out), "\n")
            } else resultLog(Log, "OK")
        }

        ## Check for ASCII and uncompressed/unoptimized saves in 'data'
        if (!is_base_pkg && R_check_compact_data && dir.exists("data")) {
            checkingLog(Log, "data for ASCII and uncompressed saves")
            out <- R_runR("tools:::.check_package_compact_datasets('.', TRUE)",
                          R_opts2)
            out <- grep("Warning: changing locked binding", out,
                        invert = TRUE, value = TRUE, fixed = TRUE)
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, .format_lines_with_indent(out), "\n")
            } else resultLog(Log, "OK")
        }

        ## Check for ASCII and uncompressed/unoptimized saves in 'sysdata':
        ## no base package has this
        if (R_check_compact_data && file.exists(file.path("R", "sysdata.rda"))) {
            checkingLog(Log, "R/sysdata.rda")
            out <- R_runR("tools:::.check_package_compact_sysdata('.', TRUE)",
                          R_opts2)
            if (length(out)) {
                bad <- grep("^Warning:", out)
                if (length(bad)) warningLog(Log) else noteLog(Log)
                printLog0(Log, .format_lines_with_indent(out), "\n")
            } else resultLog(Log, "OK")
        }
   }

    check_doc_contents <- function()
    {
        ## Have already checked that inst/doc exists
        doc_dir <- file.path(libdir, pkgname, "doc")
        if (!dir.exists(doc_dir)) return()
        checkingLog(Log, "installed files from 'inst/doc'")
        ## special case common problems.
        any <- FALSE
        files <- dir(file.path(pkgdir, "inst", "doc"))
        already <- c("jss.cls", "jss.bst", "Rd.sty", "Sweave.sty")
        bad <- files[files %in% already]
        if (length(bad)) {
            noteLog(Log)
            any <- TRUE
            printLog(Log,
                     "The following files are already in R: ",
                     paste(sQuote(bad), collapse = ", "), "\n",
                     "Please remove them from your package.\n")
        }
        files2 <- dir(file.path(pkgdir, "inst", "doc"), recursive = TRUE,
                     pattern = "[.](cls|sty|drv)$", full.names = TRUE)
        ## Skip Rnews.sty and RJournal.sty for now
        files2 <- files2[! basename(files2) %in%
                       c("jss.cls", "jss.drv", "Rnews.sty", "RJournal.sty")]
        bad <- character()
        for(f in files2) {
            pat <- "%% (This generated file may be distributed as long as the|original source files, as listed above, are part of the|same distribution.)"
            if(length(grep(pat, readLines(f, warn = FALSE), useBytes = TRUE))
               == 3L) bad <- c(bad, basename(f))
        }
        if (length(bad)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog(Log,
                     "The following files contain a license that requires\n",
                     "distribution of original sources:\n",
                     "  ", paste(sQuote(bad), collapse = ", "), "\n",
                     "Please ensure that you have complied with it.\n")
        }

        ## Now look for TeX leftovers (and soiltexture, Amelia ...).
        bad <- grepl("[.](log|aux|bbl|blg|dvi|toc|out|Rd|Rout|dbj|drv|ins)$",
                     files, ignore.case = TRUE)
        if (any(bad)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog(Log,
                     "The following files look like leftovers/mistakes:\n",
                     paste(strwrap(paste(sQuote(files[bad]), collapse = ", "),
                                   indent = 2, exdent = 2), collapse = "\n"),
                     "\nPlease remove them from your package.\n")
        }

        files <- dir(doc_dir)
        files <- files[! files %in% already]
        bad <- grepl("[.](tex|lyx|png|jpg|jpeg|gif|ico|bst|cls|sty|ps|eps|img)$",
                     files, ignore.case = TRUE)
        bad <- bad | grepl("(Makefile|~$)", files)
        ## How about any pdf files which look like figures files from vignettes?
        vigns <- pkgVignettes(dir = pkgdir)
        if (!is.null(vigns) && length(vigns$docs)) {
            vf <- vigns$names
            pat <- paste(vf, collapse="|")
            pat <- paste0("^(", pat, ")-[0-9]+[.]pdf")
            bad <- bad | grepl(pat, files)
        }
        bad <- bad | grepl("^fig.*[.]pdf$", files)
        badf <- files[bad]
        dirs <- basename(list.dirs(doc_dir, recursive = FALSE))
        badd <- dirs[dirs %in% c("auto", "Bilder", "fig", "figs", "figures",
                                 "Figures", "img", "images", "JSSstyle",
                                 "jssStyle", "screenshots2", "src", "tex", "tmp")]
        if (length(c(badf, badd))) {
            if(!any) noteLog(Log)
            any <- TRUE
            if(length(badf))
                printLog(Log,
                         "The following files should probably not be installed:\n",
                         paste(strwrap(paste(sQuote(badf), collapse = ", "),
                                       indent = 2, exdent = 2), collapse = "\n"),
                         "\n")
            if(length(badd))
                printLog(Log,
                         "The following directories should probably not be installed:\n",
                         paste(strwrap(paste(sQuote(badd), collapse = ", "),
                                       indent = 2, exdent = 2), collapse = "\n"),
                         "\n")
            printLog(Log, "\nConsider the use of a .Rinstignore file: see ",
                     sQuote("Writing R Extensions"), ",\n",
                     "or move the vignette sources from ",
                     sQuote("inst/doc"), " to ", sQuote("vignettes"), ".\n")
        }
        if (!any) resultLog(Log, "OK")
    }

    check_doc_size <- function()
    {
        ## Have already checked that inst/doc exists and qpdf can be found
        pdfs <- dir('inst/doc', pattern="\\.pdf",
                    recursive = TRUE, full.names = TRUE)
        pdfs <- setdiff(pdfs, "inst/doc/Rplots.pdf")
        if (length(pdfs)) {
            checkingLog(Log, "sizes of PDF files under 'inst/doc'")
            any <- FALSE
            td <- tempfile('pdf')
            dir.create(td)
            file.copy(pdfs, td)
            res <- compactPDF(td, gs_quality = "none") # use qpdf
            res <- format(res, diff = 1e5)
            if(length(res)) {
                noteLog(Log)
                any <- TRUE
                printLog(Log,
                         "  'qpdf' made some significant size reductions:\n",
                         paste("  ", res, collapse = "\n"),
                         "\n",
                         "  consider running tools::compactPDF() on these files\n")
            }
            if (R_check_doc_sizes2) {
                gs_cmd <- find_gs_cmd(Sys.getenv("R_GSCMD", ""))
                if (nzchar(gs_cmd)) {
                    res <- compactPDF(td, gs_cmd = gs_cmd, gs_quality = "ebook")
                    res <- format(res, diff = 2.5e5) # 250 KB for now
                    if(length(res)) {
                        if (!any) warningLog(Log)
                        any <- TRUE
                        printLog(Log,
                                 "  'gs+qpdf' made some significant size reductions:\n",
                                 paste("  ", res, collapse = "\n"),
                                 "\n",
                                 '  consider running tools::compactPDF(gs_quality = "ebook") on these files\n')
                    }
                } else {
                    if (!any) noteLog(Log)
                    any <- TRUE
                    printLog(Log, "Unable to find GhostScript executable to run checks on size reduction\n")
                }

            }
            if (!any) resultLog(Log, "OK")
        }
    }

    check_src_dir <- function()
    {
        ## Check C/C++/Fortran sources/headers for CRLF line endings.
        ## <FIXME>
        ## Does ISO C really require LF line endings?  (Reference?)
        ## We know that some versions of Solaris cc and f77/f95
        ## will not accept CRLF or CR line endings.
        ## (Sun Studio 12 definitely objects to CR in both C and Fortran).
        ## </FIXME>
        checkingLog(Log, "line endings in C/C++/Fortran sources/headers")
        ## pattern is "([cfh]|cc|cpp)"
        files <- dir("src", pattern = "\\.([cfh]|cc|cpp)$",
                     full.names = TRUE, recursive = TRUE)
        ## exclude dirs starting src/win, e.g for tiff
        files <- grep("^src/[Ww]in", files, invert = TRUE, value = TRUE)
        bad_files <- character()
        for(f in files) {
            contents <- readChar(f, file.info(f)$size, useBytes = TRUE)
            if (grepl("\r", contents, fixed = TRUE, useBytes = TRUE))
                bad_files <- c(bad_files, f)
        }
        if (length(bad_files)) {
            warningLog(Log, "Found the following sources/headers with CR or CRLF line endings:")
            printLog(Log, .format_lines_with_indent(bad_files), "\n")
            printLog(Log, "Some Unix compilers require LF line endings.\n")
        } else resultLog(Log, "OK")

        ## Check src/Make* for LF line endings, as Sun make does not accept CRLF
        checkingLog(Log, "line endings in Makefiles")
        bad_files <- character()
        ## .win files are not checked, as CR/CRLF work there
        all_files <-
            dir("src",
                pattern = "^(Makevars|Makevars.in|Makefile|Makefile.in)$",
                full.names = TRUE, recursive = TRUE)
        for(f in all_files) {
            if (!file.exists(f)) next
            contents <- readChar(f, file.info(f)$size, useBytes = TRUE)
            if (grepl("\r", contents, fixed = TRUE, useBytes = TRUE))
                bad_files <- c(bad_files, f)
        }
        if (length(bad_files)) {
            warningLog(Log, "Found the following Makefiles with CR or CRLF line endings:")
            printLog(Log, .format_lines_with_indent(bad_files), "\n")
            printLog(Log, "Some Unix 'make' programs require LF line endings.\n")
        } else resultLog(Log, "OK")

        ## Check src/Makevars[.in] for portable compilation flags.
        if (any(file.exists(file.path("src", c("Makevars", "Makevars.in")))) ) {
            checkingLog(Log, "for portable compilation flags in Makevars")
            Rcmd <- "tools:::.check_make_vars(\"src\")\n"
            out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## check src/Makevar*, src/Makefile* for correct use of BLAS_LIBS
        ## FLIBS is not needed on Windows, at least currently (as it is
        ## statically linked).
        checkingLog(Log, "for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS)")
        makefiles <- Sys.glob(file.path("src",
                                        c("Makevars", "Makevars.in",
                                          "Makefile", "Makefile.win")))
        any <- FALSE
        for (f in makefiles) {
            lines <- readLines(f, warn = FALSE)
            ## Combine lines ending in escaped newlines.
            if(any(ind <- grepl("[\\]$", lines, useBytes = TRUE))) {
                ## Eliminate escape.
                lines[ind] <-
                    sub("[\\]$", "", lines[ind], useBytes = TRUE)
                ## Determine ids of blocks that need to be joined.
                ind <- seq_along(ind) - c(0, cumsum(ind)[-length(ind)])
                ## And join.
                lines <- unlist(lapply(split(lines, ind), paste,
                                       collapse = " "))
            }
            c1 <- grepl("^[[:space:]]*PKG_LIBS", lines, useBytes = TRUE)
            c2l <- grepl("\\$[{(]{0,1}LAPACK_LIBS", lines, useBytes = TRUE)
            c2b <- grepl("\\$[{(]{0,1}BLAS_LIBS", lines, useBytes = TRUE)
            c3 <- grepl("\\$[{(]{0,1}FLIBS", lines, useBytes = TRUE)
            if (any(c1 & c2l & !c2b)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "  apparently using $(LAPACK_LIBS) without $(BLAS_LIBS) in ",
                         sQuote(f), "\n")
            }
            if (any(c1 & (c2b | c2l) & !c3)) {
                if (!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "  apparently PKG_LIBS is missing $(FLIBS) in ",
                         sQuote(f), "\n")
            }
        }
        if (!any) resultLog(Log, "OK")
    }

    check_sos <- function() {
        checkingLog(Log, "compiled code")
        ## from sotools.R
        Rcmd <- paste("options(warn=1)\n",
                      sprintf("tools:::check_compiled_code(\"%s\")",
                              file.path(libdir, pkgname)))
        out <- R_runR(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if(length(out) == 1L && grepl("^Note:", out)) {
            ## This will be a note about symbols.rds not being available
            if(!is_base_pkg) {
                noteLog(Log)
                printLog0(Log, c(out, "\n"))
            } else resultLog(Log, "OK")
        } else if(length(out)) {
            ## If we have named objects then we have symbols.rds and
            ## will not be picking up symbols just in system libraries.
            haveObjs <- any(grepl("^ *Object", out))
            if(haveObjs && any(grepl("(abort|assert|exit)", out)) &&
               !pkgname %in% c("multicore", "parallel")) # these need to call exit
                warningLog(Log)
            else noteLog(Log)
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            nAPIs <- length(grep("Found non-API", out))
            nBad <- length(grep(", possibly from ", out))
            msg <- if (nBad) {
                if(haveObjs)
                    c("Compiled code should not call entry points which",
                      "might terminate R nor write to stdout/stderr instead",
                      "of to the console.\n")
                else
                    c("Compiled code should not call entry points which",
                      "might terminate R nor write to stdout/stderr instead",
                      "of to the console.  The detected symbols are linked",
                      "into the code but might come from libraries",
                      "and not actually be called.\n")
            } else character()
            if(nAPIs)
                msg <- c(msg,
                         "Compiled code should not call non-API entry points in R.\n")
            wrapLog("\n", paste(msg, collapse = " "), "\n",
                    "See 'Writing portable packages'",
                    "in the 'Writing R Extensions' manual.\n")
        } else resultLog(Log, "OK")
    }

    check_loading <- function(arch = "")
    {
        checkingLog(Log, "whether the package can be loaded")
        Rcmd <- sprintf("library(%s)", pkgname)
        opts <- if(nzchar(arch)) R_opts4 else R_opts2
        env <- "R_DEFAULT_PACKAGES=NULL"
        env1 <- if(nzchar(arch)) env0 else character()
        out <- R_runR(Rcmd, opts, env1, arch = arch)
        if(length(st <- attr(out, "status"))) {
            errorLog(Log)
            wrapLog("Loading this package had a fatal error",
                    "status code ", st,  "\n")
            if(length(out))
                printLog(Log, paste(c("Loading log:", out, ""),
                                    collapse = "\n"))
            do_exit()
        }
        if (any(grepl("^Error", out))) {
            errorLog(Log)
            printLog(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("\nIt looks like this package",
                    "has a loading problem: see the messages",
                    "for details.\n")
            do_exit()
        } else resultLog(Log, "OK")

        checkingLog(Log, "whether the package can be loaded with stated dependencies")
        out <- R_runR(Rcmd, opts, c(env, env1), arch = arch)
        if (any(grepl("^Error", out)) || length(attr(out, "status"))) {
            warningLog(Log)
            printLog(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("\nIt looks like this package",
                    "(or one of its dependent packages)",
                    "has an unstated dependence on a standard",
                    "package.  All dependencies must be",
                    "declared in DESCRIPTION.\n")
            wrapLog(msg_DESCRIPTION)
        } else resultLog(Log, "OK")

        checkingLog(Log, "whether the package can be unloaded cleanly")
        Rcmd <- sprintf("suppressMessages(library(%s)); cat('\n---- unloading\n'); detach(\"package:%s\")", pkgname, pkgname)
        out <- R_runR(Rcmd, opts, c(env, env1), arch = arch)
        if (any(grepl("^(Error|\\.Last\\.lib failed)", out)) ||
            length(attr(out, "status"))) {
            warningLog(Log)
            ll <- grep("---- unloading", out)
            if(length(ll)) {
                ll <- ll[length(ll)]
                out <- out[ll:length(out)]
            }
            printLog(Log, paste(c(out, ""), collapse = "\n"))
        } else resultLog(Log, "OK")

        ## and if it has a namespace, that we can load/unload just
        ## the namespace
        if (file.exists(file.path(pkgdir, "NAMESPACE"))) {
            checkingLog(Log, "whether the namespace can be loaded with stated dependencies")
            Rcmd <- sprintf("loadNamespace(\"%s\")", pkgname)
            out <- R_runR(Rcmd, opts, c(env, env1), arch = arch)
            if (any(grepl("^Error", out)) || length(attr(out, "status"))) {
                warningLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("\nA namespace must be able to be loaded",
                        "with just the base namespace loaded:",
                        "otherwise if the namespace gets loaded by a",
                        "saved object, the session will be unable",
                        "to start.\n\n",
                        "Probably some imports need to be declared",
                        "in the NAMESPACE file.\n")
            } else resultLog(Log, "OK")

            checkingLog(Log,
                        "whether the namespace can be unloaded cleanly")
            Rcmd <- sprintf("invisible(suppressMessages(loadNamespace(\"%s\"))); cat('\n---- unloading\n'); unloadNamespace(\"%s\")",
                            pkgname, pkgname)
            out <- if (is_base_pkg && pkgname != "stats4")
                R_runR(Rcmd, opts, "R_DEFAULT_PACKAGES=NULL", arch = arch)
            else R_runR(Rcmd, opts, env1)
            if (any(grepl("^(Error|\\.onUnload failed)", out)) ||
                length(attr(out, "status"))) {
                warningLog(Log)
                ll <- grep("---- unloading", out)
                if(length(ll)) {
                    ll <- ll[length(ll)]
                    out <- out[ll:length(out)]
                }
                printLog(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## No point in this test if already installed in .Library
        if (!pkgname %in% dir(.Library)) {
            checkingLog(Log, "loading without being on the library search path")
            Rcmd <- sprintf("library(%s, lib.loc = '%s')", pkgname, libdir)
            opts <- if(nzchar(arch)) R_opts4 else R_opts2
            env <- setRlibs(pkgdir = pkgdir, libdir = libdir,
                            self2 = FALSE, quote = TRUE)
            if(nzchar(arch)) env <- c(env, "R_DEFAULT_PACKAGES=NULL")
            out <- R_runR(Rcmd, opts, env, arch = arch)
            if (any(grepl("^Error", out))) {
                warningLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("\nIt looks like this package",
                        "has a loading problem when not on .libPaths:",
                        "see the messages for details.\n")
            } else resultLog(Log, "OK")
        }
    }

    run_examples <- function()
    {
        run_one_arch <- function(exfile, exout, arch = "")
        {
            Ropts <- if (nzchar(arch)) R_opts3 else R_opts
            if (use_valgrind) Ropts <- paste(Ropts, "-d valgrind")
            t1 <- proc.time()
            ## might be diff-ing results against tests/Examples later
            ## so force LANGUAGE=en
            status <- R_runR(NULL, c(Ropts, enc),
                             c("LANGUAGE=en", "_R_CHECK_INTERNALS2_=1",
                               if(nzchar(arch)) env0,
                               jitstr, elibs),
                             stdout = exout, stderr = exout,
                             stdin = exfile, arch = arch)
            t2 <- proc.time()
            if (status) {
                errorLog(Log, "Running examples in ",
                         sQuote(basename(exfile)),
                         " failed")
                ## Try to spot the offending example right away.
                txt <- paste(readLines(exout, warn = FALSE),
                             collapse = "\n")
                ## Look for the header section anchored by a
                ## subsequent call to flush(): needs to be kept in
                ## sync with the code in massageExamples (in
                ## testing.R).  Should perhaps also be more
                ## defensive about the prompt ...
                chunks <- strsplit(txt,
                                   "> ### \\* [^\n]+\n> \n> flush[^\n]+\n> \n", useBytes = TRUE)[[1L]]
                                       if((ll <- length(chunks)) >= 2) {
                                           printLog(Log,
                                                    "The error most likely occurred in:\n\n")
                                           printLog0(Log, chunks[ll], "\n")
                                       } else {
                                           ## most likely error before the first example
                                           ## so show all the output.
                                           printLog(Log, "The error occurred in:\n\n")
                                           printLog0(Log, txt, "\n")
                                       }
                return(FALSE)
            }

            print_time(t1, t2, Log)
            ## Look at the output from running the examples.  For
            ## the time being, report warnings about use of
            ## deprecated , as the next release will make
            ## them defunct and hence using them an error.
            any <- FALSE
            lines <- readLines(exout, warn = FALSE)
            bad_lines <- grep("^Warning: .*is deprecated.$", lines,
                              useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                any <- TRUE
                warningLog(Log, "Found the following significant warnings:\n")
                printLog(Log, .format_lines_with_indent(bad_lines), "\n")
                wrapLog("Deprecated functions may be defunct as",
                        "soon as of the next release of R.\n",
                        "See ?Deprecated.\n")
            }
            if (!any) resultLog(Log, "OK")

            ## Try to compare results from running the examples to
            ## a saved previous version.
            exsave <- file.path(pkgdir, "tests", "Examples",
                                paste0(pkgname, "-Ex.Rout.save"))
            if (file.exists(exsave)) {
                checkingLog(Log, "differences from ",
                            sQuote(basename(exout)),
                            " to ", sQuote(basename(exsave)))
                cmd <- paste0("invisible(tools::Rdiff('",
                              exout, "', '", exsave, "',TRUE,TRUE))")
                out <- R_runR(cmd, R_opts2)
                if(length(out))
                    printLog0(Log, paste(c("", out, ""), collapse = "\n"))
                resultLog(Log, "OK")
            }
            if (do_timings) {
                tfile <- paste0(pkgname, "-Ex.timings")
                times <- read.table(tfile, header = TRUE, row.names = 1L, colClasses = c("character", rep("numeric", 3)))
                o <- order(times[[1]]+times[[2]], decreasing = TRUE)
                times <- times[o, ]
                keep <- (times[[1]] + times[[2]] > 5) | (times[[3]] > 5)
                if(any(keep)) {
                    printLog(Log, "Examples with CPU or elapsed time > 5s\n")
                    times <- capture.output(format(times[keep, ]))
                    printLog(Log, paste(times, collapse = "\n"), "\n")
                }
            }
            TRUE
        }

        checkingLog(Log, "examples")
        if (!do_examples) resultLog(Log, "SKIPPED")
        else {
            pkgtopdir <- file.path(libdir, pkgname)
            cmd <- sprintf('tools:::.createExdotR("%s", "%s", silent = TRUE, use_gct = %s, addTiming = %s)', pkgname, pkgtopdir, use_gct, do_timings)
            Rout <- tempfile("Rout")
            ## any arch will do here
            status <- R_runR(cmd, R_opts2, "LC_ALL=C",
                             stdout = Rout, stderr = Rout)
            if (status) {
                errorLog(Log,
                         paste("Running massageExamples to create",
                               sQuote(exfile), "failed"))
                printLog(Log, paste(readLines(Rout, warn = FALSE),
                                    collapse = "\n"), "\n")
                do_exit(1L)
            }
            ## It ran, but did it create any examples?
            exfile <- paste0(pkgname, "-Ex.R")
            if (file.exists(exfile)) {
                enc <- if (!is.na(e <- desc["Encoding"])) {
                    if (is_ascii)
                        warningLog(Log,
                                   paste("checking a package with encoding ",
                                         sQuote(e), " in an ASCII locale\n"))
                    paste("--encoding", e, sep="=")
                } else ""
                if (!this_multiarch) {
                    exout <- paste0(pkgname, "-Ex.Rout")
                    if(!run_one_arch(exfile, exout)) do_exit(1L)
                } else {
                    printLog(Log, "\n")
                    Log$stars <<-  "**"
                    res <- TRUE
                    for (arch in inst_archs) {
                        printLog(Log, "** running examples for arch ",
                                 sQuote(arch), " ...")
                        if (arch %in% R_check_skip_examples_arch) {
                            resultLog(Log, "SKIPPED")
                        } else {
                            tdir <- paste0("examples_", arch)
                            dir.create(tdir)
                            if (!dir.exists(tdir)) {
                                errorLog(Log,
                                         "unable to create examples directory")
                                do_exit(1L)
                            }
                            od <- setwd(tdir)
                            exout <- paste0(pkgname, "-Ex_", arch, ".Rout")
                            res <- res & run_one_arch(file.path("..", exfile),
                                                      file.path("..", exout),
                                                      arch)
                            setwd(od)
                        }
                    }
                    Log$stars <<-  "*"
                    if (!res) do_exit(1L)
                }
            } else resultLog(Log, "NONE")
        }
    }

    run_tests <- function()
    {
        if (!extra_arch && !is_base_pkg) {
            checkingLog(Log, "for unstated dependencies in tests")
            Rcmd <- paste("options(warn=1, showErrorCalls=FALSE)\n",
                          sprintf("tools:::.check_packages_used_in_tests(\"%s\")\n", pkgdir))

            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                # wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")
        }

        checkingLog(Log, "tests")
        run_one_arch <- function(arch = "")
        {
            testsrcdir <- file.path(pkgdir, "tests")
            testdir <- file.path(pkgoutdir, "tests")
            if(nzchar(arch)) testdir <- paste(testdir, arch, sep = "_")
            if(!dir.exists(testdir)) dir.create(testdir, mode = "0755")
            if(!dir.exists(testdir)) {
                errorLog(Log,
                         sprintf("unable to create %s", sQuote(testdir)))
                do_exit(1L)
            }
            file.copy(Sys.glob(paste0(testsrcdir, "/*")),
                      testdir, recursive = TRUE)
            setwd(testdir)
            extra <- character()
            if (use_gct) extra <- c(extra, "use_gct = TRUE")
            if (use_valgrind) extra <- c(extra, "use_valgrind = TRUE")
            tf <- gsub("\\", "/", tempfile(), fixed=TRUE)
            extra <- c(extra, paste0('Log="', tf, '"'))
            ## might be diff-ing results against tests/*.R.out.save
            ## so force LANGUAGE=en
            cmd <- paste0("tools:::.runPackageTestsR(",
                         paste(extra, collapse = ", "), ")")
            t1 <- proc.time()
            status <- R_runR(cmd,
                             if(nzchar(arch)) R_opts4 else R_opts2,
                             env = c("LANGUAGE=en",
                             "_R_CHECK_INTERNALS2_=1",
                             if(nzchar(arch)) env0,
                             jitstr, elibs),
                             stdout = "", stderr = "", arch = arch)
            t2 <- proc.time()
            if (status) {
                errorLog(Log)
                ## Don't just fail: try to log where the problem occurred.
                ## First, find the test which failed.
                ## (Maybe there was an error without a failing test.)
                bad_files <- dir(".", pattern="\\.Rout\\.fail")
                if (length(bad_files)) {
                    ## Read in output from the (first) failed test
                    ## and retain at most the last 13 lines
                    ## (13? why not?).
                    file <- bad_files[1L]
                    lines <- readLines(file, warn = FALSE)
                    file <- file.path("tests", sub("out\\.fail", "", file))
                    ll <- length(lines)
                    lines <- lines[max(1, ll-12):ll]
                    if (R_check_suppress_RandR_message)
                        lines <- grep('^Xlib: *extension "RANDR" missing on display',
                                      lines, invert = TRUE, value = TRUE)
                    printLog(Log, sprintf("Running the tests in %s failed.\n", sQuote(file)))
                    printLog(Log, "Last 13 lines of output:\n")
                    printLog0(Log, .format_lines_with_indent(lines), "\n")
                }
                return(FALSE)
            } else {
                print_time(t1, t2, Log)
                resultLog(Log, "OK")
                if (Log$con > 0L && file.exists(tf)) {
                    ## write results only to 00check.log
                    lines <- readLines(tf, warn = FALSE)
                    cat(lines, sep="\n", file = Log$con)
                    unlink(tf)
                }
            }
            setwd(pkgoutdir)
            TRUE
        }
        if (do_install && do_tests) {
            if (!this_multiarch) {
                res <- run_one_arch()
            } else {
                printLog(Log, "\n")
                res <- TRUE
                for (arch in inst_archs)
                    if (!(arch %in% R_check_skip_tests_arch)) {
                        printLog(Log, "** running tests for arch ", sQuote(arch))
                        res <- res & run_one_arch(arch)
                    }
            }
            if (!res) do_exit(1L)
        } else resultLog(Log, "SKIPPED")
    }

    run_vignettes <- function(desc)
    {
        vigns <- pkgVignettes(dir = pkgdir)
        if (is.null(vigns) || !length(vigns$docs)) return()

        if(do_install && !spec_install && !is_base_pkg && !extra_arch) {
            ## fake installs don't install inst/doc
            checkingLog(Log, "for unstated dependencies in vignettes")
            Rcmd <- paste("options(warn=1, showErrorCalls=FALSE)\n",
                          sprintf("tools:::.check_packages_used_in_vignettes(package = \"%s\")\n", pkgname))
            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                noteLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        checkingLog(Log, "package vignettes in ", sQuote("inst/doc"))
        any <- FALSE
        ## Do PDFs or HTML files exist for all package vignettes?
        ## A base source package need not have PDFs to avoid
        ## frequently-changing binary files in the SVN archive.
        if (!is_base_pkg) {
            dir <- file.path(pkgdir, "inst", "doc")
            outputs <- character(length(vigns$docs))
            for (i in seq_along(vigns$docs)) {
                file <- vigns$docs[i]
                name <- vigns$names[i]
                engine <- vignetteEngine(vigns$engines[i])
                outputs[i] <- tryCatch({
                    find_vignette_product(name, what="weave", final=TRUE, dir=dir, engine = engine)
                }, error = function(ex) NA)
            }
            bad_vignettes <- vigns$docs[is.na(outputs)]
            if (nb <- length(bad_vignettes)) {
                any <- TRUE
                warningLog(Log)
                msg <- ngettext(nb,
                                "Package vignette without corresponding PDF/HTML:\n",
                                "Package vignettes without corresponding PDF/HTML:\n", domain = NA)
                printLog(Log, msg)
                printLog(Log,
                         paste(c(paste("  ",
                                       sQuote(basename(bad_vignettes))),
                                 "", ""), collapse = "\n"))
            }
            encs <- vapply(vigns$docs, getVignetteEncoding, "")
            bad_vignettes <- vigns$docs[encs == "non-ASCII"]
            if(nb <- length(bad_vignettes)) {
                if(!any) warningLog(Log)
                any <- TRUE
                msg <- ngettext(nb,
                         "Non-ASCII package vignette without specified encoding:\n",
                         "Non-ASCII package vignettes without specified encoding:\n", domain = NA)
                printLog(Log, "  ", msg)
                printLog(Log,
                         paste(c(paste("  ",
                                       sQuote(basename(bad_vignettes))),
                                 "", ""), collapse = "\n"))
            }
        }

# FIXME:  we should do this check in build, not here.  Currently not doing it at all.
#        ## Do any of the .R files which will be generated
#        ## exist in inst/doc?  If so the latter will be ignored,
#        sources <-
#            basename(list_files_with_exts(file.path(pkgdir, "inst/doc"), "R"))
#        custom <- !is.na(desc["VignetteBuilder"])
#        if (length(sources) && !custom) {
#            new_sources <- paste0(vigns$names, ".R")
#            dups <- sources[sources %in% new_sources]
#            if(nb <- length(dups)) {
#                if(!any) warningLog(Log)
#                any <- TRUE
#                msg <- ngettext(nb,
#                                "Unused file in 'inst/doc' which is pointless or misleading",
#                                "Unused files in 'inst/doc' which are pointless or misleading", domain = NA)
#                printLog(Log, "  ",
#                         paste(msg,
#                               "  as they will be re-created from the vignettes:", "",
#                               sep = "\n"))
#                printLog(Log,
#                         paste(c(paste("  ", dups), "", ""),
#                               collapse = "\n"))
#            }
#        }
        ## avoid case-insensitive matching
        if ("makefile" %in% dir(vigns$dir)) {
            if(!any) warningLog(Log)
            any <- TRUE
            printLog(Log,
                     "  Found 'inst/doc/makefile': should be 'Makefile' and will be ignored\n")
        }
        if ("Makefile" %in% dir(vigns$dir)) {
            f <- file.path(vigns$dir, "Makefile")
            lines <- readLines(f, warn = FALSE)
            ## remove comment lines
            lines <- grep("^[[:space:]]*#", lines, invert = TRUE, value = TRUE)
            if(any(grepl("[^/]R +CMD", lines))) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "  Found 'R CMD' in Makefile: should be '\"$(R_HOME)/bin/R\" CMD'\n")
            }
            contents <- readChar(f, file.info(f)$size, useBytes = TRUE)
            if(any(grepl("\r", contents, fixed = TRUE, useBytes = TRUE))) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log, "Found Makefile with CR or CRLF line endings:\n")
                printLog(Log, "some Unix 'make' programs require LF line endings.\n")
           }
            if(any(grepl("[^/]Rscript", lines))) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "  Found 'Rscript' in Makefile: should be '\"$(R_HOME)/bin/Rscript\"'\n")
            }
        }

        ## If the vignettes declare an encoding, are they actually in it?
        ## (We don't check the .tex, though)
        bad_vignettes <- character()
        for (v in vigns$docs) {
            enc <- getVignetteEncoding(v, TRUE)
            if (enc %in% c("", "non-ASCII", "unknown")) next
            lines <- readLines(v, warn = FALSE) # some miss final NA
            lines2 <- iconv(lines, enc, "UTF-16LE", toRaw = TRUE)
            if(any(vapply(lines2, is.null, TRUE)))
                bad_vignettes <- c(bad_vignettes, v)
            if(nb <- length(bad_vignettes)) {
                if(!any) warningLog(Log)
                any <- TRUE
                msg <- ngettext(nb,
                                "Package vignette which is not in its specified encoding:\n",
                                "Package vignettes which are not in their specified encoding:\n", domain = NA)
                printLog(Log, "  ", msg)
                printLog(Log,
                         paste(c(paste("  ",
                                       sQuote(basename(bad_vignettes))),
                                 "", ""), collapse = "\n"))
            }
        }

        if (!any) resultLog(Log, "OK")

        if (do_install && do_vignettes) {
            ## Can we run the code in the vignettes?
            ## Should checking the vignettes assume the system default
            ## packages, or just base?
            ## FIXME: should we do this for multiple sub-archs?

            checkingLog(Log, "running R code from vignettes")
            vigns <- pkgVignettes(dir = pkgdir)
            problems <- list()
            res <- character()
            cat("\n")
            def_enc <- desc["Encoding"]
            if( (is.na(def_enc))) def_enc <- ""
            t1 <- proc.time()
            for (i in seq_along(vigns$docs)) {
                file <- vigns$docs[i]
                name <- vigns$names[i]
                enc <- getVignetteEncoding(file, TRUE)
                if(enc %in% c("non-ASCII", "unknown")) enc <- def_enc
                cat("  ", sQuote(basename(file)),
                    if(nzchar(enc)) paste("using", sQuote(enc)),
                    "...")
                Rcmd <- paste0("options(warn=1)\ntools:::.run_one_vignette('",
                               basename(file), "', '", vigns$dir, "'",
                               if (nzchar(enc))
                                   paste0(", encoding = '", enc, "'"),
                               ", pkgdir='", vigns$pkgdir, "')")
                outfile <- paste0(basename(file), ".log")
                t1b <- proc.time()
                status <- R_runR(Rcmd,
                                 if (use_valgrind) paste(R_opts2, "-d valgrind") else R_opts2,
                                 ## add timing as footer, as BATCH does
                                 env = c(jitstr, "R_BATCH=1234", elibs,
                                 "_R_CHECK_INTERNALS2_=1"),
                                 stdout = outfile, stderr = outfile)
                t2b <- proc.time()
                out <- readLines(outfile, warn = FALSE)
                savefile <- paste0(name, ".Rout.save")
                if(length(grep("^  When (running|tangling|sourcing)", out,
                               useBytes = TRUE))) {
                    cat(" failed\n")
                    res <- c(res,
                             paste("when running code in", sQuote(basename(file))),
                             "  ...",
                             utils::tail(out, as.numeric(Sys.getenv("_R_CHECK_VIGNETTES_NLINES_", 10))))
                } else if(status || ! " *** Run successfully completed ***" %in% out) {
                    ## (Need not be the final line if running under valgrind)
                    cat(" failed to complete the test\n")
                    out <- c(out, "", "... incomplete output.  Crash?")
                    res <- c(res,
                             paste("when running code in", sQuote(basename(file))),
                             "  ...",
                             utils::tail(out, as.numeric(Sys.getenv("_R_CHECK_VIGNETTES_NLINES_", 10))))
                } else if (file.exists(savefile)) {
                    cmd <- paste0("invisible(tools::Rdiff('",
                                 outfile, "', '", savefile, "',TRUE,TRUE))")
                    out2 <- R_runR(cmd, R_opts2)
                    if(length(out2)) {
                        print_time(t1b, t2b, NULL)
                        cat("\ndifferences from ", sQuote(basename(savefile)),
                            "\n", sep = "")
                        writeLines(c(out2, ""))
                    } else {
                        print_time(t1b, t2b, NULL)
                        cat(" OK\n")
                        if (!config_val_to_logical(Sys.getenv("_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_", use_valgrind)))
                            unlink(outfile)
                    }
                } else {
                    print_time(t1b, t2b, NULL)
                    cat(" OK\n")
                    if (!config_val_to_logical(Sys.getenv("_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_", use_valgrind)))
                        unlink(outfile)
                }
            }
            t2 <- proc.time()
            print_time(t1, t2, Log)
            if (R_check_suppress_RandR_message)
                res <- grep('^Xlib: *extension "RANDR" missing on display', res,
                            invert = TRUE, value = TRUE, useBytes = TRUE)
            if(length(res)) {
                if(length(grep("there is no package called", res,
                               useBytes = TRUE))) {
                    warningLog(Log, "Errors in running code in vignettes:")
                    printLog0(Log, paste(c(res, "", ""), collapse = "\n"))
                } else {
                    errorLog(Log, "Errors in running code in vignettes:")
                    printLog0(Log, paste(c(res, "", ""), collapse = "\n"))
                    do_exit(1L)
                }
            } else resultLog(Log, "OK")

            if (do_build_vignettes &&
                parse_description_field(desc, "BuildVignettes", TRUE)) {
                checkingLog(Log, "re-building of vignette PDFs")
                ## copy the whole pkg directory to check directory
                ## so we can work in place, and allow ../../foo references.
                dir.create(vd2 <- "vign_test")
                if (!dir.exists(vd2)) {
                    errorLog(Log, "unable to create 'vign_test'")
                    do_exit(1L)
                }
                file.copy(pkgdir, vd2, recursive = TRUE)

                ## since so many people use 'R CMD' in Makefiles,
                oPATH <- Sys.getenv("PATH")
                Sys.setenv(PATH = paste(R.home("bin"), oPATH,
                           sep = .Platform$path.sep))
                on.exit(Sys.setenv(PATH = oPATH))
                ## And too many inst/doc/Makefile are not safe for
                ## parallel makes
                Sys.setenv(MAKEFLAGS="")
                ## we could use clean = FALSE, but that would not be
                ## testing what R CMD build uses.
                Rcmd <- "options(warn=1)\nlibrary(tools)\n"
                Rcmd <- paste0(Rcmd, "buildVignettes(dir = '",
                               file.path(pkgoutdir, "vign_test", pkgname0),
                               "')")
                t1 <- proc.time()
                outfile <- tempfile()
                status <- R_runR(Rcmd, R_opts2, jitstr,
                                 stdout = outfile, stderr = outfile)
                t2 <- proc.time()
                if (status) {
                    noteLog(Log)
                    out <- readLines(outfile, warn = FALSE)
                    if (R_check_suppress_RandR_message)
                        out <- grep('^Xlib: *extension "RANDR" missing on display', out,
                                    invert = TRUE, value = TRUE, useBytes = TRUE)
                    out <- utils::tail(out, 25)
                    printLog0(Log,
                              paste(c("Error in re-building vignettes:",
                                      "  ...", out, "", ""), collapse = "\n"))
                } else {
                    ## clean up
                    if (config_val_to_logical(Sys.getenv("_R_CHECK_CLEAN_VIGN_TEST_", "true")))
                        unlink(vd2, recursive = TRUE)
                    print_time(t1, t2, Log)
                    resultLog(Log, "OK")
                }
            } else {
                checkingLog(Log, "re-building of vignettes")
                resultLog(Log, "SKIPPED")
            }
        } else {
            checkingLog(Log, "running R code from vignettes")
            resultLog(Log, "SKIPPED")
            checkingLog(Log, "re-building of vignettes")
            resultLog(Log, "SKIPPED")
        }
    }

    check_pkg_manual <- function(pkgdir, pkgname)
    {
        ## Run Rd2pdf on the manual, if there are man pages
        ## If it is installed there is a 'help' dir
        ## and for a source package, there is a 'man' dir
        if (dir.exists(file.path(pkgdir, "help")) ||
            dir.exists(file.path(pkgdir, "man"))) {
            topdir <- pkgdir
            Rd2pdf_opts <- "--batch --no-preview"
            checkingLog(Log, "PDF version of manual")
            build_dir <- gsub("\\", "/", tempfile("Rd2pdf"), fixed = TRUE)
            man_file <- paste0(pkgname, "-manual.pdf ")
            ## precautionary remove in case some other attempt left it behind
            if(file.exists(man_file)) unlink(man_file)
            args <- c( "Rd2pdf ", Rd2pdf_opts,
                      paste0("--build-dir=", shQuote(build_dir)),
                      "--no-clean", "-o ", man_file , shQuote(topdir))
            res <- run_Rcmd(args,  "Rdlatex.log")
            latex_log <- file.path(build_dir, "Rd2.log")
            if (file.exists(latex_log))
                file.copy(latex_log, paste0(pkgname, "-manual.log"))
            if (res == 11) { ## return code from Rd2pdf
                errorLog(Log, "Rd conversion errors:")
                lines <- readLines("Rdlatex.log", warn = FALSE)
                lines <- grep("^(Hmm|Execution)", lines,
                              invert = TRUE, value = TRUE)
                printLog0(Log, paste(c(lines, ""), collapse = "\n"))
                unlink(build_dir, recursive = TRUE)
                do_exit(1L)
            } else if (res > 0) {
                latex_file <- file.path(build_dir, "Rd2.tex")
                if (file.exists(latex_file))
                    file.copy(latex_file, paste0(pkgname, "-manual.tex"))
                warningLog(Log)
                printLog(Log,
                         paste0("LaTeX errors when creating PDF version.\n",
                                "This typically indicates Rd problems.\n"))
                ## If possible, indicate the problems found.
                if (file.exists(latex_log)) {
                    lines <- .get_LaTeX_errors_from_log_file(latex_log)
                    printLog(Log, "LaTeX errors found:\n")
                    printLog0(Log, paste(c(lines, ""), collapse = "\n"))
                }
                unlink(build_dir, recursive = TRUE)
                ## for Windows' sake: errors can make it unwritable
                build_dir <- gsub("\\", "/", tempfile("Rd2pdf"), fixed = TRUE)
                checkingLog(Log, "PDF version of manual without hyperrefs or index")
                ## Also turn off hyperrefs.
                Sys.setenv(R_RD4PDF = "times")
                args <- c( "Rd2pdf ", Rd2pdf_opts,
                          paste0("--build-dir=", shQuote(build_dir)),
                          "--no-clean", "--no-index",
                          "-o ", man_file, topdir)
                if (run_Rcmd(args, "Rdlatex.log")) {
                    ## FIXME: the info is almost certainly in Rdlatex.log
                    errorLog(Log)
                    latex_log <- file.path(build_dir, "Rd2.log")
                    if (file.exists(latex_log))
                        file.copy(latex_log, paste0(pkgname, "-manual.log"))
                    else {
                        ## No log file and thus no chance to find out
                        ## what went wrong.  Hence, re-run without
                        ## redirecting stdout/stderr and hope that this
                        ## gives the same problem ...
                        # printLog(Log, "Error when running command:\n")
                        # cmd <- paste(c("R CMD", args), collapse = " ")
                        # printLog(Log, strwrap(cmd, indent = 2, exdent = 4), "\n")
                        printLog(Log, "Re-running with no redirection of stdout/stderr.\n")
                        unlink(build_dir, recursive = TRUE)
                        build_dir <- gsub("\\", "/", tempfile("Rd2pdf"), fixed = TRUE)
                        args <- c( "Rd2pdf ", Rd2pdf_opts,
                                  paste0("--build-dir=", shQuote(build_dir)),
                                  "--no-clean", "--no-index",
                                  "-o ", paste0(pkgname, "-manual.pdf "),
                                  topdir)
                        run_Rcmd(args)
                    }
                    unlink(build_dir, recursive = TRUE)
                    do_exit(1L)
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

    check_executables <- function()
    {
        owd <- setwd(pkgdir)
        allfiles <- dir(".", all.files = TRUE, full.names = TRUE,
                        recursive = TRUE)
        allfiles <- sub("^./","", allfiles)
        ## this is tailored to the FreeBSD/Linux 'file',
        ## see http://www.darwinsys.com/file/
        ## (Solaris has a different 'file' without --version)
        ## Most systems are now on 5.03/7, but Mac OS 10.5 is 4.17
        ## version 4.21 writes to stdout,
        ## 4.23 to stderr and sets an error status code
        lines <- suppressWarnings(tryCatch(system2("file", "--version", TRUE, TRUE), error = function(e) "error"))
        ## a reasonable check -- it does not identify itself well
        have_free_file <-
            any(grepl("^(file-[45]|magic file from)", lines))
        if (have_free_file) {
            checkingLog(Log, "for executable files")
            ## Watch out for spaces in file names here
            ## Do in parallel for speed on Windows, but in batches
            ## since there may be a line-length limit.
            execs <- character()
            files <- allfiles
            while(ll <- length(files)) {
                chunk <- seq_len(min(100, ll))
                these <- files[chunk]
                files <- files[-chunk]
                lines <- suppressWarnings(system2("file", shQuote(these), TRUE, TRUE))
                ## avoid match to is_executable.Rd
                ex <- grepl(" executable", lines, useBytes=TRUE)
		ex2 <- grepl("script", lines, useBytes=TRUE) &
		       grepl("text", lines, useBytes=TRUE)
                execs <- c(execs, lines[ex & !ex2])
            }
            if(length(execs)) {
                execs <- sub(":[[:space:]].*$", "", execs, useBytes = TRUE)
                known <- rep(FALSE, length(execs))
                pexecs <- file.path(pkgname, execs)
                ## known false positives
                for(fp in  c("foreign/tests/datefactor.dta",
                             "msProcess/inst/data[12]/.*.txt",
                             "WMBrukerParser/inst/Examples/C3ValidationExtractSmall/RobotRun1/2-100kDa/0_B1/1/1SLin/fid") )
                    known <- known | grepl(fp, pexecs)
                execs <- execs[!known]
            }
        } else {
            ## no 'file', so just check extensions
            checkingLog(Log, "for .dll and .exe files")
            execs <- grep("\\.(exe|dll)$", allfiles, value = TRUE)
        }
        if (R_check_executables_exclusions && file.exists("BinaryFiles")) {
            excludes <- readLines("BinaryFiles")
            execs <- execs[!execs %in% excludes]
        }
        if (grepl("^check", install) && file.exists(".install_timestamp"))
            execs <- execs[file_test("-ot", execs, ".install_timestamp")]
        if (nb <- length(execs)) {
            msg <- ngettext(nb,
                            "Found the following executable file:",
                            "Found the following executable files:",
                            domain = NA)
            warningLog(Log, msg)
            printLog(Log, .format_lines_with_indent(execs), "\n")
            wrapLog("Source packages should not contain undeclared executable files.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
        } else resultLog(Log, "OK")
        setwd(owd)
    }

    ## CRAN-pack knows about
    .hidden_file_exclusions <-
        c(".Renviron", ".Rprofile", ".Rproj.user",
          ".Rhistory", ".Rapp.history",
          ".tex", ".log", ".aux", ".pdf", ".png",
          ".backups", ".cvsignore", ".cproject", ".directory",
          ".dropbox", ".exrc", ".gdb.history",
          ".gitattributes", ".gitignore", ".gitmodules",
          ".hgignore", ".hgtags",
          ".project", ".seed", ".settings", ".tm_properties")

    check_dot_files <- function(cran = FALSE)
    {
        checkingLog(Log, "for hidden files and directories")
        owd <- setwd(pkgdir)
        dots <- dir(".", all.files = TRUE, full.names = TRUE,
                        recursive = TRUE, pattern = "^[.]")
        dots <- sub("^./","", dots)
        allowed <-
            c(".Rbuildignore", ".Rinstignore", "vignettes/.install_extras",
              ".install_timestamp") # Kurt uses this
        dots <- dots[!dots %in% allowed]
        alldirs <- list.dirs(".", full.names = TRUE, recursive = TRUE)
        alldirs <- sub("^./","", alldirs)
        alldirs <- alldirs[alldirs != "."]
        bases <- basename(alldirs)
        dots <- c(dots, alldirs[grepl("^[.]", bases)])
        if (length(dots)) {
            noteLog(Log, "Found the following hidden files and directories:")
            printLog(Log, .format_lines_with_indent(dots), "\n")
            wrapLog("These were most likely included in error.",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            if(cran) {
                known <- basename(dots) %in% .hidden_file_exclusions
                known <- known | grepl("^.Rbuildindex[.]", dots) |
                  grepl("inst/doc/[.](Rinstignore|build[.]timestamp)$", dots) |
                  grepl("vignettes/[.]Rinstignore$", dots) |
                  grepl("^src.*/[.]deps$", dots)
               if (all(known))
                    printLog(Log, "\nCRAN-pack knows about all of these\n")
                else if (any(!known)) {
                    printLog(Log, "\nCRAN-pack does not know about\n")
                    printLog(Log, .format_lines_with_indent(dots[!known]), "\n")
                }
            }
        } else resultLog(Log, "OK")
        setwd(owd)
    }

    check_install <- function()
    {
        ## Option '--no-install' turns off installation and the tests
        ## which require the package to be installed.  When testing
        ## recommended packages bundled with R we can skip
        ## installation, and do so if '--install=skip' was given.  If
        ## command line option '--install' is of the form
        ## 'check:FILE', it is assumed that installation was already
        ## performed with stdout/stderr redirected to FILE, the
        ## contents of which need to be checked (without repeating the
        ## installation).  In this case, one also needs to specify
        ## *where* the package was installed to using command line
        ## option '--library'.

        if (install == "skip")
            messageLog(Log, "skipping installation test")
        else {
            use_install_log <-
                (grepl("^check", install) || R_check_use_install_log
                 || !isatty(stdout()))
            INSTALL_opts <- install_args
            ## don't use HTML, checkRd goes over the same ground.
            INSTALL_opts <- c(INSTALL_opts,  "--no-html")
            if (install == "fake")
                INSTALL_opts <- c(INSTALL_opts,  "--fake")
            else if (!multiarch)
                INSTALL_opts <- c(INSTALL_opts,  "--no-multiarch")
            INSTALL_opts <- paste(INSTALL_opts, collapse = " ")
            args <- c("INSTALL", "-l", shQuote(libdir), INSTALL_opts,
                      shQuote(if (WINDOWS) shortPathName(pkgdir) else pkgdir))
            if (!use_install_log) {
                ## Case A: No redirection of stdout/stderr from installation.
                ## This is very rare: needs _R_CHECK_USE_INSTALL_LOG_ set
                ## to false.
                message("")
                ## Rare use of R CMD INSTALL
                if (run_Rcmd(args)) {
                    errorLog(Log, "Installation failed.")
                    do_exit(1L)
                }
                message("")
            } else {
                ## Case B. All output from installation redirected,
                ## or already available in the log file.
                checkingLog(Log,
                            "whether package ",
                            sQuote(desc["Package"]),
                            " can be installed")
                outfile <- file.path(pkgoutdir, "00install.out")
                if (grepl("^check", install)) {
                    if (!nzchar(arg_libdir))
                        printLog(Log, "\nWarning: --install=check... specified without --library\n")
                    thislog <- substr(install, 7L, 1000L)
                                        #owd <- setwd(startdir)
                    if (!file.exists(thislog)) {
                        errorLog(Log,
                                 sprintf("install log %s does not exist", sQuote(thislog)))
                        do_exit(2L)
                    }
                    file.copy(thislog, outfile)
                                        #setwd(owd)
                    install <- "check"
                    lines <- readLines(outfile, warn = FALSE)
                    ## <NOTE>
                    ## We used to have
                    ## $install_error = ($lines[$#lines] !~ /^\* DONE/);
                    ## but what if there is output from do_cleanup
                    ## in (Unix) R CMD INSTALL?
                    ## </NOTE>
                    install_error <- !any(grepl("^\\* DONE", lines))
                } else {
                    ## record in the log what options were used
                    cat("* install options ", sQuote(INSTALL_opts),
                        "\n\n", sep = "", file = outfile)
                    env <- ""
                    ## Normal use of R CMD INSTALL
                    t1 <- proc.time()
                    install_error <- run_Rcmd(args, outfile)
                    t2 <- proc.time()
                    print_time(t1, t2, Log)
                    lines <- readLines(outfile, warn = FALSE)
                }
                if (install_error) {
                    errorLog(Log, "Installation failed.")
                    printLog(Log, "See ", sQuote(outfile),
                             " for details.\n")
                    do_exit(1L)
                }

                ## There could still be some important warnings that
                ## we'd like to report.  For the time being, start
                ## with compiler warnings about non ISO C code (or
                ## at least, what looks like it), and also include
                ## warnings resulting from the const char * CHAR()
                ## change in R 2.6.0.  (In theory, we should only do
                ## this when using GCC ...)

                if (install != "check")
                    lines <- readLines(outfile, warn = FALSE)

                lines0 <- lines
                warn_re <- c("^WARNING:",
                             "^Warning:",
                             ## <FIXME>
                             ## New style Rd conversion
                             ## which may even show errors:
                             "^Rd (warning|error): ",
                             ## </FIXME>
                             ": warning: .*ISO C",
                             ": warning: .* discards qualifiers from pointer target type",
                             ": warning: .* is used uninitialized",
                             ": warning: .* set but not used",
                             ": warning: unused",
                             ": #warning",
                             # these are from era of static HTML
                             "missing links?:")
                ## Warnings spotted by gcc with
                ## '-Wimplicit-function-declaration', which is
                ## implied by '-Wall'.  Currently only accessible
                ## via an internal environment variable.
                check_src_flag <- Sys.getenv("_R_CHECK_SRC_MINUS_W_IMPLICIT_", "FALSE")
                ## (Not quite perfect, as the name should really
                ## include 'IMPLICIT_FUNCTION_DECLARATION'.)
                if (config_val_to_logical(check_src_flag)) {
                    warn_re <- c(warn_re,
                                 ": warning: implicit declaration of function",
                                 ": warning: incompatible implicit declaration of built-in function")
                }

                warn_re <- paste0("(", paste(warn_re, collapse = "|"), ")")

                lines <- grep(warn_re, lines, value = TRUE, useBytes = TRUE)

                ## Ignore install-time readLines() warnings about
                ## files with incomplete final lines.  Most of these
                ## come from .install_package_indices(), and should be
                ## safe to ignore ...
                lines <- grep("Warning: incomplete final line found by readLines",
                              lines, invert = TRUE, value = TRUE, useBytes = TRUE)

                check_Stangle <- Sys.getenv("_R_CHECK_STANGLE_WARNINGS_", "TRUE")
                if (!config_val_to_logical(check_Stangle))
                lines <- grep("Warning: value of .* option should be lowercase",
                              lines, invert = TRUE, value = TRUE, useBytes = TRUE)

                ## Package writers cannot really do anything about
                ## non ISO C code in *system* headers.  Also,
                ## GCC >= 3.4 warns about function pointers
                ## casts which are "needed" for dlsym(), but it
                ## seems that all systems which have dlsym() also
                ## support the cast.  Hence, try to ignore these by
                ## default, but make it possible to get all ISO C
                ## warnings via an environment variable.
                if (!R_check_all_non_ISO_C) {
                    lines <- grep("^ */.*: warning: .*ISO C",
                                  lines, invert = TRUE, value = TRUE, useBytes = TRUE)
                    lines <- grep("warning: *ISO C forbids.*function pointer",
                                  lines, invert = TRUE, value = TRUE, useBytes = TRUE)
                }

                ## Warnings spotted by gcc with '-Wunused', which is
                ## implied by '-Wall'.  Currently only accessible
                ## via an internal environment variable.
                check_src_flag <-
                    Sys.getenv("_R_CHECK_SRC_MINUS_W_UNUSED_", "FALSE")
                if (!config_val_to_logical(check_src_flag)) {
                    lines <- grep("warning: unused", lines, ignore.case = TRUE,
                                  invert = TRUE, value = TRUE, useBytes = TRUE)
                    lines <- grep("warning: .* set but not used", lines,
                                  ignore.case = TRUE,
                                  invert = TRUE, value = TRUE, useBytes = TRUE)
                }
                ## (gfortran seems to use upper case.)


                ## Warnings spotted by gfortran >= 4.0 with
                ## -Wall.  Justified in principle, it seems.  Let's
                ## filter them for the time being, and maybe revert
                ## this later on ... but make it possible to suppress
                ## filtering out by setting the internal environment
                ## variable _R_CHECK_WALL_FORTRAN_ to something
                ## "true".
                check_src_flag <- Sys.getenv("_R_CHECK_WALL_FORTRAN_", "FALSE")
                if (!config_val_to_logical(check_src_flag)) {
                    warn_re <-
                        c("Label .* at \\(1\\) defined but not used",
                          "Line truncated at \\(1\\)",
                          "ASSIGN statement at \\(1\\)",
                          "Assigned GOTO statement at \\(1\\)",
                          "arithmetic IF statement at \\(1\\)",
                          "Nonconforming tab character (in|at)")
                    warn_re <- paste0("(", paste(warn_re, collapse = "|"), ")")
                    lines <- grep(warn_re, lines, invert = TRUE, value = TRUE)
                }

                if (WINDOWS) {
                    ## Warning on Windows with some packages that
                    ## cannot transparently be installed bi-arch.
                    lines <- grep("Warning: this package has a non-empty 'configure.win' file",
                                  lines, invert = TRUE, value = TRUE)
                    ## Warning on x64 Windows gcc 4.5.1 that
                    ## seems to be spurious
                    lines <- grep("Warning: .drectve .* unrecognized",
                                  lines, invert = TRUE, value = TRUE)
                }

                ## Warnings about replacing imports are almost always
                ## due to auto-generated namespaces
                check_imports_flag <-
                    Sys.getenv("_R_CHECK_REPLACING_IMPORTS_", "FALSE")
                if (!config_val_to_logical(check_imports_flag))
                    lines <- grep("Warning: replacing previous import", lines,
                                  fixed = TRUE, invert = TRUE, value = TRUE)
                check_FirstLib_flag <-
                    Sys.getenv("_R_CHECK_DOT_FIRSTLIB_", "FALSE")
                if (!config_val_to_logical(check_FirstLib_flag))
                    lines <- grep("Warning: ignoring .First.lib()", lines,
                                  fixed = TRUE, invert = TRUE, value = TRUE)

                if (length(lines)) {
                    warningLog(Log, "Found the following significant warnings:")
                    printLog0(Log, .format_lines_with_indent(lines), "\n")
                    printLog0(Log, sprintf("See %s for details.\n",
                                           sQuote(outfile)))
                } else resultLog(Log, "OK")
            }   ## end of case B
        }

    }

    ## This requires a GNU-like 'du' with 1k block sizes,
    ## so use -k (which POSIX requires).
    ## It also depends on the total being last.
    check_install_sizes <- function()
    {
        ## if we used a log, the installation need not still exist.
        pd <- file.path(libdir, pkgname)
        if (!dir.exists(pd)) return()
        checkingLog(Log, "installed package size")
        owd <- setwd(pd)
        res <- system2("du", "-k", TRUE, TRUE)
        sizes <- as.integer(sub("\\D.*", "", res))
        dirs <- sub("^\\d*\\s*", "", res)
        res2 <- data.frame(size = sizes, dir = I(dirs))
        total <- res2[nrow(res2), 1L]
        if(!is.na(total) && total > 1024*5) { # report at 5Mb
            noteLog(Log)
            printLog(Log, sprintf("  installed size is %4.1fMb\n", total/1024))
            rest <- res2[-nrow(res2), ]
            rest[, 2L] <- sub("./", "", rest[, 2L])
            # keep only top-level directories
            rest <- rest[!grepl("/", rest[, 2L]), ]
            rest <- rest[rest[, 1L] > 1024, ] # > 1Mb
            if(nrow(rest)) {
                o <- sort.list(rest[, 2L])
                printLog(Log, "  sub-directories of 1Mb or more:\n")
                size <- sprintf('%4.1fMb', rest[, 1L]/1024)
                printLog(Log, paste("    ",
                                    format(rest[o, 2L], justify = "left"),
                                    "  ",
                                    format(size[o], justify = "right"),
                                    "\n", sep=""))
            }
        } else resultLog(Log, "OK")
        setwd(owd)
    }

    check_description <- function()
    {
        checkingLog(Log, "for file ",
                    sQuote(file.path(pkgname0, "DESCRIPTION")))
        if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
            desc <- try(.read_description(f))
            if (inherits(desc, "try-error") || !length(desc)) {
                resultLog(Log, "EXISTS but not correct format")
                do_exit(1L)
            }
            if(!grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", desc["Package"])
               || grepl("[.]$", desc["Package"])) {
                warningLog(Log)
                printLog(Log,"  Package name is not portable:\n",
                         "  It must start with a letter, contain letters, digits or dot\n",
                         "  have at least 2 characters and not end with a dot.\n")
            } else resultLog(Log, "OK")
            encoding <- desc["Encoding"]
        } else {
            resultLog(Log, "NO")
            do_exit(1L)
        }
        if (!is.na(desc["Type"])) { # standard packages do not have this
            checkingLog(Log, "extension type")
            resultLog(Log, desc["Type"])
            if (desc["Type"] != "Package") {
                printLog(Log,
                         "Only 'Type = Package' extensions can be checked.\n")
                do_exit(0L)
            }
        }
        if (!is.na(desc["Bundle"])) {
            messageLog(Log, "looks like ", sQuote(pkgname0),
                       " is a package bundle -- they are defunct")
            errorLog(Log, "")
            do_exit(1L)
        }

        messageLog(Log,
                   sprintf("this is package %s version %s",
                           sQuote(desc["Package"]),
                           sQuote(desc["Version"])))

        if (!is.na(encoding))
            messageLog(Log, "package encoding: ", encoding)

        desc
    }

    check_CRAN_incoming <- function()
    {
        checkingLog(Log, "CRAN incoming feasibility")
        res <- .check_package_CRAN_incoming(pkgdir)
        if(length(res)) {
            out <- format(res)
            if(length(res$bad_package)) {
                errorLog(Log)
                printLog(Log, paste(c(out, ""), collapse = "\n"))
                do_exit(1L)
            } else if(length(res$bad_version))
                warningLog(Log)
            else if(length(res) > 1L) noteLog(Log)
            else resultLog(Log, "OK")
            printLog(Log, paste(out, "\n", sep = ""))
        } else resultLog(Log, "OK")
    }

    check_dependencies <- function()
    {
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

        if (!extra_arch &&
            file.exists(file.path(pkgdir, "NAMESPACE"))) {
            checkingLog(Log, "package namespace information")
            msg_NAMESPACE <-
                c("See section 'Package namespaces'",
                  " of the 'Writing R Extensions' manual.\n")
            tryCatch(parseNamespaceFile(basename(pkgdir), dirname(pkgdir)),
                     error = function(e) {
                         errorLog(Log)
                         printLog0(Log,
                                   "Invalid NAMESPACE file, parsing gives:", "\n",
                                   as.character(e), "\n")
                         wrapLog(msg_NAMESPACE)
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
        res <- .check_package_depends(pkgdir, R_check_force_suggests)
        if(any(sapply(res, length) > 0L)) {
            out <- format(res)
            if(!all(names(res) %in% c("suggests_but_not_installed",
                                      "enhances_but_not_installed"))) {
                errorLog(Log)
                printLog(Log, paste(out, collapse = "\n"), "\n")
                if(length(res$suggested_but_not_installed))
                   wrapLog("The suggested packages are required for",
                           "a complete check.\n",
                           "Checking can be attempted without them",
                           "by setting the environment variable",
                           "_R_CHECK_FORCE_SUGGESTS_",
                           "to a false value.\n\n")
                wrapLog(msg_DESCRIPTION)
                do_exit(1L)
            } else {
                noteLog(Log)
                printLog(Log, paste(out, collapse = "\n"))
            }
        } else resultLog(Log, "OK")
    }

    check_sources <- function()
    {
        checkingLog(Log, "if this is a source package")
        ## <NOTE>
        ## This check should be adequate, but would not catch a manually
        ## installed package, nor one installed prior to 1.4.0.
        ## </NOTE>
        if (!is.na(desc["Built"])) {
            errorLog(Log)
            printLog(Log, "Only *source* packages can be checked.\n")
            do_exit(1L)
        } else if (!grepl("^check", install)) {
            ini <- character()
            ## Check for package 'src' subdirectories with object
            ## files (but not if installation was already performed).
            pat <- "(a|o|[ls][ao]|sl|obj|dll)" # Object file/library extensions.
            any <- FALSE
            srcd <- file.path(pkgdir, "src")
            if (dir.exists(srcd) &&
                length(of <- list_files_with_exts(srcd, pat))) {
                if (!any) warningLog(Log)
                any <- TRUE
                of <- sub(paste0(".*/", file.path(pkgname, "src"), "/"),
                          "", of)
                printLog(Log,
                         sprintf("Subdirectory %s contains apparent object files/libraries\n",
                                 sQuote(file.path(pkgname, "src"))),
                         paste(strwrap(paste(of, collapse = " "),
                                       indent = 2L, exdent = 2L),
                               collapse = "\n"),
                         "\nObject files/libraries should not be included in a source package.\n")
                ini <- ""
            }
            ## A submission had src-i386 etc from multi-arch builds
            ad <- list.dirs(pkgdir, recursive = FALSE)
            if(thispkg_src_subdirs != "no" &&
               any(ind <- grepl("/src-(i386|x64|x86_64|ppc)$", ad))) {
                if(!any) warningLog(Log)
                any <- TRUE
                msg <- ngettext(sum(ind),
                                "Found the following directory with a name of a multi-arch build directory:\n",
                                "Found the following directories with names of multi-arch build directories:\n",
                                domain = NA)
                printLog(Log,
                         ini,
                         msg,
                         .format_lines_with_indent(basename(ad[ind])),
                         "\n",
                         "Most likely, these were included erroneously.\n")
                ini <- ""
            }
            if (thispkg_src_subdirs != "no" && dir.exists(srcd)) {
                setwd(srcd)
                if (!file.exists("Makefile") &&
                    !file.exists("Makefile.win") &&
                    !(file.exists("Makefile.in") && spec_install)) {
                    ## Recognized extensions for sources or headers.
                    srcfiles <- dir(".", all.files = TRUE)
                    fi <- file.info(srcfiles)
                    srcfiles <- srcfiles[!fi$isdir]
                    srcfiles <- grep("(\\.([cfmCM]|cc|cpp|f90|f95|mm|h|o|so)$|^Makevars|-win\\.def$)",
                                     srcfiles,
                                     invert = TRUE, value = TRUE)
                    if (length(srcfiles)) {
                        if (!any) warningLog(Log)
                        any <- TRUE
                        msg <- c(ini,
                                 paste("Subdirectory",
                                       sQuote("src"),
                                       "contains:"),
                                 strwrap(paste(srcfiles, collapse = " "),
                                         indent = 2, exdent = 2),
                                 strwrap("These are unlikely file names for src files."),
                                 "")
                        printLog(Log, paste(msg, collapse = "\n"))
                        ini <- ""
                    }
                }
                setwd(startdir)
            }
            ## All remaining checks give notes and not warnings.
            if(length(ini))
                ini <- c("",
                         "In addition to the above warning(s), found the following notes:",
                         "")
            files <- list.files(pkgdir, recursive = TRUE)
            ## Check for object files not directly in src.
            ## (Note that the above does not look for object files in
            ## subdirs of src.)
            bad <- files[grepl(sprintf("\\.%s$", pat), basename(files))]
            bad <- bad[dirname(bad) != "src" |
                       dirname(dirname(bad)) != "."]
            if(length(bad)) {
                if(!any) noteLog(Log)
                any <- TRUE
                msg <- c(ini,
                         "Found the following apparent object files/libraries:",
                         strwrap(paste(bad, collapse = " "),
                                 indent = 2L, exdent = 2L),
                         "Object files/libraries should not be included in a source package.\n")
                printLog(Log, paste(msg, collapse = "\n"))
                ini <- ""
            }
            ## Check for installed copies of the package in some subdir.
            files <- files[basename(dirname(files)) == "Meta"]
            if(length(files) &&
               all(!is.na(match(c("nsInfo.rds", "package.rds"),
                                basename(files))))) {
                if(!any) noteLog(Log)
                any <- TRUE
                msg <- c(ini,
                         sprintf("Subdirectory %s seems to contain an installed version of the package.\n",
                                 sQuote(dirname(dirname(files[1L])))))
                printLog(Log, paste(msg, collapse = "\n"))
            }
            if (!any) resultLog(Log, "OK")
        } else resultLog(Log, "OK")
    }

    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    do_exit <- function(status = 1L) q("no", status = status, runLast = FALSE)

    env_path <- function(...) {
        paths <- c(...)
        paste(paths[nzchar(paths)], collapse = .Platform$path.sep)
    }

    Usage <- function() {
        cat("Usage: R CMD check [options] pkgs",
            "",
            "Check R packages from package sources, which can be directories or",
            "package 'tar' archives with extension '.tar.gz', '.tar.bz2',",
            "'.tar.xz' or '.tgz'.",
            "",
            "A variety of diagnostic checks on directory structure, index and",
            "control files are performed.  The package is installed into the log",
            "directory and production of the package PDF manual is tested.",
            "All examples and tests provided by the package are tested to see if",
            "they run successfully.  Code in the vignettes is tested,",
            "as is re-building the vignette PDFs.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -l, --library=LIB     library directory used for test installation",
            "			of packages (default is outdir)",
            "  -o, --output=DIR      directory for output, default is current directory.",
            "			Logfiles, R output, etc. will be placed in 'pkg.Rcheck'",
            "			in this directory, where 'pkg' is the name of the",
            "			checked package",
            "      --no-clean        do not clean 'outdir' before using it",
            "      --no-codoc        do not check for code/documentation mismatches",
            "      --no-examples     do not run the examples in the Rd files",
            "      --no-install      skip installation and associated tests",
            "      --no-tests        do not run code in 'tests' subdirectory",
            "      --no-manual       do not produce the PDF manual",
            "      --no-vignettes    do not run R code in vignettes",
            "      --no-build-vignettes    do not build PDFs of vignettes",
            "      --use-gct         use 'gctorture(TRUE)' when running examples/tests",
            "      --use-valgrind    use 'valgrind' when running examples/tests/vignettes",
            "      --timings         record timings for examples",
            "      --install-args=	command-line args to be passed to INSTALL",
            "      --check-subdirs=default|yes|no",
            "			run checks on the package subdirectories",
            "			(default is yes for a tarball, no otherwise)",
            "      --as-cran         select customizations similar to those used",
            "                        for CRAN incoming checking",
            "",
            "The following options apply where sub-architectures are in use:",
            "      --extra-arch      do only runtime tests needed for an additional",
            "                        sub-architecture.",
            "      --multiarch       do runtime tests on all installed sub-archs",
            "      --no-multiarch    do runtime tests only on the main sub-architecture",
            "      --force-multiarch run tests on all sub-archs even for packages",
            "                        with no compiled code",
            "",
            "By default, all test sections are turned on.",
            "",
            "Report bugs at bugs.r-project.org .", sep="\n")
    }

###--- begin{.check_packages()} "main" ---

    options(showErrorCalls=FALSE, warn = 1)

    ## Read in check environment file.
    Renv <- Sys.getenv("R_CHECK_ENVIRON", unset = NA)
    if(!is.na(Renv)) {
        ## Do not read any check environment file if R_CHECK_ENVIRON is
        ## set to empty of something non-existent.
        if(nzchar(Renv) && file.exists(Renv)) readRenviron(Renv)
    } else {
        ## Read in ~/.R/check.Renviron[.rarch] (if it exists).
        rarch <- .Platform$r_arch
        if (nzchar(rarch) &&
            file.exists(Renv <- paste("~/.R/check.Renviron", rarch, sep = ".")))
            readRenviron(Renv)
        else if (file.exists(Renv <- "~/.R/check.Renviron"))
            readRenviron(Renv)
    }

    td0 <- as.numeric(Sys.getenv("_R_CHECK_TIMINGS_"))
    if (is.na(td0)) td0 <- Inf

    ## A user might have turned on JIT compilation.  That does not
    ## work well, so mostly disable it
    jit <- Sys.getenv("R_ENABLE_JIT")
    jitstr <- if(nzchar(jit)) {
        Sys.setenv(R_ENABLE_JIT = "0")
        paste0("R_ENABLE_JIT=", jit)
    } else character()

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
    do_build_vignettes <- TRUE
    do_manual <- TRUE
    use_gct <- FALSE
    use_valgrind <- FALSE
    do_timings <- FALSE
    install_args <- NULL
    check_subdirs <- ""           # defaults to R_check_subdirs_strict
    extra_arch <- FALSE
    spec_install <- FALSE
    multiarch <- NA
    force_multiarch <- FALSE
    as_cran <- FALSE

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
                "Copyright (C) 1997-2011 The R Core Team.",
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
        } else if (a == "--no-build-vignettes") {
            do_build_vignettes  <- FALSE
        } else if (a == "--no-rebuild-vignettes") { # pre-3.0.0 version
            do_build_vignettes  <- FALSE
        } else if (a == "--no-vignettes") {
            do_vignettes  <- FALSE
        } else if (a == "--no-manual") {
            do_manual  <- FALSE
        } else if (a == "--no-latex") {
            stop("'--no-latex' is defunct: use '--no-manual' instead",
                 call. = FALSE, domain = NA)
        } else if (a == "--use-gct") {
            use_gct  <- TRUE
        } else if (a == "--use-valgrind") {
            use_valgrind  <- TRUE
        } else if (a == "--timings") {
            do_timings  <- TRUE
        } else if (substr(a, 1, 15) == "--install-args=") {
            install_args <- substr(a, 16, 1000)
        } else if (substr(a, 1, 16) == "--check-subdirs=") {
            check_subdirs <- substr(a, 17, 1000)
        } else if (a == "--extra-arch") {
            extra_arch  <- TRUE
        } else if (a == "--multiarch") {
            multiarch  <- TRUE
        } else if (a == "--no-multiarch") {
            multiarch  <- FALSE
        } else if (a == "--force-multiarch") {
            force_multiarch  <- TRUE
        } else if (a == "--as-cran") {
            as_cran  <- TRUE
        } else if (substr(a, 1, 9) == "--rcfile=") {
            warning("configuration files are not supported as from R 2.12.0")
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }

    ## record some of the options used.
    opts <- character()
    if (install == "fake") opts <- c(opts, "--install=fake")
    if (!do_install) opts <- c(opts, "--no-install")
    if (install == "no") {
        opts <- c(opts, "--install=no")
        do_install <- FALSE
    }

    if (install == "fake") {
        ## If we fake installation, then we cannot *run* any code.
        do_examples <- do_tests <- do_vignettes <- do_build_vignettes <- 0
        spec_install <- TRUE
        multiarch <- FALSE
    }

    if (!identical(multiarch, FALSE)) {
        ## see if there are multiple installed architectures, and if they work
        if (WINDOWS) {
            ## always has sub-archs as from R 2.12.0.
            ## usually if two are installed, it was done on a 64-bit OS,
            ## but the filesystem might be shared betweeen OSes.
            f <- dir(file.path(R.home(), "bin"))
            archs <- f[f %in% c("i386", "x64")]
            ## if we have x64, can only run it on a 64-bit OS
            if (length(archs) > 1L && !grepl("x64", utils:::win.version()))
                archs <- "i386"
        } else {
            wd2 <- setwd(file.path(R.home("bin"), "exec"))
            archs <- Sys.glob("*")
            setwd(wd2)
            if (length(archs) > 1L)
                for (arch in archs) {
                    if (arch == rarch) next
                    cmd <- paste0(file.path(R.home(), "bin", "R"),
                                  " --arch=", arch,
                                  " --version > /dev/null")
                    if (system(cmd)) archs <- archs[archs != arch]
                }
        }
        if (length(archs) <= 1L && isTRUE(multiarch))
            warning("'--multiarch' specified with only one usable sub-architecture",
                    call.=FALSE, immediate. = TRUE)
        multiarch <- length(archs) > 1L
    }


    ## Use system default unless explicitly specified otherwise.
    Sys.setenv(R_DEFAULT_PACKAGES="")

    ## Configurable variables
    R_check_use_install_log <-
        config_val_to_logical(Sys.getenv("_R_CHECK_USE_INSTALL_LOG_", "TRUE"))
    R_check_subdirs_nocase <-
        config_val_to_logical(Sys.getenv("_R_CHECK_SUBDIRS_NOCASE_", "TRUE"))
    R_check_all_non_ISO_C <-
        config_val_to_logical(Sys.getenv("_R_CHECK_ALL_NON_ISO_C_", "FALSE"))
    R_check_subdirs_strict <-
        Sys.getenv("_R_CHECK_SUBDIRS_STRICT_", "default")
    R_check_Rd_xrefs <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_XREFS_", "TRUE"))
    R_check_use_codetools <-
        config_val_to_logical(Sys.getenv("_R_CHECK_USE_CODETOOLS_", "TRUE"))
    R_check_Rd_style <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_STYLE_", "TRUE"))
    R_check_executables <-
        config_val_to_logical(Sys.getenv("_R_CHECK_EXECUTABLES_", "TRUE"))
    R_check_executables_exclusions <-
        config_val_to_logical(Sys.getenv("_R_CHECK_EXECUTABLES_EXCLUSIONS_", "TRUE"))
    R_check_permissions <-
        config_val_to_logical(Sys.getenv("_R_CHECK_PERMISSIONS_",
                                         as.character(.Platform$OS.type == "unix")))
    R_check_dot_internal <-
        config_val_to_logical(Sys.getenv("_R_CHECK_DOT_INTERNAL_", "TRUE"))
    R_check_depr_def <-
        config_val_to_logical(Sys.getenv("_R_CHECK_DEPRECATED_DEFUNCT_", "FALSE"))
    R_check_Rd_contents <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_CONTENTS_", "TRUE"))
    R_check_ascii_code <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_ASCII_CODE_", "TRUE"))
    R_check_ascii_data <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_ASCII_DATA_", "TRUE"))
     R_check_compact_data <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_COMPACT_DATA_", "TRUE"))
    R_check_vc_dirs <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_VC_DIRS_", "FALSE"))
    R_check_pkg_sizes <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_PKG_SIZES_", "TRUE")) &&
        nzchar(Sys.which("du"))
    R_check_doc_sizes <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_DOC_SIZES_", "TRUE")) &&
        nzchar(Sys.which(Sys.getenv("R_QPDF", "qpdf")))
    R_check_doc_sizes2 <-
    	config_val_to_logical(Sys.getenv("_R_CHECK_DOC_SIZES2_", "FALSE"))
    R_check_code_assign_to_globalenv <-
        config_val_to_logical(Sys.getenv("_R_CHECK_CODE_ASSIGN_TO_GLOBALENV_",
                                         "FALSE"))
    R_check_code_attach <-
        config_val_to_logical(Sys.getenv("_R_CHECK_CODE_ATTACH_", "FALSE"))
    R_check_code_data_into_globalenv <-
        config_val_to_logical(Sys.getenv("_R_CHECK_CODE_DATA_INTO_GLOBALENV_",
                                         "FALSE"))

    ## Only relevant when the package is loaded, thus installed.
    R_check_suppress_RandR_message <-
        do_install && config_val_to_logical(Sys.getenv("_R_CHECK_SUPPRESS_RANDR_MESSAGE_", "TRUE"))
    R_check_force_suggests <-
        config_val_to_logical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_", "TRUE"))
    R_check_skip_tests_arch <-
        unlist(strsplit(Sys.getenv("_R_CHECK_SKIP_TESTS_ARCH_"), ",")[[1]])
    R_check_skip_examples_arch <-
        unlist(strsplit(Sys.getenv("_R_CHECK_SKIP_EXAMPLES_ARCH_"), ",")[[1]])
    R_check_skip_arch <-
        unlist(strsplit(Sys.getenv("_R_CHECK_SKIP_ARCH_"), ",")[[1]])
    R_check_unsafe_calls <-
        config_val_to_logical(Sys.getenv("_R_CHECK_UNSAFE_CALLS_", "TRUE"))
    R_check_depends_only <-
        config_val_to_logical(Sys.getenv("_R_CHECK_DEPENDS_ONLY_", "FALSE"))
    R_check_suggests_only <-
        config_val_to_logical(Sys.getenv("_R_CHECK_SUGGESTS_ONLY_", "FALSE"))

    if (!nzchar(check_subdirs)) check_subdirs <- R_check_subdirs_strict

    if (as_cran) {
        if (extra_arch) {
            message("'--as-cran' turns off '--extra-arch'")
            extra_arch <- FALSE
        }
        Sys.setenv("_R_CHECK_TIMINGS_" = "10")
        Sys.setenv("_R_CHECK_INSTALL_DEPENDS_" = "TRUE")
        Sys.setenv("_R_CHECK_NO_RECOMMENDED_" = "TRUE")
        Sys.setenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = "TRUE")
        Sys.setenv("_R_CHECK_DOT_FIRSTLIB_" = "TRUE")
        R_check_vc_dirs <- TRUE
        R_check_executables_exclusions <- FALSE
        R_check_doc_sizes2 <- TRUE
        R_check_suggests_only <- TRUE
        R_check_code_assign_to_globalenv <- TRUE
        R_check_code_attach <- TRUE
        R_check_code_data_into_globalenv <- TRUE
        R_check_depr_def <- TRUE
    } else {
        ## do it this way so that INSTALL produces symbols.rds
        ## when called from check but not in general.
        if(is.na(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", NA)))
            Sys.setenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = "TRUE")
    }


    if (extra_arch)
        R_check_Rd_contents <- R_check_all_non_ISO_C <-
            R_check_Rd_xrefs <- R_check_use_codetools <- R_check_Rd_style <-
                R_check_executables <- R_check_permissions <-
                    R_check_dot_internal <- R_check_ascii_code <-
                    	R_check_ascii_data <- R_check_compact_data <-
                            R_check_pkg_sizes <- R_check_doc_sizes <-
                                R_check_doc_sizes2 <-
                                    R_check_unsafe_calls <- FALSE

    startdir <- getwd()
    if (is.null(startdir))
        stop("current working directory cannot be ascertained")
    if (!nzchar(outdir)) outdir <- startdir
    setwd(outdir)
    outdir <- getwd()
    setwd(startdir)

    R_LIBS <- Sys.getenv("R_LIBS")
    arg_libdir <- libdir
    if (nzchar(libdir)) {
        setwd(libdir)
        libdir <- getwd()
        Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
        setwd(startdir)
    }

    ## all the analysis code is run with --slave
    ## examples and tests are not.
    R_opts <- "--vanilla"
    R_opts2 <- "--vanilla --slave"
    ## do run Renviron.site for some multiarch runs
    ## We set R_ENVIRON_USER to skip .Renviron files.
    R_opts3 <- "--no-site-file --no-init-file --no-save --no-restore"
    R_opts4 <- "--no-site-file --no-init-file --no-save --no-restore --slave"
    env0 <- if(WINDOWS) "R_ENVIRON_USER='no_such_file'" else "R_ENVIRON_USER=''"

    msg_DESCRIPTION <- c("See the information on DESCRIPTION files",
                         " in the chapter 'Creating R packages'",
                         " of the 'Writing R Extensions' manual.\n")

    if (!length(pkgs)) {
        message("Error: no packages were specified")
        do_exit(1L)
    }

    ## This is the main loop over all packages to be checked.
    for (pkg in pkgs) {
        ## pkg should be the path to the package root source
        ## directory, either absolute or relative to startdir.
        ## As from 2.1.0 it can also be a tarball

        ## $pkgdir is the corresponding absolute path.
        ## pkgname0 is the name of the top-level directory
        ## (and often the name of the package).
        setwd(startdir)
        pkg <- sub("/$", "", pkg)       # strip any trailing '/'
        pkgname0 <- basename(pkg)
        is_ascii <- FALSE

        thispkg_subdirs <- check_subdirs
        ## is this a tar archive?
        if (dir.exists(pkg)) {
            istar <- FALSE
            if (thispkg_subdirs == "default") thispkg_subdirs <- "no"
        } else if (file.exists(pkg)) {
            istar <- TRUE
            if (thispkg_subdirs == "default") thispkg_subdirs <- "yes-maybe"
            pkgname0 <- sub("\\.(tar\\.gz|tgz|tar\\.bz2|tar\\.xz)$", "", pkgname0)
            pkgname0 <- sub("_[0-9.-]*$", "", pkgname0)
        } else {
            warning(sQuote(pkg), " is neither a file nor directory, skipping\n",
                    domain = NA, call. = FALSE, immediate. = TRUE)
            next
        }
        pkgoutdir <- file.path(outdir, paste(pkgname0, "Rcheck", sep = "."))
        if (clean && dir.exists(pkgoutdir)) {
            unlink(pkgoutdir, recursive = TRUE)
            if(WINDOWS) Sys.sleep(0.5) # allow for antivirus interference
        }
        dir.create(pkgoutdir, mode = "0755")
        if (!dir.exists(pkgoutdir)) {
            message(sprintf("ERROR: cannot create check dir %s", sQuote(pkgoutdir)))
            do_exit(1L)
        }
        Log <- newLog(file.path(pkgoutdir, "00check.log"))
        if (istar) {
            dir <- file.path(pkgoutdir, "00_pkg_src")
            dir.create(dir, mode = "0755")
            if (!dir.exists(dir)) {
                errorLog(Log, sprintf("cannot create %s", sQuote(dir)))
                do_exit(1L)
            }
            ## force the use of internal untar unless over-ridden
            ## so e.g. .tar.xz works everywhere
            if (untar(pkg, exdir = dir,
                      tar =  Sys.getenv("R_INSTALL_TAR", "internal"))) {
                errorLog(Log, sprintf("cannot unpack %s", sQuote(pkg)))
                do_exit(1L)
            }
            ## this assumes foo_x.y.tar.gz unpacks to foo, but we are about
            ## to test that.
            pkg <- file.path(dir, pkgname0)
        }
        if (!dir.exists(pkg))
            stop(gettextf("package directory %s does not exist",
                          sQuote(pkg)), domain = NA)
        setwd(pkg)
        pkgdir <- getwd()
        thispkg_src_subdirs <- thispkg_subdirs
        if (thispkg_src_subdirs == "yes-maybe") {
            ## now see if there is a 'configure' file
            ## configure files are only used if executable, but
            ## -x is always false on Windows.
            if (WINDOWS) {
                if (file_test("-f", "configure")) thispkg_src_subdirs <- "no"
            } else {
                if (file_test("-x", "configure")) thispkg_src_subdirs <- "no"
            }
        }
        setwd(startdir)

        messageLog(Log, "using log directory ", sQuote(pkgoutdir))
        messageLog(Log, "using ", R.version.string)
        messageLog(Log, "using platform: ", R.version$platform,
                   " (", 8*.Machine$sizeof.pointer, "-bit)")
        charset <-
            if (l10n_info()[["UTF-8"]]) "UTF-8" else utils::localeToCharset()
        messageLog(Log, "using session charset: ", charset)
        is_ascii <- charset == "ASCII"

        ## report options used
        if (!do_codoc) opts <- c(opts, "--no-codoc")
        if (!do_examples && !spec_install) opts <- c(opts, "--no-examples")
        if (!do_tests && !spec_install) opts <- c(opts, "--no-tests")
        if (!do_vignettes && !spec_install) opts <- c(opts, "--no-vignettes")
        if (!do_build_vignettes && !spec_install)
            opts <- c(opts, "--no-build-vignettes")
        if (use_gct) opts <- c(opts, "--use-gct")
        if (use_valgrind) opts <- c(opts, "--use-valgrind")
        if (length(opts) > 1L)
            messageLog(Log, "using options ", sQuote(paste(opts, collapse=" ")))
        else if (length(opts) == 1L)
            messageLog(Log, "using option ", sQuote(opts))

        if (!nzchar(libdir)) {
            libdir <- pkgoutdir
            Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
        }
        if (WINDOWS && grepl(" ", libdir)) # need to avoid spaces in libdir
            libdir <- gsub("\\", "/", shortPathName(libdir), fixed = TRUE)

        ## Package sources from the R distribution are special.  They
        ## have a 'DESCRIPTION.in' file (instead of 'DESCRIPTION'),
        ## with Version and License fields containing '@VERSION@' for
        ## substitution by configure.  Earlier bundles had packages
        ## containing DESCRIPTIION.in, hence the extra check for
        ## Makefile.in.

        is_base_pkg <- is_rec_pkg <- FALSE
        if (file.exists(f <- file.path(pkgdir, "DESCRIPTION.in")) &&
            file.exists(file.path(pkgdir, "Makefile.in"))) {
            desc <- try(read.dcf(f))
            if (inherits(desc, "try-error") || !length(desc)) {
                resultLog(Log, "EXISTS but not correct format")
                do_exit(1L)
            }
            desc <- desc[1L, ]
            if (desc["Priority"] == "base") {
                messageLog(Log, "looks like ", sQuote(pkgname0),
                           " is a base package")
                messageLog(Log, "skipping installation test")
                is_base_pkg <- TRUE
                pkgname <- desc["Package"] # should be same as pkgname0
            }
        }

        this_multiarch <- multiarch
        if (!is_base_pkg) {
            desc <- check_description()
            pkgname <- desc["Package"]
            is_rec_pkg <- desc["Priority"] %in% "recommended"

            ## Check if we have any hope of installing
            OS_type <- desc["OS_type"]
            if (do_install && !is.na(OS_type)) {
                if (WINDOWS && OS_type != "windows") {
                    messageLog(Log, "will not attempt to install this package on Windows")
                    do_install <- FALSE
                }
                if (!WINDOWS && OS_type == "windows") {
                    messageLog(Log, "this is a Windows-only package, skipping installation")
                    do_install <- FALSE
                }
            }

            check_incoming <- Sys.getenv("_R_CHECK_CRAN_INCOMING_", "NA")
            check_incoming <- if(check_incoming == "NA") as_cran else {
                config_val_to_logical(check_incoming)
            }
            if (check_incoming) check_CRAN_incoming()

            ## <NOTE>
            ## We want to check for dependencies early, since missing
            ## dependencies may make installation fail, and in any case we
            ## give up if they are missing.  But we don't check them if
            ## we are not going to install and hence not run any code.
            ## </NOTE>
            if (do_install) check_dependencies()

            check_sources()
            checkingLog(Log, "if there is a namespace")
            if (file.exists(file.path(pkgdir, "NAMESPACE")))
                resultLog(Log, "OK")
            else if (dir.exists(file.path(pkgdir, "R"))) {
                warningLog(Log)
                wrapLog("All packages need a namespace as from R 3.0.0.\n",
                        "R CMD build will produce a suitable starting point,",
                        "but it is better to handcraft a NAMESPACE file.")
            } else {
                noteLog(Log)
                wrapLog("Packages without R code can be instaled without",
                        "a NAMESPACE file, but it is cleaner to add",
                        "an empty one.")
            }

            ## we need to do this before installation
            if (R_check_executables) check_executables()

            check_dot_files(check_incoming)

	    setwd(pkgdir)
            allfiles <- check_file_names()
            if (R_check_permissions) check_permissions(allfiles)
	    setwd(startdir)

            if (do_install) {
                check_install()
                if(R_check_pkg_sizes) check_install_sizes()
            }
            if (multiarch) {
                if (force_multiarch) inst_archs <- archs
                else {
                    ## check which architectures this package is installed for
                    if (dir.exists(dd <- file.path(libdir, pkgname, "libs"))) {
                        inst_archs <- dir(dd)
                        ## xlsReadWrite has spurious subdir 'template'
                        inst_archs <- inst_archs[inst_archs %in% archs]
                        if (!identical(inst_archs, archs)) {
                            if (length(inst_archs) > 1)
				printLog(Log, "NB: this package is only installed for sub-architectures ",
					 paste(sQuote(inst_archs), collapse=", "), "\n")
			    else {
				printLog(Log, "NB: this package is only installed for sub-architecture ",
					 sQuote(inst_archs), "\n")
                                if(inst_archs == .Platform$r_arch)
                                    this_multiarch <- FALSE
                            }
                        }
                    } else this_multiarch <- FALSE  # no compiled code
                }
                if (this_multiarch && length(R_check_skip_arch))
                    inst_archs <- inst_archs[!(inst_archs %in% R_check_skip_arch)]
            }
        }   ## end of if (!is_base_pkg)

        elibs <- if(is_base_pkg) character()
        else if(R_check_depends_only)
            setRlibs(pkgdir = pkgdir, libdir = libdir)
        else if(R_check_suggests_only)
            setRlibs(pkgdir = pkgdir, libdir = libdir, suggests = TRUE)
        else character()

        setwd(startdir)
        check_pkg(pkgdir, pkgname, pkgoutdir, startdir, libdir, desc,
                  is_base_pkg, is_rec_pkg, thispkg_subdirs, extra_arch)
        if (!extra_arch && do_manual) {
            setwd(pkgoutdir)
            instdir <- file.path(libdir, pkgname)
            if (dir.exists(file.path(instdir, "help")))
                check_pkg_manual(instdir, desc["Package"])
            else
                check_pkg_manual(pkgdir, desc["Package"])
        }

        if ((Log$warnings > 0L) || (Log$notes > 0L)) {
            message(""); summaryLog(Log)
        }

        closeLog(Log)
        message("")

    } ## end for (pkg in pkgs)

} ## end{ .check_packages }

.format_lines_with_indent <-
function(x)
    paste0("  ", x, collapse = "\n")
    ## Hard-wire indent of 2 for now.

### Local variables:
### mode: R
### page-delimiter: "^###[#-]"
### End:
