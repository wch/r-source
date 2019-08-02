#  File src/library/tools/R/check.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

###- R based engine for R CMD check

## R developers can use this to debug the function by running it
## directly as tools:::.check_packages(args), where the args should
## be what commandArgs(TRUE) would return, that is a character vector
## of (space-delimited) terms that would be passed to R CMD check.


get_timeout <- function(tlim)
{
    if(is.character(tlim)) {
        if(endsWith(tlim, "m"))
            tlim <- 60*as.numeric(sub("m$", "", tlim))
        else if(endsWith(tlim, "h"))
            tlim <- 3600*as.numeric(sub("h$", "", tlim))
        else if(endsWith(tlim, "s"))  # for completeness, like GNU timeout.
            tlim <- as.numeric(sub("s$", "", tlim))
    }
    tlim <- as.numeric(tlim)
    if(is.na(tlim) || tlim < 0) tlim <- 0
    tlim
}

report_timeout <- function(tlim)
{
    tlim <- trunc(tlim)
    if (tlim >= 3600)
        warning(gettextf("elapsed-time limit of %g %s reached for sub-process",
                         round(tlim/3600, 1L), "hours"),
                domain = NA, call. = FALSE)
    else if (tlim >= 60)
        warning(gettextf("elapsed-time limit of %g %s reached for sub-process",
                         round(tlim/60, 1L), "minutes"),
                domain = NA, call. = FALSE)
    else
        warning(gettextf("elapsed-time limit of %g %s reached for sub-process",
                         tlim, "seconds"),
                domain = NA, call. = FALSE)
}

## Find serialized objects (for load() and for readRDS()) in "allfiles" and
## report serialization versions (0 means not a serialized object,
## 1 means either version-1 or not a serialized object, 2 and more means
## serialized object of that version).
##
## These are most commonly data/*.{Rdata,rda}, R/sysdata.rda files,
## and build/vignette.rds
## But packages have other .rds files in many places.
## Despite its name, build/partial.rdb is created by saveRDS.
##
get_serialization_version <- function(allfiles)
{
    getVerLoad <- function(file)
    {
        ## This could look at the magic number, but for a short
        ## while version 3 files were produced with a version-2
        ## magic number. loadInfoFromConn2 checks if the magic number
        ## is sensible.
        con <- gzfile(file, "rb"); on.exit(close(con))
        ## The .Internal gives an error on version-1 files
        ## (and on non-serialized files)
        tryCatch(.Internal(loadInfoFromConn2(con))$version,
                 error = function(e) 1L)
    }
    getVerSer <- function(file)
    {
        con <- gzfile(file, "rb"); on.exit(close(con))
        ## In case this is not a serialized object
        tryCatch(.Internal(serializeInfoFromConn(con))$version,
                 error = function(e) 0L)
    }
    loadfiles <- grep("[.](rda|RData|Rdata|rdata|Rda|bam|Rbin)$",
                      allfiles, value = TRUE)
    serfiles <- c(grep("[.](rds|RDS|Rds|rdx)$", allfiles, value = TRUE),
                  grep("build/partial[.]rdb$", allfiles, value = TRUE))
    vers1 <- sapply(loadfiles, getVerLoad)
    vers2 <- sapply(serfiles, getVerSer)
    c(vers1, vers2)
}

## Used for INSTALL and Rd2pdf
run_Rcmd <- function(args, out = "", env = "", timeout = 0)
{
    status <- if(.Platform$OS.type == "windows")
        system2(file.path(R.home("bin"), "Rcmd.exe"), args, out, out,
                timeout = get_timeout(timeout))
    else
        system2(file.path(R.home("bin"), "R"), c("CMD", args), out, out,
                env = env, timeout = get_timeout(timeout))
    if(identical(status, 124L)) report_timeout(timeout)
    status
}

R_runR <- function(cmd = NULL, Ropts = "", env = "",
                   stdout = TRUE, stderr = TRUE, stdin = NULL,
                   arch = "", timeout = 0)
{
    timeout <- get_timeout(timeout)
    out <- if (.Platform$OS.type == "windows") {
        ## workaround Windows problem with input = cmd
        if (!is.null(cmd)) {
            ## In principle this should escape \
           Rin <- tempfile("Rin"); on.exit(unlink(Rin)); writeLines(cmd, Rin)
        } else Rin <- stdin
        suppressWarnings(system2(if(nzchar(arch)) file.path(R.home(), "bin", arch, "Rterm.exe")
                                 else file.path(R.home("bin"), "Rterm.exe"),
                                 c(Ropts, paste("-f", Rin)), stdout, stderr,
                                 env = env, timeout = timeout))
    } else {
        suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                 c(if(nzchar(arch)) paste0("--arch=", arch), Ropts),
                                 stdout, stderr, stdin, input = cmd, env = env,
                                 timeout = timeout))
    }
    if(identical(out, 124L) || identical(attr(out, "status"), 124L))
        report_timeout(timeout)
    out
}

setRlibs <-
    function(lib0 = "", pkgdir = ".", suggests = FALSE, libdir = NULL,
             self = FALSE, self2 = TRUE, quote = FALSE, LinkingTo = FALSE,
             tests = FALSE)
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
        recommended <- .get_standard_package_names()$recommended
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
        recommended <- recommended %w/o% exceptions
        for(pkg in recommended) {
            if(pkg == thispkg) next
            dir.create(pd <- file.path(tmplib, pkg))
            ## some people remove recommended packages ....
            f <- file.path(.Library, pkg, "DESCRIPTION")
            if(file.exists(f)) file.copy(f, pd)
            ## to make sure find.package throws an error:
            close(file(file.path(pd, "dummy_for_check"), "w"))
        }
    }

    sug <- if (suggests)  names(pi$Suggests)
    else {
        ## we always need to be able to recognise 'vignettes'
        VB <- unname(pi$DESCRIPTION["VignetteBuilder"])
        sug <- if(is.na(VB)) character()
        else {
            VB <- unlist(strsplit(VB, ","))
            sug <- unique(gsub('[[:space:]]', '', VB))
            ## too many people forgot this, but it will never get fixed if made an exception.
            ## if("knitr" %in% VB) sug <- c(sug, "rmarkdown")
            sug
        }
        if(tests) ## we need the test-suite package available
            c(sug, intersect(names(pi$Suggests), c("RUnit", "testthat")))
        else sug
    }
    deps <- unique(c(names(pi$Depends), names(pi$Imports),
                     if(LinkingTo) names(pi$LinkingTo),
                     sug))
    if(length(libdir) && self2) flink(file.path(libdir, thispkg), tmplib)
    ## .Library is not necessarily canonical, but the .libPaths version is.
    lp <- .libPaths()
    poss <- c(lp[length(lp)], .Library)
    already <- thispkg
    more <- unique(deps %w/o% already) # should not depend on itself ...
    while(length(more)) {
        m0 <- more; more <- character()
        for (pkg in m0) {
            if (test_recommended) {
                if (pkg %in% recommended) unlink(file.path(tmplib, pkg), TRUE)
                ## hard-code dependencies for now.
                if (pkg == "mgcv")
                    unlink(file.path(tmplib, c("Matrix", "lattice", "nlme") %w/o% thispkg), TRUE)
                if (pkg == "Matrix")
                    unlink(file.path(tmplib, "lattice" %w/o% thispkg), TRUE)
                if (pkg == "class")
                    unlink(file.path(tmplib, "MASS" %w/o% thispkg), TRUE)
                if (pkg == "nlme")
                    unlink(file.path(tmplib, "lattice" %w/o% thispkg), TRUE)
            }
            where <- find.package(pkg, quiet = TRUE)
            if(length(where)) {
                if (dirname(where) %notin% poss)
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
        more <- unique(more %w/o% already)
    }
    if (self) flink(normalizePath(pkgdir), tmplib)
    # print(dir(tmplib))
    rlibs <- tmplib
    if (nzchar(lib0)) rlibs <- c(lib0, rlibs)
    rlibs <- paste(rlibs, collapse = .Platform$path.sep)
    if(quote) rlibs <- shQuote(rlibs)
    c(paste0("R_LIBS=", rlibs),
      if(WINDOWS) " R_ENVIRON_USER='no_such_file'" else "R_ENVIRON_USER=''",
      if(WINDOWS) " R_LIBS_USER='no_such_dir'" else "R_LIBS_USER=''",
      " R_LIBS_SITE='no_such_dir'")
}

add_dummies <- function(dir, Log)
{
    dir1 <- file.path(dir, "R_check_bin")
    if (dir.exists(file.path(dir1))) {
        messageLog(Log, "directory ", sQuote(dir1), " already exists")
        return()
    }
    dir.create(dir1)
    if (!dir.exists(dir1)) {
        messageLog(Log, "creation of directory ", sQuote(dir1), " failed")
        return()
    }
    Sys.setenv(PATH = env_path(dir1, Sys.getenv("PATH")))
    if(.Platform$OS.type != "windows") {
        writeLines(c('echo "\'R\' should not be used without a path -- see par. 1.6 of the manual"',
                     'exit 1'),
                   p1 <- file.path(dir1, "R"))
        writeLines(c('echo "\'Rscript\' should not be used without a path -- see par. 1.6 of the manual"',
                     'exit 1'),
                   p2 <- file.path(dir1, "Rscript"))
        Sys.chmod(c(p1, p2), "0755")
    } else {
        ## currently untested
        writeLines(c('@ECHO OFF',
                     'echo "\'R\' should not be used without a path -- see par. 1.6 of the manual"',
                     'exit /b 1'),
                   p1 <- file.path(dir1, "R.bat"))
        writeLines(c('@ECHO OFF',
                     'echo "\'Rscript\' should not be used without a path -- see par. 1.6 of the manual"',
                     'exit /b 1'),
                   p2 <- file.path(dir1, "Rscript.bat"))
   }
}

###- The main function for "R CMD check"
.check_packages <- function(args = NULL, no.q = interactive(), warnOption = 1)
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

  ## used for R_runR2 and
  ## .check_package_description
  ## .check_package_description_encoding
  ## .check_package_license
  ## .check_demo_index
  ## .check_vignette_index
  ## .check_package_subdirs
  ## .check_citation
  ## .check_package_ASCII_code
  ## .check_package_code_syntax
  ## .check_packages_used
  ## .check_package_code_shlib
  ## .check_package_code_startup_functions
  ## .check_package_code_unload_functions
  ## .check_package_code_tampers
  ## .check_package_code_assign_to_globalenv
  ## .check_package_code_attach
  ## .check_package_code_data_into_globalenv
  ## .check_package_parseRd
  ## .check_Rd_metadata
  ## .check_Rd_line_widths
  ## .check_Rd_xrefs
  ## .check_Rd_contents
  ## .check_package_datasets
  ## .check_package_compact_datasets
  ## .check_package_compact_sysdata
  ## .check_make_vars
  ## check_compiled_code
  ## Checking loading
  ## Rdiff on reference output
  ## Creating -Ex.R
  ## Running examples (run_one_arch)
  ## .runPackageTestsR
  ## .run_one_vignette
  ## buildVignettes

    def_tlim <- get_timeout(Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_"))
    R_runR0 <- function(..., timeout = def_tlim)
            R_runR(..., timeout = timeout)

    ## Used for
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
        status <- if(WINDOWS) {
            function(cmd,
                     env = "R_DEFAULT_PACKAGES=utils,grDevices,graphics,stats",
                     timeout = 0)
                {
                    out <- R_runR(cmd, R_opts2, env, timeout = timeout)
                    ## pesky gdata ....
                    filtergrep("^(ftype: not found|File type)", out)
                }
        } else
            function(cmd,
                     env = "R_DEFAULT_PACKAGES='utils,grDevices,graphics,stats'",
                     timeout = 0)
            {
                out <- R_runR(cmd, R_opts2, env, timeout = timeout)
                ## htmltools produced non-UTF-8 output in Dec 2015
                if (R_check_suppress_RandR_message)
                    out <- filtergrep('^Xlib: *extension "RANDR" missing on display',
                                      out, useBytes = TRUE)
                filtergrep("^OMP:", out)  ## LLVM's OpenMP with limits set
            }

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
    print_time0 <- function(t1, t2)
    {
        td <- t2 - t1
        if(td[3L] < td0) return(character())
        td2 <- if (td[3L] > 600) {
            td <- td/60
            if(WINDOWS) sprintf(" [%dm]", round(td[3L]))
            else sprintf(" [%dm/%dm]", round(sum(td[-3L])), round(td[3L]))
        } else {
            if(WINDOWS) sprintf(" [%ds]", round(td[3L]))
            else sprintf(" [%ds/%ds]", round(sum(td[-3L])), round(td[3L]))
        }
        td2
    }

    parse_description_field <- function(desc, field, default)
        str_parse_logic(desc[field], default=default)

    check_pkg <- function(pkg, pkgname, pkgoutdir, startdir, libdir, desc,
                          is_base_pkg, is_rec_pkg, subdirs, extra_arch)
    {
        Sys.setenv("_R_CHECK_PACKAGE_NAME_" = pkgname)
        on.exit(Sys.unsetenv("_R_CHECK_PACKAGE_NAME_"))

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
            summaryLog(Log)
            do_exit(1L)
        }

        if (config_val_to_logical(Sys.getenv("_R_CHECK_FUTURE_FILE_TIMESTAMPS_",
                                             "FALSE"))) {
            now_local <- Sys.time()
            any <- FALSE
            checkingLog(Log, "for future file timestamps")
            ## allow skipping clock check on CRAN incoming systems
            if(config_val_to_logical(Sys.getenv("_R_CHECK_SYSTEM_CLOCK_", "TRUE"))) {
                ## First check time on system running 'check',
                ## by reading an external source in UTC
                now <- tryCatch({
                    foo <- suppressWarnings(readLines("http://worldtimeapi.org/api/timezone/UTC",
                                                      warn = FALSE))
                    ## gives time in sub-secs
                    as.POSIXct(gsub(".*\"datetime\":\"([^Z]*).*", "\\1", foo),
                               "UTC", "%Y-%m-%dT%H:%M:%S")
                }, error = function(e) NA)
                if (is.na(now)) {
                    now <- tryCatch({
                        foo <- suppressWarnings(readLines("http://worldclockapi.com/api/json/utc/now",
                                                          warn = FALSE))
                        ## gives time in mins
                        as.POSIXct(gsub(".*\"currentDateTime\":\"([^Z]*).*", "\\1", foo),
                                   "UTC", "%Y-%m-%dT%H:%M")
                    }, error = function(e) NA)
                }
                if (is.na(now)) {
                    any <- TRUE
                    noteLog(Log, "unable to verify current time")
                } else {
                    ## 5 mins leeway seems a reasonable compromise
                    if (abs(unclass(now_local) - unclass(now)) > 300) {
                        any <- TRUE
                        fmt <- "%Y-%m-%d %H:%M"
                        errorLog(Log, "This system is set to the wrong time: please correct")
                        now0 <- sprintf("  correct: %s (UTC)\n",
                                        format(now, fmt, tz = "UTC"))
                        local0 <- sprintf("   system: %s (UTC)\n",
                                          format(now_local, fmt, tz = "UTC"))
                        printLog0(Log, local0, now0)
                        summaryLog(Log)
                        do_exit(1L)
                    }
                }
            }

            ## Both files and directories get timestamps in the
            ## tarball, so future stamps give annoying messages.
            files <- list.files(all.files = TRUE, full.names = TRUE,
                                include.dirs = TRUE)
            files <- setdiff(files, c("./.", "./.."))
            ftimes <- file.mtime(files)
            ## Default 5 mins leeway is to allow for clock-skew from a file server.
            leeway <- Sys.getenv("_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_", "5m")
            leeway <- get_timeout(leeway)
            if (leeway <= 0) leeway <- 600
            fu <- unclass(ftimes) > unclass(now_local) + leeway
            if (any(fu)) {
                if (!any) warningLog(Log)
                any <- TRUE
                wrong <- sub("^[.]/", "", files[fu])
                printLog(Log, "Files with future time stamps:\n")
                printLog0(Log, .format_lines_with_indent(wrong), "\n")
            }
            if(!any) resultLog(Log, "OK")
        }

        haveR <- dir.exists("R") && !extra_arch

        if (!extra_arch) {
            if(dir.exists("build")) check_build()
            db <- check_meta()  # Check DESCRIPTION meta-information.
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

        if (!is_base_pkg && !extra_arch) check_src_dir(desc)

        check_src()
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
        if (dir.exists("vignettes")) check_vign_contents(ignore_vignettes)
        if (!ignore_vignettes) {
            if (dir.exists("inst/doc") && !dir.exists("vignettes")) {
                pattern <- vignetteEngine("Sweave")$pattern
                sources <- setdiff(list.files(file.path("inst", "doc"),
                                              pattern = pattern),
                                   list.files("vignettes", pattern = pattern))
                buildPkgs <- .get_package_metadata(".")["VignetteBuilder"]
                if (!is.na(buildPkgs)) {
                    buildPkgs <- unlist(strsplit(buildPkgs, ","))
                    buildPkgs <- unique(gsub('[[:space:]]', '', buildPkgs))
                    engineList <- vignetteEngine(package = buildPkgs)
                    for(nm in names(engineList)) {
                        pattern <- engineList[[nm]]$pattern
                        sources <- c(sources,
                                     setdiff(list.files(file.path("inst", "doc"),
                                                        pattern = pattern),
                                             list.files("vignettes", pattern = pattern)))
                    }
                }
                sources <- unique(sources)
                if(length(sources)) {
                    checkingLog(Log, "for old-style vignette sources")
                    msg <- c("Vignette sources only in 'inst/doc':",
                             strwrap(paste(sQuote(sources), collapse = ", "),
                                     indent = 2L, exdent = 2L),
                             "A 'vignettes' directory is required as from R 3.1.0",
                             "and these will not be indexed nor checked")
                    ## warning or error eventually
                    noteLog(Log, paste(msg, collapse = "\n"))
                }
            }
        }

        setwd(pkgoutdir)

        ## Run the examples: this will be skipped if installation was
        if (dir.exists(file.path(libdir, pkgname, "help"))) {
            run_examples()
        } else if (dir.exists(file.path(pkgdir, "man"))) {
            checkingLog(Log, "examples")
            resultLog(Log, "SKIPPED")
        }

        ## Run the package-specific tests.
        tests_dir <- file.path(pkgdir, test_dir)
        if (test_dir != "tests" && !dir.exists(tests_dir)) {
            warningLog(Log)
            printLog(Log, "directory ", sQuote(test_dir), " not found\n")
        }
        if (dir.exists(tests_dir) && # trackObjs has only *.Rin
            length(dir(tests_dir, pattern = "\\.(R|r|Rin)$")))
            run_tests()

        ## Check package vignettes.
        setwd(pkgoutdir)
        if (!ignore_vignettes) run_vignettes(desc)

    } ## end{ check_pkg }

    check_file_names <- function()
    {
        ## Check for portable file names.
        checkingLog(Log, "for portable file names")

        ## Build list of exclude patterns.
        ignore <- get_exclude_patterns()
        ignore_file <- ".Rbuildignore"
        if (ignore_file %in% dir())
            ignore <- c(ignore, readLines(ignore_file))

        ## Ensure that the names of the files in the package are valid
        ## for at least the supported OS types.  Under Unix, we
        ## definitely cannot have '/'.  Under Windows, the control
        ## characters as well as " * : < > ? \ | (i.e., ASCII
        ## characters 1 to 31 and 34, 36, 58, 60, 62, 63, 92, and 124)
        ## are or can be invalid.  (In addition, one cannot have
        ## one-character file names consisting of just ' ', '.', or
        ## '~'., and '~' has a special meaning for 8.3 short file
        ## names).

        ## Based on information by Uwe Ligges, Duncan Murdoch, and
        ## Brian Ripley: see also
        ## http://msdn.microsoft.com/en-us/library/aa365247%28VS.85%29.aspx

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
        allfiles <- filtergrep(ignore_re, allfiles)
        bad_files <- allfiles[grepl("[[:cntrl:]\"*/:<>?\\|]",
                                    basename(allfiles))]
        is_man <- endsWith(dirname(allfiles), "man")
        bad <- vapply(strsplit(basename(allfiles[is_man]), ""),
                      function(x) any(grepl("[^ -~]|%", x)),
                      NA)
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
            printLog0(Log, .format_lines_with_indent(bad_files), "\n")
            wrapLog("These are not valid file names",
                    "on all R platforms.\n",
                    "Please rename the files and try again.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            maybe_exit(1L)
        }

        ## Next check for name clashes on case-insensitive file systems
        ## (that is on Windows and (by default) on macOS).

        dups <- unique(allfiles[duplicated(tolower(allfiles))])
        if (nb <- length(dups)) {
            errorLog(Log)
            wrapLog("Found the following files with duplicate lower-cased file names:\n")
            printLog0(Log, .format_lines_with_indent(dups), "\n")
            wrapLog("File names must not differ just by case",
                    "to be usable on all R platforms.\n",
                    "Please rename the files and try again.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            maybe_exit(1L)
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
            printLog0(Log, .format_lines_with_indent(non_ASCII_files), "\n")
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
            printLog0(Log, .format_lines_with_indent(bad_files), "\n\n")
            wrapLog("Tarballs are only required to store paths of up to 100",
                    "bytes and cannot store those of more than 256 bytes,",
                    "with restrictions including to 100 bytes for the",
                    "final component.\n",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            if (!OK)
                maybe_exit(1L)
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
            mode <- file.mode(allfiles)
            bad_files <- allfiles[(mode & "400") < as.octmode("400")]
        }
        if(length(alldirs <- unique(dirname(allfiles)))) {
            mode <- file.mode(alldirs)
            bad_files <- c(bad_files,
                           alldirs[(mode & "700") < as.octmode("700")])
        }
        if (length(bad_files)) {
            errorLog(Log)
            wrapLog("Found the following files with insufficient permissions:\n")
            printLog0(Log, .format_lines_with_indent(bad_files), "\n")
            wrapLog("Permissions should be at least 700 for directories and 400 for files.\nPlease fix permissions and try again.\n")
            maybe_exit(1L)
        }

        ## Phase B.  Top-level scripts 'configure' and 'cleanup'
        ## should really be mode at least 500, or they will not be
        ## necessarily be used (or should we rather change *that*?)
        bad_files <- character()
        for (f in c("configure", "cleanup")) {
            if (!file.exists(f)) next
            mode <- file.mode(f)
            if ((mode & "500") < as.octmode("500"))
                bad_files <- c(bad_files, f)
        }
        if (length(bad_files)) {
            warningLog(Log)
            wrapLog("The following files should most likely be executable (for the owner):\n")
            printLog0(Log, .format_lines_with_indent(bad_files), "\n")
            printLog(Log, "Please fix their permissions\n")
        } else resultLog(Log, "OK")
    }

    ## Look for serialized objects, and check their version

    ## We need to so this before installation, which may create
    ## src/symbols.rds in the sources.
    check_serialization <- function(allfiles)
    {
        checkingLog(Log, "serialization versions")
        bad <- get_serialization_version(allfiles)
        bad <- names(bad[bad >= 3L])
        if(length(bad)) {
            msg <- "Found file(s) with version 3 serialization:"
            warningLog(Log, msg)
            printLog0(Log, paste0(.pretty_format(sort(bad)), "\n"))
            wrapLog("Such files are only readable in R >= 3.5.0.\n",
                    "Recreate them with R < 3.5.0 or",
                    "save(version = 2) or saveRDS(version = 2)",
                    "as appropriate")
        } else resultLog(Log, "OK")
    }

    check_meta <- function()
    {
        ## If we just installed the package (via R CMD INSTALL), we already
        ## validated most of the package DESCRIPTION metadata.  Otherwise,
        ## let us be defensive about this ...

        checkingLog(Log, "DESCRIPTION meta-information")
        dfile <- if (is_base_pkg) "DESCRIPTION.in" else "DESCRIPTION"
        any <- FALSE

        ## Check the encoding -- do first as it gives a WARNING
        Rcmd <- sprintf("tools:::.check_package_description_encoding(\"%s\")", dfile)
        out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            warningLog(Log)
            any <- TRUE
            printLog0(Log, paste(out, collapse = "\n"), "\n")
        }

        ## FIXME: this does not need to be run in another process
        ## but that needs conversion to format().
        Rcmd <- sprintf("tools:::.check_package_description(\"%s\", TRUE)",
                        dfile)
        out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            if(any(!grepl("^Malformed (Title|Description)", out))) {
                errorLog(Log)
                printLog0(Log, paste(out, collapse = "\n"), "\n")
                summaryLog(Log)
                do_exit(1L)
            } else {
                noteLog(Log)
                any <- TRUE
                printLog0(Log, paste(out, collapse = "\n"), "\n")
            }
        }

        ## Check the license.
        ## For base packages, the DESCRIPTION.in files have non-canonical
        ##   License: Part of R @VERSION@
        ## entries because these really are a part of R: hence, skip the
        ## check.
        check_license <- if (!is_base_pkg) {
            Check_license <- Sys.getenv("_R_CHECK_LICENSE_", NA_character_)
            if(is.na(Check_license)) {
                ## The check code conditionalizes *output* on _R_CHECK_LICENSE_.
                Sys.setenv('_R_CHECK_LICENSE_' = "TRUE")
                TRUE
            } else config_val_to_logical(Check_license)
        } else FALSE
        if (!isFALSE(check_license)) {
            Rcmd <- sprintf("tools:::.check_package_license(\"%s\", \"%s\")",
                            dfile, pkgdir)
            ## FIXME: this does not need to be run in another process
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if (check_license == "maybe") {
                    if (!any) warningLog(Log)
                } else if(any(startsWith(out, "Standardizable: FALSE"),
                              startsWith(out, "Invalid license file pointers:"))) {
                    if (!any) warningLog(Log)
                } else {
                    if (!any) noteLog(Log)
                }
                any <- TRUE
                printLog0(Log, paste(out, collapse = "\n"), "\n")
            }
        }

        ## .check_package_description() only checks Authors@R "if needed",
        ## and does not check for persons with no valid roles.
        db <- .read_description(dfile)
        if(!is.na(aar <- db["Authors@R"])) {
            lev <- if(check_incoming) 2L else 1L
            out <- .check_package_description_authors_at_R_field(aar,
                                                                 strict = lev)
            if(length(out)) {
                if(!any) noteLog(Log)
                any <- TRUE
                out <- .format_check_package_description_authors_at_R_field_results(out)
                printLog0(Log, paste(out, collapse = "\n"), "\n")
            }
            ## and there might be stale Authors and Maintainer fields
            yorig <- db[c("Author", "Maintainer")]
            if(check_incoming && any(!is.na(yorig))) {
                enc <- db["Encoding"]
                aar <- utils:::.read_authors_at_R_field(aar)
                y <- c(Author =
                       utils:::.format_authors_at_R_field_for_author(aar),
                       Maintainer =
                       utils:::.format_authors_at_R_field_for_maintainer(aar))
                ## ignore formatting as far as possible
                clean_up <- function(x) trimws(gsub("[[:space:]]+", " ", x))
                yorig <- sapply(yorig, clean_up)
                y <- sapply(y, clean_up)
                diff <- y != yorig
                ## <FIXME>
                if(diff[1L]
                   && grepl("https://orcid.org/", y[1L], fixed = TRUE)) {
                    ## Argh.  Might be from using the new ORCID id
                    ## mechanism but having built with R < 3.5.0.
                    ## Let's ignore ...
                    ## Remove eventually.
                    aar$comment <- lapply(aar$comment, unname)
                    y1 <- utils:::.format_authors_at_R_field_for_author(aar)
                    diff[1L] <- clean_up(y1) != yorig[1L]
                }
                ## </FIXME>
                if(any(diff)) {
                    if(!any) noteLog(Log)
                    any <- TRUE
                    if(diff[1L]) {
                        printLog(Log, "Author field differs from that derived from Authors@R", "\n")
                        printLog(Log, "  Author:    ", sQuote(yorig[1L]), "\n")
                        printLog(Log, "  Authors@R: ", sQuote(y[1L]), "\n")
                        printLog(Log, "\n")
                    }
                    if(diff[2L]) {
                        printLog(Log, "Maintainer field differs from that derived from Authors@R", "\n")
                        printLog(Log, "  Maintainer: ", sQuote(yorig[2L]), "\n")
                        printLog(Log, "  Authors@R:  ", sQuote(y[2L]), "\n")
                        printLog(Log, "\n")
                    }
                }
            }
        }

        if(!is_base_pkg && is.na(db["Packaged"])) {
            if(!any) (noteLog(Log))
            any <- TRUE
            printLog(Log,
                     "Checking should be performed on sources prepared by 'R CMD build'.",
                     "\n")
        }

        if(!is.na(ncomp <- db["NeedsCompilation"])) {
            if (ncomp %notin% c("yes", "no")) {
                if(!any) noteLog(Log)
                any <- TRUE
                printLog(Log, "NeedsCompilation field must take value 'yes' or 'no'", "\n")
            }
            if((ncomp == "no") && dir.exists("src")) {
                if(!any) noteLog(Log)
                any <- TRUE
                printLog(Log, "NeedsCompilation field should likely be 'yes'", "\n")
            }
        }

        ## check for BugReports field added at R 3.4.0
        ## This used to check for empty first line as that
        ## breaks bug.report() in R <= 3.3.2 -- but read.dcf in those
        ## versions adds back the newline.
        if(!is.na(BR <- db["BugReports"])) {
            if (nzchar(BR)) {
                msg <- ""
                ## prior to 3.4.0 this was said to be
                ## 'a URL to which bug reports about the package
                ## should be submitted'
                ## We will take that to mean a http[s]:// URL,
                isURL <- grepl("^https?://[^ ]*$", BR)
                ## As from 3.4.0 bug,report() is able to extract
                ## an email addr.
                if(!isURL) {
                    findEmail <- function(x) {
                        x <- paste(x, collapse = " ")
                        if (grepl("mailto:", x))
                            sub(".*mailto:([^ ]+).*", "\\1", x)
                        else if (grepl("[^<]*<([^>]+)", x))
                            sub("[^<]*<([^>]+)>.*", "\\1", x)
                        else NA_character_
                    }
                    msg <- if (is.na(findEmail(BR))) {
                        if (grepl("(^|.* )[^ ]+@[[:alnum:]._]+", BR))
                            "BugReports field is not a suitable URL but appears to contain an email address\n  not specified by mailto: nor contained in < >"
                        else
                            "BugReports field should be the URL of a single webpage"
                    } else
                        "BugReports field is not a suitable URL but contains an email address\n  which will be used as from R 3.4.0"
                }
            } else {
                msg <- "BugReports field should not be empty"
            }
            if (nzchar(msg)) {
                if(!any) noteLog(Log)
                any <- TRUE
                printLog(Log, msg, "\n")
           }
        }


        out <- format(.check_package_description2(dfile))
        if (length(out)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log, paste(out, collapse = "\n"), "\n")
        }

        ## Dependence on say R >= 3.4.3 when 3.4 is current can
        ## cause problems with revdeps (and did for 3.2.x).
        ## We only check recent ones: maybe previous two
        ## (R-release and R-old-release) while this is R-devel
        Check_R_deps <- Sys.getenv("_R_CHECK_R_DEPENDS_", "FALSE")
        act <- if(Check_R_deps %in% c("note", "warn")) TRUE
               else config_val_to_logical(Check_R_deps)
        if(act) {
            Rver <-.split_description(db, verbose = TRUE)$Rdepends2
            if(length(Rver) && Rver[[1L]]$op == ">=") {
                ver <- unclass(Rver[[1L]]$version)[[1L]]
                thisver <- unclass(getRversion())[[1L]]
                ## needs updating if we ever go to 4.0
                tv <- if(thisver[1L] == 3L) thisver[2L] - 2L else 4L
                if (length(ver) == 3L && ver[3L] != 0 &&
                    ((ver[1L] > 3L) ||
                     (ver[1L] == 3L) && (ver[2L] >= tv) )) {
                    ## This is not quite right: may have NOTE-d above
                    if(Check_R_deps == "warn") warningLog(Log)
                    else if(!any) noteLog(Log)
                    any <- TRUE
                    printLog0(Log,
                              sprintf("Dependence on R version %s not with patchlevel 0\n",
                                      sQuote(format(Rver[[1L]]$version))))
              }
            }
        }
        if (!any) resultLog(Log, "OK")
        return(db)
    }

    check_build <- function()
    {
        ## currently only checks vignettes
        if (ignore_vignettes) return()
        fv <- file.path("build", "vignette.rds")
        if(!file.exists(fv)) return()
        checkingLog(Log, "'build' directory")
        any <- FALSE
        db <- readRDS(fv)
        ## do as CRAN-pack does
        keep <- nzchar(db$PDF)
        if(any(!keep)) {
            if(!any) warningLog(Log)
            any <- TRUE
            msg <- c("Vignette(s) without any output listed in 'build/vignette.rds'",
                     strwrap(sQuote(db$file[!keep]), indent = 2L, exdent = 2L))
            printLog0(Log, paste(msg, collapse = "\n"), "\n")
        }
        pdfs <- file.path("inst", "doc", db[keep, ]$PDF)
        missing <- !file.exists(pdfs)
        if(any(missing)) {
            if(!any) warningLog(Log)
            any <- TRUE
            msg <- c("Output(s) listed in 'build/vignette.rds' but not in package:",
                     strwrap(sQuote(pdfs[missing]), indent = 2L, exdent = 2L))
            printLog0(Log, paste(msg, collapse = "\n"), "\n")
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
            printLog0(Log, .format_lines_with_indent(topfiles), "\n")
            wrapLog("These files are defunct.",
                    "See manual 'Writing R Extensions'.\n")
        }

        ## Look for sources for an autoconf-generated 'configure' file.
        ## (autoconf allows TEMPLATE-FILE to be specified, but no one does.)
        ## Should we try to re-generate by autoconf?
        if (file.exists("configure")
           && any(grepl("Generated by GNU Autoconf",
                        readLines("configure", warn = FALSE),
                        ignore.case = TRUE))) {
            if (!any(file.exists(c("configure.ac", "configure.in")))) {
                desc <- .read_description("DESCRIPTION")
                is_FOSS <- parse_description_field(desc, "License_is_FOSS", NA)
                if (is.na(is_FOSS)) {
                    val <- desc["License"]
                    is_FOSS <-
                        !is.na(val) && isTRUE(analyze_license(val)$is_FOSS)
                }
                if (is_FOSS) {
                    if (!any) warningLog(Log)
                    wrapLog("Found a", sQuote("configure"),
                            "file without source file",
                            sQuote("configure.ac"), "or",
                            sQuote("configure.in"), ".",
                            "An Open Source package must include its",
                            "autoconf sources.\n")
                } else {
                    if (!any) noteLog(Log)
                    wrapLog("Found a", sQuote("configure"),
                            "file without source file",
                            sQuote("configure.ac"), "or",
                            sQuote("configure.in"), ".",
                            "It is good practice to include autoconf sources.\n")
                }
                any <- TRUE
            } else {
                if (file.exists("configure.in")) {
                    any <- TRUE
                    noteLog(Log)
                    wrapLog("Found a", sQuote("configure.in"),
                            "file:", sQuote("configure.ac"),
                            "has long been preferred.\n")
                }
                check_autoconf <- check_incoming ||
                    config_val_to_logical(Sys.getenv("_R_CHECK_AUTOCONF_", "FALSE"))
                if (check_autoconf && nzchar(Sys.which("autoreconf"))) {
                    td <- tempfile()
                    dir.create(td)
                    file.copy(".", td, recursive = TRUE)
                    od <- setwd(td)
                    out <- suppressWarnings(system2("autoreconf", "-fi",
                                                    stdout = TRUE, stderr = TRUE, timeout = 60))
                    setwd(od); unlink(td, recursive = TRUE)
                    if (length(out)) {
                        if(!any) {
                            any <- TRUE
                            warningLog(Log)
                        }
                        printLog0(Log, "  Output from running autoreconf:\n")
                        printLog0(Log, .format_lines_with_indent(out), "\n")
                    }
                }
            }
        }

        if(check_incoming) {
            ## CRAN must be able to convert
            ##   inst/README.md or README.md
            ##   inst/NEWS.md or NEWS.md
            ## to HTML using pandoc: check that this works fine.
            md_files <-
                c(Filter(file.exists,
                         c(file.path("inst", "README.md"),
                           "README.md"))[1L],
                  Filter(file.exists,
                         c(file.path("inst", "NEWS.md"),
                           "NEWS.md"))[1L])
            md_files <- md_files[!is.na(md_files)]
            if(length(md_files)) {
                if(nzchar(Sys.which("pandoc"))) {
                    for(ifile in md_files) {
                        ofile <- tempfile("pandoc", fileext = ".html")
                        out <- .pandoc_md_for_CRAN(ifile, ofile)
                        if(out$status) {
                            if(!any) warningLog(Log)
                            any <- TRUE
                            printLog(Log,
                                     sprintf("Conversion of '%s' failed:\n",
                                             ifile),
                                     paste(out$stderr, collapse = "\n"),
                                     "\n")
                        }
                        unlink(ofile)
                    }
                } else {
                    if(!any) noteLog(Log)
                    any <- TRUE
                    printLog(Log,
                             "Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being installed.\n")
                }
            }
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
                    if(!any) noteLog(Log)
                    any <- TRUE
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
        topfiles <- Sys.glob(file.path("inst", c("LICENCE", "LICENSE")))
        if (length(topfiles)) {
            ## Are these mentioned in DESCRIPTION?
            lic <- desc["License"]
            if(!is.na(lic)) {
                found <- sapply(basename(topfiles),
                                function(x) grepl(x, lic, fixed = TRUE))
                topfiles <- topfiles[!found]
                if (length(topfiles)) {
                    if(!any) noteLog(Log)
                    any <- TRUE
                    one <- (length(topfiles) == 1L)
                    msg <- c(if(one) "File" else "Files",
                             "\n",
                             .format_lines_with_indent(topfiles),
                             "\n",
                             if(one) {
                                 "will install at top-level and is not mentioned in the DESCRIPTION file.\n"
                             } else {
                                 "will install at top-level and are not mentioned in the DESCRIPTION file.\n"
                             })
                    printLog(Log, msg)
                }
            }
        }
        if (!is_base_pkg && R_check_toplevel_files) {
            ## any others?
            if(is.null(topfiles0)) {
                topfiles <- dir()
                ## Now check if any of these were created since we started
                topfiles <-
                    topfiles[file.info(topfiles, extra_cols = FALSE)$ctime
                             <= .unpack.time]
            } else topfiles <- topfiles0
            known <- c("DESCRIPTION", "INDEX", "LICENCE", "LICENSE",
                       "LICENCE.note", "LICENSE.note",
                       "MD5", "NAMESPACE", "NEWS", "PORTING",
                       "COPYING", "COPYING.LIB", "GPL-2", "GPL-3",
                       "BUGS", "Bugs",
                       "ChangeLog", "Changelog", "CHANGELOG", "CHANGES", "Changes",
                       "INSTALL", "README", "THANKS", "TODO", "ToDo",
                       "INSTALL.windows",
                       "README.md", "NEWS.md",
                       "configure", "configure.win", "cleanup", "cleanup.win",
                       "configure.ac", "configure.in",
                       "datafiles",
                       "R", "data", "demo", "exec", "inst", "man",
                       "po", "src", "tests", "vignettes",
                       "build",       # used by R CMD build
                       ".aspell",     # used for spell checking packages
                       "java", "tools", "noweb") # common dirs in packages.
            topfiles <- setdiff(topfiles, known)
            if (file.exists(file.path("inst", "AUTHORS")))
                topfiles <- setdiff(topfiles, "AUTHORS")
            if (file.exists(file.path("inst", "COPYRIGHTS")))
                topfiles <- setdiff(topfiles, "COPYRIGHTS")
            if (lt <- length(topfiles)) {
                if(!any) noteLog(Log)
                any <- TRUE
                printLog(Log, ## dirs are files, but maybe not on Windows
                         if(lt > 1L) "Non-standard files/directories found at top level:\n"
                         else "Non-standard file/directory found at top level:\n" )
                msg <- strwrap(paste(sQuote(topfiles), collapse = " "),
                               indent = 2L, exdent = 2L)
                printLog0(Log, paste(c(msg, ""), collapse="\n"))
                cp <- grep("^copyright", topfiles,
                           ignore.case = TRUE, value = TRUE)
                if (length(cp))
                    printLog(Log, "Copyright information should be in file inst/COPYRIGHTS\n")
                if("AUTHORS" %in% topfiles)
                    printLog(Log, "Authors information should be in file inst/AUTHORS\n")
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
            printLog0(Log,
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
                printLog0(Log,
                          sprintf("Empty or missing file %s.\n",
                                  sQuote(index)))
            } else {
                Rcmd <- paste0(opWarn_string, "\ntools:::.check_demo_index(\"demo\")\n")
                ## FIXME: this does not need to be run in another process
                out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
                if(length(out)) {
                    if(!any) warningLog(Log)
                    any <- TRUE
                    printLog0(Log, paste(c(out, ""), collapse = "\n"))
                }
            }
        }
        if (dir.exists(file.path("inst", "doc"))) {
            Rcmd <- paste0(opWarn_string, "\ntools:::.check_vignette_index(\"inst/doc\")\n")
            ## FIXME: this does not need to be run in another process
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            }
        }
        if (any)
            wrapLog("See sections 'The INDEX file' and 'Package subdirectories' in the 'Writing R Extensions' manual.\n")
        else resultLog(Log, "OK")
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
        ind <- endsWith(all_dirs, ".Rcheck")
        if(any(ind)) {
            if(!any) warningLog(Log)
            any <- TRUE
            msg <- ngettext(sum(ind),
                            "Found the following directory with the name of a check directory:\n",
                            "Found the following directories with names of check directories:\n", domain = NA)
            printLog0(Log, msg,
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
           printLog0(Log, msg,
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
                printLog0(Log, msg,
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
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
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
                        printLog0(Log, "\n",
                                  .format_lines_with_indent(bad), "\n")
                    else printLog0(Log, "  ", bad, "\n")
                    wrapLog("Portable packages must use only ASCII",
                            "characters in their demos.\n",
                            "Use \\uxxxx escapes for other characters.\n")
                    demos <- demos[basename(demos) %notin% bad]
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
                        printLog0(Log, "\n",
                                  .format_lines_with_indent(bad), "\n")
                    else printLog0(Log, "  ", bad, "\n")
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
                                           c("Makefile", "Makefile.win",
                                             "install.libs.R"))))) {
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
                  "man", "help", "html", "latex", "R-ex", "build")
            allfiles <- dir("inst", full.names = TRUE)
            alldirs <- allfiles[dir.exists(allfiles)]
            suspect <- basename(alldirs) %in% R_system_subdirs
            if (any(suspect)) {
                ## check they are non-empty
                suspect <- alldirs[suspect]
                suspect <- suspect[vapply(suspect, function(x) {
                    length(dir(x, all.files = TRUE)) > 2L
                    },
                    NA)]
                if (length(suspect)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    wrapLog("Found the following non-empty",
                            "subdirectories of 'inst' also",
                            "used by R:\n")
                    printLog0(Log, .format_lines_with_indent(suspect), "\n")
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
                printLog0(Log,
                          paste0("  ",
                                 unlist(strsplit(msg, "\n", fixed = TRUE)),
                                 collapse = "\n"),
                          "\n")
            }
        }

        ## Valid CITATION metadata?
        if (file.exists(file.path("inst", "CITATION"))) {
            Rcmd <- if(do_install)
                sprintf("tools:::.check_citation(\"inst/CITATION\", \"%s\")\n",
                        file.path(if(is_base_pkg) .Library else libdir,
                                  pkgname))
            else
                "tools:::.check_citation(\"inst/CITATION\")\n"
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=utils")
            if(length(out)) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "Invalid citation information in 'inst/CITATION':\n")
                printLog0(Log, .format_lines_with_indent(out), "\n")
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
            printLog0(Log, .format_lines_with_indent(files), "\n")
            wrapLog("Most likely 'inst/CITATION' should be used instead.\n")
        }

        if(!any) resultLog(Log, "OK")
    }

    check_non_ASCII <- function()
    {
        checkingLog(Log, "R files for non-ASCII characters")
        out <- R_runR0("tools:::.check_package_ASCII_code('.')",
                       R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (length(out)) {
            warningLog(Log)
            msg <- ngettext(length(out),
                            "Found the following file with non-ASCII characters:\n",
                            "Found the following files with non-ASCII characters:\n",
                            domain = NA)
            wrapLog(msg)
            printLog0(Log, .format_lines_with_indent(out), "\n")
            wrapLog("Portable packages must use only ASCII",
                    "characters in their R code,\n",
                    "except perhaps in comments.\n",
                    "Use \\uxxxx escapes for other characters.\n")
        } else resultLog(Log, "OK")

        checkingLog(Log, "R files for syntax errors")
        Rcmd  <- paste0(opWarn_string, ";tools:::.check_package_code_syntax(\"R\")")
        out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if (any(startsWith(out, "Error"))) {
            errorLog(Log)
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            maybe_exit(1L)
        } else if (length(out)) {
            warningLog(Log)
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
        } else resultLog(Log, "OK")
    }

    check_R_code <- function()
    {
        checkingLog(Log, "dependencies in R code")
        if (do_install) {
            Rcmd <- paste(opW_shE_F_str,
                          sprintf("tools:::.check_packages_used(package = \"%s\")\n", pkgname))

            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if(any(grepl("(not declared from|Including base/recommended)", out))) warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                ## wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")
        } else {
            ## this needs to read the package code, and will fail on
            ## syntax errors such as non-ASCII code.
            Rcmd <- paste(opW_shE_F_str,
                          sprintf("tools:::.check_packages_used(dir = \"%s\")\n", pkgdir))

            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if(any(grepl("not declared from", out))) warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                ## wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")
        }

        ## Check whether methods have all arguments of the corresponding
        ## generic.
        checkingLog(Log, "S3 generic/method consistency")
        Rcmd <- paste(opWarn_string, "\n",
                      "options(expressions=1000)\n",
                      if (do_install)
                      sprintf("tools::checkS3methods(package = \"%s\")\n", pkgname)
                      else
                      sprintf("tools::checkS3methods(dir = \"%s\")\n", pkgdir))
        out <- R_runR2(Rcmd)
        if (length(out)) {
            pos <- which(startsWith(out,
                                    "Found the following apparent S3 methods"))
            if(!length(pos)) {
                out1 <- out
                out2 <- character()
            } else {
                pos <- pos[1L]
                out1 <- out[seq_len(pos - 1L)]
                out2 <- out[seq.int(pos, length(out))]
            }
            if(length(out1)) {
                warningLog(Log)
                printLog0(Log, paste(c(out1, ""), collapse = "\n"))
                wrapLog("See section 'Generic functions and methods'",
                        "in the 'Writing R Extensions' manual.\n")
            } else
                noteLog(Log)
            if(length(out2)) {
                printLog0(Log,
                          paste(c(if(length(out1)) "", out2, ""),
                                collapse = "\n"))
                wrapLog("See section 'Registering S3 methods'",
                        "in the 'Writing R Extensions' manual.\n")
            }
        } else resultLog(Log, "OK")

        ## Check whether replacement functions have their final argument
        ## named 'value'.
        checkingLog(Log, "replacement functions")
        Rcmd <- paste(opWarn_string, "\n",
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
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("The argument of a replacement function",
                    "which corresponds to the right hand side",
                    "must be named 'value'.\n")
        } else resultLog(Log, "OK")

        ## Check foreign function calls.
        ## The neverending story ...
        ## For the time being, allow to turn this off by setting the environment
        ## variable _R_CHECK_FF_CALLS_ to an empty value.
        if (nzchar(R_check_FF)) {
            registration <-
                identical(R_check_FF, "registration") && install != "fake"
            checkingLog(Log, "foreign function calls")
            DUP <- R_check_FF_DUP
            if(as_cran) {
                Sys.setenv("_R_CHECK_FF_AS_CRAN_" = "TRUE")
                DUP <- TRUE
            }
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                          sprintf("tools::checkFF(package = \"%s\", registration = %s, check_DUP = %s)\n",
                                  pkgname, registration, DUP)
                          else
                          sprintf("tools::checkFF(dir = \"%s\", registration = %s, check_DUP = %s)\n",
                                  pkgdir, "FALSE", DUP))
            out <- R_runR2(Rcmd)
            Sys.unsetenv("_R_CHECK_FF_AS_CRAN_")
            if (length(out)) {
                if(any(grepl("^Foreign function calls? with(out| empty)", out)) ||
                   (!is_base_pkg && any(grepl("to a base package:", out))) ||
                   any(grepl("^Undeclared packages? in", out)) ||
                   any(grepl("parameter[s]*, expected ", out))
                   ) warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                if(!is_base_pkg && any(grepl("to a base package:", out)))
                    wrapLog("Packages should not make",
                            ".C/.Call/.External/.Fortran",
                            "calls to a base package.",
                            "They are not part of the API,",
                            "for use only by R itself",
                            "and subject to change without notice.")
                else if(any(grepl("with DUP:", out)))
                    wrapLog("DUP is no longer supported and will be ignored.")
                else
                    wrapLog("See chapter 'System and foreign language interfaces' in the 'Writing R Extensions' manual.\n")
            } else resultLog(Log, "OK")
        }
    }

    check_R_files <- function(is_rec_pkg)
    {
        checkingLog(Log, "R code for possible problems")
        t1 <- proc.time()
        if (!is_base_pkg) {
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_code_shlib(dir = \"%s\")\n",
                                  pkgdir))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                errorLog(Log)
                wrapLog("Incorrect (un)loading of package",
                        "shared object.\n")
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("The system-specific extension for",
                        "shared objects must not be added.\n",
                        "See ?library.dynam.\n")
                maybe_exit(1L)
            }
        }

        Rcmd <- paste(opWarn_string, "\n",
                      sprintf("tools:::.check_package_code_startup_functions(dir = \"%s\")\n",
                              pkgdir))
        out1 <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        Rcmd <- paste(opWarn_string, "\n",
                      sprintf("tools:::.check_package_code_unload_functions(dir = \"%s\")\n",
                              pkgdir))
        out1a <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        out1 <- if (length(out1) && length(out1a)) c(out1, "", out1a)
                else c(out1, out1a)

        out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- NULL

        if (!is_base_pkg && R_check_unsafe_calls) {
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_code_tampers(dir = \"%s\")\n",
                                  pkgdir))
            out2 <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        }

        if (R_check_use_codetools && do_install) {
            Rcmd <-
                paste(opWarn_string, "\n",
                      sprintf("tools:::.check_code_usage_in_package(package = \"%s\")\n", pkgname))
            if(config_val_to_logical(Sys.getenv("_R_CHECK_CODE_USAGE_WITH_ONLY_BASE_ATTACHED_",
                                                "true"))) {
                out3 <-  R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
                if(length(pos <- which(startsWith(out3,
                                                  "Undefined global functions or variables:")))) {
                    Rcmd <-
                        sprintf("writeLines(strwrap(tools:::imports_for_undefined_globals(\"%s\"), exdent = 11))\n",
                                paste(utils::tail(out3, -pos),
                                      collapse = " "))
                    miss <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
                    ## base has no NAMESPACE
                    if(length(miss) && pkgname != "base") {
                        msg3 <- if(any(startsWith(miss,
                                                  "importFrom(\"methods\""))) {
                            strwrap("to your NAMESPACE file (and ensure that your DESCRIPTION Imports field contains 'methods').")
                        } else "to your NAMESPACE file."
                        out3 <- c(out3,
                                  c("Consider adding",
                                    paste0("  ", miss),
                                    msg3))
                    }
                }
            } else
                out3 <-  R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
        }

        if(!is_base_pkg && R_check_use_codetools && R_check_dot_internal) {
            details <- pkgname != "relax" # has .Internal in a 10,000 line fun
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                              sprintf("tools:::.check_dotInternal(package = \"%s\",details=%s)\n", pkgname, details)
                          else
                              sprintf("tools:::.check_dotInternal(dir = \"%s\",details=%s)\n", pkgdir, details))
            out4 <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
            ## Hmisc, gooJSON, quantmod give spurious output
            if (!any(grepl("^Found.* .Internal call", out4))) out4 <- NULL
        }

        if(!is_base_pkg && R_check_code_assign_to_globalenv) {
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_code_assign_to_globalenv(dir = \"%s\")\n",
                                  pkgdir))
            out5 <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }

        if(!is_base_pkg && R_check_code_attach) {
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_code_attach(dir = \"%s\")\n",
                                  pkgdir))
            out6 <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }
        if(!is_base_pkg && R_check_code_data_into_globalenv) {
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_code_data_into_globalenv(dir = \"%s\")\n",
                                  pkgdir))
            out7 <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=")
        }

        ## Use of deprecated, defunct and platform-specific devices?
        if(!is_base_pkg && R_check_use_codetools && R_check_depr_def) {
            win <- !is.na(OS_type) && OS_type == "windows"
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                              sprintf("tools:::.check_depdef(package = \"%s\", WINDOWS = %s)\n", pkgname, win)
                          else
                              sprintf("tools:::.check_depdef(dir = \"%s\", WINDOWS = %s)\n", pkgdir, win))
            out8 <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=")
        }
        t2 <- proc.time()
        print_time(t1, t2, Log)

        if (length(out1) || length(out2) || length(out3) ||
            length(out4) || length(out5) || length(out6) ||
            length(out7) || length(out8)) {
            ini <- character()
            if(length(out4) ||
               (length(out8) &&
                any(startsWith(out8,
                               "Found the defunct/removed function"))))
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
                if(any(startsWith(out8, "Found the defunct/removed function")))
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
            c("See chapter 'Writing R documentation files' in the 'Writing R Extensions' manual.\n")

        if (dir.exists("man") && !extra_arch) {
            checkingLog(Log, "Rd files")
            minlevel <- Sys.getenv("_R_CHECK_RD_CHECKRD_MINLEVEL_", "-1")
            Rcmd <- paste(opWarn_string, "\n",
                          sprintf("tools:::.check_package_parseRd('.', minlevel=%s)\n", minlevel))
            ## This now evaluates \Sexpr, so run with usual packages.
            out <- R_runR0(Rcmd, R_opts2, elibs)
            if (length(out)) {
                if(length(grep("^prepare.*Dropping empty section", out,
                               invert = TRUE)))
                    warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")

            checkingLog(Log, "Rd metadata")
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_metadata(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_metadata(dir = \"%s\")\n", pkgdir))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check Rd line widths.
        if(dir.exists("man") && R_check_Rd_line_widths) {
            checkingLog(Log, "Rd line widths")
            Rcmd <- paste(opWarn_string, "\n",
                          if(do_install)
                          sprintf("tools:::.check_Rd_line_widths(\"%s\", installed = TRUE)\n",
                                  file.path(if(is_base_pkg) .Library else libdir,
                                            pkgname))
                          else
                          sprintf("tools:::.check_Rd_line_widths(\"%s\")\n",
                                  pkgdir))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if(length(out)) {
                noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("These lines will be truncated in the PDF manual.\n")

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
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_xrefs(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_xrefs(dir = \"%s\")\n", pkgdir))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if (!all(grepl("Package[s]? unavailable to check", out)))
                    warningLog(Log)
                else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check for missing documentation entries.
        if (!extra_arch && (haveR || dir.exists("data"))) {
            checkingLog(Log, "for missing documentation entries")
            Rcmd <- paste(opWarn_string, "\n",
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
            out <- out[!startsWith(out, "Loading required package:")]
            err <- startsWith(out, "Error")
            if (any(err)) {
                errorLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                maybe_exit(1L)
            } else if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("All user-level objects",
                        "in a package",
                        if (any(startsWith(out, "Undocumented S4")))
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
                    Rcmd <- paste(opWarn_string, "\n",
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
                    Rcmd <- paste(opWarn_string, "\n",
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
                    Rcmd <- paste(opWarn_string, "\n",
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
            Rcmd <- paste(opWarn_string, "\n",
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

                Rcmd <- paste(opWarn_string, "\n",
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
            Rcmd <- paste(opWarn_string, "\n",
                          if (do_install)
                          sprintf("tools:::.check_Rd_contents(package = \"%s\")\n", pkgname)
                          else
                          sprintf("tools:::.check_Rd_contents(dir = \"%s\")\n", pkgdir))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## Check undeclared dependencies in examples (if any)
        if (dir.exists("man") && do_install && !extra_arch && !is_base_pkg) {
            checkingLog(Log, "for unstated dependencies in examples")
            Rcmd <- paste(opW_shE_F_str,
                          sprintf("tools:::.check_packages_used_in_examples(package = \"%s\")\n", pkgname))

            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                # wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")

        } ## FIXME, what if no install?
    }

    check_data <- function()
    {
        ## Check contents of 'data'
        if (!is_base_pkg && dir.exists("data")) {
            checkingLog(Log, "contents of 'data' directory")
            warn <- FALSE
            msgs <- character()
            fi <- list.files("data")
            dataFiles <- basename(list_files_with_type("data", "data"))
            if (!any(grepl("\\.[Rr]$", fi))) { # code files can do anything
                odd <- fi %w/o% c(dataFiles, "datalist")
                if (length(odd)) {
                    warn <-TRUE
                    msgs <-
                        c(sprintf("Files not of a type allowed in a %s directory:\n",
                                  sQuote("data")),
                          paste0(.pretty_format(odd), "\n"),
                          sprintf("Please use e.g. %s for non-R data files\n",
                                  sQuote("inst/extdata")),
                          "\n")
                }
            }
            if ("datalist" %in% fi) {
                if(file.info(sv <- file.path("data", "datalist"))$isdir) {
                    warn <- TRUE
                    msgs <- c(msgs, sprintf("%s is a directory\n",
                                            sQuote("data/datalist"), "\n"))
                }  else {
                    ## Now check it has the right format:
                    ## it is read in list_data_in_pkg()
                    ## Allowed lines are
                    ## foo
                    ## foo: bar ...
                    ## where bar ... and standalone foo are object names
                    dl <- readLines(sv, warn = FALSE)
                    if (any(bad <- !grepl("^[^ :]*($|: +[[:alpha:].])", dl))) {
                        warn <- TRUE
                        msgs <- c(msgs,
                                  sprintf("File %s contains malformed line(s):\n",
                                          sQuote("data/datalist")),
                                 paste0(.pretty_format(dl[bad]), "\n"))
                    }
                }
            }
            ans <- list_data_in_pkg(dataDir = file.path(pkgdir, "data"))
            if (length(ans)) {
                bad <-
                    names(ans)[sapply(ans, function(x) ".Random.seed" %in% x)]
                if (length(bad)) {
                    warn <- TRUE
                    msg <- if (length(bad) > 1L)
                         c(sprintf("Object named %s found in datasets:\n",
                                  sQuote(".Random.seed")),
                          paste0(.pretty_format(bad), "\n"),
                          "Please remove it.\n")
                    else
                        c(sprintf("Object named %s found in dataset: ",
                                  sQuote(".Random.seed")),
                          sQuote(bad), "\nPlease remove it.\n")
                    msgs <- c(msgs, msg)
                }
            }
            if (do_install) {
                ## check that all the datasets can be loaded cleanly by data()
                ## except for LazyData.
                instdir <- file.path(libdir, pkgname)
                if (!file.exists(file.path(instdir, "data", "Rdata.rdb"))) {
                    files <- basename(list_files_with_type("data", "data"))
                    files <- unique(basename(file_path_sans_ext(files, TRUE)))
                    for (f in files) {
                        cmd <- sprintf('tools:::.check_package_datasets2("%s", "%s")',
                                       f, pkgname)
                        out <- R_runR(cmd, R_opts2)
                        out <- filtergrep("^OMP:", out)
                        if (length(out)) {
                            if (any(grepl("^(Warning|Error|No dataset created|Search path was changed)", out)))
                                warn <- TRUE
                            msgs <- c(msgs,
                                     sprintf('Output for data("%s", package = "%s"):\n', f, pkgname),
                                     paste(c(paste0("  ",out), ""),
                                           collapse = "\n"))
                        }
                    }
                }
            }
            if (length(msgs)) {
                if (warn) warningLog(Log) else noteLog(Log)
                printLog0(Log, msgs)
            } else resultLog(Log, "OK")
        }

        ## Check for non-ASCII characters in 'data'
        if (!is_base_pkg && R_check_ascii_data && dir.exists("data")) {
            checkingLog(Log, "data for non-ASCII characters")
            out <- R_runR0("tools:::.check_package_datasets('.')",
                           R_opts2, elibs)
            out <- filtergrep("Loading required package", out)
            out <- filtergrep("Warning: changing locked binding", out, fixed = TRUE)
            out <- filtergrep("^OMP:", out)
            if (length(out)) {
                bad <- startsWith(out, "Warning:")
                bad2 <-  any(grepl("(unable to find required package|there is no package called)", out))
                if(any(bad) || bad2) warningLog(Log) else noteLog(Log)
                printLog0(Log, .format_lines_with_indent(out), "\n")
                if(bad2)
                     printLog0(Log,
                               "  The dataset(s) may use package(s) not declared in the DESCRIPTION file.\n")
            } else resultLog(Log, "OK")
        }

        ## Check for ASCII and uncompressed/unoptimized saves in 'data'
        if (!is_base_pkg && R_check_compact_data && dir.exists("data")) {
            checkingLog(Log, "data for ASCII and uncompressed saves")
            out <- R_runR0("tools:::.check_package_compact_datasets('.', TRUE)",
                           R_opts2)
            out <- filtergrep("Warning: changing locked binding", out, fixed = TRUE)
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, .format_lines_with_indent(out), "\n")
            } else resultLog(Log, "OK")
        }

        ## Check for ASCII and uncompressed/unoptimized saves in 'sysdata':
        ## no base package has this
        if (R_check_compact_data && file.exists(file.path("R", "sysdata.rda"))) {
            checkingLog(Log, "R/sysdata.rda")
            out <- R_runR0("tools:::.check_package_compact_sysdata('.', TRUE)",
                           R_opts2)
            if (length(out)) {
                bad <- startsWith(out, "Warning:")
                if (any(bad)) warningLog(Log) else noteLog(Log)
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
            printLog0(Log,
                      "The following files are already in R: ",
                      paste(sQuote(bad), collapse = ", "), "\n",
                      "Please remove them from your package.\n")
        }
        files2 <- dir(file.path(pkgdir, "inst", "doc"), recursive = TRUE,
                     pattern = "[.](cls|sty|drv)$", full.names = TRUE)
        ## Skip Rnews.sty and RJournal.sty for now
        files2 <- files2[basename(files2) %notin%
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
            printLog0(Log,
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
            printLog0(Log,
                      "The following files look like leftovers/mistakes:\n",
                      paste(strwrap(paste(sQuote(files[bad]), collapse = ", "),
                                    indent = 2, exdent = 2), collapse = "\n"),
                      "\nPlease remove them from your package.\n")
        }

        files <- dir(doc_dir)
        files <- files %w/o% already
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
                printLog0(Log,
                          "The following files should probably not be installed:\n",
                          paste(strwrap(paste(sQuote(badf), collapse = ", "),
                                        indent = 2, exdent = 2), collapse = "\n"),
                          "\n")
            if(length(badd))
                printLog0(Log,
                          "The following directories should probably not be installed:\n",
                          paste(strwrap(paste(sQuote(badd), collapse = ", "),
                                        indent = 2, exdent = 2), collapse = "\n"),
                          "\n")
            printLog0(Log, "\nConsider the use of a .Rinstignore file: see ",
                      sQuote("Writing R Extensions"), ",\n",
                      "or move the vignette sources from ",
                      sQuote("inst/doc"), " to ", sQuote("vignettes"), ".\n")
        }
        if (!any) resultLog(Log, "OK")
    }

    check_vign_contents <- function(ignore_vignettes = FALSE)
    {
        checkingLog(Log, "files in 'vignettes'")
        if (ignore_vignettes) {
            resultLog(Log, "SKIPPED")
            return()
        }
        ## special case common problems.
        any <- FALSE
        pattern <- vignetteEngine("Sweave")$pattern
        vign_dir <- file.path(pkgdir, "vignettes")
        sources <- setdiff(list.files(file.path(pkgdir, "inst", "doc"),
                                      pattern = pattern),
                           list.files(vign_dir, pattern = pattern))
        if(length(sources)) {
            warningLog(Log)
            any <- TRUE
            msg <- c("Vignette sources in 'inst/doc' missing from the 'vignettes' directory:",
                    strwrap(paste(sQuote(sources), collapse = ", "),
                            indent = 2L, exdent = 4L),
                     "")
            printLog0(Log, paste(msg, collapse = "\n"))
        }

        ## Did the vignettes get updated in inst/doc?
        inst_doc_files <- list.files(file.path(pkgdir, "inst", "doc"),
                                     recursive = TRUE)
        vignette_files <- list.files(vign_dir, recursive = TRUE)
        if (!is_base_pkg && length(vignette_files)) {
            if (!length(inst_doc_files)) {
                if (!any) warningLog(Log)
                any <- TRUE
                msg <- c("Files in the 'vignettes' directory but no files in 'inst/doc':",
                         strwrap(paste(sQuote(vignette_files), collapse = ", "),
                                 indent = 2L, exdent = 4L),
                         "")
                printLog0(Log, paste(msg, collapse = "\n"))
            } else {
                ## allow for some imprecision in file times (in secs)
                time_tol <- as.double(Sys.getenv("_R_CHECK_FILE_TIMES_TOL_", 10))
                vignette_times <- file.mtime(file.path(vign_dir, vignette_files))
                inst_doc_times <- file.mtime(file.path(pkgdir, "inst", "doc", inst_doc_files))
                if (sum(!is.na(vignette_times)) && sum(!is.na(inst_doc_times)) &&
                    max(vignette_times, na.rm = TRUE) > max(inst_doc_times, na.rm = TRUE) + time_tol) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    msg <- c("Files in the 'vignettes' directory newer than all files in 'inst/doc':",
                             strwrap(paste(sQuote(vignette_files[!is.na(vignette_times) & vignette_times > max(inst_doc_times, na.rm = TRUE)]),
                                           collapse = ", "),
                                     indent = 2L, exdent = 4L),
                             "")
                    keep <- is.na(vignette_times) |
                        vignette_times <= max(inst_doc_times, na.rm = TRUE) + time_tol
                    vignette_files <- vignette_files[keep]
                    vignette_times <- vignette_times[keep]
                    printLog0(Log, paste(msg, collapse = "\n"))
                }
                matches <- match(vignette_files, inst_doc_files)
                newer <- vignette_times > inst_doc_times[matches] + time_tol
                newer <- !is.na(matches) & !is.na(newer) & newer
                if (any(newer)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    msg <- c("Files in the 'vignettes' directory newer than same file in 'inst/doc':",
                             strwrap(paste(sQuote(vignette_files[newer]),
                                           collapse = ", "),
                                     indent = 2L, exdent = 4L),
                             "")
                    printLog0(Log, paste(msg, collapse = "\n"))
                }
            }
        }

        files <- dir(file.path(pkgdir, "vignettes"))
        if(length(files) &&
           !length(dir(file.path(pkgdir, "vignettes"),
                       pattern = pattern)) &&
           is.na(desc["VignetteBuilder"])) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log,
                      "Package has no Sweave vignette sources and no VignetteBuilder field.\n")
        }

        vigns <- pkgVignettes(dir = pkgdir, check = TRUE)
        if(length(msg <- vigns[["msg"]])) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log, paste(msg, collapse = "\n"), "\n")
        }

        already <- c("jss.cls", "jss.bst", "Rd.sty", "Sweave.sty")
        bad <- files[files %in% already]
        if (length(bad)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log,
                      "The following files are already in R: ",
                      paste(sQuote(bad), collapse = ", "), "\n",
                      "Please remove them from your package.\n")
        }
        files2 <- dir(file.path(pkgdir, "vignettes"), recursive = TRUE,
                     pattern = "[.](cls|sty|drv)$", full.names = TRUE)
        files2 <- files2[basename(files2) %notin%
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
            printLog0(Log,
                      "The following files contain a license that requires\n",
                      "distribution of original sources:\n",
                      "  ", paste(sQuote(bad), collapse = ", "), "\n",
                      "Please ensure that you have complied with it.\n")
        }

        ## Now look for TeX leftovers (and soiltexture, Amelia ...).
        bad <- grepl("[.](log|aux|bbl|blg|dvi|toc|out|Rd|Rout|dbj|drv|ins)$",
                     files, ignore.case = TRUE)
        bad <- bad | (files %in% c("Rplots.ps", "Rplots.pdf"))
        if (any(bad)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log,
                      "The following files look like leftovers/mistakes:\n",
                      paste(strwrap(paste(sQuote(files[bad]), collapse = ", "),
                                    indent = 2, exdent = 2), collapse = "\n"),
                      "\nPlease remove them from your package.\n")
        }

        ## Probable leftovers from knitr
        dirs <- file.path(pkgdir, "vignettes", c("cache", "figure"))
        dirs <- basename(dirs[dir.exists(dirs)])
        if(length(dirs)) {
            if(!any) noteLog(Log)
            any <- TRUE
            printLog0(Log,
                      if(length(dirs)> 1L) "The following directories look like leftovers from 'knitr':\n"
                      else "The following directory looks like a leftover from 'knitr':\n",
                      paste(strwrap(paste(sQuote(dirs), collapse = ", "),
                                    indent = 2, exdent = 2), collapse = "\n"),
                      "\nPlease remove from your package.\n")
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
                gs_cmd <- find_gs_cmd()
                if (nzchar(gs_cmd)) {
                    res <- compactPDF(td, gs_cmd = gs_cmd, gs_quality = "ebook")
                    res <- format(res, diff = 2.56e5) # 250 KB for now
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

    check_src_dir <- function(desc)
    {
        ## Added in R 3.4.2: check line endings for shell scripts:
        ## for Unix CRLF line endings are fatal but these are not used
        ## on Windows and hence this is not detected.
        ## Packages could have arbitrary scripts, so we could
        ## extend this to look for scripts at top level or elsewhere.
        scripts <- dir(".", pattern = "^(configure|configure.in|configure.ac|cleanup)$")
        if(length(scripts)) {
            checkingLog(Log, "line endings in shell scripts")
            bad_files <- character()
            for(f in scripts) {
                contents <- readChar(f, file.size(f), useBytes = TRUE)
                if (grepl("\r", contents, fixed = TRUE, useBytes = TRUE))
                    bad_files <- c(bad_files, f)
            }
            if (length(bad_files)) {
                warningLog(Log, "Found the following shell script(s) with CR or CRLF line endings:")
                printLog0(Log, .format_lines_with_indent(bad_files), "\n")
                printLog(Log, "Non-Windows OSes require LF line endings.\n")
            } else resultLog(Log, "OK")
       }


        ## Check C/C++/Fortran sources/headers for CRLF line endings.
        ## <FIXME>
        ## Does ISO C really require LF line endings?
        ## (ISO C does not comment on OSes ....)
        ## Solaris compilers still do, with a warning but no longer an error.
        ## </FIXME>
        if(dir.exists("src") || dir.exists("inst/include")) {
            checkingLog(Log, "line endings in C/C++/Fortran sources/headers")
            ## pattern is "([cfh]|cc|cpp)"
            files <- dir("src", pattern = "\\.([cfh]|cc|cpp|hpp)$",
                         full.names = TRUE, recursive = TRUE)
            ## exclude dirs starting src/win, e.g for tiff
            files <- filtergrep("^src/[Ww]in", files)
            files2 <- dir("inst/include", pattern = "\\.([cfh]|cc|cpp|hpp)$",
                          full.names = TRUE, recursive = TRUE)
            bad_files <- character()
            no_eol <- character()
            for(f in c(files, files2)) {
                contents <- readChar(f, file.size(f), useBytes = TRUE)
                if (grepl("\r", contents, fixed = TRUE, useBytes = TRUE))
                    bad_files <- c(bad_files, f)
                else if (nzchar(contents) &&  ## allow empty dummy files
                         !grepl("\n$", contents, useBytes = TRUE))
                    no_eol <- c(no_eol, f)
            }
            if (length(bad_files) || length(no_eol)) noteLog(Log, "")
            else resultLog(Log, "OK")
            if (length(bad_files)) {
                printLog(Log, "Found the following sources/headers with CR or CRLF line endings:\n")
                printLog0(Log, .format_lines_with_indent(bad_files), "\n")
                printLog(Log, "Some Unix compilers require LF line endings.\n")
            } else if (length(no_eol)) {
                printLog(Log, "Found the following sources/headers not terminated with a newline:\n")
                printLog0(Log, .format_lines_with_indent(no_eol), "\n")
                printLog(Log, "Some compilers warn on such files.\n")
            }
        }

        ## Check src/Make* for LF line endings, as Sun make does not accept CRLF
        ## .win files are not checked, as CR/CRLF work there
        all_files <-
            dir("src",
                pattern = "^(Makevars|Makevars.in|Makefile|Makefile.in)$",
                full.names = TRUE, recursive = TRUE)
        all_files <- c(all_files,
                       dir(".", pattern = "^Makefile$",
                           full.names = TRUE, recursive = TRUE))
        all_files <- sub("^[.]/", "", all_files)
        all_files <- unique(sort(all_files))
        if(length(all_files)) {
            checkingLog(Log, "line endings in Makefiles")
            bad_files <- noEOL<- character()
            for(f in all_files) {
                if (!file.exists(f)) next
                contents <- readChar(f, file.size(f), useBytes = TRUE)
                if (grepl("\r", contents, fixed = TRUE, useBytes = TRUE))
                    bad_files <- c(bad_files, f)
                if (!grepl("\n$", contents, useBytes = TRUE))
                    noEOL <- c(noEOL, f)
            }
            if (length(bad_files)) {
                warningLog(Log, "Found the following Makefile(s) with CR or CRLF line endings:")
                printLog0(Log, .format_lines_with_indent(bad_files), "\n")
                printLog(Log, "Some Unix 'make' programs require LF line endings.\n")
            } else if (length(noEOL)) {
                noteLog(Log, "Found the following Makefile(s) without a final LF:")
                printLog0(Log, .format_lines_with_indent(noEOL), "\n")
                printLog(Log, "Some 'make' programs ignore lines not ending in LF.\n")
            } else resultLog(Log, "OK")
        }
        ## Check src/Makevars[.in] compilation flags.
        if (length(makevars)) {
            checkingLog(Log, "compilation flags in Makevars")

            Rcmd <- sprintf("tools:::.check_make_vars(\"src\", %s)\n",
                            deparse(makevars))
            out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                if(any(grepl("^(Non-portable flags|Variables overriding)", out)))
                   warningLog(Log) else noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }
        ## Check GNUisms
        if (length(all_files)) {
            checkingLog(Log, "for GNU extensions in Makefiles")
            bad_files <- character()
            for(f in all_files) {
                contents <- readLines(f, warn = FALSE)
                contents <- filtergrep("^ *#", contents)
                ## Things like $(SUBDIRS:=.a)
                contents <- filtergrep("[$][(].+:=.+[)]", contents)
                if (any(grepl("([+]=|:=|[$][(]wildcard|[$][(]shell|[$][(]eval|[$][(]call|[$][(]patsubst|^ifeq|^ifneq|^ifdef|^ifndef|^endifi|[.]NOTPARALLEL)",
                              contents)))
                    bad_files <- c(bad_files, f)
            }
            SysReq <- desc["SystemRequirements"]
            if (length(bad_files)) {
                if(!is.na(SysReq) && grepl("GNU [Mm]ake", SysReq)) {
                    if(!config_val_to_logical(Sys.getenv("_R_CHECK_CRAN_INCOMING_NOTE_GNU_MAKE_", "FALSE"))) {
                        noteLog(Log, "GNU make is a SystemRequirements.")
                    } else resultLog(Log, "OK")
                } else {
                    warningLog(Log, "Found the following file(s) containing GNU extensions:")
                    printLog0(Log, .format_lines_with_indent(bad_files), "\n")
                    wrapLog("Portable Makefiles do not use GNU extensions",
                            "such as +=, :=, $(shell), $(wildcard),",
                            "ifeq ... endif, .NOTPARALLEL",
                            "See section 'Writing portable packages'",
                            "in the 'Writing R Extensions' manual.\n")
                }
            } else resultLog(Log, "OK")
        }

        ## check src/Makevar*, src/Makefile* for correct use of BLAS_LIBS
        ## FLIBS is not needed on Windows, at least currently (as it is
        ## statically linked).
        makefiles <- Sys.glob(file.path("src",
                                        c("Makevars", "Makevars.in",
                                          "Makefile", "Makefile.win")))
        if(length(makefiles)) {
            checkingLog(Log, "for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS)")
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
                ## Truncate at first comment char
                lines <- sub("#.*", "", lines)
                c1 <- grepl("^[[:space:]]*PKG_LIBS", lines, useBytes = TRUE)
                c2l <- grepl("\\$[{(]{0,1}LAPACK_LIBS", lines, useBytes = TRUE)
                c2b <- grepl("\\$[{(]{0,1}BLAS_LIBS", lines, useBytes = TRUE)
                c2lb <- grepl("\\$[{(]{0,1}LAPACK_LIBS.*\\$[{(]{0,1}BLAS_LIBS",
                              lines, useBytes = TRUE)
                c2bf <- grepl("\\$[{(]{0,1}BLAS_LIBS.*\\$[{(]{0,1}FLIBS",
                              lines, useBytes = TRUE)
                if (any(c1 & c2l & !c2lb)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    printLog(Log,
                             "  apparently using $(LAPACK_LIBS) without following $(BLAS_LIBS) in ",
                             sQuote(f), "\n")
                }
                if (any(c1 & c2b & !c2bf)) {
                    if (!any) warningLog(Log)
                    any <- TRUE
                    printLog(Log,
                             "  apparently using $(BLAS_LIBS) without following $(FLIBS) in ",
                             sQuote(f), "\n")
                }
            }
            if (!any) resultLog(Log, "OK")
        }

        test_omp <-
            config_val_to_logical(Sys.getenv("_R_CHECK_SHLIB_OPENMP_FLAGS_", "FALSE"))
        makefiles <- Sys.glob(file.path("src",
                                        c("Makevars", "Makevars.in",
                                          "Makevars.win",
                                          "Makefile", "Makefile.win")))

        if(length(makefiles) && test_omp) {
            checkingLog(Log, "use of SHLIB_OPENMP_*FLAGS in Makefiles")
            ## If any of these flags are included in PKG_*FLAGS, it
            ## should also be included in PKG_LIBS.  And it is
            ## not portable to use more than one of these in one package.
            any <- msg2 <- msg3 <- FALSE
            for (m in makefiles) {
                lines <- readLines(m, warn = FALSE)
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
                ## Truncate at first comment char, skip empty lines
                lines <- sub("#.*", "", lines)
                lines <- lines[nzchar(lines)]

                c1 <- grepl("^[[:space:]]*PKG_LIBS", lines, useBytes = TRUE)
                anyInLIBS <- any(grepl("SHLIB_OPENMP_", lines[c1], useBytes = TRUE))

                ## Now see what sort of files we have
                have_c <- length(dir('src', pattern = "[.]c$", recursive = TRUE)) > 0L
                have_cxx <- length(dir('src', pattern = "[.](cc|cpp)$", recursive = TRUE)) > 0L
                have_f <- length(dir('src', pattern = "[.]f$", recursive = TRUE)) > 0L
                have_f9x <- length(dir('src', pattern = "[.]f9[05]$", recursive = TRUE)) > 0L
                used <- character()
                for (f in c("C", "CXX", "F", "FC"))  {
                    this <- this2 <- paste0(f, "FLAGS")
                    if (f == "FC") this2 <- "(F|FC)FLAGS"
                    pat <- paste0("^[[:space:]]*PKG_", this, ".*SHLIB_OPENMP_", this2)
                    if(any(grepl(pat, lines, useBytes = TRUE))) {
                        used <- c(used, this)
                        if(f == "C" && !have_c) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            msg <- "SHLIB_OPENMP_CFLAGS is included in PKG_CFLAGS without any C files\n"
                            printLog(Log, "  ", m, ": ", msg)
                            next
                        }
                        ## as from R 3.6.0, PKG_FFLAGS is by default
                        ## used for both fixed- and free-form files.
                        if(f == "F" && !(have_f || have_f9x)) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            msg <- "SHLIB_OPENMP_FFLAGS is included in PKG_FFLAGS without any Fortran files\n"
                            printLog(Log, "  ", m, ": ", msg)
                            next
                        }
                        f_or_fc <- "F"
                        if(f == "FC") {
                            if(any(grepl("SHLIB_OPENMP_FCFLAGS",
                                         lines, useBytes = TRUE))) {
                                f_or_fc <- "FC"
                                if (!any) noteLog(Log)
                                any <- TRUE
                                msg <- "SHLIB_OPENMP_FFLAGS is preferred to SHLIB_OPENMP_FCFLAGS in PKG_FCFLAGS\n"
                                printLog(Log, "  ", m, ": ", msg)
                            }
                        }
                        if(f == "FC" && !have_f9x) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            msg <- sprintf("SHLIB_OPENMP_%sFLAGS is included in PKG_FCFLAGS without any free-form Fortran files\n", f_or_fc)
                            printLog(Log, "  ", m, ": ", msg)
                            next
                        }
                        if(f == "CXX" && !have_cxx) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            msg <- "SHLIB_OPENMP_CXXFLAGS is included in PKG_CXXFLAGS without any C++ files\n"
                            printLog(Log, "  ", m, ": ", msg)
                            next
                        }
                        ## The recommendation is to use _F[C]FLAGS to
                        ## compile and _CFLAGS or _CXXFLAGS to link with Fortran
                        ## code (which is linked by the C or C++ compiler)
                        c_or_cxx <- if(have_cxx) "CXXFLAGS" else "CFLAGS"
                        this2 <- if (f %in% c("F", "FC")) c_or_cxx else this
                        pat2 <- paste0("SHLIB_OPENMP_", this2)
                        if(!any(grepl(pat2, lines[c1], useBytes = TRUE))) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            msg <- if(anyInLIBS) {
                                if (f == "F")
                                    sprintf("SHLIB_OPENMP_FFLAGS is included in PKG_FFLAGS but not SHLIB_OPENMP_%s in PKG_LIBS\n", c_or_cxx)
                                 else if (f == "FC")
                                     sprintf("SHLIB_OPENMP_%sFLAGS is included in PKG_FCFLAGS but not SHLIB_OPENMP_%s in PKG_LIBS\n", f_or_fc, c_or_cxx)
                               else
                                    sprintf("SHLIB_OPENMP_%s is included in PKG_%s but not in PKG_LIBS\n",
                                            this, this)
                            } else {
                                msg3 <- TRUE
                                sprintf("SHLIB_OPENMP_%s is included in PKG_%s but no OPENMP macro in PKG_LIBS\n",
                                           this, this)
                            }
                            printLog(Log, "  ", m, ": ", msg)
                        }
                    } else {
                        ## several packages have the wrong flag
                        pat <- paste0("^[[:space:]]*PKG_", this, ".*SHLIB_OPENMP_")
                        if(any(c2 <- grepl(pat, lines, useBytes = TRUE))) {
                            if (!any) noteLog(Log)
                            any <- TRUE
                            if (!anyInLIBS) msg3 <- TRUE
                            ## assume just one
                            l <- lines[c2][1L]
                            found <- sub(".*SHLIB_OPENMP_([A-Z]*).*", "\\1", l, useBytes = TRUE)
                            printLog(Log,"  ", m, ": ",
                                     sprintf("incorrect macro SHLIB_OPENMP_%s included in PKG_%s\n",
                                             found, this))
                        }
                    }
                }
                ## Now check if PKG_LIBS includes a macro that is not used
                ## in PKG_*FLAGS, or more than one.
                cnt <- 0L
                for (f in c("C", "CXX", "F", "FC"))  {
                    this <- paste0(f, "FLAGS")
                    pat2 <- paste0("SHLIB_OPENMP_", this)
                    res <- any(grepl(pat2 , lines[c1], useBytes = TRUE))
                    cnt <- cnt + res
                    if (res && f %in% c( "F", "FC"))  {
                        if (!any) noteLog(Log)
                        any <- TRUE
                        printLog(Log,"  ", m, ": ",
                                 sprintf("SHLIB_OPENMP_%s is included in PKG_LIBS but linking is by %s\n",
                                         this,
                                         if(have_cxx) "C++" else "C"))
                         next
                    }
                     if (res &&
                         ((!have_cxx && f == "CXX") || (have_cxx && f == "C"))) {
                        if (!any) noteLog(Log)
                        any <- TRUE
                        printLog(Log,"  ", m, ": ",
                                 sprintf("SHLIB_OPENMP_%s is included in PKG_LIBS but linking is by %s\n",
                                         this,
                                         if(have_cxx) "C++" else "C"))
                         next
                    }
                    if (this %in% used) next
                    ## Fortran exceptions
                    if (((!have_cxx && f == "C") || (have_cxx && f == "CXX"))
                        && any(c("FFLAGS", "FCFLAGS") %in% used)) next
                    if (res) {
                        if (!any) noteLog(Log)
                        any <- TRUE
                        printLog(Log,"  ", m, ": ",
                                 sprintf("SHLIB_OPENMP_%s is included in PKG_LIBS but not in PKG_%s\n",
                                         this, this))
                   }
                }
                if (cnt > 1L) {
                    if (!any) noteLog(Log)
                    any <- TRUE
                    if (!anyInLIBS) msg3 <- TRUE
                    printLog(Log, "  ", m, ": ",
                             "it is not portable to include multiple",
                             " SHLIB_OPENMP_*' macros in PKG_LIBS",
                             "\n")
                }

                ## several packages include one of these in PKG_CPPFLAGS
                ## which is used for both C and C++ code.
                pat <- "^[[:space:]]*PKG_CPPFLAGS.*SHLIB_OPENMP_"
                if(any(grepl(pat, lines, useBytes = TRUE))) {
                    if (!any) noteLog(Log)
                    any <- TRUE
                    msg2 <- TRUE
                    printLog(Log,"  ", m, ": ",
                             "it is not portable to include",
                             " SHLIB_OPENMP_*' macros in PKG_CPPFLAGS",
                             "\n")
                }
            }
            if (!any) resultLog(Log, "OK")
            else {
                wrapLog("Use of these macros is discussed in sect 1.2.1.1 of",
                        paste0(sQuote("Writing R Extensions"), "."),
                        "The macros for different languages may differ",
                        "so the matching macro must be used in",
                        "PKG_CXXFLAGS (etc) and match that used in",
                        "PKG_LIBS (except for Fortran: see the manual).\n")
                if (msg2)
                    wrapLog("PKG_CPPFLAGS is used for both C and C++ code",
                            "so it is not portable to use it",
                            "for these macros.\n")
                if (msg3)
                    wrapLog("Using a SHLIB_OPENMP_ macro for compilation",
                            "but none in PKG_LIBS",
                            "is not portable and may result in",
                            "installation errors.\n")
            }
        }

        ## Check include directives for use of R_HOME which may contain
        ## spaces for which there is no portable way to quote/escape.
        all_files <-
            dir(".",
                pattern = "^(Makefile|Makefile.in|Makefile.win|makefile|GNUmakefile)$",
                recursive = TRUE)
        all_files <- unique(sort(all_files))
        if(length(all_files)) {
            checkingLog(Log, "include directives in Makefiles")
            bad_lines <-
                lapply(all_files,
                       function(f) {
                           s <- readLines(f, warn = FALSE)
                           grep("^include .*R_HOME", s, value = TRUE)
                       })
            bad_files <- all_files[lengths(bad_lines) > 0L]
            if(length(bad_files)) {
                noteLog(Log,
                        "Found the following Makefile(s) with an include directive with a pathname using R_HOME:")
                printLog0(Log, .format_lines_with_indent(bad_files),
                          "\n")
                msg <-
                    c("Even though not recommended, variable R_HOME may contain spaces.",
                      "Makefile directives use space as a separator and there is no portable",
                      "way to quote/escape the space in Make rules and directives.  However,",
                      "one can and should quote pathnames when passed from Makefile to the",
                      "shell, and this can be done specifically when invoking Make recursively.",
                      "It is therefore recommended to use the Make '-f' option to include files",
                      "in directories specified using R_HOME.  This option can be specified",
                      "multiple times to include multiple Makefiles.  Note that 'Makeconf' is",
                      "included automatically into top-level makefile of a package.",
                      "More information can be found in 'Writing R Extensions'.")
                printLog0(Log, paste(msg, collapse = "\n"), "\n")
            } else resultLog(Log, "OK")
        }

    }

    check_src <- function() {
        Check_pragmas <- Sys.getenv("_R_CHECK_PRAGMAS_", "FALSE")
        if(config_val_to_logical(Check_pragmas) &&
           any(dir.exists(c("src", "inst/include")))) {
            checkingLog(Log, "pragmas in C/C++ headers and code")
            ans <- .check_pragmas('.')
            if(length(ans)) {
                warn <- attr(ans, "warn")
                port <- attr(ans, "port")
                if(length(warn) || length(port))
                {
                    warningLog(Log)
                    msg <- character()
                    rest <- ans
                    if(length(warn)) {
                        msg <- c(msg, if(length(warn) == 1L)
                                          "File which contains pragma(s) suppressing important diagnostics"
                                      else
                                          "Files which contain pragma(s) suppressing important diagnostics",
                                 .pretty_format(warn))
                        rest <- setdiff(ans, warn)
                    }
                    if(length(port)) {
                        msg <- c(msg, if(length(port) == 1L)
                                          "File which contains non-portable pragma(s)"
                                      else
                                          "Files which contain non-portable pragma(s)",
                                 .pretty_format(port))
                    }
                    if(length(rest)) {
                        msg <- c(msg, if(length(rest) == 1L)
                                          "File which contains pragma(s) suppressing diagnostics:"
                                      else
                                          "Files which contain pragma(s) suppressing diagnostics:",
                                 .pretty_format(rest))
                    }
                } else {
                    noteLog(Log)
                    msg <- if(length(ans) == 1L)
                               "File which contains pragma(s) suppressing diagnostics:"
                           else
                               "Files which contain pragma(s) suppressing diagnostics:"
                    msg <- c(msg, .pretty_format(ans))
                }
                printLog0(Log, paste(c(msg,""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        Check_flags <- Sys.getenv("_R_CHECK_COMPILATION_FLAGS_", "FALSE")
        if(config_val_to_logical(Check_flags)) {
            instlog <- if (startsWith(install, "check"))
                           install_log_path
                       else
                           file.path(pkgoutdir, "00install.out")
            if (file.exists(instlog) && dir.exists('src')) {
                checkingLog(Log, "compilation flags used")
                lines <- readLines(instlog, warn = FALSE)
                ## skip stuff before building libs
                ll <- grep("^[*][*][*] libs", lines, useBytes = TRUE)
                if (length(ll)) lines <- lines(-1:ll[1L])
                poss <- grep(" -[Wmf]", lines,  useBytes = TRUE, value = TRUE)
                ## compilation lines start at the left margin,
                ## and are not configure/echo lines
                poss <- grep("^(\\s|checking|echo)", poss, perl = TRUE,
                             invert = TRUE, value = TRUE, useBytes = TRUE)
                tokens <- unique(unlist(strsplit(poss, " ", perl = TRUE,
                                                 useBytes = TRUE)))
                tokens <- gsub('["\']$', "", tokens,
                               perl = TRUE, useBytes = TRUE)
                warns <- grep("^[-]W", tokens,
                              value = TRUE, perl = TRUE, useBytes = TRUE)
                ## Not sure -Wextra and -Weverything are portable, though
                ## -Werror is not compiler independent
                ##   (as what is a warning is not)
                ## -Wno-dev is from qt, not a compiler flag.
                except <- Sys.getenv("_R_CHECK_COMPILATION_FLAGS_KNOWN_", "")
                except <- unlist(strsplit(except, "\\s", perl = TRUE))
                warns <- setdiff(warns,
                                 c(except, "-Wall", "-Wextra", "-Weverything",
                                   "-Wno-dev"))
                warns <- warns[!startsWith(warns, "-Wl,")] # linker flags
                diags <- grep(" -fno-diagnostics-show-option", tokens,
                              useBytes = TRUE, value = TRUE)
                ## next set are about unsafe optimizations
                opts <- grep("-f(fast-math|unsafe-math-optimizations|associative-math|reciprocal-math)",
                             tokens, useBytes = TRUE, value = TRUE)
                machs <- grep("^[-]m", tokens,
                              value = TRUE, perl = TRUE, useBytes = TRUE)
                ## The only -m flag which is reasonably portable is -mtune
                machs <- setdiff(machs,
                                 c(except, c("-m", # not a flag
                                             "-msse2", "-mfpmath=sse", # SAFE_FFLAGS
                                             "-m32", # BRugs
                                             "-m64", # RcppParallel
                                             "-multiply_defined" # macOS
                                             )))
                machs <- machs[!startsWith(machs, "-mtune=")]
                warns <- c(warns, diags, opts, machs)
                if(any(startsWith(warns, "-Wno-")) || length(diags)) {
                    warningLog(Log)
                    msg <- c("Compilation used the following non-portable flag(s):",
                             .pretty_format(sort(warns)),
                             "including flag(s) suppressing warnings")
                    printLog0(Log, paste(c(msg,""), collapse = "\n"))
                } else if(length(warns)) {
                    noteLog(Log) # or warningLog?
                    msg <- c("Compilation used the following non-portable flag(s):",
                             .pretty_format(sort(warns)))
                    printLog0(Log, paste(c(msg,""), collapse = "\n"))
                } else
                    resultLog(Log, "OK")
            }
        }
    }

    check_sos <- function() {
        checkingLog(Log, "compiled code")
        ## from sotools.R
        Rcmd <- paste(opWarn_string, "\n",
                      sprintf("tools:::check_compiled_code(\"%s\")",
                              file.path(libdir, pkgname)))
        out <- R_runR0(Rcmd, R_opts2, "R_DEFAULT_PACKAGES=NULL")
        if(length(out) == 1L && startsWith(out, "Note:")) {
            ## This will be a note about symbols.rds not being available
            if(!is_base_pkg) {
                noteLog(Log)
                printLog0(Log, c(out, "\n"))
            } else resultLog(Log, "OK")
        } else if(length(out)) {
            ## If we have named objects then we have symbols.rds and
            ## will not be picking up symbols just in system libraries.
            haveObjs <- any(grepl("^ *Object", out))
            pat <- paste("possibly from",
                         sQuote("(abort|assert|exit|_exit|_Exit|stop)"))
            if(haveObjs && any(grepl(pat, out)) && pkgname %notin% "parallel")
                ## need _exit in forked child
                warningLog(Log)
            else {
                ## look for Fortran detritus
                pat1 <- paste("possibly from", sQuote("(open|close|rewind)"))
                pat2 <- paste("possibly from", sQuote("(read|write)"))
                pat3 <- paste("possibly from", sQuote("close"))
                pat4 <- paste("possibly from", sQuote("open"))
                if(haveObjs &&
                   (any(grepl(pat1, out)) && !any(grepl(pat2, out))) ||
                   (any(grepl(pat3, out)) && !any(grepl(pat4, out))) ||
                   (any(grepl(pat4, out)) && !any(grepl(pat3, out))))
                    warningLog(Log)
                else noteLog(Log)
            }
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            nAPIs <- length(grep("Found non-API", out))
            nRS <- length(grep("Found no call", out))
            nBad <- length(grep(", possibly from ", out))
            msg <- if (nBad) {
                if(haveObjs)
                    c("Compiled code should not call entry points which",
                      "might terminate R nor write to stdout/stderr instead of",
                      "to the console, nor use Fortran I/O nor system RNGs.\n")
                else
                    c("Compiled code should not call entry points which",
                      "might terminate R nor write to stdout/stderr instead of",
                      "to the console, nor use Fortran I/O nor system RNGs.",
                      "The detected symbols are linked",
                      "into the code but might come from libraries",
                      "and not actually be called.\n")
            } else character()
            if(nAPIs)
                msg <- c(msg,
                         "Compiled code should not call non-API entry points in R.\n")
            if(nRS)
                msg <- c(msg,
                         "It is good practice to register native routines and to disable symbol search.\n")
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
        out <- R_runR0(Rcmd, opts, env1, arch = arch)
        if(length(st <- attr(out, "status"))) {
            errorLog(Log)
            wrapLog("Loading this package had a fatal error",
                    "status code ", st,  "\n")
            if(length(out))
                printLog0(Log,
                          paste(c("Loading log:", out, ""),
                                collapse = "\n"))
            summaryLog(Log)
            do_exit(1L)
        }
        if (any(startsWith(out, "Error"))) {
            errorLog(Log)
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("\nIt looks like this package",
                    "has a loading problem: see the messages",
                    "for details.\n")
            maybe_exit(1L)
        } else resultLog(Log, "OK")

        checkingLog(Log, "whether the package can be loaded with stated dependencies")
        out <- R_runR0(Rcmd, opts, c(env, env1), arch = arch)
        if (any(startsWith(out, "Error")) || length(attr(out, "status"))) {
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
            wrapLog("\nIt looks like this package",
                    "(or one of its dependent packages)",
                    "has an unstated dependence on a standard",
                    "package.  All dependencies must be",
                    "declared in DESCRIPTION.\n")
            wrapLog(msg_DESCRIPTION)
        } else resultLog(Log, "OK")

        checkingLog(Log, "whether the package can be unloaded cleanly")
        Rcmd <- sprintf("suppressMessages(library(%s)); cat('\n---- unloading\n'); detach(\"package:%s\")",
                        pkgname, pkgname)
        out <- R_runR0(Rcmd, opts, c(env, env1), arch = arch)
        if (any(grepl("^(Error|\\.Last\\.lib failed)", out)) ||
            length(attr(out, "status"))) {
            warningLog(Log)
            ll <- grep("---- unloading", out)
            if(length(ll)) {
                ll <- ll[length(ll)]
                out <- out[ll:length(out)]
            }
            printLog0(Log, paste(c(out, ""), collapse = "\n"))
        } else resultLog(Log, "OK")

        ## and if it has a namespace, that we can load/unload just
        ## the namespace
        if (file.exists(file.path(pkgdir, "NAMESPACE"))) {
            checkingLog(Log, "whether the namespace can be loaded with stated dependencies")
            Rcmd <-
                sprintf("%s\ntools:::.load_namespace_rather_quietly(\"%s\")",
                        opWarn_string, pkgname)
            out <- R_runR0(Rcmd, opts, c(env, env1), arch = arch)
            any <- FALSE
            if (any(startsWith(out, "Error")) || length(attr(out, "status"))) {
                warningLog(Log)
                any <- TRUE
            } else {
                ## Drop tcltk warning if no DISPLAY variable
                if(pkgname == "tcltk")
                    out <- filtergrep("Warning: no DISPLAY variable so Tk is not available",
                                      out, fixed = TRUE)
                ## Drop warnings about replacing previous imports unless
                ## these were disabled for the installation check.
                check_imports_flag <-
                    Sys.getenv("_R_CHECK_REPLACING_IMPORTS_", "TRUE")
                if(config_val_to_logical(check_imports_flag))
                    out <- filtergrep("Warning: replacing previous import", out,
                                      fixed = TRUE)
                if(any(startsWith(out, "Warning"))) {
                    noteLog(Log)
                    any <- TRUE
                }
            }
            if(any) {
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
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
                R_runR0(Rcmd, opts, "R_DEFAULT_PACKAGES=NULL", arch = arch)
            else R_runR0(Rcmd, opts, env1)
            if (any(grepl("^(Error|\\.onUnload failed)", out)) ||
                length(attr(out, "status"))) {
                warningLog(Log)
                ll <- grep("---- unloading", out)
                if(length(ll)) {
                    ll <- ll[length(ll)]
                    out <- out[ll:length(out)]
                }
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
            } else resultLog(Log, "OK")
        }

        ## No point in this test if already installed in .Library
        if (pkgname %notin% dir(.Library)) {
            checkingLog(Log, "loading without being on the library search path")
            Rcmd <- sprintf("library(%s, lib.loc = '%s')", pkgname, libdir)
            opts <- if(nzchar(arch)) R_opts4 else R_opts2
            env <- setRlibs(pkgdir = pkgdir, libdir = libdir,
                            self2 = FALSE, quote = TRUE)
            if(nzchar(arch)) env <- c(env, "R_DEFAULT_PACKAGES=NULL")
            out <- R_runR0(Rcmd, opts, env, arch = arch)
            if (any(startsWith(out, "Error"))) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                wrapLog("\nIt looks like this package",
                        "has a loading problem when not on .libPaths:",
                        "see the messages for details.\n")
            } else resultLog(Log, "OK")
        }
        if(!extra_arch && !is_base_pkg) {
            check_S3reg <-
                Sys.getenv("_R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_", "NA")
            check_S3reg <- if(check_S3reg == "NA") check_incoming else {
                config_val_to_logical(check_S3reg)
            }
            if(check_S3reg) {
                checkingLog(Log, "use of S3 registration")
                Rcmd <- sprintf("suppressWarnings(suppressPackageStartupMessages(loadNamespace('%s', lib.loc = '%s')))",
                                pkgname, libdir)
                opts <- if(nzchar(arch)) R_opts4 else R_opts2
                env <- Sys.getenv("_R_LOAD_CHECK_OVERWRITE_S3_METHODS_",
                                  "NA")
                env <- paste0("_R_LOAD_CHECK_OVERWRITE_S3_METHODS_=",
                              if(env == "all") env else pkgname)
                ## <FIXME>
                ## Oh dear.  R-ints says that if env var
                ## '_R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_' is set to
                ## something true,
                ##   report already registered S3 methods in
                ##   base/recommended packages which are overwritten
                ##   when this package's namespace is loaded.
                ## As of 2017-12, to make this work as documented we
                ## really need to load all base and recommended
                ## ("standard") packages which register S3 methods
                ## first, which takes *quite some time*.
                ## There really should be a better way ...
                ## Running with
                ##   R_DEFAULT_PACKAGES=MASS,Matrix,boot,class,cluster,grDevices,graphics,grid,lattice,mgcv,nlme,nnet,parallel,rpart,spatial,splines,stats,survival,tcltk,tools,utils
                ## does not suppress package startup messages: so try to
                ## load the relevant standard package namespaces quietly.
                ## When checking a standard package p we should preload
                ## only the standard packages not depending on p.
                preloads <-
                    c("MASS", "Matrix", "boot", "class", "cluster",
                      "grDevices",  "graphics", "grid", "lattice",
                      "mgcv", "nlme", "nnet", "parallel", "rpart",
                      "spatial", "splines", "stats", "survival",
                      "tcltk", "tools", "utils")
                if(!is.na(match(pkgname, preloads))) {
                    rdepends <-
                        .get_standard_package_dependencies(reverse = TRUE,
                                                           recursive = TRUE)
                    preloads <- setdiff(preloads,
                                        c(pkgname, rdepends[[pkgname]]))
                }
                Rcmd <-
                    c(sprintf("suppressPackageStartupMessages(loadNamespace('%s', lib.loc = '%s'))",
                              preloads,
                              .Library),
                      Rcmd)
                env <- c(env, "R_DEFAULT_PACKAGES=NULL")
                out <- R_runR0(Rcmd, opts, env, arch = arch)
                ## </FIXME>
                if (any(grepl("^Registered S3 method.*standard package.*overwritten", out, useBytes = TRUE))) {
                    out <- out[!startsWith(out, "<environment: namespace:")]
                    warningLog(Log)
                    printLog0(Log, paste(out, collapse = "\n"), "\n")
                } else resultLog(Log, "OK")
            }
        }
    }

    run_examples <- function()
    {
        run_one_arch <- function(exfile, exout, arch = "")
        {
            any <- FALSE
            ## moved here to avoid WARNING + OK
            if (nzchar(enc) && is_ascii) {
                warningLog(Log,
                           paste("checking a package with encoding ",
                                 sQuote(e), " in an ASCII locale\n"))
                any <- TRUE
            }
            Ropts <- if (nzchar(arch)) R_opts3 else R_opts
            if (use_valgrind) Ropts <- paste(Ropts, "-d valgrind")
            t1 <- proc.time()
            tlim <- get_timeout(Sys.getenv("_R_CHECK_EXAMPLES_ELAPSED_TIMEOUT_",
                                Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
            ## might be diff-ing results against tests/Examples later
            ## so force LANGUAGE=en
            status <- R_runR0(NULL, c(Ropts, enc),
                              c("LANGUAGE=en", "_R_CHECK_INTERNALS2_=1",
                                if(nzchar(arch)) env0, jitstr, elibs),
                              stdout = exout, stderr = exout,
                              stdin = exfile, arch = arch, timeout = tlim)
            t2 <- proc.time()
            if (status) {
                errorLog(Log, "Running examples in ",
                         sQuote(basename(exfile)),
                         " failed")
                ## Try to spot the offending example right away.
                ## Sometimes processes need extra time to shut down,
                ## particularly parallel cluster on Windows, hence a hack to retry after 2 sec:
                txt <- tryCatch(suppressWarnings(readLines(exout, warn = FALSE)),
                                error = function(e){Sys.sleep(2); readLines(exout, warn = FALSE)})
                txt <- paste(txt, collapse = "\n")
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
            bad <- FALSE
            ## Sometimes processes need extra time to shut down,
            ## particularly parallel cluster on Windows, hence a hack to retry after 2 sec:
            lines <- tryCatch(suppressWarnings(readLines(exout, warn = FALSE)),
                              error = function(e){Sys.sleep(2); readLines(exout, warn = FALSE)})
            bad_lines <- grep("^Warning: .*is deprecated.$",
                              lines, useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                bad <- TRUE
                warningLog(Log, "Found the following significant warnings:\n")
                printLog0(Log, .format_lines_with_indent(bad_lines), "\n")
                wrapLog("Deprecated functions may be defunct as",
                        "soon as of the next release of R.\n",
                        "See ?Deprecated.\n")
            }
            bad_lines <- grep("^Warning.*screen devices should not be used in examples",
                              lines, useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                if(!bad) {
                    warningLog(Log,
                               "Found the following significant warnings:")
                    bad <- TRUE
                }
                printLog0(Log, .format_lines_with_indent(bad_lines), "\n")
                wrapLog("dev.new() is the preferred way to open a new device,",
                        "in the unlikely event one is needed.")
            }
            bad_lines <- grep("^Warning: .*simultaneous processes spawned$",
                              lines, useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                if(!bad) {
                    warningLog(Log,
                               "Found the following significant warnings:")
                    bad <- TRUE
                }
                printLog0(Log, .format_lines_with_indent(bad_lines), "\n")
                wrapLog("Note that CRAN packages must never use more than two",
                        "cores simultaneously during their checks.")
            }
            bad_lines <- grep("^Warning: working directory was changed to",
                              lines, useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                if(!bad) {
                    warningLog(Log,
                               "Found the following significant warnings:")
                    bad <- TRUE
                }
                printLog0(Log, .format_lines_with_indent(bad_lines), "\n")
            }
            bad_lines <- grep("^Warning: items .* were removed from the search path",
                              lines, useBytes = TRUE, value = TRUE)
            if(length(bad_lines)) {
                if(!bad) {
                    warningLog(Log,
                               "Found the following significant warnings:")
                    bad <- TRUE
                }
                printLog0(Log, .format_lines_with_indent(bad_lines), "\n")
            }
            any <- any || bad

            if (!any && !(check_incoming && do_timings))
                resultLog(Log, "OK")

            if (do_timings) {
                theta <-
                    as.numeric(Sys.getenv("_R_CHECK_EXAMPLE_TIMING_THRESHOLD_",
                                          "5"))
                tfile <- paste0(pkgname, "-Ex.timings")
                times <-
                    utils::read.table(tfile, header = TRUE, row.names = 1L,
                                      colClasses = c("character", rep.int("numeric", 3)))
                o <- order(times[[1L]] + times[[2L]], decreasing = TRUE)
                times <- times[o, ]

                keep <- ((times[[1L]] + times[[2L]] > theta) |
                         (times[[3L]] > theta))
                if(any(keep)) {
                    if(!any && check_incoming) {
                        noteLog(Log)
                        any <- TRUE
                    }
                    printLog(Log,
                             sprintf("Examples with CPU or elapsed time > %gs\n",
                                     theta))
                    out <- utils::capture.output(format(times[keep, ]))
                    printLog0(Log, paste(out, collapse = "\n"), "\n")
                }

                theta <-
                    as.numeric(Sys.getenv("_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_",
                                          NA_character_))
                if(!is.na(theta)) {
                    keep <- ((times[[1L]] + times[[2L]]) >=
                              pmax(theta * times[[3L]], 1))
                    if(any(keep)) {
                        if(!any && check_incoming) {
                            noteLog(Log)
                            any <- TRUE
                        }
                        printLog(Log,
                                 sprintf("Examples with CPU time > %g times elapsed time\n",
                                         theta))
                        bad <- times[keep, ]
                        ratio <- (bad[[1L]] + bad[[2L]]) / bad[[3L]]
                        bad <- cbind(bad, ratio = round(ratio, 3L))
                        bad <- bad[order(bad$ratio, decreasing = TRUE), ]
                        out <- utils::capture.output(format(bad))
                        printLog0(Log, paste(out, collapse = "\n"), "\n")
                    }
                }

                if(!any && check_incoming)
                    resultLog(Log, "OK")
            }

            ## Try to compare results from running the examples to
            ## a saved previous version.
            exsave <- file.path(pkgdir, test_dir, "Examples",
                                paste0(pkgname, "-Ex.Rout.save"))
            if (file.exists(exsave)) {
                checkingLog(Log, "differences from ",
                            sQuote(basename(exout)),
                            " to ", sQuote(basename(exsave)))
                cmd <- paste0("invisible(tools::Rdiff('",
                              exout, "', '", exsave, "',TRUE,TRUE))")
                out <- R_runR0(cmd, R_opts2)
                resultLog(Log, "OK")
                if(length(out))
                    printLog0(Log, paste(c("", out, ""), collapse = "\n"))
            }

            TRUE
        }

        checkingLog(Log, "examples")
        if (!do_examples) resultLog(Log, "SKIPPED")
        else {
            pkgtopdir <- file.path(libdir, pkgname)
            cmd <- sprintf('tools:::.createExdotR("%s", "%s", silent = TRUE, use_gct = %s, addTiming = %s, commentDontrun = %s, commentDonttest = %s)',
                           pkgname, pkgtopdir, use_gct, do_timings,
                           !run_dontrun, !run_donttest)
            Rout <- tempfile("Rout")
            ## any arch will do here
            status <- R_runR0(cmd, R_opts2, "LC_ALL=C",
                              stdout = Rout, stderr = Rout)
            exfile <- paste0(pkgname, "-Ex.R")
            if (status) {
                errorLog(Log,
                         paste("Running massageExamples to create",
                               sQuote(exfile), "failed"))
                printLog0(Log,
                          paste(readLines(Rout, warn = FALSE),
                                collapse = "\n"),
                          "\n")
                maybe_exit(1L)
            }
            ## It ran, but did it create any examples?
            if (file.exists(exfile)) {
                enc <- if (!is.na(e <- desc["Encoding"])) {
                    paste0("--encoding=", e)
                } else ""
                if (!this_multiarch) {
                    exout <- paste0(pkgname, "-Ex.Rout")
                    if(!run_one_arch(exfile, exout)) maybe_exit(1L)
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
                                summaryLog(Log)
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
                    if (!res) maybe_exit(1L)
                }
                cntFile <- paste0(exfile, "-cnt")
                if (file.exists(cntFile)) {
                    unlink(cntFile)
                    if (as_cran)
                        printLog(Log, "** found \\donttest examples:",
                                 " check also with --run-donttest\n")
                }
            } else {
                resultLog(Log, "NONE")
                no_examples <<- TRUE
            }
        }
    }

    run_tests <- function()
    {
        if (!extra_arch && !is_base_pkg) {
            checkingLog(Log, "for unstated dependencies in ", sQuote(test_dir))
            Rcmd <- paste(opW_shE_F_str,
                          sprintf("tools:::.check_packages_used_in_tests(\"%s\", \"%s\")\n", pkgdir, test_dir))

            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                warningLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
                # wrapLog(msg_DESCRIPTION)
            } else resultLog(Log, "OK")
        }

        if (test_dir == "tests")
            checkingLog(Log, "tests")
        else
            checkingLog(Log, "tests in ", sQuote(test_dir))

        run_one_arch <- function(arch = "")
        {
            testsrcdir <- file.path(pkgdir, test_dir)
            testdir <- file.path(pkgoutdir, "tests")
            if(nzchar(arch)) testdir <- paste(testdir, arch, sep = "_")
            if(!dir.exists(testdir)) dir.create(testdir, mode = "0755")
            if(!dir.exists(testdir)) {
                errorLog(Log,
                         sprintf("unable to create %s", sQuote(testdir)))
                summaryLog(Log)
                do_exit(1L)
            }
            file.copy(Sys.glob(paste0(testsrcdir, "/*")),
                      testdir, recursive = TRUE)
            setwd(testdir)
            logf <- gsub("\\", "/", tempfile(), fixed=TRUE)
            extra <- c(if(use_gct) "use_gct = TRUE",
                       if(use_valgrind) "use_valgrind = TRUE",
                       if(!stop_on_test_error) "stop_on_error = FALSE",
                       paste0('Log="', logf, '"'))
            ## might be diff-ing results against tests/*.R.out.save
            ## so force LANGUAGE=en
            cmd <- paste0("tools:::.runPackageTestsR(",
                          paste(extra, collapse = ", "), ")")
            t1 <- proc.time()
            tlim <- get_timeout(Sys.getenv("_R_CHECK_TESTS_ELAPSED_TIMEOUT_",
                                Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
            status <- R_runR0(cmd,
                              if(nzchar(arch)) R_opts4 else R_opts2,
                              env = c("LANGUAGE=en",
                                     "_R_CHECK_INTERNALS2_=1",
                              if(nzchar(arch)) env0, jitstr, elibs_tests),
                              stdout = "", stderr = "", arch = arch,
                              timeout = tlim)
            t2 <- proc.time()
            if (status) {
                print_time(t1, t2, Log)
                errorLog(Log)
                if (Log$con > 0L && file.exists(logf)) {
                    ## write individual results only to 00check.log
                    cat(readLines(logf, warn = FALSE),
                        sep = "\n", file = Log$con)
                }
                ## Don't just fail: try to log where the problem occurred.
                ## First, find the test(s) which failed.
                ## (Maybe there was an error without a failing test.)
                bad_files <- dir(".", pattern="\\.Rout\\.fail$")
                if (length(bad_files)) {
                    ## Read in output from the failed test(s)
                    ## (As from R 3.4.0 there can be more than one
                    ## with option --no-stop-on-test-error.)
                    for(f in bad_files) {
                        lines <- readLines(f, warn = FALSE)
                        f <- file.path(test_dir, sub("out\\.fail$", "", f))
                        src_files <- dir(".", pattern = "\\.[rR]$")
                        if (basename(f) %notin% src_files) {
                            f <- sub("R$", "r", f) # This assumes only one of foo.r and foo.R exists.
                            if (basename(f) %notin% src_files)
                                f <- sub("r$", "[rR]", f) # Just in case the test script got deleted somehow, show the pattern.
                        }
                        keep <- as.integer(Sys.getenv("_R_CHECK_TESTS_NLINES_",
                                                      "13"))
                        ## keep = 0 means keep all of it, but we will
                        ## always omit the R preamble and start at the first
                        ## line with an R prompt.
                        ll <- length(lines)
                        st <- grep("^>", lines, useBytes = TRUE)
                        if (length(st)) {
                            lines <- lines[st[1L]:ll]
                            ll <- length(lines)
                        }
                        if (keep > 0L)
                            lines <- lines[max(1L, ll-keep-1L):ll]
                        if (R_check_suppress_RandR_message)
                            lines <- filtergrep('^Xlib: *extension "RANDR" missing on display',
                                                lines, useBytes = TRUE)
                        printLog(Log, sprintf("Running the tests in %s failed.\n",
                                              sQuote(f)))
                        printLog(Log, if(keep > 0L && keep < ll)
                                 sprintf("Last %i lines of output:\n", keep)
                                 else "Complete output:\n")
                        printLog0(Log, .format_lines_with_indent(lines), "\n")
                    }
                }
                return(FALSE)
            } else {
                print_time(t1, t2, Log)
                resultLog(Log, "OK")
                if (Log$con > 0L && file.exists(logf)) {
                    ## write results only to 00check.log
                    lines <- readLines(logf, warn = FALSE)
                    cat(lines, sep="\n", file = Log$con)
                    unlink(logf)
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
                    if (arch %notin% R_check_skip_tests_arch) {
                        printLog(Log, "** running tests for arch ",
                                 sQuote(arch), " ...")
                        res <- res & run_one_arch(arch)
                    }
            }
            if (!res) maybe_exit(1L)
        } else resultLog(Log, "SKIPPED")
    }

    run_vignettes <- function(desc)
    {
        theta <-
            as.numeric(Sys.getenv("_R_CHECK_VIGNETTE_TIMING_CPU_TO_ELAPSED_THRESHOLD_",
                                  NA_character_))

        libpaths <- .libPaths()
        .libPaths(c(libdir, libpaths))
        vigns <- pkgVignettes(dir = pkgdir)
        .libPaths(libpaths)
        if (is.null(vigns) || !length(vigns$docs)) return()

        if(do_install && !spec_install && !is_base_pkg && !extra_arch) {
            ## fake installs don't install inst/doc
            checkingLog(Log, "for unstated dependencies in vignettes")
            Rcmd <- paste(opW_shE_F_str,
                          sprintf("tools:::.check_packages_used_in_vignettes(package = \"%s\")\n",
                                  pkgname))
            out <- R_runR2(Rcmd, "R_DEFAULT_PACKAGES=NULL")
            if (length(out)) {
                noteLog(Log)
                printLog0(Log, paste(c(out, ""), collapse = "\n"))
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
	    .msg <- character()
            for (i in seq_along(vigns$docs)) {
                file <- vigns$docs[i]
                name <- vigns$names[i]
                engine <- vignetteEngine(vigns$engines[i])
                outputs[i] <- tryCatch({
                    find_vignette_product(name, what="weave", final=TRUE, dir=dir, engine = engine)
                }, error = function(e) {
		    .msg <<- c(.msg, conditionMessage(e))
	            NA}
		)
            }
            bad_vignettes <- vigns$docs[is.na(outputs)]
            if (nb <- length(bad_vignettes)) {
                any <- TRUE
                warningLog(Log)
		if (length(.msg)) printLog0(Log, .msg, "\n")
                msg <- ngettext(nb,
                                "Package vignette without corresponding single PDF/HTML:\n",
                                "Package vignettes without corresponding single PDF/HTML:\n", domain = NA)
                printLog0(Log, msg)
                printLog0(Log,
                          paste(c(paste("  ",
                                        sQuote(basename(bad_vignettes))),
                                  "", ""), collapse = "\n"))
            }
            defaultEncoding <- .get_package_metadata(pkgdir)["Encoding"]
            encs <- vapply(vigns$docs, getVignetteEncoding, "", default = defaultEncoding)
            bad_vignettes <- vigns$docs[encs == "non-ASCII"]
            if(nb <- length(bad_vignettes)) {
                if(!any) warningLog(Log)
                any <- TRUE
                msg <- ngettext(nb,
                         "Non-ASCII package vignette without specified encoding:\n",
                         "Non-ASCII package vignettes without specified encoding:\n", domain = NA)
                printLog0(Log, "  ", msg)
                printLog0(Log,
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
            lines <- filtergrep("^[[:space:]]*#", lines)
            if(any(grepl("[^/]R +CMD", lines))) {
                if(!any) warningLog(Log)
                any <- TRUE
                printLog(Log,
                         "  Found 'R CMD' in Makefile: should be '\"$(R_HOME)/bin/R\" CMD'\n")
            }
            contents <- readChar(f, file.size(f), useBytes = TRUE)
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
        for (i in seq_along(vigns$docs)) {
            v <- vigns$docs[i]
            enc <- vigns$encodings[i]
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
                printLog0(Log, "  ", msg)
                printLog0(Log,
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

            ## Re-building the vignette outputs also runs the code, so
            ## doing so as well creates no additional value unless the
            ## results are compared against saved results (which could
            ## perhaps also be integrated into buildVignettes().
            ## Hence, when re-building, skip running the code when there
            ## are no saved results.
            ## Could make this controllable via some env var ...

            build_vignettes <-
                parse_description_field(desc, "BuildVignettes", TRUE)
            if (!build_vignettes && as_cran) {
                ## FOSS packages must be able to rebuild their vignettes
                info <- analyze_license(desc["License"])
                build_vignettes <- info$is_verified
            }
            do_build_vignettes <- do_build_vignettes && build_vignettes
            skip_run_maybe <-
                R_check_vignettes_skip_run_maybe && do_build_vignettes

            vigns <- pkgVignettes(dir = pkgdir)
            savefiles <-
                file.path(dirname(vigns$docs),
                          paste0(vigns$names, ".Rout.save"))
            ran <- FALSE

            if(!skip_run_maybe || any(file.exists(savefiles))) {
                checkingLog(Log, "running R code from vignettes")
                res <- character()
                def_enc <- desc["Encoding"]
                if( (is.na(def_enc))) def_enc <- ""
                t1 <- proc.time()
                iseq <- seq_along(savefiles)
                if(skip_run_maybe)
                    iseq <- iseq[file.exists(savefiles)]
                out0 <- character()
                anyNOTE <- FALSE
                cat("\n")
                for (i in iseq) {
                    file <- vigns$docs[i]
                    name <- vigns$names[i]
                    enc <- vigns$encodings[i]
                    out1 <- c("  ", sQuote(basename(file)),
                              if(nzchar(enc)) paste("using", sQuote(enc)),
                              "...")
                    Rcmd <- paste0(opWarn_string, "\ntools:::.run_one_vignette('",
                                   basename(file), "', '", vigns$dir, "'",
                                   if (nzchar(enc))
                                       paste0(", encoding = '", enc, "'"),
                                   ", pkgdir='", vigns$pkgdir, "')")
                    outfile <- paste0(basename(file), ".log")
                    tlim <- get_timeout(Sys.getenv("_R_CHECK_ONE_VIGNETTE_ELAPSED_TIMEOUT_",
                                        Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
                    t1b <- proc.time()
                    status <- R_runR0(Rcmd,
                                      if (use_valgrind) paste(R_opts2, "-d valgrind") else R_opts2,
                                      ## add timing as footer, as BATCH does
                                      env = c(jitstr, "R_BATCH=1234", elibs,
                                              "_R_CHECK_INTERNALS2_=1"),
                                      stdout = outfile, stderr = outfile,
                                      timeout = tlim)
                    t2b <- proc.time()
                    out <- readLines(outfile, warn = FALSE)
                    pos <- which(out == " *** Run successfully completed ***")
                    if(!length(pos) || any(nzchar(out[seq_len(pos[1L] - 1L)])))
                        ran <- TRUE
                    savefile <- savefiles[i]
                    if(length(grep("^  When (running|tangling|sourcing)", out,
                                   useBytes = TRUE))) {
                        out1 <- c(out1, " failed\n")
                        keep <- as.numeric(Sys.getenv("_R_CHECK_VIGNETTES_NLINES_",
                                                      "10"))
                        res <- if (keep > 0)
                            c(res,
                              paste("when running code in", sQuote(basename(file))),
                              "  ...",
                              utils::tail(out, keep))
                        else
                            c(res,
                              paste("when running code in", sQuote(basename(file))),
                              out)

                    } else if(status || " *** Run successfully completed ***" %notin% out) {
                        ## (Need not be the final line if running under valgrind)
                        keep <- as.numeric(Sys.getenv("_R_CHECK_VIGNETTES_NLINES_",
                                                      "10"))
                        out1 <- c(out1, " failed to complete the test\n")
                        out <- c(out, "", "... incomplete output.  Crash?")
                        res <- if (keep > 0)
                            c(res,
                                 paste("when running code in", sQuote(basename(file))),
                                 "  ...",
                                 utils::tail(out, keep))
                        else
                            c(res,
                                 paste("when running code in", sQuote(basename(file))),
                                 out)
                    } else if (file.exists(savefile)) {
                        cmd <- paste0("invisible(tools::Rdiff('",
                                      outfile, "', '", savefile, "',TRUE,TRUE))")
                        out2 <- R_runR0(cmd, R_opts2)
                        if(length(out2)) {
                            out1 <- c(out1, print_time0(t1b, t2b))
                            anyNOTE <- TRUE
                            out1 <- c(out1, " NOTE\n")
                            out1 <- c(out1, paste("differences from",
                                                  sQuote(basename(savefile))))
                            out1 <- c(out1,
                                      paste(c("", out2, ""), collapse = "\n"))
                        } else {
                            out1 <- c(out1, print_time0(t1b, t2b))
                            out1 <- c(out1, " OK\n")
                            if (!config_val_to_logical(Sys.getenv("_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_", use_valgrind)))
                                unlink(outfile)
                        }
                    } else {
                        out1 <- c(out1, print_time0(t1b, t2b))
                        out1 <- c(out1, " OK\n")
                        if (!config_val_to_logical(Sys.getenv("_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_", use_valgrind)))
                            unlink(outfile)
                    }
                    out0 <- c(out0, out1)
                    cat(out1, sep = "")
                    if(!WINDOWS && !is.na(theta)) {
                        td <- t2b - t1b
                        cpu <- sum(td[-3L])
                        if(cpu >= pmax(theta * td[3L], 1)) {
                            ratio <- round(cpu/td[3L], 1L)
                            cat(sprintf("Running R code from vignette %s had CPU time %g times elapsed time\n",
                                        sQuote((basename(file))), ratio))
                        }
                    }
                }
                t2 <- proc.time()
                if(!ran) {
                    print_time(t1, t2, Log)
                    resultLog(Log, "NONE")
                    ## printLog0(Log, out0)
                    if (!is.null(Log) && Log$con > 0L)
                        cat(out0, sep ="", file = Log$con)
                } else {
                    print_time(t1, t2, Log)
                    if(R_check_suppress_RandR_message)
                        res <- filtergrep('^Xlib: *extension "RANDR" missing on display',
                                          res, useBytes = TRUE)
                    if(length(res)) {
                        if(length(grep("there is no package called", res,
                                       useBytes = TRUE))) {
                            warningLog(Log, "Errors in running code in vignettes:")
                            printLog0(Log, paste(c(res, "", ""), collapse = "\n"))
                        } else {
                            errorLog(Log, "Errors in running code in vignettes:")
                            printLog0(Log, paste(c(res, "", ""), collapse = "\n"))
                            maybe_exit(1L)
                        }
                    } else if(anyNOTE) noteLog(Log)
                    else resultLog(Log, "OK")
##                    printLog0(Log, out0)
                    if (!is.null(Log) && Log$con > 0L)
                        cat(out0, sep = "", file = Log$con)
                    if(!WINDOWS && !is.na(theta)) {
                        td <- t2 - t1
                        cpu <- sum(td[-3L])
                        if(cpu >= pmax(theta * td[3L], 1)) {
                            ratio <- round(cpu/td[3L], 1L)
                            printLog(Log,
                                     sprintf("Running R code from vignettes had CPU time %g times elapsed time\n",
                                             ratio))
                        }
                    }
                }
            }

            if (do_build_vignettes) {
                checkingLog(Log, "re-building of vignette outputs")
                ## copy the whole pkg directory to check directory
                ## so we can work in place, and allow ../../foo references.
                dir.create(vd2 <- "vign_test")
                if (!dir.exists(vd2)) {
                    errorLog(Log, "unable to create 'vign_test'")
                    summaryLog(Log)
                    do_exit(1L)
                }
                file.copy(pkgdir, vd2, recursive = TRUE)

                ## since so many people use 'R CMD' in Makefiles,
                oPATH <- Sys.getenv("PATH")
                Sys.setenv(PATH = paste(R.home("bin"), oPATH,
                                        sep = .Platform$path.sep))
                on.exit(Sys.setenv(PATH = oPATH))
                ## And too many 'vignettes/Makefile's are not safe for
                ## parallel makes
                Sys.setenv(MAKEFLAGS="")
                ## we could use clean = FALSE, but that would not be
                ## testing what R CMD build uses.
                Rcmd <-
                    if (!config_val_to_logical(Sys.getenv("_R_CHECK_BUILD_VIGNETTES_SEPARATELY_", "TRUE")))
                        sprintf("%s\ntools::buildVignettes(dir = '%s')",
                                opWarn_string,
                                file.path(pkgoutdir, "vign_test", pkgname0))
                    else {
                        ## serialize elibs to avoid quotation hell
                        tf <- gsub("\\", "/", tempfile(fileext = ".rds"),
                                   fixed=TRUE)
                        saveRDS(c(jitstr, elibs), tf)
                        sprintf("%s\ntools:::buildVignettes(dir = '%s', ser_elibs = '%s')",
                                opWarn_string,
                                file.path(pkgoutdir, "vign_test", pkgname0),
                                tf)
                    }
                tlim <- get_timeout(Sys.getenv("_R_CHECK_BUILD_VIGNETTES_ELAPSED_TIMEOUT_",
                                    Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
                t1 <- proc.time()
                outfile <- file.path(pkgoutdir, "build_vignettes.log")
                status <- R_runR0(Rcmd, R_opts2, c(jitstr, elibs),
                                  stdout = outfile, stderr = outfile,
                                  timeout = tlim)
                t2 <- proc.time()
                print_time(t1, t2, Log)
                out <- readLines(outfile, warn = FALSE)
                if(R_check_suppress_RandR_message)
                    out <- filtergrep('^Xlib: *extension "RANDR" missing on display',
                                      out, useBytes = TRUE)
                warns <- grep("^Warning: file .* is not portable",
                              out, value = TRUE, useBytes = TRUE)
                if (status) {
                    keep <- as.numeric(Sys.getenv("_R_CHECK_VIGNETTES_NLINES_",
                                                  "25"))
                    if(skip_run_maybe || !ran) warningLog(Log) else noteLog(Log)
                    if(keep > 0  && length(out) < keep) {
                        out <- utils::tail(out, keep)
                        printLog0(Log,
                                  paste(c("Error(s) in re-building vignettes:",
                                          "  ...", out, "", ""), collapse = "\n"))
                    } else
                        printLog0(Log,
                                  paste(c("Error(s) in re-building vignettes:",
                                          out, "", ""), collapse = "\n"))
                } else if(nw <- length(warns)) {
                    if(skip_run_maybe || !ran) warningLog(Log) else noteLog(Log)
                    msg <- ngettext(nw,
                                    "Warning in re-building vignettes:\n",
                                    "Warnings in re-building vignettes:\n",
                                    domain = NA)
                    wrapLog(msg)
                    printLog0(Log, .format_lines_with_indent(warns), "\n")
                } else {
                    ## clean up
                    if (config_val_to_logical(Sys.getenv("_R_CHECK_CLEAN_VIGN_TEST_", "true")))
                        unlink(vd2, recursive = TRUE)
                    if (!config_val_to_logical(Sys.getenv("_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_", "false")))
                            unlink(outfile)
                    resultLog(Log, "OK")
                }
                if(!WINDOWS && !is.na(theta)) {
                    td <- t2 - t1
                    cpu <- sum(td[-3L])
                    if(cpu >= pmax(theta * td[3L], 1)) {
                        ratio <- round(cpu/td[3L], 1L)
                        printLog(Log,
                                 sprintf("Re-building vignettes had CPU time %g times elapsed time\n",
                                        ratio))
                    }
                }
            } else {
                checkingLog(Log, "re-building of vignette outputs")
                resultLog(Log, "SKIPPED")
            }
        } else {
            checkingLog(Log, "running R code from vignettes")
            resultLog(Log, "SKIPPED")
            checkingLog(Log, "re-building of vignette outputs")
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
            tlim <- get_timeout(Sys.getenv("_R_CHECK_PKGMAN_ELAPSED_TIMEOUT_",
                                Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
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
            res <- run_Rcmd(args,  "Rdlatex.log", timeout = tlim)
            latex_log <- file.path(build_dir, "Rd2.log")
            if (file.exists(latex_log))
                file.copy(latex_log, paste0(pkgname, "-manual.log"))
            if (res == 11) { ## return code from Rd2pdf
                errorLog(Log, "Rd conversion errors:")
                lines <- readLines("Rdlatex.log", warn = FALSE)
                lines <- filtergrep("^(Hmm|Execution)", lines)
                printLog0(Log, paste(c(lines, ""), collapse = "\n"))
                unlink(build_dir, recursive = TRUE)
                maybe_exit(1L)
            } else if (res > 0) {
                latex_file <- file.path(build_dir, "Rd2.tex")
                if (file.exists(latex_file))
                    file.copy(latex_file, paste0(pkgname, "-manual.tex"))
                warningLog(Log)
                printLog0(Log,
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
                if (run_Rcmd(args, "Rdlatex.log", timeout = tlim)) {
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
                        run_Rcmd(args, timeout = tlim)
                    }
                    unlink(build_dir, recursive = TRUE)
                    maybe_exit(1L)
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
        ## Most systems are now on >= 5.03, but macOS 10.5 had 4.17
        ## version 4.21 writes to stdout,
        ## 4.23 to stderr and sets an error status code
        FILE <- "file"
        lines <- suppressWarnings(tryCatch(system2(FILE, "--version", TRUE, TRUE), error = function(e) "error"))
        ## a reasonable check -- it does not identify itself well
        have_free_file <- any(grepl("^(file-[45]|magic file from)", lines))
        if (!have_free_file) {
            ## OpenCSW calls this 'gfile'
            FILE <- "gfile"
            lines <- suppressWarnings(tryCatch(system2(FILE, "--version", TRUE, TRUE), error = function(e) "error"))
            have_free_file <- any(grepl("magic file from", lines))
        }
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
                lines <- suppressWarnings(system2(FILE, shQuote(these), TRUE, TRUE))
                ## avoid match to is_executable.Rd
                ex <- grepl(" executable", lines, useBytes=TRUE)
                ex2 <- grepl("script", lines, useBytes=TRUE) &
                    grepl("text", lines, useBytes=TRUE)
                execs <- c(execs, lines[ex & !ex2])
            }
            if(length(execs)) {
                execs <- sub(":[[:space:]].*$", "", execs, useBytes = TRUE)
                known <- rep.int(FALSE, length(execs))
                pexecs <- file.path(pkgname, execs)
                ## known false positives
                for(fp in  c("foreign/tests/datefactor.dta",
                             "msProcess/inst/data[12]/.*.txt",
                             "WMBrukerParser/inst/Examples/C3ValidationExtractSmall/RobotRun1/2-100kDa/0_B1/1/1SLin/fid",
                             "bayesLife/inst/ex-data/bayesLife.output/predictions/traj_country104.rda", # file 5.16
                             "alm/inst/vign/cache/signposts1_c96f55a749822dd089b636087766def2.rdb" # Sparc Solaris, file 5.16
                             ) )
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
            execs <- execs %w/o% excludes
        }
        if (nb <- length(execs)) {
            msg <- ngettext(nb,
                            "Found the following executable file:",
                            "Found the following executable files:",
                            domain = NA)
            warningLog(Log, msg)
            printLog0(Log, .format_lines_with_indent(execs), "\n")
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
            c(".Rbuildignore", ".Rinstignore", "vignettes/.install_extras")
        dots <- dots %w/o% allowed
        alldirs <- list.dirs(".", full.names = TRUE, recursive = TRUE)
        alldirs <- sub("^./","", alldirs)
        alldirs <- alldirs[alldirs != "."]
        bases <- basename(alldirs)
        dots <- c(dots, setdiff(alldirs[startsWith(bases, ".")], ".aspell"))
        if (length(dots)) {
            noteLog(Log, "Found the following hidden files and directories:")
            printLog0(Log, .format_lines_with_indent(dots), "\n")
            wrapLog("These were most likely included in error.",
                    "See section 'Package structure'",
                    "in the 'Writing R Extensions' manual.\n")
            if(cran) {
                known <- basename(dots) %in% .hidden_file_exclusions
                known <- known | grepl("^.Rbuildindex[.]", dots) |
                    ## or?      startsWith(dots,".Rbuildindex.") |
                    endsWith(dots, "inst/doc/.Rinstignore") |
                    endsWith(dots, "inst/doc/.build.timestamp") |
                    endsWith(dots, "vignettes/.Rinstignore") |
                    grepl("^src.*/[.]deps$", dots)
                if (all(known))
                    printLog(Log, "\nCRAN-pack knows about all of these\n")
                else if (any(!known)) {
                    printLog(Log, "\nCRAN-pack does not know about\n")
                    printLog0(Log, .format_lines_with_indent(dots[!known]), "\n")
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
            tlim <- get_timeout(Sys.getenv("_R_CHECK_INSTALL_ELAPSED_TIMEOUT_",
                                Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
            use_install_log <-
                (startsWith(install, "check") || R_check_use_install_log
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
                      shQuote(if (WINDOWS) utils::shortPathName(pkgdir) else pkgdir))
            if (!use_install_log) {
                ## Case A: No redirection of stdout/stderr from installation.
                ## This is very rare: needs _R_CHECK_USE_INSTALL_LOG_ set
                ## to false.
                message("")
                ## Rare use of R CMD INSTALL
                if (run_Rcmd(args, timeout = tlim)) {
                    errorLog(Log, "Installation failed.")
                    summaryLog(Log)
                    do_exit(1L)
                }
                message("")
            } else {
                ## Case B. All output from installation redirected,
                ## or already available in the log file.
                checkingLog(Log,
                            "whether package ", sQuote(desc["Package"]),
                            " can be installed")
                outfile <- file.path(pkgoutdir, "00install.out")
                if (startsWith(install, "check")) {
                    if (!nzchar(arg_libdir))
                        printLog(Log, "\nWarning: --install=check... specified without --library\n")
                    thislog <- install_log_path
                    if(!nzchar(thislog)) {
                        errorLog(Log,
                                 sprintf("install log %s does not exist", sQuote(thislog)))
                        summaryLog(Log)
                        do_exit(2L)
                    }
                    file.copy(thislog, outfile)
                    install <- "check"
                    lines <- readLines(outfile, warn = FALSE)
                    ## <NOTE>
                    ## We used to have
                    ## $install_error = ($lines[$#lines] !~ /^\* DONE/);
                    ## but what if there is output from do_cleanup
                    ## in (Unix) R CMD INSTALL?
                    ## </NOTE>
                    install_error <- !any(startsWith(lines, "* DONE"))
                } else {
                    ## record in the log what options were used
                    cat("* install options ", sQuote(INSTALL_opts),
                        "\n\n", sep = "", file = outfile)
##                    env <- ""
                    ## Normal use of R CMD INSTALL
                    t1 <- proc.time()
                    install_error <-
                        run_Rcmd(args, outfile, timeout = tlim)
                    t2 <- proc.time()
                    print_time(t1, t2, Log)
                    lines <- readLines(outfile, warn = FALSE)
                }
                if (install_error) {
                    errorLog(Log, "Installation failed.")
                    printLog0(Log, "See ", sQuote(outfile),
                             " for details.\n")
                    summaryLog(Log)
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
                             ## This fires on ODS 12.5 warnings like
                             ##   Warning: original hides icu_55::PtnSkeleton::original.
                             ## so filter out later.
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
                             ": warning: .* makes pointer from integer", # gcc
                             ": warning: .* pointer.* conversion", # clang
                             ": warning: improper pointer", # Solaris
                             ": warning: unknown escape sequence", # gcc
                             ": warning: use of non-standard escape character", # clang
                             ## clang warning about invalid returns.
                             "warning: void function",
                             "warning: control reaches end of non-void function",
                             "warning: no return statement in function returning non-void",
                             ## gcc-only form
                             ## ": #warning",
                             ## gcc indents these, igraph has space after #
                             "^ *# *warning",
                             ## Solaris cc has
                             "Warning: # *warning",
                             # these are from era of static HTML
                             "missing links?:",
                             ## From the byte compiler's 'warn' methods
                             "^Note: possible error in",
                             "^Note: (break|next) used in wrong context: no loop is visible",
                             ## Warnings about S4 classes
                             "^  The prototype for class.*undefined slot"
                             )
                ## Warnings spotted by gcc with
                ##   '-Wimplicit-function-declaration'
                ## which is implied by '-Wall'.
                ## Currently only accessible via an internal environment
                ## variable.
                check_src_flag <-
                    Sys.getenv("_R_CHECK_SRC_MINUS_W_IMPLICIT_", "FALSE")
                ## (Not quite perfect, as the name should really
                ## include 'IMPLICIT_FUNCTION_DECLARATION'.)
                if (config_val_to_logical(check_src_flag)) {
                    warn_re <- c(warn_re,
                                 ": warning: implicit declaration of function",
                                 ": warning: incompatible implicit declaration of built-in function")
                }

                ## Warnings spotted by clang with
                ## '-Wreturn-type-c-linkage':
                warn_re <- c(warn_re,
                             ": warning: .* \\[-Wreturn-type-c-linkage\\]")

                ## gcc and clang warnings about sequencing

                ## gcc warnings
                warn_re <- c(warn_re,
                             ": warning: pointer of type .* used in arithmetic",
                             ": warning: .* \\[-Wformat-contains-nul\\]",
                             ": warning: .* \\[-Wformat-zero-length\\]",
                             ": warning: .* \\[-Wpointer-to-int-cast\\]",
                             ": warning: .* \\[-Wsequence-point\\]",
                             ": warning: .* \\[-Wformat-overflow=\\]",
                             ": warning: .* \\[-Wformat-truncation=\\]",
                             ": warning: .* \\[-Wnonull",
                             ": warning: .* \\[-Walloc-size-larger-than=\\]",
                             ": warning: .* \\[-Wterminate\\]",
                             ## Solaris warns on this next one. Also clang
                             ": warning: .* \\[-Wint-conversion\\]",
                             ## clang calls these 'a GNU extension'
                             ": warning: .* \\[-Wconversion-null\\]",
                             ": warning: .* GCC extension",
                             ": warning: .* \\[-Wsizeof-pointer-memaccess\\]",
                             ## usually | for ||, = for == (etc)
                             ": warning: suggest parentheses around (comparison|assignment)",
                             ": warning: .* \\[-Wstringop", # mainly gcc8
                             ": warning: .* \\[-Wclass-memaccess\\]", # gcc8
                             ## used for things deprecated in C++11, for example
                             ": warning: .* \\[-Wdeprecated\\]",
                             ": warning: .* \\[-Waligned-new",
                             ## new in gcc 8
                             ": warning: .* \\[-Wcatch-value=\\]",
                             # warns on code deprecated in C++11
                            ## Fatal, not warning, for clang and Solaris ODS
                             ": warning: .* with a value, in function returning void"
                            )

                ## clang warnings
                warn_re <- c(warn_re,
                             ": warning: .* GNU extension",
                             ": warning: .* \\[-Wdeprecated-register\\]",
                             ": warning: .* \\[-Wformat-extra-args\\]", # also gcc
                             ": warning: .* \\[-Wformat-security\\]",
                             ": warning: .* \\[-Wheader-guard\\]",
                             ": warning: .* \\[-Wpointer-arith\\]",
                             ": warning: .* \\[-Wunsequenced\\]",
                             ": warning: .* \\[-Wvla-extension\\]",
                             ": warning: .* \\[-Wmismatched-new-delete\\]",
                             ": warning: .* \\[-Wabsolute-value\\]",
                             ": warning: .* \\[-Wreorder\\]", # also gcc
                             ": warning: .* \\[-Wself-assign",
                             ": warning: .* \\[-Wtautological",  # also gcc
                             ": warning: .* \\[-Wincompatible-pointer-types\\]",
                             ": warning: format string contains '[\\]0'",
                             ": warning: .* \\[-Wc[+][+]11-long-long\\]",
                             ": warning: empty macro arguments are a C99 feature",
                             ": warning: .* \\[-Wunused-result\\]",  # also gcc
                             ## for non-portable flags (seen in sub-Makefiles)
                             "warning: .* \\[-Wunknown-warning-option\\]"
                             )

                warn_re <- paste0("(", paste(warn_re, collapse = "|"), ")")

                lines <- grep(warn_re, lines, value = TRUE, useBytes = TRUE)

                ## gcc (even 9) seems not to know the size of pointers, so skip
                ## some from -Walloc-size-larger-than= and -Wstringop-overflow=
                lines <- grep("exceeds maximum object size.*-W(alloc-size-larger-than|stringop-overflow)", lines,
                              value = TRUE, useBytes = TRUE, invert = TRUE)
                ## skip for now some c++11-long-long warnings.
                ex_re <- "(/BH/include/boost/|/RcppParallel/include/|/usr/include/|/usr/local/include/|/opt/X11/include/|/usr/X11/include/).*\\[-Wc[+][+]11-long-long\\]"
                lines <- filtergrep(ex_re, lines, useBytes = TRUE)

                ## and GNU extensions in system headers
                ex_re <- "^ *(/usr/|/opt/).*GNU extension"
                lines <- filtergrep(ex_re, lines, useBytes = TRUE)

                ## and ODS 12.5 warnings
                ex_re <- "^Warning: [[:alnum:]]+ hides"
                lines <- filtergrep(ex_re, lines, useBytes = TRUE)

                ## and gfortran 9 warnings about F2018
                ex_re <- "^Warning: Fortran 2018 deleted feature:"
                lines <- filtergrep(ex_re, lines, useBytes = TRUE)

                ## Ignore install-time readLines() warnings about
                ## files with incomplete final lines.  Most of these
                ## come from .install_package_indices(), and should be
                ## safe to ignore ...
                lines <- filtergrep("Warning: incomplete final line found by readLines",
                                    lines, useBytes = TRUE)

                check_Stangle <- Sys.getenv("_R_CHECK_STANGLE_WARNINGS_", "TRUE")
                if (!config_val_to_logical(check_Stangle))
                lines <- filtergrep("Warning: value of .* option should be lowercase",
                                    lines, useBytes = TRUE)

                ## Package writers cannot really do anything about
                ## non ISO C code in *system* headers.  Also,
                ## GCC >= 3.4 warns about function pointers
                ## casts which are "needed" for dlsym(), but it
                ## seems that all systems which have dlsym() also
                ## support the cast.  Hence, try to ignore these by
                ## default, but make it possible to get all ISO C
                ## warnings via an environment variable.
                if (!R_check_all_non_ISO_C) {
                    lines <- filtergrep("^ */.*: warning: .*ISO C",
                                        lines, useBytes = TRUE)
                    lines <- filtergrep("warning: *ISO C forbids.*function pointer",
                                        lines, useBytes = TRUE)
                    if(WINDOWS) lines <- filtergrep(
                                    "warning: *ISO C does not support.*ms_printf length modifier",
                                    lines, useBytes = TRUE)
                }

                ## Warnings spotted by gcc with
                ##   '-Wunused'
                ## which is implied by '-Wall'.
                ## Currently only accessible via an internal environment
                ## variable.
                check_src_flag <-
                    Sys.getenv("_R_CHECK_SRC_MINUS_W_UNUSED_", "FALSE")
                if (!config_val_to_logical(check_src_flag)) {
                    lines <- filtergrep("warning: unused", lines,
                                        ignore.case = TRUE, useBytes = TRUE)
                    lines <- filtergrep("warning: .* set but not used", lines,
                                        ignore.case = TRUE, useBytes = TRUE)
                }
                ## (gfortran seems to use upper case.)

                ## Warnings spotted by clang with
                ##   '-Wsometimes-uninitialized'
                ## which is implied by '-Wall'.
                ## Currently only accessible via an internal environment
                ## variable.
                check_src_flag <-
                    Sys.getenv("_R_CHECK_SRC_MINUS_W_SOMETIMES_UNINITIALIZED_",
                               "FALSE")
                if (!config_val_to_logical(check_src_flag)) {
                    lines <- filtergrep("warning: .* is used uninitialized whenever",
                                        lines, useBytes = TRUE)
                }

                ## Warnings spotted by gfortran >= 4.0 with '-Wall'.
                ## Justified in principle, it seems.
                ## Let's filter them for the time being, and maybe
                ## revert this later on ... but make it possible to
                ## suppress filtering out by setting the internal
                ## environment variable _R_CHECK_WALL_FORTRAN_ to
                ## something "true".
                ## All gfortran -Wall warnings start Warning: so have been
                ## included.  We exclude some now.
                check_src_flag <- Sys.getenv("_R_CHECK_WALL_FORTRAN_", "FALSE")
                if (!config_val_to_logical(check_src_flag)) {
                    warn_re <-
                        c("Label .* at \\(1\\) defined but not used",
                          "Line truncated at \\(1\\)",
                          "ASSIGN statement at \\(1\\)",
                          "Assigned GOTO statement at \\(1\\)",
                          "arithmetic IF statement at \\(1\\)",
                          "Nonconforming tab character (in|at)",
                          "Obsolescent feature:")
                    warn_re <- c(warn_re,
                                 "Warning: .*\\[-Wconversion]",
                                 ## We retain [-Wuninitialized]
                                 "Warning: .*\\[-Wmaybe-uninitialized]",
                                 "Warning: .*\\[-Wintrinsic-shadow]",
                                 ## R itself uses these, the latter in LAPACK
                                 "Warning: GNU Extension: DOUBLE COMPLEX",
                                 "Warning: GNU Extension: .*COMPLEX[*]16"
                                )
                    check_src_flag <-
                        Sys.getenv("_R_CHECK_SRC_MINUS_W_UNUSED_", "FALSE")
                    if (!config_val_to_logical(check_src_flag))
                        warn_re <- c(warn_re,
                                     "Warning: .*\\[-Wunused-function]",
                                     "Warning: .*\\[-Wunused-dummy-argument]")
                    warn_re <- paste0("(", paste(warn_re, collapse = "|"), ")")
                    lines <- filtergrep(warn_re, lines)
                }

                if (WINDOWS) {
                    ## Warning on Windows with some packages that
                    ## cannot transparently be installed bi-arch.
                    lines <- filtergrep("Warning: this package has a non-empty 'configure.win' file",
                                        lines)
                    ## Warning on x64 Windows gcc 4.5.1 that
                    ## seems to be spurious
                    lines <- filtergrep("Warning: .drectve .* unrecognized", lines)
                }

                check_imports_flag <-
                    Sys.getenv("_R_CHECK_REPLACING_IMPORTS_", "TRUE")
                if (!config_val_to_logical(check_imports_flag))
                    lines <- filtergrep("Warning: replacing previous import", lines,
                                        fixed = TRUE)
                else {
                    this <- unique(grep("Warning: replacing previous import",
                                        lines, fixed = TRUE, value = TRUE))
                    this <- grep(paste0(sQuote(pkgname), "$"), this,
                                 value = TRUE)
                    lines <- filtergrep("Warning: replacing previous import", lines,
                                        fixed = TRUE)
                    lines <- c(lines, this)
                }
                check_FirstLib_flag <-
                    Sys.getenv("_R_CHECK_DOT_FIRSTLIB_", "FALSE")
                if (!config_val_to_logical(check_FirstLib_flag))
                    lines <- filtergrep("Warning: ignoring .First.lib()", lines,
                                        fixed = TRUE)

                lines <- unique(lines)

                ## Can get reports like
                ## Warning: No generic function `as.vector' found corresponding to requested imported methods from package `Matrix' when loading `MatrixModels' (malformed exports?)
                ## Exclude these unless they are about the current package.
                load_re <- "Warning: No generic function.*corresponding to requested imported methods"
                ex <- grepl(load_re, lines, useBytes = TRUE) &
                    !grepl(pkgname, lines, fixed = TRUE, useBytes = TRUE)
                lines <- lines[!ex]

                note_re <-
                    "warning: control may reach end of non-void function"

                notes <- grep(note_re, lines0, value = TRUE, useBytes = TRUE)
                notes <- unique(notes)
                if (any(grepl("using non-staged installation", lines0,
                               useBytes = TRUE)))
                    notes <- c("Non-staged installation was used", notes)
                if (length(lines)) {
                    warningLog(Log, "Found the following significant warnings:")
                    printLog0(Log, .format_lines_with_indent(lines), "\n")
                    if(length(notes)) {
                        printLog(Log,
                                 "Found the following additional notes/warnings:\n")
                        printLog0(Log, .format_lines_with_indent(notes),
                                  "\n")
                    }
                    printLog0(Log, sprintf("See %s for details.\n",
                                           sQuote(outfile)))
                    if(any(grepl("^Note:", lines, useBytes = TRUE)))
                        wrapLog("Information on the location(s)",
                                "of code generating the",
                                paste0(sQuote("Note"), "s"),
                                "can be obtained by re-running with",
                                "environment variable R_KEEP_PKG_SOURCE",
                                "set to 'yes'.\n")

                } else if(length(notes)) {
                    noteLog(Log, "Found the following notes/warnings:")
                    printLog0(Log, .format_lines_with_indent(notes), "\n")
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
        pd <- file.path(libdir, pkgname)
        ## if we used a log, the installation would not need to remain.
        if (!dir.exists(pd)) return()
        checkingLog(Log, "installed package size")
        owd <- setwd(pd)
        res <- system2("du", "-k", TRUE, TRUE)
        sizes <- as.integer(sub("\\D.*", "", res))
        dirs <- sub("^\\d*\\s*", "", res)
        res2 <- data.frame(size = sizes, dir = I(dirs))
        total <- res2[nrow(res2), 1L]
        if(!is.na(total) &&
           total > 1024 * as.numeric(Sys.getenv("_R_CHECK_PKG_SIZES_THRESHOLD_", unset = 5)) && # report at 5Mb
           pkgname != "Matrix") { # <- large recommended package
            noteLog(Log)
            printLog(Log, sprintf("  installed size is %4.1fMb\n", total/1024))
            rest <- res2[-nrow(res2), ]
            rest[, 2L] <- sub("./", "", rest[, 2L])
            ## keep only top-level directories
            rest <- rest[!grepl("/", rest[, 2L]), ]
            rest <- rest[rest[, 1L] > 1024, ] # > 1Mb
            if(nrow(rest)) {
                o <- sort.list(rest[, 2L])
                printLog(Log, "  sub-directories of 1Mb or more:\n")
                size <- sprintf('%4.1fMb', rest[, 1L]/1024)
                printLog0(Log,
                          paste0("    ", format(rest[o, 2L], justify = "left"),
                                 "  ", format(size[o], justify = "right"), "\n"))
            }
        } else resultLog(Log, "OK")
        setwd(owd)
    }

    check_description <- function()
    {
        checkingLog(Log, "for file ",
                    sQuote(file.path(pkgname0, "DESCRIPTION")))
        if ("DESCRIPTION" %in% dir(pkgdir)) {
            f <- file.path(pkgdir, "DESCRIPTION")
            desc <- tryCatch(.read_description(f), error = identity)
            if(inherits(desc, "error")) {
                errorLog(Log, conditionMessage(desc))
                summaryLog(Log)
                do_exit(1L)
            } else if(!length(desc)) {
                errorLog(Log, "File DESCRIPTION exists but is not in correct format")
                summaryLog(Log)
                do_exit(1L)
            }
            mandatory <- c("Package", "Version", "License", "Description",
                           "Title", "Author", "Maintainer")
            OK <- vapply(desc[mandatory],
                         function(x) !is.na(x) && nzchar(x),
                         NA)
            if(!all(OK)) {
                fail <- mandatory[!OK]
                msg <- ngettext(length(fail),
                                "Required field missing or empty:",
                                "Required fields missing or empty:")
                msg <- paste0(msg, "\n", .pretty_format(fail))
                errorLog(Log, msg)
                summaryLog(Log)
                do_exit(1L)
            }
            if(!grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", desc["Package"])
               || endsWith(desc["Package"], ".")) {
                warningLog(Log)
                printLog(Log,"  Package name is not portable:\n",
                         "  It must start with a letter, contain letters, digits or dot\n",
                         "  have at least 2 characters and not end with a dot.\n")
            } else resultLog(Log, "OK")
            encoding <- desc["Encoding"]
        } else if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
            errorLog(Log,
                     "File DESCRIPTION does not exist but there is a case-insensitive match.")
            summaryLog(Log)
            do_exit(1L)
        } else {
            errorLog(Log,
                     "File DESCRIPTION does not exist")
            summaryLog(Log)
            do_exit(1L)
        }
        if(!is.na(desc["Type"])) { # standard packages do not have this
            checkingLog(Log, "extension type")
            if(desc["Type"] != "Package") {
                errorLog(Log,
                         sprintf("Extensions with Type %s cannot be checked.",
                                 sQuote(desc["Type"])))
                summaryLog(Log)
                do_exit(0L)
            } else resultLog(Log, desc["Type"])
        }
        if(!is.na(desc["Bundle"])) {
            checkingLog(Log, "package bundle")
            errorLog(Log,
                     sprintf("Looks like %s is a package bundle -- they are defunct",
                             sQuote(pkgname0)))
            summaryLog(Log)
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

    check_CRAN_incoming <- function(localOnly)
    {
        checkingLog(Log, "CRAN incoming feasibility")
        res <- .check_package_CRAN_incoming(pkgdir, localOnly)
        bad <- FALSE
        if(length(res)) {
            out <- format(res)
            if(length(out) == 1L && startsWith(out, "Maintainer: ")) {
                ## Special-case when there is only the maintainer
                ## address to note (if at all).
                maintainer <- res$Maintainer
                if(nzchar(maintainer) &&
                   identical(maintainer,
                             Sys.getenv("_R_CHECK_MAINTAINER_ADDRESS_"))) {
                    resultLog(Log, "OK")
                    out <- character()
                }
                else resultLog(Log, "Note_to_CRAN_maintainers")
            } else if(length(res$bad_package)) {
                errorLog(Log)
                bad <- TRUE
            } else if(length(res$bad_version) ||
                      length(res$strong_dependencies_not_in_mainstream_repositories) ||
                      isTRUE(res$foss_with_BuildVignettes) ||
                      res$Maintainer_invalid_or_multi_person ||
                      res$empty_Maintainer_name ||
                      res$Maintainer_needs_quotes)
                warningLog(Log)
            else if(length(res) > 1L) noteLog(Log)
            else resultLog(Log, "OK")
            printLog0(Log, c(paste(out, collapse = "\n\n"), "\n"))
            if(bad) maybe_exit(1L)
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
            ns <- tryCatch(parseNamespaceFile(basename(pkgdir),
                                              dirname(pkgdir)),
                     error = function(e) {
                         errorLog(Log)
                         printLog0(Log,
                                   "Invalid NAMESPACE file, parsing gives:",
                                   "\n", as.character(e), "\n")
                         msg_NAMESPACE <-
                             c("See section 'Package namespaces'",
                               " in the 'Writing R Extensions' manual.\n")
                         wrapLog(msg_NAMESPACE)
                         summaryLog(Log)
                         do_exit(1L)
                     })
            OK <- TRUE
            ## Look for empty importFrom
            imp <- ns$imports
            lens <- lengths(imp)
            imp <- imp[lens == 2L]
            nm <- sapply(imp, "[[", 1)
            lens <- vapply(imp, function(x) length(x[[2L]]), 0L)
            bad <- nm[lens == 0L]
            if(length(bad)) {
                OK <- FALSE
                msg <- if(length(bad) == 1L)
                    sprintf("  Namespace with empty importFrom: %s", sQuote(bad))
                else
                    paste0("  Namespaces with empty importFrom:\n",
                           .pretty_format(sort(bad)))
                noteLog(Log, msg)
            }
            nS3methods <- nrow(ns$S3methods)
            if (nS3methods > 500L) {
                ## check that this is installable in R 3.0.1
                meta <- .read_description(file.path(pkgdir, "DESCRIPTION"))
                deps <- .split_description(meta, verbose = TRUE)$Rdepends2
                status <- 0L
                current <- as.numeric_version("3.0.1")
                for(depends in deps) {
                    ## .check_package_description will insist on these operators
                    if(depends$op %notin% c("<=", ">=", "<", ">", "==", "!="))
                        next
                    status <- if(inherits(depends$version, "numeric_version"))
                        !do.call(depends$op, list(current, depends$version))
                    else {
                        ver <- R.version
                        if (ver$status %in% c("", "Patched")) FALSE
                        else !do.call(depends$op,
                                      list(ver[["svn rev"]],
                                           as.numeric(sub("^r", "", depends$version))))
                    }
                    if(status != 0L)  break
                }
                if (status == 0L) {
                    OK <- FALSE
                    msg <- sprintf("R < 3.0.2 had a limit of 500 registered S3 methods: found %d",
                                   nS3methods)
                    noteLog(Log, msg)
                }
            }
            if(OK) resultLog(Log, "OK")
        }

        checkingLog(Log, "package dependencies")
        ## Everything listed in Depends or Suggests or Imports
        ## should be available for successfully running R CMD check.
        ## \VignetteDepends{} entries not "required" by the package code
        ## must be in Suggests.  Note also that some of us think that a
        ## package vignette must require its own package, which OTOH is
        ## not required in the package DESCRIPTION file.
        ## Namespace imports must really be in Depends.
        res <- .check_package_depends(pkgdir, R_check_force_suggests,
                                      check_incoming, ignore_vignettes)
        if(any(lengths(res) > 0L)) {
            out <- format(res)
            allowed <- c("suggests_but_not_installed",
                         "enhances_but_not_installed",
                         "many_depends",
                         "skipped",
                         "hdOnly",
                         if(!check_incoming) "bad_engine")
            if(!all(names(res) %in% allowed)) {
                errorLog(Log)
                printLog0(Log, paste(out, collapse = "\n"), "\n")
                if(length(res$suggested_but_not_installed))
                   wrapLog("The suggested packages are required for",
                           "a complete check.\n",
                           "Checking can be attempted without them",
                           "by setting the environment variable",
                           "_R_CHECK_FORCE_SUGGESTS_",
                           "to a false value.\n\n")
                wrapLog(msg_DESCRIPTION)
                summaryLog(Log)
                do_exit(1L)
            } else {
                noteLog(Log)
                printLog0(Log, paste(out, collapse = "\n"))
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
            summaryLog(Log)
            do_exit(1L)
        } else if (!startsWith(install, "check")) {
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
                printLog0(Log,
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
                printLog0(Log,
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
                    srcfiles <- srcfiles[!dir.exists(srcfiles)]
                    srcfiles <- filtergrep(
                        "(\\.([cfmCM]|cc|cpp|f90|f95|mm|h|o|so)$|^Makevars|-win\\.def|^install\\.libs\\.R$)",
                        srcfiles)
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
                        printLog0(Log, paste(msg, collapse = "\n"))
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
                printLog0(Log, paste(msg, collapse = "\n"))
                ini <- ""
            }
            ## Check for installed copies of the package in some subdir.
            files <- files[basename(dirname(files)) == "Meta"]
            if(length(files) &&
               all(!is.na(match(c("package.rds", "hsearch.rds"),
                                basename(files))))) {
                if(!any) noteLog(Log)
                any <- TRUE
                msg <- c(ini,
                         sprintf("Subdirectory %s seems to contain an installed version of the package.\n",
                                 sQuote(dirname(dirname(files[1L])))))
                printLog0(Log, paste(msg, collapse = "\n"))
            }
            if (!any) resultLog(Log, "OK")
        } else resultLog(Log, "OK")
    }

    do_exit <-
        if(no.q)
            function(status) (if(status) stop else message)(
                ".check_packages() exit status ", status)
        else
            function(status) q("no", status = status, runLast = FALSE)

    maybe_exit <- function(status = 1L) {
        if (R_check_exit_on_first_error) {
            printLog(Log, "NOTE:  Quitting check on first error.\n")
            summaryLog(Log)
            do_exit(status)
        }
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
            "they run successfully.  By default code in the vignettes is tested,",
            "as is re-building the vignette PDFs.",
            "",
            "Options:",
            "  -h, --help       print short help message and exit",
            "  -v, --version        print version info and exit",
            "  -l, --library=LIB     library directory used for test installation",
            "           of packages (default is outdir)",
            "  -o, --output=DIR      directory for output, default is current directory.",
            "           Logfiles, R output, etc. will be placed in 'pkg.Rcheck'",
            "           in this directory, where 'pkg' is the name of the",
            "           checked package",
            "      --no-clean        do not clean 'outdir' before using it",
            "      --no-codoc        do not check for code/documentation mismatches",
            "      --no-examples     do not run the examples in the Rd files",
            "      --no-install      skip installation and associated tests",
            "      --no-tests        do not run code in 'tests' subdirectory",
            "      --no-manual       do not produce the PDF manual",
            "      --no-vignettes    do not run R code in vignettes nor build outputs",
            "      --no-build-vignettes    do not build vignette outputs",
            "      --ignore-vignettes    skip all tests on vignettes",
            "      --run-dontrun     do run \\dontrun sections in the Rd files",
            "      --run-donttest    do run \\donttest sections in the Rd files",
            "      --use-gct         use 'gctorture(TRUE)' when running examples/tests",
            "      --use-valgrind    use 'valgrind' when running examples/tests/vignettes",
            "      --timings         record timings for examples",
            "      --install-args=   command-line args to be passed to INSTALL",
            "      --test-dir=       look in this subdirectory for test scripts (default tests)",
            "      --no-stop-on-test-error   do not stop running tests after first error",
            "      --check-subdirs=default|yes|no",
            "           run checks on the package subdirectories",
            "           (default is yes for a tarball, no otherwise)",
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
            "Report bugs at <https://bugs.R-project.org>.", sep="\n")
    }

###--- begin{.check_packages()} "main" ---

    warnOption <- max(getOption("warn"), warnOption)# notably allow caller to set 2
    op <- options(showErrorCalls=FALSE, warn = warnOption)
    opWarn_string <- sprintf("options(warn = %d)", warnOption)
    opW_shE_F_str <- sprintf("options(warn = %d, showErrorCalls=FALSE)\n", warnOption)
    on.exit(options(op), add=TRUE)

    ## Read in check environment file.
    Renv <- Sys.getenv("R_CHECK_ENVIRON", unset = NA_character_)
    if(!is.na(Renv)) {
        ## Do not read any check environment file if R_CHECK_ENVIRON is
        ## set to empty of something non-existent.
        if(nzchar(Renv) && file.exists(Renv)) readRenviron(Renv)
    } else {
        ## Read in ~/.R/check.Renviron[.rarch] (if it exists).
        rarch <- .Platform$r_arch
        if (nzchar(rarch) &&
            file.exists(Renv <- paste0("~/.R/check.Renviron.", rarch)))
            readRenviron(Renv)
        else if (file.exists(Renv <- "~/.R/check.Renviron"))
            readRenviron(Renv)
    }

    td0 <- as.numeric(Sys.getenv("_R_CHECK_TIMINGS_"))
    if (is.na(td0)) td0 <- Inf

    ## A user might have turned on JIT compilation.  That does not
    ## work well, so mostly disable it.
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
    do_install_arg <- TRUE; install <- ""
    do_tests <- TRUE
    do_vignettes <- TRUE
    do_build_vignettes <- TRUE
    ignore_vignettes <- FALSE
    do_manual <- TRUE
    use_gct <- FALSE
    use_valgrind <- FALSE
    do_timings <- FALSE
    install_args <- NULL
    test_dir <- "tests"
    check_subdirs <- ""           # defaults to R_check_subdirs_strict
    extra_arch <- FALSE
    spec_install <- FALSE
    multiarch <- NA
    force_multiarch <- FALSE
    as_cran <- FALSE
    run_dontrun <- FALSE
    run_donttest <- FALSE
    stop_on_test_error <- TRUE

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
                .R_copyright_msg(1997),
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
            do_install_arg  <- FALSE
        } else if (substr(a, 1, 10) == "--install=") {
            install <- substr(a, 11, 1000)
        } else if (a == "--no-tests") {
            do_tests  <- FALSE
        } else if (a == "--no-build-vignettes") {
            do_build_vignettes  <- FALSE
        } else if (a == "--no-rebuild-vignettes") { # pre-3.0.0 version
            stop("'--no-rebuild-vignettes' is defunct: use '--no-build-vignettes' instead",
                 call. = FALSE, domain = NA)
        } else if (a == "--no-vignettes") {
            do_vignettes  <- FALSE
        } else if (a == "--ignore-vignettes") {
            ignore_vignettes  <- TRUE
            do_vignettes  <- FALSE
            do_build_vignettes  <- FALSE
        } else if (a == "--no-manual") {
            do_manual  <- FALSE
        } else if (a == "--no-latex") {
            stop("'--no-latex' is defunct: use '--no-manual' instead",
                 call. = FALSE, domain = NA)
        } else if (a == "--run-dontrun") {
            run_dontrun  <- TRUE
        } else if (a == "--run-donttest") {
            run_donttest  <- TRUE
        } else if (a == "--use-gct") {
            use_gct  <- TRUE
        } else if (a == "--use-valgrind") {
            use_valgrind  <- TRUE
        } else if (a == "--timings") {
            do_timings  <- TRUE
        } else if (substr(a, 1, 15) == "--install-args=") {
            install_args <- substr(a, 16, 1000)
        } else if (substr(a, 1, 11) == "--test-dir=") {
            test_dir <- substr(a, 12, 1000)
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
        } else if (a == "--no-stop-on-test-error") {
            stop_on_test_error <- FALSE
        } else if (substr(a, 1, 9) == "--rcfile=") {
            warning("configuration files are not supported as from R 2.12.0")
        } else if (startsWith(a, "-")) {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }

    ## record some of the options used.
    opts <- character()
    if (install == "fake") opts <- c(opts, "--install=fake")
    if (!do_install_arg) opts <- c(opts, "--no-install")
    if (install == "no") {
        opts <- c(opts, "--install=no")
        do_install_arg <- FALSE
        ## If we do not install, then we cannot *run* any code.
        do_examples <- do_tests <- do_vignettes <- do_build_vignettes <- 0
    }
    if (run_dontrun) opts <- c(opts, "--run-dontrun")
    if (run_donttest) opts <- c(opts, "--run-donttest")
    opts0 <- opts # other options are added later.

    if (install == "fake") {
        ## If we fake installation, then we cannot *run* any code.
        do_examples <- do_tests <- do_vignettes <- do_build_vignettes <- 0
        spec_install <- TRUE
        multiarch <- FALSE
    }

    install_log_path <- ""
    if(startsWith(install, "check")) {
        ## Expand relative to absolute if possible.
        install_log_path <-
            tryCatch(file_path_as_absolute(substr(install, 7L, 1000L)),
                     error = function(e) "")
    }

    if (!isFALSE(multiarch)) {
        ## see if there are multiple installed architectures, and if they work
        if (WINDOWS) {
            ## always has sub-archs as from R 2.12.0.
            ## usually if two are installed, it was done on a 64-bit OS,
            ## but the filesystem might be shared betweeen OSes.
            f <- dir(file.path(R.home(), "bin"))
            archs <- f[f %in% c("i386", "x64")]
            ## if we have x64, can only run it on a 64-bit OS
            if (length(archs) > 1L && !grepl("x64", utils::win.version()))
                archs <- "i386"
        } else {
            wd2 <- setwd(file.path(R.home("bin"), "exec"))
            archs <- Sys.glob("*")
            setwd(wd2)
            if (length(archs) > 1L)
                for (arch in archs) {
                    if (arch == rarch) next
                    cmd <- paste0(shQuote(file.path(R.home(), "bin", "R")),
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
    R_check_Rd_contents <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_CONTENTS_", "TRUE"))
    R_check_Rd_line_widths <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_LINE_WIDTHS_", "FALSE"))
    R_check_Rd_style <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_STYLE_", "TRUE"))
    R_check_Rd_xrefs <-
        config_val_to_logical(Sys.getenv("_R_CHECK_RD_XREFS_", "TRUE"))
    R_check_use_codetools <-
        config_val_to_logical(Sys.getenv("_R_CHECK_USE_CODETOOLS_", "TRUE"))
    ## However, we cannot use this if we did not install the recommended
    ## packages.
    if(R_check_use_codetools) {
        tmp <- tryCatch(find.package('codetools'), error = identity)
        if(inherits(tmp, "error")) R_check_use_codetools <- FALSE
    }
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
        do_install_arg && config_val_to_logical(Sys.getenv("_R_CHECK_SUPPRESS_RANDR_MESSAGE_", "TRUE"))
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
    R_check_FF <- Sys.getenv("_R_CHECK_FF_CALLS_", "true")
    R_check_FF_DUP <-
        config_val_to_logical(Sys.getenv("_R_CHECK_FF_DUP_", "TRUE"))
    R_check_toplevel_files <-
        config_val_to_logical(Sys.getenv("_R_CHECK_TOPLEVEL_FILES_", "FALSE"))
    R_check_exit_on_first_error <-
        config_val_to_logical(Sys.getenv("_R_CHECK_EXIT_ON_FIRST_ERROR_", "FALSE"))
    R_check_vignettes_skip_run_maybe <-
        config_val_to_logical(Sys.getenv("_R_CHECK_VIGNETTES_SKIP_RUN_MAYBE_",
                                         "FALSE"))
    R_check_serialization <-
        config_val_to_logical(Sys.getenv("_R_CHECK_SERIALIZATION_", "FALSE"))

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
        Sys.setenv("_R_CHECK_PACKAGES_USED_CRAN_INCOMING_NOTES_" = "TRUE")
        prev <- Sys.getenv("_R_CHECK_LIMIT_CORES_", NA_character_)
        if(is.na(prev)) Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
        prev <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", NA_character_)
        if(is.na(prev)) Sys.setenv("_R_CHECK_SCREEN_DEVICE_" = "stop")
        Sys.setenv("_R_CHECK_CODE_USAGE_VIA_NAMESPACES_" = "TRUE")
        Sys.setenv("_R_CHECK_CODE_USAGE_WITH_ONLY_BASE_ATTACHED_" = "TRUE")
        Sys.setenv("_R_CHECK_S3_METHODS_NOT_REGISTERED_" = "TRUE")
        Sys.setenv("_R_CHECK_PACKAGE_DATASETS_SUPPRESS_NOTES_" = "TRUE")
        Sys.setenv("_R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_" = "TRUE")
        Sys.setenv("_R_CHECK_NATIVE_ROUTINE_REGISTRATION_" = "TRUE")
        Sys.setenv("_R_CHECK_NO_STOP_ON_TEST_ERROR_" = "TRUE")
        Sys.setenv("_R_CHECK_PRAGMAS_" = "TRUE")
        Sys.setenv("_R_CHECK_COMPILATION_FLAGS_" = "TRUE")
        if(!nzchar(Sys.getenv("_R_CHECK_R_DEPENDS_")))
            Sys.setenv("_R_CHECK_R_DEPENDS_" = "warn")
        ## until this is tested on Windows
        Sys.setenv("_R_CHECK_R_ON_PATH_" = if(WINDOWS) "FALSE" else "TRUE")
        Sys.setenv("_R_CHECK_PACKAGES_USED_IN_TESTS_USE_SUBDIRS_" = "TRUE")
        Sys.setenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_" = "TRUE")
        Sys.setenv("_R_CHECK_SHLIB_OPENMP_FLAGS_" = "TRUE")
        Sys.setenv("_R_CHECK_FUTURE_FILE_TIMESTAMPS_" = "TRUE")
        Sys.setenv("_R_CHECK_RD_CONTENTS_KEYWORDS_" = "TRUE")
        Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" =
                       "package:_R_CHECK_PACKAGE_NAME_,abort,verbose")
        R_check_vc_dirs <- TRUE
        R_check_executables_exclusions <- FALSE
        R_check_doc_sizes2 <- TRUE
        R_check_suggests_only <- TRUE
        R_check_code_assign_to_globalenv <- TRUE
        R_check_code_attach <- TRUE
        R_check_code_data_into_globalenv <- TRUE
        R_check_depr_def <- TRUE
        R_check_Rd_line_widths <- TRUE
        R_check_FF <- "registration"
        do_timings <- TRUE
        R_check_toplevel_files <- TRUE
        R_check_vignettes_skip_run_maybe <- TRUE
        R_check_serialization <- TRUE
    } else {
        ## do it this way so that INSTALL produces symbols.rds
        ## when called from check but not in general.
        if(is.na(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_",
                            NA_character_)))
            Sys.setenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = "TRUE")
    }


    if (extra_arch) {
        R_check_Rd_contents <- R_check_all_non_ISO_C <-
            R_check_Rd_xrefs <- R_check_use_codetools <- R_check_Rd_style <-
                R_check_executables <- R_check_permissions <-
                    R_check_dot_internal <- R_check_ascii_code <-
                        R_check_ascii_data <- R_check_compact_data <-
                            R_check_pkg_sizes <- R_check_doc_sizes <-
                                R_check_doc_sizes2 <-
                                    R_check_unsafe_calls <-
                                        R_check_toplevel_files <- FALSE
        R_check_Rd_line_widths <- FALSE
    }

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
        Sys.setenv(R_LIBS = path_and_libPath(libdir, R_LIBS))
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

    msg_DESCRIPTION <-
        c("See section 'The DESCRIPTION file' in the 'Writing R Extensions' manual.\n")

    if (!length(pkgs)) {
        message("Error: no packages were specified")
        do_exit(1L)
    }

    ## This is the main loop over all packages to be checked.
    for (pkg in pkgs) {
        ## pkg should be the path to the package root source
        ## directory, either absolute or relative to startdir.
        ## As from 2.1.0 it can also be a tarball

        ## The previous package may have set do_install to FALSE
        do_install <- do_install_arg
        no_examples <- FALSE

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
        pkgoutdir <- file.path(outdir, paste0(pkgname0, ".Rcheck"))
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

        messageLog(Log, "using log directory ", sQuote(pkgoutdir))
        messageLog(Log, "using ", R.version.string)
        messageLog(Log, "using platform: ", R.version$platform,
                   " (", 8*.Machine$sizeof.pointer, "-bit)")
        charset <-
            if (l10n_info()[["UTF-8"]]) "UTF-8" else utils::localeToCharset()
        messageLog(Log, "using session charset: ", charset)
        is_ascii <- charset == "ASCII"

        if(config_val_to_logical(Sys.getenv("_R_CHECK_R_ON_PATH_", "FALSE")))
            add_dummies(file_path_as_absolute(pkgoutdir), Log)

        if (istar) {
            dir <- file.path(pkgoutdir, "00_pkg_src")
            dir.create(dir, mode = "0755")
            if (!dir.exists(dir)) {
                checkingLog(Log, "whether tarball can be unpacked")
                errorLog(Log, sprintf("cannot create %s", sQuote(dir)))
                summaryLog(Log)
                do_exit(1L)
            }
            ## force the use of internal untar unless over-ridden
            ## so e.g. .tar.xz works everywhere
            if (utils::untar(pkg, exdir = dir,
                             tar = Sys.getenv("R_INSTALL_TAR", "internal"))) {
                checkingLog(Log, "whether tarball can be unpacked")
                errorLog(Log, sprintf("cannot unpack %s", sQuote(pkg)))
                summaryLog(Log)
                do_exit(1L)
            }
            size <- file.info(pkg)$size
            Sys.setenv("_R_CHECK_SIZE_OF_TARBALL_" = size)
            ## this assumes foo_x.y.tar.gz unpacks to foo, but we are about
            ## to test that.
            pkg <- file.path(dir, pkgname0)
        }
        if (!dir.exists(pkg)) {
            checkingLog(Log, "package directory")
            errorLog(Log,
                     gettextf("package directory %s does not exist",
                              sQuote(pkg)))
            summaryLog(Log)
            do_exit(1L)
        }
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

        .unpack.time <- Sys.time()

        ## report options used
        opts <- opts0
        if (!do_codoc) opts <- c(opts, "--no-codoc")
        if (!do_examples && !spec_install) opts <- c(opts, "--no-examples")
        if (!do_tests && !spec_install) opts <- c(opts, "--no-tests")
        if (!do_manual && !spec_install) opts <- c(opts, "--no-manual")
        if (ignore_vignettes) opts <- c(opts, "--ignore-vignettes")
        else {
            if (!do_vignettes && !spec_install)
                opts <- c(opts, "--no-vignettes")
            if (!do_build_vignettes && !spec_install)
                opts <- c(opts, "--no-build-vignettes")
        }
        if (use_gct) opts <- c(opts, "--use-gct")
        if (use_valgrind) opts <- c(opts, "--use-valgrind")
        if (!stop_on_test_error) opts <- c(opts, "--no-stop-on-test-error")
        if (as_cran) opts <- c(opts, "--as-cran")
        if (length(opts) > 1L)
            messageLog(Log, "using options ", sQuote(paste(opts, collapse=" ")))
        else if (length(opts) == 1L)
            messageLog(Log, "using option ", sQuote(opts))

        if(identical(config_val_to_logical(Sys.getenv("_R_CHECK_NO_STOP_ON_TEST_ERROR_",
                                                      "FALSE")),
                     TRUE)) {
            stop_on_test_error <- FALSE
        }

        if (!nzchar(libdir)) { # otherwise have set R_LIBS above
            libdir <- pkgoutdir
            Sys.setenv(R_LIBS = path_and_libPath(libdir, R_LIBS))
        }
        if (WINDOWS && grepl(" ", libdir)) # need to avoid spaces in libdir
            libdir <- gsub("\\", "/", utils::shortPathName(libdir), fixed = TRUE)

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
                errorLog(Log, "File DESCRIPTION exists but is not in correct format")
                summaryLog(Log)
                do_exit(1L)
            }
            desc <- desc[1L, ]
            if (identical(desc["Priority"], c(Priority = "base"))) {    # Priority might be missing
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
            is_rec_pkg <- identical(desc["Priority"], c(Priority = "recommended"))

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
            } else OS_type <- NA

            check_incoming <- Sys.getenv("_R_CHECK_CRAN_INCOMING_", "NA")
            check_incoming <- if(check_incoming == "NA") as_cran else {
                config_val_to_logical(check_incoming)
            }
            check_incoming_remote <- Sys.getenv("_R_CHECK_CRAN_INCOMING_REMOTE_", "NA")
            check_incoming_remote <- if(check_incoming_remote == "NA") as_cran else {
                config_val_to_logical(check_incoming_remote)
            }
            if (check_incoming) check_CRAN_incoming(!check_incoming_remote)

            ## <NOTE>
            ## We want to check for dependencies early, since missing
            ## dependencies may make installation fail, and in any case we
            ## give up if they are missing.  But we don't check them if
            ## we are not going to install and hence not run any code.
            ## </NOTE>
            if (do_install) {
                topfiles0 <- dir(pkgdir)
                check_dependencies()
            } else topfiles0 <- NULL

            check_sources()
            checkingLog(Log, "if there is a namespace")
            ## careful: we need a case-sensitive match
            if ("NAMESPACE" %in% dir(pkgdir))
                resultLog(Log, "OK")
            else  if (file.exists(file.path(pkgdir, "NAMESPACE"))) {
                errorLog(Log,
                       "File NAMESPACE does not exist but there is a case-insenstiive match.")
                summaryLog(Log)
                do_exit(1L)
            } else if (dir.exists(file.path(pkgdir, "R"))) {
                errorLog(Log)
                wrapLog("All packages need a namespace as from R 3.0.0.\n",
                        "R CMD build will produce a suitable starting point,",
                        "but it is better to handcraft a NAMESPACE file.")
                maybe_exit(1L)
            } else {
                noteLog(Log)
                wrapLog("Packages without R code can be installed without",
                        "a NAMESPACE file, but it is cleaner to add",
                        "an empty one.")
            }

            ## we need to do this before installation
            if (R_check_executables) check_executables()
            ## (Alternatively, could use .unpack.time.)

            check_dot_files(check_incoming)

            setwd(pkgdir)
            allfiles <- check_file_names()
            if (R_check_permissions) check_permissions(allfiles)
            if (!is_base_pkg && R_check_serialization) {
                ## We should not not do this if there is a dependence
                ## on R >= 3.5.0, and we have to check that on the sources.
                db <- .read_description("DESCRIPTION")
                Rver <-.split_description(db, verbose = TRUE)$Rdepends2
                if(length(Rver) && Rver[[1L]]$op == ">="
                   && Rver[[1L]]$version >= "3.5.0") {
                       ## skip
                } else check_serialization(allfiles)
            }
            setwd(startdir)

            ## record this before installation.
            ## <NOTE>
            ## Could also teach the code to check 'src/Makevars[.in]'
            ## files to use .unpack.time.
            ## (But we want to know if the sources contain
            ## 'src/Makevars' and INSTALL re-creates this.)
            ## </NOTE>
            makevars <-
                Sys.glob(file.path(pkgdir, "src",
                                   c("Makevars.in", "Makevars")))
            makevars <- basename(makevars)

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
                                printLog0(Log,
                                          "NB: this package is only installed for sub-architectures ",
                                          paste(sQuote(inst_archs), collapse=", "), "\n")
                            else {
                                printLog0(Log,
                                          "NB: this package is only installed for sub-architecture ",
                                          sQuote(inst_archs), "\n")
                                if(inst_archs == .Platform$r_arch)
                                    this_multiarch <- FALSE
                            }
                        }
                    } else this_multiarch <- FALSE  # no compiled code
                }
                if (this_multiarch && length(R_check_skip_arch))
                    inst_archs <- inst_archs %w/o% R_check_skip_arch
            }
        } else check_incoming <- FALSE  ## end of if (!is_base_pkg)

        elibs <- if(is_base_pkg) character()
        else if(R_check_depends_only)
            setRlibs(pkgdir = pkgdir, libdir = libdir)
        else if(R_check_suggests_only)
            setRlibs(pkgdir = pkgdir, libdir = libdir, suggests = TRUE)
        else character()

        elibs_tests <- if(is_base_pkg) character()
        else if(R_check_depends_only)
            setRlibs(pkgdir = pkgdir, libdir = libdir, tests = TRUE)
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

        if (!is_base_pkg && check_incoming && no_examples &&
            dir.exists(file.path(pkgdir, "R"))) {
            tests_dir <- file.path(pkgdir, test_dir)
            if (dir.exists(tests_dir) &&
                length(dir(tests_dir, pattern = "\\.(r|R|Rin)$")))
                no_examples <- FALSE
            vigns <- pkgVignettes(dir = pkgdir)
            if (!is.null(vigns) && length(vigns$docs)) no_examples <- FALSE
            if (no_examples) {
                ## figure out if the R code exercises anything
                ns <- parseNamespaceFile(basename(pkgdir), dirname(pkgdir))
                if(length(ns$exports) || length(ns$exportPatterns) ||
                   length(ns$exportMethods) || length(ns$S3methods)) {
                    checkingLog(Log, "for code which exercises the package")
                    warningLog(Log, "No examples, no tests, no vignettes")
                }
            }
        }
        summaryLog(Log)

        if(config_val_to_logical(Sys.getenv("_R_CHECK_CRAN_STATUS_SUMMARY_",
                                            "FALSE"))) {
            s <- summarize_CRAN_check_status(pkgname)
            if(nzchar(s)) {
                writeLines(c("", s), Log$con)
            }
        }

        if(Log$errors > 0L)
            do_exit(1L)

        closeLog(Log)
        message("")

    } ## end for (pkg in pkgs)

}
###--- end{ .check_packages }

.format_lines_with_indent <-
function(x)
    paste0("  ", x, collapse = "\n")
    ## Hard-wire indent of 2 for now.

### Local variables:
### mode: R
### page-delimiter: "^###[#-]"
### End:
