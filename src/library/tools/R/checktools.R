#  File src/library/tools/R/checktools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2013-2019 The R Core Team
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

### ** check_packages_in_dir

check_packages_in_dir <-
function(dir,
         check_args = character(), check_args_db = list(),
         reverse = NULL,
         check_env = character(),
         xvfb = FALSE,
         Ncpus = getOption("Ncpus", 1L),
         clean = TRUE,
         ...)
{
    owd <- getwd()
    dir <- normalizePath(dir)
    setwd(dir)
    on.exit(setwd(owd))

    .check_packages_in_dir_retval <-
    function(dir,
             pfiles,
             pnames = character(),
             rnames = character()) {
        structure(pfiles,
                  dir = dir,
                  pnames = pnames,
                  rnames = rnames,
                  class = "check_packages_in_dir")
    }

    pfiles <- Sys.glob("*.tar.gz")
    if(!length(pfiles)) {
        message("no packages to check")
        return(invisible(.check_packages_in_dir_retval(dir, pfiles)))
    }

    pnames <- sub("_.*", "", pfiles)

    os_type <- .Platform$OS.type

    ## Xvfb usage and options.
    ## We do not use Xvfb on Windows.
    ## Otherwise, if argument 'xvfb' is
    ## * a logical, Xvfb is used only if identical to TRUE;
    ## * something else, then as.character(xvfb) gives the Xvfb options.
    xvfb_options <- "-screen 0 1280x1024x24"
    if(os_type == "windows")
        xvfb <- FALSE
    else if(is.logical(xvfb)) {
        if(!isTRUE(xvfb))
            xvfb <- FALSE
    } else {
        xvfb_options <- as.character(xvfb)
        xvfb <- TRUE
    }

    curl <- if(os_type == "windows")
        sprintf("file:///%s", dir)
    else
        sprintf("file://%s", dir)

    libdir <- file.path(dir, "Library")
    dir.create(libdir, showWarnings = FALSE)
    outdir <- file.path(dir, "Outputs")
    dir.create(outdir, showWarnings = FALSE)

    ## Determine packages using fake/no install for checking.
    ## Handle these as follows:
    ## * For packages using '--install=no', forward dependencies do not
    ##   need to installed, and reverse dependencies do not need to be
    ##   checked.
    ## * For packages using '--install=fake', forward dependencies must
    ##   be available for checking, and checking reverse dependencies
    ##   makes sense (e.g, to spot missing Rd xrefs).
    pnames_using_install_no <- character()
    pnames_using_install_fake <- character()
    check_args_db <- as.list(check_args_db)
    if(length(check_args_db) &&
       !is.null(nms <- names(check_args_db))) {
        args <- lapply(check_args_db,
                       function(e)
                       scan(text = e, what = character(), quiet = TRUE))
        pnames_using_install_no <-
            nms[vapply(args, function(e) any(e == "--install=no"), NA)]
        pnames_using_install_fake <-
            nms[vapply(args, function(e) any(e == "--install=fake"), NA)]
    } else {
        ## If check_args_db has no names it is useless.
        ## Perhaps complain?
        check_args_db <- list()
    }

    ## Build a package db from the source packages in the working
    ## directory.
    write_PACKAGES(dir, type = "source")
    if(dir.exists(depdir <- file.path(dir, "Depends"))) {
        write_PACKAGES(depdir, type = "source")
        curl <- c(curl, paste0(curl, "/Depends"))
    }
    ## Determine packages available locally (for checking) and in the
    ## repositories, and merge the information giving preference to the
    ## former.
    localones <- utils::available.packages(contriburl = curl,
                                           type = "source")
    curls <- utils::contrib.url(getOption("repos"), type = "source")
    available <- utils::available.packages(contriburl = curls,
                                           type = "source")
    available <- rbind(localones, available)
    available <-
        available[!duplicated(available[, "Package"]), , drop = FALSE]
    curls <- c(curl, curls)

    ## As of c52164, packages with OS_type different from the current
    ## one are *always* checked with '--install=no'.
    ## These packages are also filtered out by default (via the OS_type
    ## filter) from the repository package computations.
    ## Hence move packages in the install=fake list not listed by
    ## available.packages() to the install=no list.
    pnames_using_install_no <-
        c(pnames_using_install_no,
          setdiff(pnames_using_install_fake, available[, "Package"]))
    pnames_using_install_fake <-
        intersect(pnames_using_install_fake, available[, "Package"])

    if(!is.null(reverse) && !isFALSE(reverse)) {
        ## Determine and download reverse dependencies to be checked as
        ## well.

        reverse <- as.list(reverse)
        ## Merge with defaults, using partial name matching.
        defaults <- list(which = c("Depends", "Imports", "LinkingTo"),
                         recursive = FALSE,
                         repos = getOption("repos"))
        pos <- pmatch(names(reverse), names(defaults), nomatch = 0L)
        defaults[pos] <- reverse[pos > 0L]

        rnames <- if(is.list(defaults$which)) {
            ## No recycling of repos for now.
            defaults$recursive <- rep_len(as.list(defaults$recursive),
                                          length(defaults$which))
            unlist(Map(function(w, r)
                       package_dependencies(setdiff(pnames,
                                                    pnames_using_install_no),
                                            available,
                                            which = w,
                                            recursive = r,
                                            reverse = TRUE),
                       defaults$which,
                       defaults$recursive),
                   use.names = FALSE)
        } else {
            package_dependencies(setdiff(pnames,
                                         pnames_using_install_no),
                                 available,
                                 which = defaults$which,
                                 recursive = defaults$recursive,
                                 reverse = TRUE)
        }

        add_recommended_maybe <-
            config_val_to_logical(Sys.getenv("_R_TOOLS_C_P_I_D_ADD_RECOMMENDED_MAYBE_",
                                             "FALSE"))
        if(add_recommended_maybe) {
            ## Add all recommended packages with any dependency on the
            ## packages to be checked.
            rnames <-
                c(rnames,
                  names(Filter(length,
                               lapply(package_dependencies(.get_standard_package_names()$recommended,
                                                           available,
                                                           which = "all"),
                                      intersect,
                                      pnames))))
        }

        rnames <- intersect(unlist(rnames, use.names = FALSE),
                            available[, "Package"])
        rnames <- setdiff(rnames, pnames)

        pos <- match(rnames, available[, "Package"], nomatch = 0L)
        if(!identical(defaults$repos, getOption("repos"))) {
            pos <- split(pos[pos > 0L], available[pos, "Repository"])
            ## Only want the reverse dependencies for which Repository
            ## starts with an entry in defaults$repos.
            nms <- names(pos)
            ind <- (rowSums(outer(nms, defaults$repos, startsWith)) > 0)
            pos <- unlist(pos[ind], use.names = FALSE)
        }
        rnames <- available[pos, "Package"]
        rfiles <- sprintf("%s_%s.tar.gz",
                          rnames,
                          available[pos, "Version"])

        if(length(rfiles)) {
            message("downloading reverse dependencies ...")
            rfurls <- sprintf("%s/%s",
                              available[pos, "Repository"],
                              rfiles)
            for(i in seq_along(rfiles)) {
                message(sprintf("downloading %s ... ", rfiles[i]),
                        appendLF = FALSE)
                status <- if(!utils::download.file(rfurls[i], rfiles[i],
                                                   quiet = TRUE))
                    "ok" else "failed"
                message(status)
            }
            message("")
        }

    } else {
        rfiles <- rnames <- character()
    }

    pnames <- c(pnames, rnames)

    ## Install what is needed.

    if(xvfb) {
        pid <- start_virtual_X11_fb(xvfb_options)
        on.exit(close_virtual_X11_db(pid), add = TRUE)
    }

    depends <-
        package_dependencies(pnames, available, which = "most")
    depends <- setdiff(unique(unlist(depends, use.names = FALSE)),
                       .get_standard_package_names()$base)

    ## Need to install depends which are not installed or installed but
    ## old.
    libs <- c(libdir, .libPaths())
    installed <- utils::installed.packages(libs)[, "Package"]
    depends <-
        c(setdiff(depends, installed),
          intersect(intersect(depends, installed),
                    utils::old.packages(libs,
                                        available = available)[, "Package"]))
    if(length(depends)) {
        message(paste(strwrap(sprintf("installing dependencies %s",
                                      paste(sQuote(sort(depends)),
                                            collapse = ", ")),
                              exdent = 2L),
                      collapse = "\n"), domain = NA)
        ## <NOTE>
        ## Ideally we would capture stdout and stderr in e.g.
        ##   outdir/install_stdout.txt
        ##   outdir/install_stderr.txt
        ## But using several CPUs uses Make to install, which seems to
        ## write to stdout/stderr "directly" ... so using sink() will
        ## not work.  Hence, use 'keep_outputs' to capture "outputs"
        ## (combining install stdout and stderr into one file).
        message("")
        iflags <- as.list(rep.int("--fake",
                                  length(pnames_using_install_fake)))
        names(iflags) <- pnames_using_install_fake
        tmpdir <- tempfile(tmpdir = outdir)
        dir.create(tmpdir)
        utils::install.packages(depends, lib = libdir,
                                contriburl = curls,
                                available = available,
                                dependencies = NA,
                                INSTALL_opts = iflags,
                                keep_outputs = tmpdir,
                                Ncpus = Ncpus,
                                type = "source")
        outfiles <- Sys.glob(file.path(tmpdir, "*.out"))
        file.rename(outfiles,
                    file.path(outdir,
                              sprintf("install_%s",
                                      basename(outfiles))))
        unlink(tmpdir, recursive = TRUE)
        message("")
        ## </NOTE>
    }

    ## Merge check_args and check_args_db into check_args_db used for
    ## checking.
    check_args <- if(is.list(check_args)) {
        c(rep.int(list(check_args[[1L]]), length(pfiles)),
          rep.int(list(check_args[[2L]]), length(rfiles)))
    } else {
        rep.int(list(check_args), length(pnames))
    }
    check_args_db <- check_args_db[pnames]
    check_args_db <- Map(c, check_args, check_args_db)
    names(check_args_db) <- pnames

    check_env <- if(is.list(check_env)) {
        c(rep.int(list(check_env[[1L]]), length(pfiles)),
          rep.int(list(check_env[[2L]]), length(rfiles)))
    } else {
        rep.int(list(check_env), length(pnames))
    }
    ## No user level check_env_db for now.
    check_env_db <- as.list(check_env)
    names(check_env_db) <- pnames

    pfiles <- c(pfiles, rfiles)

    check_package <- function(pfile, args_db = NULL, env_db = NULL) {
        message(sprintf("checking %s ...", pfile))
        pname <- sub("_.*", "", basename(pfile))
        out <- file.path(outdir,
                         sprintf("check_%s_stdout.txt", pname))
        err <- file.path(outdir,
                         sprintf("check_%s_stderr.txt", pname))
        env <- c(check_env_db[[pname]],
                 sprintf("R_LIBS=%s", shQuote(libdir)))
        lim <- get_timeout(Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_"))
        system.time(system2(file.path(R.home("bin"), "R"),
                            c("CMD",
                              "check",
                              "--timings",
                              args_db[[pname]],
                              pfile),
                            stdout = out,
                            stderr = err,
                            env = env,
                            timeout = lim))
    }

    if(Ncpus > 1L) {
        if(os_type != "windows") {
            timings <- parallel::mclapply(pfiles,
                                          check_package,
                                          check_args_db,
                                          check_env_db,
                                          mc.cores = Ncpus)
        } else {
            cl <- parallel::makeCluster(Ncpus)
            timings <- parallel::parLapply(cl,
                                           pfiles,
                                           check_package,
                                           check_args_db,
                                           check_env_db)
            parallel::stopCluster(cl)
        }
    } else {
        timings <- lapply(pfiles,
                          check_package,
                          check_args_db,
                          check_env_db)
    }

    timings <- do.call(rbind, lapply(timings, summary))
    rownames(timings) <- pnames
    utils::write.table(timings, "timings.tab")

    file.rename(sprintf("%s.Rcheck", rnames),
                sprintf("rdepends_%s.Rcheck", rnames))

    if(clean) {
        file.remove(rfiles)
    } else {
        file.rename(rfiles, sprintf("rdepends_%s", rfiles))
    }

    invisible(.check_packages_in_dir_retval(dir,
                                            pfiles,
                                            setdiff(pnames, rnames),
                                            rnames))
}

### ** print.check_packages_in_dir

print.check_packages_in_dir <-
function(x, ...)
{
    if(!length(x)) {
        writeLines("No packages checked.")
        return(invisible(x))
    }

    dir <- attr(x, "dir")
    writeLines(c(strwrap(sprintf("Check results for packages in dir '%s':",
                                 dir)),
                 sprintf("Package sources: %d, Reverse depends: %d",
                         length(attr(x, "pnames")),
                         length(attr(x, "rnames"))),
                 "Use summary() for more information."))
    invisible(x)
}

### ** summary.check_packages_in_dir

summary.check_packages_in_dir <-
function(object, all = TRUE, full = FALSE, ...)
{
    if(!length(object)) {
        writeLines("No packages checked.")
        return(invisible(object))
    }

    dir <- attr(object, "dir")
    writeLines(c(strwrap(sprintf("Check results for packages in dir '%s':",
                                 dir)),
                 ""))
    details <- summarize_check_packages_in_dir_results(dir)
    if(!full && details) {
        writeLines("\nUse summary(full = TRUE) for details.")
    }
    invisible(object)
}

### ** start_virtual_X11_fb

start_virtual_X11_fb <-
function(options)
{
    ## Determine the display number from the options, or the PID of the
    ## current R process (alternatively, could mimic xvfb-run).
    args <- scan(text = options, what = character(), quiet = TRUE)
    ind <- grepl("^:[[:digit:]]+$", args)
    if(any(ind)) {
        num <- args[ind][1L]
    } else {
        num <- paste0(":", Sys.getpid())
        options <- c(num, options)
    }

    dis <- Sys.getenv("DISPLAY", unset = NA_character_)

    ## We need to start Xvfb with the given options and obtain its pid
    ## so that we can terminate it when done checking.
    ## This could be done via
    ##   system2("Xvfb", options, stdout = FALSE, stderr = FALSE,
    ##           wait = FALSE)
    ## and then determine the pid as
    ##   pid <- scan(text =
    ##               grep(sprintf("Xvfb %s", num),
    ##                    system2("ps", "auxw", stdout = TRUE),
    ##                    value = TRUE,
    ##                    fixed = TRUE),
    ##               what = character(),
    ##               quiet = TRUE)[2L]
    ## A better approach (suggested by BDR) is to create a shell script
    ## containing the call to start Xvfb in the background and display
    ## the pid of this as available in the shell's $! parameter.
    tf <- tempfile()
    on.exit(unlink(tf))
    writeLines(c(paste(c(shQuote("Xvfb"), options, ">/dev/null 2>&1 &"),
                       collapse = " "),
                 "echo ${!}"),
               tf)
    pid <- system2("sh", tf, stdout = TRUE)
    Sys.setenv("DISPLAY" = num)

    ## Propagate both pid and original setting of DISPLAY so that the
    ## latter can be restored when Xvfb is closed.
    attr(pid, "display") <- dis
    pid
}

### ** close_virtual_X11_db

close_virtual_X11_db <-
function(pid)
{
    pskill(pid)
    if(is.na(dis <- attr(pid, "display")))
        Sys.unsetenv("DISPLAY")
    else
        Sys.setenv("DISPLAY" = dis)
}

### ** R_check_outdirs

R_check_outdirs <-
function(dir, all = FALSE, invert = FALSE)
{
    dir <- normalizePath(dir)
    outdirs <- dir(dir, pattern = "\\.Rcheck")
    ind <- startsWith(basename(outdirs), "rdepends_")
    ## Re-arrange to have reverse dependencies last if at all.
    outdirs <- if(invert)
        c(if(all) outdirs[!ind], outdirs[ind])
    else
        c(outdirs[!ind], if(all) outdirs[ind])
    file.path(dir, outdirs)
}

### ** summarize_check_packages_in_dir_depends

summarize_check_packages_in_dir_depends <-
function(dir, all = FALSE, which = c("Depends", "Imports", "LinkingTo"))
{
    ## See tools::package_dependencies(): should perhaps separate out.
    if(identical(which, "all"))
        which <- c("Depends", "Imports", "LinkingTo", "Suggests",
                   "Enhances")
    else if(identical(which, "most"))
        which <- c("Depends", "Imports", "LinkingTo", "Suggests")

    for(d in R_check_outdirs(dir, all = all)) {
        dfile <- Sys.glob(file.path(d, "00_pkg_src", "*",
                                    "DESCRIPTION"))[1L]
        if(file_test("-f", dfile)) {
            meta <- .read_description(dfile)
            package <- meta["Package"]
            meta <- meta[match(which, names(meta), nomatch = 0L)]
            if(length(meta)) {
                writeLines(c(sprintf("Package: %s", package),
                             unlist(Map(function(tag, val) {
                                 strwrap(sprintf("%s: %s", tag, val),
                                         indent = 2L, exdent = 4L)
                             },
                                        names(meta),
                                        meta))))
            }
        }
    }

    invisible()
}

### ** summarize_check_packages_in_dir_results

summarize_check_packages_in_dir_results <-
function(dir, all = TRUE, full = FALSE, ...)
{
    dir <- normalizePath(dir)
    outdirs <- R_check_outdirs(dir, all = all)
    logs <- file.path(outdirs, "00check.log")
    logs <- logs[file_test("-f", logs)]

    results <- check_packages_in_dir_results(logs = logs, ...)

    writeLines("Check status summary:")
    tab <- check_packages_in_dir_results_summary(results)
    rownames(tab) <- paste0("  ", rownames(tab))
    print(tab)
    writeLines("")

    writeLines("Check results summary:")
    Map(function(p, r) {
        writeLines(c(sprintf("%s ... %s", p, r$status), r$lines))
    },
        names(results),
        results)

    if(full &&
       !all(as.character(unlist(lapply(results, `[[`, "status"))) ==
            "OK")) {
        writeLines(c("", "Check results details:"))
        details <- check_packages_in_dir_details(logs = logs, ...)
        writeLines(paste(format(details), collapse = "\n\n"))
        invisible(TRUE)
    } else {
        invisible(FALSE)
    }
}

### ** summarize_check_packages_in_dir_timings

summarize_check_packages_in_dir_timings <-
function(dir, all = FALSE, full = FALSE)
{
    dir <- normalizePath(dir)
    tfile <- file.path(dir, "timings.tab")
    if(file_test("-f", tfile)) {
        timings <- utils::read.table(tfile)
        ## Should we store the information about reverse dependencies in
        ## some place (rather than rely on the naming convention)?
        if(!all) {
            rdepends <- Sys.glob(file.path(dir, "rdepends_*.Rcheck"))
            timings <- timings[is.na(match(rownames(timings),
                                           sub("rdepends_(.*).Rcheck",
                                               "\\1",
                                               basename(rdepends)))),
                               ]
        }
        print(timings)
    }
    if(full) {
        tfiles <- Sys.glob(file.path(R_check_outdirs(dir, all = all),
                                     "*-Ex.timings"))
        if(length(tfiles)) message("")
        timings <- lapply(tfiles, utils::read.table, header = TRUE)
        ## Order by CPU time.
        timings <- lapply(timings,
                          function(x)
                          x[order(x$user, decreasing = TRUE), ])
        ## This looks silly, but we want a common alignment.
        timings <- split(as.data.frame(lapply(do.call(rbind, timings),
                                              format)),
                         rep.int(sub("\\.Rcheck$", "",
                                     basename(dirname(tfiles))),
                                 vapply(timings, nrow, 0L)))
        invisible(Map(function(x, y) {
            writeLines(sprintf("Example timings for package '%s':", x))
            cat(rbind(" ", t(as.matrix(y))),
                sep = c(" ", " ", " ", " ", "\n"))
        },
                      names(timings), timings))
    }

    invisible()
}

### ** check_packages_in_dir_results

## <FIXME>
## For new-style logs from successful check runs (a '* DONE' line
## followed by a 'Status: ' line), we could simply get the status from
## the 'Status: ' line.
## Change to preferably rely on the new format eventually.
## Note that check logs can end up incomplete in which case there is no
## final status line ...
## </FIXME>

check_packages_in_dir_results <-
function(dir, logs = NULL, ...)
{
    if(is.null(logs))
        logs <- Sys.glob(file.path(dir, "*.Rcheck", "00check.log"))

    ## <NOTE>
    ## Perhaps make the individual non-OK check values more readily
    ## available?
    ## </NOTE>

    results <- lapply(logs, function(log, ...) {
        lines <- read_check_log(log, ...)
        ## See analyze_lines() inside analyze_check_log():
        re <- "^\\* (loading checks for arch|checking (examples|tests) \\.\\.\\.$)"
        pos <- grep(re, lines, perl = TRUE, useBytes = TRUE)
        if(length(pos <- pos[pos < length(lines)]))
            lines <- lines[-pos]
        re <- "^\\*\\*? ((checking|creating|running examples for arch|running tests for arch) .*) \\.\\.\\.( (\\[[^ ]*\\]))?( (NOTE|WARNING|ERROR)|)$"
        m <- regexpr(re, lines, perl = TRUE, useBytes = TRUE)
        ind <- (m > 0L)
        ## Note that we use WARN instead of WARNING for the summary.
        status <-
            if(any(ind)) {
                status <- sub(re, "\\6", lines[ind],
                              perl = TRUE, useBytes = TRUE)
                if(any(status == "")) "FAIL"
                else if(any(status == "ERROR")) "ERROR"
                else if(any(status == "WARNING")) "WARN"
                else "NOTE"
            } else {
                "OK"
            }
        list(status = status, lines = lines[ind])
    }, ...)
    names(results) <- sub("\\.Rcheck$", "", basename(dirname(logs)))

    results
}

### ** check_packages_in_dir_results_summary

check_packages_in_dir_results_summary <-
function(results)
{
    if(!length(results)) return()
    status <- vapply(results, `[[`, "", "status")
    ind <- startsWith(names(results), "rdepends_")
    tab <- table(ifelse(ind, "Reverse depends", "Source packages"),
                 status, deparse.level = 0L)
    tab <- tab[match(c("Source packages", "Reverse depends"),
                     rownames(tab), nomatch = 0L),
               match(c("FAIL", "ERROR", "WARN", "NOTE", "OK"),
                     colnames(tab), nomatch = 0L),
               drop = FALSE]
    names(dimnames(tab)) <- NULL
    tab
}

### ** read_check_log

read_check_log <-
function(log, drop = TRUE, ...)
{
    lines <- readLines(log, warn = FALSE, ...)

    if(drop) {
        ## Drop CRAN check status footer.
        ## Ideally, we would have a more general mechanism to detect
        ## footer information to be skipped (e.g., a line consisting of
        ## a single non-printing control character?)
        pos <- grep("^Current CRAN status:", lines,
                    perl = TRUE, useBytes = TRUE)
        if(length(pos) && lines[pos <- (pos[1L] - 1L)] == "") {
            lines <- lines[seq_len(pos - 1L)]
        }
    }

    ## <FIXME>
    ## Remove eventually.
    len <- length(lines)
    end <- lines[len]
    if(length(end) &&
       grepl(re <- "^(\\*.*\\.\\.\\.)(\\* elapsed time.*)$", end,
             perl = TRUE, useBytes = TRUE)) {
        lines <- c(lines[seq_len(len - 1L)],
                   sub(re, "\\1", end, perl = TRUE, useBytes = TRUE),
                   sub(re, "\\2", end, perl = TRUE, useBytes = TRUE))
    }
    ## </FIXME

    lines
}

### ** analyze_check_log

## <FIXME>
## New-style check logs should have a '* DONE' line followed by a
## 'Status:' line.  If not, a check failure occurred.
## Change to fully rely on the new format eventually.
## </FIXME>

analyze_check_log <-
function(log, drop_ok = TRUE, ...)
{
    make_results <- function(package, version, flags, chunks)
        list(Package = package, Version = version,
             Flags = flags, Chunks = chunks)

    ## Alternatives for left and right quotes.
    lqa <- paste0("'|", intToUtf8(0x2018))
    rqa <- paste0("'|", intToUtf8(0x2019))
    ## Group when used ...

    if(is.character(drop_ok)) {
        drop_ok_status_tags <- drop_ok
        drop_ok <- TRUE
    } else {
        drop_ok_status_tags <- c("OK", "NONE", "SKIPPED")
    }

    ## Start by reading in.
    lines <- read_check_log(log, ...)

    ## Re-encode to UTF-8 using the session charset info.
    ## All regexp computations will be done using perl = TRUE and
    ## use useBytes = TRUE for matching and extracting ASCII content.
    re <- "^\\* using session charset: "
    pos <- grep(re, lines, perl = TRUE, useBytes = TRUE)
    if(length(pos)) {
        enc <- sub(re, "", lines[pos[1L]])
        lines <- iconv(lines, enc, "UTF-8", sub = "byte")
        ## If the check log uses ASCII, there should be no non-ASCII
        ## characters in the message lines: could check for this.
        if(any(bad <- !validEnc(lines)))
            lines[bad] <- iconv(lines[bad], to = "ASCII", sub = "byte")
    } else {
        ## In case of a fundamental immediate problem which renders
        ## further checking pointless, we currently do not provide the
        ## header information with the session charset.  (Perhaps this
        ## should be changed.)
        if(!any(grepl("^\\* checking ", lines,
                      perl = TRUE, useBytes = TRUE)))
            return()
    }

    package <- "???"
    version <- ""

    ## Get header.
    header <- lines
    re <- sprintf("^\\* this is package (%s)(.*)(%s) version (%s)(.*)(%s)$",
                  lqa, rqa, lqa, rqa)
    pos <- grep(re, lines, perl = TRUE, useBytes = TRUE)
    if(length(pos)) {
        pos <- pos[1L]
        txt <- lines[pos]
        package <- sub(re, "\\2", txt, perl = TRUE, useBytes = TRUE)
        version <- sub(re, "\\5", txt, perl = TRUE, useBytes = TRUE)
        header <- lines[seq_len(pos - 1L)]
        lines <- lines[-seq_len(pos)]
    } else {
        ## If there was no 'this is package %s version %s' line, then
        ## either there was a fundamental immediate problem, or an error
        ## in check_description().  In the latter case there should be a
        ## line like
        ##   * checking for file '%s/DESCRIPTION'
        ## with %s the package name implied by the invocation, but not
        ## necessarily the one recorded in DESCRIPTION: let's use that
        ## package name nevertheless, as it is better than nothing.
        re <- sprintf("^\\* checking for file (%s)(.*)/DESCRIPTION(%s).*$",
                      lqa, rqa)
        pos <- grep(re, lines, perl = TRUE, useBytes = TRUE)
        if(length(pos)) {
            pos <- pos[1L]
            txt <- lines[pos]
            package <- sub(re, "\\2", txt, perl = TRUE, useBytes = TRUE)
            header <- lines[seq_len(pos - 1L)]
        } else if(!any(grepl("^\\* checking ", lines,
                             perl = TRUE, useBytes = TRUE)))
            return()
    }
    ## Get check options from header.
    re <- sprintf("^\\* using options? (%s)(.*)(%s)$", lqa, rqa)
    flags <-
        if(length(pos <- grep(re, header,
                              perl = TRUE, useBytes = TRUE))) {
            sub(re, "\\2", header[pos[1L]],
                perl = TRUE, useBytes = TRUE)
        } else ""

    ## Get footer.
    len <- length(lines)
    pos <- which(lines == "* DONE")
    if(length(pos) &&
       ((pos <- pos[length(pos)]) < len) &&
       startsWith(lines[pos + 1L], "Status: "))
        lines <- lines[seq_len(pos - 1L)]
    else {
        ## Not really new style, or failure ... argh.
        ## Some check systems explicitly record the elapsed time in the
        ## last line:
        if(grepl("^\\* elapsed time ", lines[len],
                 perl = TRUE, useBytes = TRUE)) {
            lines <- lines[-len]
            len <- len - 1L
            while(grepl("^[[:space:]]*$", lines[len])) {
                lines <- lines[-len]
                len <- len - 1L
            }
        }
        ## Summary footers.
        if(startsWith(lines[len], "Status: ")) {
            ## New-style status summary.
            lines <- lines[-len]
            len <- len - 1L
        } else {
            ## Old-style status summary.
            num <- length(grep("^(NOTE|WARNING): There",
                               lines[c(len - 1L, len)]))
            if(num > 0L) {
                pos <- seq.int(len - num + 1L, len)
                lines <- lines[-pos]
                len <- len - num
            }
        }
        if(lines[len] == "* DONE")
            lines <- lines[-len]
    }

    analyze_lines <- function(lines) {
        ## Windows has
        ##   * loading checks for arch
        ##   * checking examples ...
        ##   * checking tests ...
        ## headers: drop these unless not followed by the appropriate
        ## 'subsection', which indicates failure.
        pos <- which(startsWith(lines, "* loading checks for arch"))
        pos <- pos[pos < length(lines)]
        pos <- pos[startsWith(lines[pos + 1L], "** checking")]
        if(length(pos))
            lines <- lines[-pos]
        pos <- which(startsWith(lines, "* checking examples"))
        pos <- pos[pos < length(lines)]
        pos <- pos[startsWith(lines[pos + 1L],
                              "** running examples for arch")]
        if(length(pos))
            lines <- lines[-pos]
        pos <- which(startsWith(lines, "* checking tests"))
        pos <- pos[pos < length(lines)]
        pos <- pos[startsWith(lines[pos + 1L],
                              "** running tests for arch")]
        if(length(pos))
            lines <- lines[-pos]
        ## We might still have
        ##   * package encoding:
        ## entries for packages declaring a package encoding.
        ## Hopefully all other log entries we still have are
        ##   * checking
        ##   * creating
        ## ones ... apparently, with the exception of
        ##   ** running examples for arch
        ##   ** running tests for arch
        ## So let's drop everything up to the first such entry.
        re <- "^\\*\\*? ((checking|creating|running examples for arch|running tests for arch) .*) \\.\\.\\.( (\\[[^ ]*\\]))?( (.*)|)$"
        ind <- grepl(re, lines, perl = TRUE, useBytes = TRUE)
        csi <- cumsum(ind)
        ind <- (csi > 0)
        chunks <-
            lapply(split(lines[ind], csi[ind]),
                   function(s) {
                       ## Note that setting
                       ##   _R_CHECK_TEST_TIMING_=yes
                       ##   _R_CHECK_VIGNETTE_TIMING_=yes
                       ## will result in a different chunk format ...
                       line <- s[1L]
                       check <- sub(re, "\\1", line,
                                    perl = TRUE, useBytes = TRUE)
                       status <- sub(re, "\\6", line,
                                     perl = TRUE, useBytes = TRUE)
                       if(status == "") status <- "FAIL"
                       list(check = check,
                            status = status,
                            output = paste(s[-1L], collapse = "\n"))
                   })

        status <- vapply(chunks, `[[`, "", "status")
        if(isTRUE(drop_ok) ||
           (is.na(drop_ok)
               && all(is.na(match(c("ERROR", "FAIL"), status)))))
            chunks <- chunks[is.na(match(status, drop_ok_status_tags))]

        chunks
    }

    chunks <- analyze_lines(lines)
    if(!length(chunks) && !isFALSE(drop_ok)) {
        chunks <- list(list(check = "*", status = "OK", output = ""))
    }

    make_results(package, version, flags, chunks)
}

### ** check_packages_in_dir_details

check_packages_in_dir_details <-
function(dir, logs = NULL, drop_ok = TRUE, ...)
{
    ## Build a data frame with columns
    ##   Package Version Check Status Output Flags
    ## and some optimizations (in particular, Check Status Flags can be
    ## factors).

    db_from_logs <- function(logs, drop_ok, ...) {
        out <- lapply(logs, analyze_check_log, drop_ok, ...)
        out <- out[lengths(out) > 0L]
        if(!length(out))
            return(matrix(character(), ncol = 6L))
        chunks <- lapply(out, `[[`, "Chunks")
        package <- sapply(out, `[[`, "Package")
        lens <- lengths(chunks)
        cbind(rep.int(package, lens),
              rep.int(sapply(out, `[[`, "Version"), lens),
              matrix(as.character(unlist(chunks)), ncol = 3L,
                     byrow = TRUE),
              rep.int(sapply(out, `[[`, "Flags"),
                      lens))
    }

    if(is.null(logs)) {
        if(inherits(dir, "check_packages_in_dir"))
            dir <- attr(dir, "dir")
        logs <- Sys.glob(file.path(dir, "*.Rcheck", "00check.log"))
    }

    db <- db_from_logs(logs, drop_ok, ...)
    colnames(db) <- c("Package", "Version", "Check", "Status",
                      "Output", "Flags")

    ## Now some cleanups.

    ## Alternatives for left and right quotes.
    lqa <- paste0("'|", intToUtf8(0x2018))
    rqa <- paste0("'|", intToUtf8(0x2019))
    ## Group when used ...

    checks <- db[, "Check"]
    checks <- sub(sprintf("checking whether package (%s).*(%s) can be installed",
                          lqa, rqa),
                  "checking whether package can be installed",
                  checks, perl = TRUE, useBytes = TRUE)
    checks <- sub("creating .*-Ex.R", "checking examples creation",
                  checks, perl = TRUE, useBytes = TRUE)
    checks <- sub("creating .*-manual\\.tex", "checking manual creation",
                  checks, perl = TRUE, useBytes = TRUE)
    checks <- sub("checking .*-manual\\.tex", "checking manual",
                  checks, perl = TRUE, useBytes = TRUE)
    checks <- sub(sprintf("checking package vignettes in (%s)inst/doc(%s)",
                          lqa, rqa),
                  "checking package vignettes",
                  checks, perl = TRUE, useBytes = TRUE)
    checks <- sub("^checking *", "",
                  checks, perl = TRUE, useBytes = TRUE)
    db[, "Check"] <- checks
    ## In fact, for tabulation purposes it would even be more convenient
    ## to shorten the check names ...

    db[, "Output"] <-
        sub("[[:space:]]+$", "", db[, "Output"], perl = TRUE)

    db <- as.data.frame(db, stringsAsFactors = FALSE)
    db$Check <- as.factor(db$Check)
    db$Status <- as.factor(db$Status)

    class(db) <- c("check_details", "data.frame")
    db
}

format.check_details <-
function(x, ...)
{
    flags <- x$Flags
    flavor <- x$Flavor
    paste0(sprintf("Package: %s %s\n",
                   x$Package, x$Version),
           ifelse(nzchar(flavor),
                  sprintf("Flavor: %s\n", flavor),
                  ""),
           ifelse(nzchar(flags),
                  sprintf("Flags: %s\n", flags),
                  ""),
           sprintf("Check: %s, Result: %s\n",
                   x$Check, x$Status),
           sprintf("  %s",
                   gsub("\n", "\n  ", x$Output, perl = TRUE))
           )
}

print.check_details <-
function(x, ...)
{
    writeLines(paste(format(x, ...), collapse = "\n\n"))
    invisible(x)
}

### ** check_packages_in_dir_changes

check_packages_in_dir_changes <-
function(dir, old, outputs = FALSE, sources = FALSE, ...)
{
    dir <- if(inherits(dir, "check_packages_in_dir"))
        dir <- attr(dir, "dir")
    else
        normalizePath(dir)

    outdirs <- R_check_outdirs(dir, all = sources, invert = TRUE)
    logs <- file.path(outdirs, "00check.log")
    logs <- logs[file_test("-f", logs)]
    new <- check_packages_in_dir_details(logs = logs, drop_ok = FALSE, ...)

    ## Use
    ##   old = tools:::CRAN_check_details(FLAVOR)
    ## to compare against the results/details of a CRAN check flavor.

    if(!inherits(old, "check_details"))
        old <- check_packages_in_dir_details(old, drop_ok = FALSE, ...)

    check_details_changes(new, old, outputs)
}

### ** check_details_changes

check_details_changes <-
function(new, old, outputs = FALSE)
{
    check_details_changes_classes <-
        c("check_details_changes", "data.frame")

    if(!inherits(new, "check_details")) stop("wrong class")
    if(!inherits(old, "check_details")) stop("wrong class")

    ## Simplify matters by considering only "changes" in *available*
    ## results/details.

    packages <- intersect(old$Package, new$Package)

    if(!length(packages)) {
        db <- data.frame(Package = character(),
                         Check = character(),
                         Old = character(),
                         New = character(),
                         stringsAsFactors = FALSE)
        class(db) <- check_details_changes_classes
        return(db)
    }

    db <- merge(old[!is.na(match(old$Package, packages)), ],
                new[!is.na(match(new$Package, packages)), ],
                by = c("Package", "Check"), all = TRUE)

    ## Complete possibly missing version information.
    chunks <-
        lapply(split(db, db$Package),
               function(e) {
                   len <- nrow(e)
                   if(length(pos <- which(!is.na(e$Version.x))))
                       e$Version.x <-
                           rep.int(e[pos[1L], "Version.x"], len)
                   if(length(pos <- which(!is.na(e$Version.y))))
                       e$Version.y <-
                           rep.int(e[pos[1L], "Version.y"], len)
                   e
               })
    db <- do.call(rbind, chunks)

    ## Drop checks that are OK in both versions
    x.issue <- !is.na(match(db$Status.x,
                            c("ERROR","FAIL","NOTE","WARNING")))
    y.issue <- !is.na(match(db$Status.y,
                            c("ERROR","FAIL","NOTE","WARNING")))
    db <- db[x.issue | y.issue,]

    ## Even with the above simplification, missing entries do not
    ## necessarily indicate "OK" (checks could have been skipped).
    ## Hence leave as missing and show as empty in the diff.
    ## An exception to this rule is made if we find an "ERROR" result
    ## as this may explain skipped checks.

    sx <- as.character(db$Status.x)
    sy <- as.character(db$Status.y)
    if(outputs) {
        ind <- nzchar(ox <- db$Output.x)
        sx[ind] <- sprintf("%s\n  %s", sx[ind],
                           gsub("\n", "\n  ", ox[ind], fixed = TRUE))
        ind <- nzchar(oy <- db$Output.y)
        sy[ind] <- sprintf("%s\n  %s", sy[ind],
                           gsub("\n", "\n  ", oy[ind], fixed = TRUE))
    }
    sx[is.na(db$Status.x)] <- ""
    sy[is.na(db$Status.y)] <- ""
    ind <- if(outputs)
        (.canonicalize_quotes(sx) != .canonicalize_quotes(sy))
    else
        (sx != sy)

    db <- cbind(db[ind, ], Old = sx[ind], New = sy[ind],
                stringsAsFactors = FALSE)

    ## Add information about possible version changes.
    ind <- (db$Version.x != db$Version.y)
    if(any(ind))
        db$Package[ind] <-
            sprintf("%s [Old version: %s, New version: %s]",
                    db$Package[ind],
                    db$Version.x[ind],
                    db$Version.y[ind])

    db <- db[c("Package", "Check", "Old", "New")]

    class(db) <- check_details_changes_classes

    db
}

`[.check_details_changes` <-
function(x, i, j, drop = FALSE)
{
    if(((nargs() - !missing(drop)) == 3L)
       && (length(i) == 1L)
       && any(!is.na(match(i, c("==", "!=", "<", "<=", ">", ">="))))) {
        levels <- c("", "OK", "NOTE", "WARNING", "ERROR", "FAIL")
        encode <- function(s) {
            s <- sub("\n.*", "", s)
            s[is.na(match(s, levels))] <- ""
            ordered(s, levels)
        }
        old <- encode(x$Old)
        new <- encode(x$New)
        i <- do.call(i, list(old, new))
    }
    NextMethod()
}

format.check_details_changes <-
function(x, ...)
{
    if(!nrow(x)) return(character())
    sprintf("Package: %s\nCheck: %s%s%s",
            x$Package,
            x$Check,
            ifelse(nzchar(old <- x$Old),
                   sprintf("\nOld result: %s", old),
                   ""),
            ifelse(nzchar(new <- x$New),
                   sprintf("\nNew result: %s", new),
                   ""))
}

print.check_details_changes <-
function(x, ...)
{
    if(length(y <- format(x)))
        writeLines(paste(y, collapse = "\n\n"))
    invisible(x)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
