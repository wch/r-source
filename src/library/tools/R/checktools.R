#  File src/library/tools/R/checktools.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2013-2013 The R Core Team
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

    pfiles <- Sys.glob("*.tar.gz")
    if(!length(pfiles)) {
        message("no packages to check")
        return(invisible())
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
        if(!identical(xvfb, TRUE))
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
            nms[sapply(args, function(e) any(e == "--install=no"))]
        pnames_using_install_fake <-
            nms[sapply(args, function(e) any(e == "--install=fake"))]
    } else {
        ## If check_args_db has no names it is useless.
        ## Perhaps complain?
        check_args_db <- list()
    }

    ## Build a package db from the source packages in the working
    ## directory.
    write_PACKAGES(dir, type = "source")
    
    curls <- c(curl,
               utils::contrib.url(getOption("repos"), type = "source"))
    available <- utils::available.packages(contriburl = curls,
                                           type = "source")

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
    
    if(!is.null(reverse) && !identical(reverse, FALSE)) {
        ## Determine and download reverse dependencies to be checked as
        ## well.
        
        reverse <- as.list(reverse)
        ## Merge with defaults, using partial name matching.
        defaults <- list(which = c("Depends", "Imports", "LinkingTo"),
                         recursive = FALSE,
                         repos = getOption("repos"))
        pos <- pmatch(names(reverse), names(defaults), nomatch = 0L)
        defaults[pos] <- reverse[pos > 0L]

        rnames <-
            package_dependencies(setdiff(pnames,
                                         pnames_using_install_no),
                                 available,
                                 which = defaults$which,
                                 recursive =
                                 defaults$recursive,
                                 reverse = TRUE)

        rnames <- intersect(unlist(rnames, use.names = FALSE),
                            available[, "Package"])
        rnames <- setdiff(rnames, pnames)

        pos <- match(rnames, available[, "Package"], nomatch = 0L)
        if(!identical(defaults$repos, getOption("repos"))) {
            pos <- split(pos[pos > 0L], available[pos, "Repository"])
            ## Only want the reverse dependencies for which Repository
            ## is pmatched by contrib.url(defaults$repos).
            nms <- names(pos)
            pos <- unlist(pos[unique(c(outer(defaults$repos, nms,
                                             pmatch, nomatch = 0L)))],
                          use.names = FALSE)
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
                status <- if(!utils::download.file(rfurls[i], rfiles[i]))
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

    depends <-
        package_dependencies(pnames, available, which = "most")
    depends <- setdiff(unique(unlist(depends, use.names = FALSE)),
                       unlist(.get_standard_package_names(),
                              use.names = FALSE))

    ## Need to install depends which are not installed or installed but
    ## old.
    libs <- c(libdir, .libPaths())
    installed <- utils::installed.packages(libs)[, "Package"]
    depends <-
        c(setdiff(depends, installed),
          intersect(intersect(depends, installed),
                    utils::old.packages(libs, contriburl = curls)[, "Package"]))
    if(length(depends)) {
        message(paste(strwrap(sprintf("installing dependencies %s",
                                      paste(sQuote(depends),
                                            collapse = ", ")),
                              exdent = 2L),
                      collapse = "\n"))
        ## <NOTE>
        ## Ideally we would capture stdout and stderr in e.g.
        ##   outdir/install_stdout.txt
        ##   outdir/install_stderr.txt
        ## But using several CPUs uses Make to install, which seems to
        ## write to stdout/stderr "directly" ... so using sink() will
        ## not work.
        message("")
        iflags <- as.list(rep.int("--fake",
                                  length(pnames_using_install_fake)))
        names(iflags) <- pnames_using_install_fake
        utils::install.packages(depends, lib = libdir,
                                contriburl = curls,
                                available = available,
                                dependencies = TRUE,
                                INSTALL_opts = iflags,
                                Ncpus = Ncpus, 
                                type = "source")
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
        system.time(system2(file.path(R.home("bin"), "R"),
                            c("CMD",
                              "check",
                              "--timings",
                              args_db[[pname]],
                              pfile),
                            stdout = out,
                            stderr = err,
                            env = env))
    }


    if(xvfb) {
        pid <- start_virtual_X11_fb(xvfb_options)
        on.exit(close_virtual_X11_db(pid), add = TRUE)
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
    write.table(timings, "timings.tab")

    file.rename(sprintf("%s.Rcheck", rnames),
                sprintf("rdepends_%s.Rcheck", rnames))

    if(clean) {
        file.remove(rfiles)
    } else {
        file.rename(rfiles, sprintf("rdepends_%s", rfiles))
    }

    invisible(pfiles)

}

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

    dis <- Sys.getenv("DISPLAY", unset = NA)

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

close_virtual_X11_db <-
function(pid)
{
    pskill(pid)
    if(is.na(dis <- attr(pid, "display")))
        Sys.unsetenv("DISPLAY")
    else
        Sys.setenv("DISPLAY" = dis)
}

R_check_outdirs <-
function(dir, all = FALSE)
{
    dir <- normalizePath(dir)
    outdirs <- dir(dir, pattern = "\\.Rcheck")
    if(!all) outdirs <- outdirs[!grepl("^rdepends_", outdirs)]
    file.path(dir, outdirs)
}

summarize_check_packages_in_dir_depends <-
function(dir, all = FALSE)
{
    for(d in R_check_outdirs(dir, all = all)) {
        dfile <- Sys.glob(file.path(d, "00_pkg_src", "*",
                                    "DESCRIPTION"))[1L]
        if(file_test("-f", dfile)) {
            meta <- .read_description(dfile)
            has_depends <- !is.na(meta["Depends"])
            has_imports <- !is.na(meta["Imports"])
            if(has_depends || has_imports) {
                writeLines(c(sprintf("Package: %s",
                                     meta["Package"]),
                             if(has_depends)
                             strwrap(sprintf("Depends: %s",
                                             meta["Depends"]),
                                     indent = 2L,
                                     exdent = 4L),
                             if(has_imports)
                             strwrap(sprintf("Imports: %s",
                                             meta["Imports"]),
                                     indent = 2L,
                                     exdent = 4L)))
            }
        }
    }
}

summarize_check_packages_in_dir_results <-
function(dir, all = TRUE)
{
    dir <- normalizePath(dir)
    outdirs <- R_check_outdirs(dir, all = all)
    ## Re-arrange to have reverse dependencies last.
    ind <- grepl("^rdepends_", basename(outdirs))
    outdirs <- c(outdirs[!ind], outdirs[ind])
    for(d in outdirs) {
        pname <- sub("\\.Rcheck$", "", basename(d))
        log <- readLines(file.path(d, "00check.log"), warn = FALSE)
        m <- regexpr("\\.\\.\\. *(\\[.*\\])? *(NOTE|WARN|ERROR)", log,
                     useBytes = TRUE)
        ind <- (m > 0L)
        if(any(ind)) {
            status <- if(all(grepl("NOTE$", regmatches(log, m),
                                   useBytes = TRUE))) {
                "NOTE"
            } else "PROBLEM"
            writeLines(c(sprintf("%s ... %s", pname, status),
                         log[ind]))
        } else {
            writeLines(sprintf("%s ... OK", pname))
        }
    }
}

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
        timings <- lapply(tfiles, read.table, header = TRUE)
        ## Order by CPU time.
        timings <- lapply(timings,
                          function(x)
                          x[order(x$user, decreasing = TRUE), ])
        ## This looks silly, but we want a common alignment.
        timings <- split(as.data.frame(lapply(do.call(rbind, timings),
                                              format)),
                         rep.int(sub("\\.Rcheck$", "",
                                     basename(dirname(tfiles))),
                                 sapply(timings, nrow)))
        invisible(Map(function(x, y) {
            writeLines(sprintf("Example timings for package '%s':", x))
            cat(rbind(" ", t(as.matrix(y))),
                sep = c(" ", " ", " ", " ", "\n"))
        },
                      names(timings), timings))
    }
}
