#  File src/library/tools/R/testing.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
#
# NB: also copyright date in Usage.
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

## functions principally for testing R and packages

massageExamples <-
    function(pkg, files, outFile = stdout(), use_gct = FALSE,
             addTiming = FALSE, ..., commentDonttest = TRUE)
{
    if(dir.exists(files[1L])) {
        old <- Sys.getlocale("LC_COLLATE")
        Sys.setlocale("LC_COLLATE", "C")
        files <- sort(Sys.glob(file.path(files, "*.R")))
        Sys.setlocale("LC_COLLATE", old)
    }

    if(is.character(outFile)) {
        out <- file(outFile, "wt")
        on.exit(close(out))
        cntFile <- paste0(outFile, "-cnt")
    } else {
        out <- outFile
        cntFile <- NULL
    }

    count <- 0L # of files using \donttest

    lines <- c(paste0('pkgname <- "', pkg, '"'),
               'source(file.path(R.home("share"), "R", "examples-header.R"))',
               if (use_gct) {
                   gct_n <- as.integer(Sys.getenv("_R_CHECK_GCT_N_", "0"))
                   if(!is.na(gct_n) && gct_n > 0L)
                       sprintf("gctorture2(%s)", gct_n)
                   else "gctorture(TRUE)"
               },
               "options(warn = 1)")
    cat(lines, sep = "\n", file = out)
    if(.Platform$OS.type == "windows")
        cat("options(pager = \"console\")\n", file = out)
    if(addTiming) {
        ## adding timings
        cat("base::assign(\".ExTimings\", \"", pkg,
            "-Ex.timings\", pos = 'CheckExEnv')\n", sep="", file = out)
        cat("base::cat(\"name\\tuser\\tsystem\\telapsed\\n\", file=base::get(\".ExTimings\", pos = 'CheckExEnv'))\n", file = out)
        ## a package left OutDec = "," at the end of an example
        cat("base::assign(\".format_ptime\",",
            "function(x) {",
            "  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]",
            "  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]",
            "  options(OutDec = '.')",
            "  format(x[1L:3L], digits = 7L)",
            "},",
            "pos = 'CheckExEnv')\n", sep = "\n", file = out)
        cat("### * </HEADER>\n", file = out)
    }

    if(pkg == "tcltk") {
        if(capabilities("tcltk")) cat("require('tcltk')\n\n", file = out)
        else cat("q()\n\n", file = out)
    } else if(pkg != "base")
        cat("library('", pkg, "')\n\n", sep = "", file = out)

    cat("base::assign(\".oldSearch\", base::search(), pos = 'CheckExEnv')\n", file = out)
    ## cat("assign(\".oldNS\", loadedNamespaces(), pos = 'CheckExEnv')\n", file = out)
    cat("base::assign(\".old_wd\", base::getwd(), pos = 'CheckExEnv')\n",
        file = out)
    for(file in files) {
        nm <- sub("\\.R$", "", basename(file))
        ## make a syntactic name out of the filename
        nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE, useBytes = TRUE)
        if (pkg == "grDevices" && nm == "postscript") next
        ## Latin-1 examples are treated separately
        if (pkg == "graphics" && nm == "text") next
        if(!file.exists(file))
            stop("file ", file, " cannot be opened", domain = NA)
        lines <- readLines(file)
        have_examples <- any(grepl("_ Examples _|### \\*+ Examples",
                                   lines, perl = TRUE, useBytes = TRUE))
        ## skip comment lines
        com <- grep("^#", lines, perl = TRUE, useBytes = TRUE)
        lines1 <- if(length(com)) lines[-com] else lines
        have_par <- any(grepl("[^a-zA-Z0-9.]par\\(|^par\\(",
                                lines1, perl = TRUE, useBytes = TRUE))
        have_contrasts <- any(grepl("options\\(contrasts",
                                   lines1, perl = TRUE, useBytes = TRUE))

        if(have_examples)
            cat("cleanEx()\nnameEx(\"", nm, "\")\n", sep = "", file = out)

        cat("### * ", nm, "\n\n", sep = "", file = out)
        cat("flush(stderr()); flush(stdout())\n\n", file = out)
        if(addTiming)
            cat("base::assign(\".ptime\", proc.time(), pos = \"CheckExEnv\")\n",
                file = out)
        if (commentDonttest) {
            dont_test <- FALSE
            for (line in lines) {
                if(any(grepl("^[[:space:]]*## No test:", line,
                             perl = TRUE, useBytes = TRUE))) {
                    dont_test <- TRUE
                    count <- count + 1L
                }
                if(!dont_test) cat(line, "\n", sep = "", file = out)
                if(any(grepl("^[[:space:]]*## End\\(No test\\)", line,
                             perl = TRUE, useBytes = TRUE)))
                    dont_test <- FALSE
            }
        } else
            for (line in lines) cat(line, "\n", sep = "", file = out)

        if(addTiming) {
            cat("base::assign(\".dptime\", (proc.time() - get(\".ptime\", pos = \"CheckExEnv\")), pos = \"CheckExEnv\")\n", file = out)
            cat("base::cat(\"", nm, "\", base::get(\".format_ptime\", pos = 'CheckExEnv')(get(\".dptime\", pos = \"CheckExEnv\")), \"\\n\", file=base::get(\".ExTimings\", pos = 'CheckExEnv'), append=TRUE, sep=\"\\t\")\n", sep = "", file = out)
        }
        if(have_par)
            cat("graphics::par(get(\"par.postscript\", pos = 'CheckExEnv'))\n", file = out)
        if(have_contrasts)
            cat("base::options(contrasts = c(unordered = \"contr.treatment\",",
                "ordered = \"contr.poly\"))\n", sep="", file = out)
    }

    cat(readLines(file.path(R.home("share"), "R", "examples-footer.R")),
        sep = "\n", file = out)

    if(count && !is.null(cntFile)) writeLines(as.character(count), cntFile)
}

## compares 2 files
## 2022-07: it is reasonable to assume that almost all users will
## have diff (it is part of Rtools), and currently only GNU diff
## (from 2022 on macOS) and FreeBSD versions semm to be in use.
## So the support without diff is minimal.
Rdiff <- function(from, to, useDiff = FALSE, forEx = FALSE,
                  nullPointers = TRUE, Log = FALSE)
{
    clean <- function(txt)
    {
        if(!length(txt)) return(txt)
        ## remove R header
        if(length(top <- grep("^(R version|R : Copyright|R Under development)",
                              txt, perl = TRUE, useBytes = TRUE)) &&
           length(bot <- grep("quit R.$", txt, perl = TRUE, useBytes = TRUE)))
            txt <- txt[-(top[1L]:bot[1L])]
        ## for massageExamples(), used for timings
        ll <- grep("</HEADER>", txt, fixed = TRUE, useBytes = TRUE)
        if(length(ll)) txt <- txt[-seq_len(max(ll))]
        ll <- grep("<FOOTER>", txt, fixed = TRUE, useBytes = TRUE)
        if(length(ll)) txt <- txt[seq_len(max(ll) - 1L)]
        ## remove header change in R 3.5.0
        if(forEx) {
            ll <- grep('".old_wd"', txt, fixed = TRUE, useBytes = TRUE)
            if(length(ll)) txt <- txt[-ll]
        }
        ## remove BATCH footer
        nl <- length(txt)
        if(nl > 3L && startsWith(txt[nl-2L], "> proc.time()"))
            txt <- txt[1:(nl-3L)]
        ## remove text between IGNORE_RDIFF markers.
        ## documented in ?Rdiff, i.e., not only  if(forEx)
        txt <- txt[(cumsum(txt == "> ## IGNORE_RDIFF_BEGIN") <=
                    cumsum(txt == "> ## IGNORE_RDIFF_END"))]
        ## (Keeps the end markers, but that's ok.)
        if (nullPointers) {
            ## remove pointer addresses from listings
            ## useBytes=TRUE as some tests intentionally use invalid strings
            txt <- gsub("<(environment|bytecode|pointer|promise): [x[:xdigit:]]+>", "<\\1: 0>", txt,
                        useBytes = TRUE)
            ## standardize hashtable, pro tem
            ## useBytes=TRUE as some tests intentionally use invalid strings
            txt <- sub("<hashtable.*>", "<hashtable output>", txt,
                       useBytes = TRUE)
        }
        ## regularize fancy quotes.  First UTF-8 ones:
        txt <- .canonicalize_quotes(txt)
        if(.Platform$OS.type == "windows") {
            ## not entirely safe ...
            txt <- gsub(paste0("(",rawToChar(as.raw(0x91)),"|",rawToChar(as.raw(0x92)),")"),
                        "'", txt, perl = TRUE, useBytes = TRUE)
            txt <- gsub(paste0("(",rawToChar(as.raw(0x93)),"|",rawToChar(as.raw(0x94)),")"),
                        '"', txt, perl = TRUE, useBytes = TRUE)
        }
        ## massageExamples() adds options(pager = "console") only for
        ## Windows, but we should ignore a corresponding diff on all
        ## platforms.
        txt <- txt[!grepl('options(pager = "console")', txt,
                          fixed = TRUE, useBytes = TRUE)]
        pat <- '(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded|^<(environment|promise|pointer|bytecode):|^End.Don\'t show)'
        txt[!grepl(pat, txt, perl = TRUE, useBytes = TRUE)]
    }
    clean2 <- function(txt)
    {
        ## useBytes=TRUE as some tests intentionally use invalid strings
        eoh <- grep("^> options\\(warn = 1\\)$", txt, useBytes = TRUE)
        if(length(eoh)) txt[-(1L:eoh[1L])] else txt
    }
    trimPDF <- function(txt)
    {
        ## drop the PDF header
        if (length(txt) < 2L || !startsWith(txt[1L], "%PDF"))
            stop("not a PDF file")
        ## drop second line if comment, often non-ASCII
        txt <- if(startsWith(txt[1L], "%")) txt[-(1:2)] else txt[-1L]
        ## Remove variable parts of the header
        pat <- '(^/CreationDate |^/ModDate |^/Producer)'
        txt[!grepl(pat, txt, perl = TRUE, useBytes = TRUE)]
    }

    useDiff0 <- useDiff

    left <- readLines(from)
    right <- readLines(to)
    asPDF <- length(left) >= 1L && startsWith(left[1L], "%PDF")
    if (useDiff && !nzchar(Sys.which("diff"))) {
        if(!asPDF)
            warning("'diff' is not available so useDiff = FALSE will be used")
        useDiff <- FALSE
    }

    if(asPDF) {
        if(!useDiff) {
            out <- if(!useDiff0) "comparing PDF files requires useDiff = TRUE"
                   else "comparing PDF files requires 'diff'"
            if (Log) return(list(status = 0L, out = out))
            else {message(out); return(invisible(0L))}
        }
        left <- trimPDF(left); right <- trimPDF(right)
    } else {
        left <- clean(left); right <- clean(right)
        if (forEx) {
            left <- clean2(left)
            ## remove lines from R CMD check --timings
            left <- filtergrep("[.](format_|)ptime", left, useBytes = TRUE)
            right <- clean2(right)
        }
    }

    if (!useDiff) {
        if(length(left) == length(right)) {
            ## The idea is to emulate diff -b, as documented by POSIX:
            ## https://pubs.opengroup.org/onlinepubs/9699919799/utilities/diff.html

            ## useBytes=TRUE as some tests intentionally use invalid strings
            bleft  <- gsub("[[:space:]]*$", "", left, useBytes=TRUE)
            bright <- gsub("[[:space:]]*$", "", right, useBytes=TRUE)
            bleft  <- gsub("[[:space:]]+", " ", bleft, useBytes=TRUE)
            bright <- gsub("[[:space:]]+", " ", bright, useBytes=TRUE)
            if(all(bleft == bright))
                return(if(Log) list(status = 0L, out = character()) else 0L)
            cat("\n")
            diff <- bleft != bright
            ## FIXME do run lengths here
            for(i in which(diff))
                cat(i,"c", i, "\n< ", left[i], "\n", "---\n> ", right[i], "\n",
                    sep = "")
            if (Log) {
                i <- which(diff)
                out <- paste0(i,"c", i, "\n< ", left[i], "\n", "---\n> ", right[i])
                list(status = 1L, out = out)
            } else invisible(1L)
        } else { ## no diff, different lengths
            out <- "files differ in number of lines"
            if (Log) list(status = 2L, out = out)
            else {message(out); invisible(2L)}
        }
    } else {
        out <- character()
        a <- tempfile("Rdiffa")
        writeLines(left, a)
        b <- tempfile("Rdiffb")
        writeLines(right, b)
        if (Log) {
            tf <- tempfile()
            status <- system2("diff", c("-bw", shQuote(a), shQuote(b)),
                              stdout = tf, stderr = tf)
            list(status = status, out = c(out, readLines(tf)))
        } else system(paste("diff -bw", shQuote(a), shQuote(b)))
    }
} ## {Rdiff}

.is.writeable <- function(dir)
{
    # see packages2.R for comment on unreliability of file.access
    ok <- TRUE
    fn <- file.path(dir, paste0("_test_dir_", Sys.getpid()))
    res <- try(dir.create(fn, showWarnings = FALSE))
    if(inherits(res, "try-error") || !res)
        ok <- FALSE
    else
        unlink(fn, recursive = TRUE)
    if (ok) {
        fn <- file.path(dir, paste0("_test_file_", Sys.getpid()))
        res <- try(file.create(fn, showWarnings = FALSE))
        if(inherits(res, "try-error") || !res)
            ok <- FALSE
        else
            unlink(fn)
    }
    ok
}

testInstalledPackages <-
    function(outDir = ".", errorsAreFatal = TRUE,
             scope = c("both", "base", "recommended"),
             types = c("examples", "tests", "vignettes"),
             srcdir = NULL, Ropts = "", ...)
{
    if (!.is.writeable(outDir))
        stop("directory ", sQuote(outDir), " is not writeable ", domain = NA)
    ow <- options(warn = 1)
    on.exit(ow)
    scope <- match.arg(scope)
    status <- 0L
    pkgs <- character()
    known_packages <- .get_standard_package_names()
    if (scope %in% c("both", "base"))
        pkgs <- known_packages$base
    if (scope %in% c("both", "recommended"))
        pkgs <- c(pkgs, known_packages$recommended)
    mc.cores <- as.integer(Sys.getenv("TEST_MC_CORES", "1"))
    if (.Platform$OS.type != "windows" &&
        !is.na(mc.cores) && mc.cores > 1L) {
        do_one <- function(pkg) {
            if(is.null(srcdir) && pkg %in% known_packages$base)
                srcdir <- R.home("tests/Examples")
            testInstalledPackage(pkg, .Library, outDir, types, srcdir, Ropts, ...)
        }
        res <- parallel::mclapply(pkgs, do_one, mc.cores = mc.cores,
                                  mc.preschedule = FALSE)
        res <- unlist(res) != 0L
        if (any(res)) {
            for(i in which(res))
                warning(gettextf("testing '%s' failed", pkgs[i]),
                        domain = NA, call. = FALSE, immediate. = TRUE)
            if (errorsAreFatal)
                stop(sprintf(ngettext(sum(res), "%d of the package tests failed",
                                      "%d of the package tests failed",
                                       domain = "R-tools"), sum(res)),
                     domain = NA, call. = FALSE)
        }
    } else {
        for (pkg in pkgs) {
            if(is.null(srcdir) && pkg %in% known_packages$base)
                srcdir <- R.home("tests/Examples")
            res <- testInstalledPackage(pkg, .Library, outDir, types, srcdir, Ropts, ...)
            if (res) {
                status <- 1L
                msg <- gettextf("testing '%s' failed", pkg)
                if (errorsAreFatal) stop(msg, domain = NA, call. = FALSE)
                else warning(msg, domain = NA, call. = FALSE, immediate. = TRUE)
            }
        }
    }
    invisible(status)
}

testInstalledPackage <-
    function(pkg, lib.loc = NULL, outDir = ".",
             types = c("examples", "tests", "vignettes"),
             srcdir = NULL, Ropts = "", ...)
{
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkgdir <- find.package(pkg, lib.loc)
    owd <- setwd(outDir)
    on.exit(setwd(owd))
    strict <- as.logical(Sys.getenv("R_STRICT_PACKAGE_CHECK", "FALSE"))
    useDiff <- nzchar(Sys.which("diff"))

    if ("examples" %in% types) {
        message(gettextf("Testing examples for package %s", sQuote(pkg)),
                domain = NA)
        Rfile <- .createExdotR(pkg, pkgdir, silent = TRUE, ...)
        if (length(Rfile)) {
            outfile <- paste0(pkg, "-Ex.Rout")
            failfile <- paste0(outfile, ".fail")
            savefile <- paste0(outfile, ".prev")
            if (file.exists(outfile)) file.rename(outfile, savefile)
            unlink(failfile)
            ## Create as .fail in case this R session gets killed
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --vanilla --no-timing", Ropts,
                         shQuote(Rfile), shQuote(failfile))
            if (.Platform$OS.type == "windows") {
                Sys.setenv(R_LIBS="")
                cmd <- paste(cmd, "LANGUAGE=C")
            } else
                cmd <- paste("R_LIBS= LANGUAGE=C", cmd)
            res <- system(cmd)
            if (res) {
                message(gettextf("Error: running examples in %s failed", sQuote(Rfile)),
                        domain = NA)
                return(invisible(1L))
            } else
                file.rename(failfile, outfile)

            savefile <- paste0(outfile, ".save")
            if (!is.null(srcdir)) savefile <- file.path(srcdir, savefile)
            else {
                tfile <- file.path(pkgdir, "tests", "Examples" , savefile)
                if(!file.exists(savefile) && file.exists(tfile))
                    savefile <- tfile
            }
            if (file.exists(savefile)) {
               if (file.exists(savefile)) {
                   message(gettextf("  comparing %s to %s ...",
                                    sQuote(outfile), sQuote(basename(savefile))),
                           appendLF = FALSE, domain = NA)
                   cmd <-
                       sprintf("invisible(tools::Rdiff('%s','%s',%s,TRUE))",
                               outfile, savefile, as.character(useDiff))
                   out <- R_runR(cmd, "--vanilla --no-echo")
                   if(length(out)) {
                       if(strict)
                           message(" ERROR")
                       else
                           message(" NOTE")
                       writeLines(paste0("  ", out))
                       if(strict)
                           stop("  ",
                                "results differ from reference results")
                   } else {
                       message(" OK")
                   }
                }
            } else {
                prevfile <- paste0(outfile, ".prev")
                if (file.exists(prevfile)) {
                    message(gettextf("  comparing %s to %s ...",
                            sQuote(outfile), sQuote(basename(prevfile))),
                            appendLF = FALSE, domain = NA)
                    cmd <-
                        sprintf("invisible(tools::Rdiff('%s','%s',%s,TRUE))",
                                outfile, prevfile, as.character(useDiff))
                    out <- R_runR(cmd, "--vanilla --no-echo")
                    if(length(out)) {
                        message(" NOTE")
                        writeLines(paste0("  ", out))
                    } else {
                        message(" OK")
                    }
                }
            }
        } else
            warning(gettextf("no examples found for package %s", sQuote(pkg)),
                    call. = FALSE, domain = NA)
    }

    ## FIXME merge with code in .runPackageTests
    if ("tests" %in% types && dir.exists(d <- file.path(pkgdir, "tests"))) {
        this <- paste0(pkg, "-tests")
        unlink(this, recursive = TRUE)
        dir.create(this)
        ## system(paste("cp -pR", file.path(d, "*"), this))
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message(gettextf("Running specific tests for package %s",
                         sQuote(pkg)), domain = NA)
        Rfiles <- dir(".", pattern="\\.[rR]$")
        for(f in Rfiles) {
            message(gettextf("  Running %s", sQuote(f)), domain = NA)
            outfile <- sub("rout$", "Rout", paste0(f, "out"))
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --vanilla --no-timing", Ropts,
                         shQuote(f), shQuote(outfile))
            cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C")
            else paste("LANGUAGE=C", cmd)
           res <- system(cmd)
            if (res) {
                file.rename(outfile, paste0(outfile, ".fail"))
                message(gettextf("Error: running the tests in %s failed", sQuote(f)),
                        domain = NA)
                return(invisible(1L))
            }
            savefile <- paste0(outfile, ".save")
            if (file.exists(savefile)) {
                message(gettextf("  comparing %s to %s ...",
                                 sQuote(outfile), sQuote(savefile)),
                        appendLF = FALSE, domain = NA)
                res <- Rdiff(outfile, savefile, useDiff)
                if (!res) message(" OK")
            }
        }
        setwd(owd)
    }

    if ("vignettes" %in% types && dir.exists(file.path(pkgdir, "doc"))) {
        message(gettextf("Running vignettes for package %s", sQuote(pkg)),
                domain = NA)
        out <- format(checkVignettes(pkg, lib.loc = lib.loc,
                                     latex = FALSE, weave = TRUE))
        if (length(out)) {
            writeLines(out)
            return(invisible(1L))
        }
    }

    invisible(0L)
}

## run all the tests in a directory: for use by R CMD check.
## trackObjs has .Rin files

## used by R CMD check
.runPackageTestsR <- function(...)
{
    cat("\n");
    status <- .runPackageTests(...)
    q("no", status = status)
}

.runPackageTests <-
    function(use_gct = FALSE, use_valgrind = FALSE, Log = NULL,
             stop_on_error = TRUE, ...)
{
    tlim <- Sys.getenv("_R_CHECK_ONE_TEST_ELAPSED_TIMEOUT_",
            Sys.getenv("_R_CHECK_TESTS_ELAPSED_TIMEOUT_",
            Sys.getenv("_R_CHECK_ELAPSED_TIMEOUT_")))
    tlim <- get_timeout(tlim)
    if (!is.null(Log)) Log <- file(Log, "wt")
    WINDOWS <- .Platform$OS.type == "windows"
    td0 <- as.numeric(Sys.getenv("_R_CHECK_TIMINGS_"))
    theta <-
        as.numeric(Sys.getenv("_R_CHECK_TEST_TIMING_CPU_TO_ELAPSED_THRESHOLD_",
                              NA_character_))
    if (is.na(td0)) td0 <- Inf
    print_time <- function(t1, t2, Log)
    {
        td <- t2 - t1
        if(td[3L] < td0) td2 <- ""
        else {
            td2 <- if (td[3L] > 600) {
                td <- td/60
                if(WINDOWS) sprintf(" [%dm]", round(td[3L]))
                else sprintf(" [%dm/%dm]", round(sum(td[-3L])), round(td[3L]))
            } else {
                if(WINDOWS) sprintf(" [%ds]", round(td[3L]))
                else sprintf(" [%ds/%ds]", round(sum(td[-3L])), round(td[3L]))
            }
        }
        message(td2, domain = NA)
        if (!is.null(Log)) cat(td2, "\n", sep = "",  file = Log)
    }
    runone <- function(f)
    {
        message(gettextf("  Running %s", sQuote(f)),
                appendLF = FALSE, domain = NA)
        if(!is.null(Log))
            cat("  Running ", sQuote(f), sep = "", file = Log)
        outfile <- sub("rout$", "Rout", paste0(f, "out"))
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --vanilla",
                     if(use_valgrind) "--debugger=valgrind",
                     shQuote(f), shQuote(outfile))
        if (WINDOWS) {
            Sys.setenv(LANGUAGE="C")
            Sys.setenv(R_TESTS="startup.Rs")
        } else
            cmd <- paste("LANGUAGE=C", "R_TESTS=startup.Rs", cmd)
        t1 <- proc.time()
        res <- system(cmd, timeout = tlim)
        t2 <- proc.time()
        print_time(t1, t2, Log)
        if (!WINDOWS && !is.na(theta)) {
            td <- t2 - t1
            cpu <- sum(td[-3L])
            if(cpu >= pmax(theta * td[3L], 1)) {
                ratio <- round(cpu/td[3L], 1L)
                msg <- sprintf("Running R code in %s had CPU time %g times elapsed time\n",
                               sQuote(f), ratio)
                cat(msg)
                if (!is.null(Log)) cat(msg, file = Log)
            }
        }
        if (res) {
            if(identical(res, 124L)) report_timeout(tlim)
            file.rename(outfile, paste0(outfile, ".fail"))
            return(1L)
        }
        savefile <- paste0(outfile, ".save")
        if (file.exists(savefile)) {
            message(gettextf("  Comparing %s to %s ...",
                             sQuote(outfile), sQuote(savefile)),
                    appendLF = FALSE, domain = NA)
            if(!is.null(Log))
                cat("  Comparing ", sQuote(outfile), " to ",
                    sQuote(savefile), " ...", sep = "", file = Log)
            if(!is.null(Log)) {
                ans <- Rdiff(outfile, savefile, TRUE, Log = TRUE)
                writeLines(ans$out)
                writeLines(ans$out, Log)
                res <- ans$status
            } else res <- Rdiff(outfile, savefile, TRUE)
            if (!res) {
                message(" OK")
                if(!is.null(Log)) cat(" OK\n", file = Log)
            }
        }
        0L
    }

    file.copy(file.path(R.home("share"), "R", "tests-startup.R"), "startup.Rs")
    if (use_gct) cat("gctorture(TRUE)" , file = "startup.Rs", append = TRUE)
    nfail <- 0L ## allow for later running all tests even if some fail.
    Rinfiles <- dir(".", pattern="\\.Rin$")
    for(f in Rinfiles) {
        message("  Processing ", sQuote(f), domain = NA)
        if (!is.null(Log))
            cat("  Processing ", sQuote(f), "\n", sep = "", file = Log)
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --no-timing --vanilla --no-echo", shQuote(f))
        if (system(cmd)) {
            nfail <- nfail + 1L
            file.rename(paste0(f, ".Rout"), paste0(f, ".Rout.fail"))
            if (stop_on_error) return(1L)
        }
    }

    Rfiles <- dir(".", pattern="\\.[rR]$")
    for(f in Rfiles) {
        nfail <- nfail + runone(f)
        if (nfail > 0 && stop_on_error) return(nfail)
    }
    if (!is.null(Log)) close(Log)
    return(nfail)
}

## Defaults for commenting are the same as per-3.2.0 version.
.createExdotR <-
    function(pkg, pkgdir, silent = FALSE, use_gct = FALSE, addTiming = FALSE,
             ..., commentDontrun = TRUE, commentDonttest = TRUE,
             installed = TRUE)
{
    Rfile <- paste0(pkg, "-Ex.R")

    db <- if(installed)
              Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
          else
              Rd_db(dir = pkgdir)
    if (!length(db)) {
        message("no parsed files found")
        return(invisible(NULL))
    }
    if (!silent) message("  Extracting from parsed Rd's ",
                         appendLF = FALSE, domain = NA)
    files <- names(db)
    if (pkg == "grDevices")
        files <- files[!grepl("^(unix|windows)/", files)]
    filedir <- tempfile()
    dir.create(filedir)
    on.exit(unlink(filedir, recursive = TRUE))
    cnt <- 0L
    for(f in files) {
        nm <- sub("\\.[Rr]d$", "", basename(f))
        Rd2ex(db[[f]],
              file.path(filedir, paste0(nm, ".R")),
              defines = NULL, commentDontrun = commentDontrun,
              commentDonttest = commentDonttest)
        cnt <- cnt + 1L
        if(!silent && cnt %% 10L == 0L)
            message(".", appendLF = FALSE, domain = NA)
    }
    if (!silent) message()
    nof <- length(Sys.glob(file.path(filedir, "*.R")))
    if(!nof) return(invisible(NULL))

    massageExamples(pkg, filedir, Rfile, use_gct, addTiming,
                    commentDonttest = commentDonttest, ...)
    invisible(Rfile)
}

## NB: Test even when tests were *not* installed, via appropriate  testSrcdir = <dir>
testInstalledBasic <- function(scope = c("basic", "devel", "both", "internet", "all"),
                               outDir = file.path(R.home(), "tests"),
                               testSrcdir = getTestSrcdir(outDir))
{
    scope <- match.arg(scope)

    ## We need to force C collation: might not work
    oLCcoll <- Sys.getlocale("LC_COLLATE") ; on.exit(Sys.setlocale("LC_COLLATE", oLCcoll))
    Sys.setlocale("LC_COLLATE", "C")
### ---- "basic" tests ("devel", etc -------> further down (!)
    ## "strict specific" (test-src-strict-1):
    tests1 <- c("eval-etc", "simple-true", "arith-true", "lm-tests",
                "ok-errors", "method-dispatch", "array-subset",
                "p-r-random-tests", "d-p-q-r-tst-2",
                "any-all", "structure", "d-p-q-r-tests")
    ## "sloppy specific":
    tests2 <- c("complex", "print-tests", "lapack", "datasets", "datetime",
                "iec60559")
    ## regression tests (strict specific, too)
    tests3 <- c("reg-tests-1a", "reg-tests-1b", "reg-tests-1c", "reg-tests-1d",
                "reg-tests-1e", "reg-tests-2",
                "reg-examples1", "reg-examples2", "reg-packages",
                "reg-S4-examples",
                "classes-methods",
                ## reg-translation, reg-ex*3 ... see "devel" below
                "datetime3",
                "p-qbeta-strict-tst",
                "reg-IO", "reg-IO2", "reg-plot", "reg-S4", "reg-BLAS")

    useDiff <- nzchar(Sys.which("diff"))  # only check once
    runone <- function(f, diffOK = FALSE, inC = TRUE)
    {
        f <- fR <- paste0(f, ".R")
        if(srcDiffers)
            f <- file.path(testSrcdir, fR)
        ## already needed for .Rin -> .R :
        if (.Platform$OS.type == "windows") {
            ## NB: Sys.setlocale("LC_ALL", "....") fails on (some) Windows
            ## --  specific LC_* seems to work, even via *.setenv():
            Sys.set2 <- function(...) { # version returning previous setting
                stopifnot(...length() == 1L, nzchar(nm <- ...names()))
                oVal <- Sys.getenv(nm)
                Sys.setenv(...)
                oVal
            }
            oRD <- Sys.set2(R_DEFAULT_PACKAGES="")
            oLa <- Sys.set2(LANGUAGE = "C")
            oLC <- Sys.set2(LC_COLLATE="C")
            oLT <- Sys.set2(LC_TIME  = "C") # e.g for month names in reg-tests-1c.R
            oSR <- Sys.set2(SRCDIR = SRCDIR)
            on.exit(Sys.setenv(LANGUAGE = oLa, R_DEFAULT_PACKAGES = oRD,
                               LC_COLLATE = oLC, LC_TIME = oLT, SRCDIR = oSR))
            if (inC) { # breaks reg-plot-latin1, so restore between tests
                oenv <- Sys.getenv("LC_CTYPE", unset = NA)
                on.exit(if (is.na(oenv)) Sys.unsetenv("LC_CTYPE")
                        else Sys.setenv(LC_CTYPE=oenv), add = TRUE)
                Sys.setenv(LC_CTYPE="C")
            }
            ## ignore all 'extra' (incl. 'inC')  and hope
            mkCmd <- identity
        } else { ## non-Windows
            extra <- if(inC) paste(extra0,  "LC_ALL=C") else extra0
            mkCmd <- function(cmd) paste(extra, cmd)
        }
        if (!file.exists(f)) { # try *.Rin, creating *.R
            if (!file.exists(fin <- paste0(f, "in")))
                stop("file ", sQuote(f), " not found", domain = NA)
            f <- fR # in outDir (= our working dir) !
            message("creating ", sQuote(f), domain = NA)
            cmd <- mkCmd(paste(shQuote(file.path(R.home("bin"), "R")),
			       "--vanilla --no-echo -f", fin))
            if (system(cmd))
                stop("creation of ", sQuote(f), " failed", domain = NA)
            ## This needs an extra trailing space to match the .Rin.R rule
            cat("\n", file = f, append = TRUE)
            on.exit(unlink(f), add = TRUE)
        }
        message("  running code in ", sQuote(f), domain = NA)
        outfile <- sub("rout$", "Rout", paste0(fR, "out"))
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --vanilla --no-timing",
                     shQuote(f), shQuote(outfile))
        res <- system(mkCmd(cmd))
        if (res) {
            file.rename(outfile, paste0(outfile, ".fail"))
            message("FAILED")
            return(1L)
        }
        savefile <- paste0(outfile, ".save")
        if(srcDiffers)
            savefile <- file.path(testSrcdir, savefile)
        if (file.exists(savefile)) {
            message(gettextf("  comparing %s to %s ...",
                             sQuote(outfile), sQuote(savefile)),
                    appendLF = FALSE, domain = NA)
            res <- Rdiff(outfile, savefile, useDiff)
            if (!res) message(" OK")
            else if (!diffOK) return(1L)
        }
        0L
    } # end{runone}

    owd <- setwd(outDir)
    on.exit(setwd(owd), add=TRUE)
    if (!.is.writeable("."))
        stop(gettextf("directory %s is not writeable ", sQuote(outDir)), domain = NA)
    ## to get the *default* testSrcdir = getTestSrcdir(outDir) :
    getTestSrcdir <- function(odir) {
        ## Know here to be inside 'odir' or 'outDir'.
        if(file.exists("eval-etc.R")) # all is fine
            return(odir)
        ## now, on unix-alike, if(build != src) the '<build>/tests/Makefile' has something like
        ##  srcdir = ../../R/tests
        if(file.exists("Makefile")) {
            lns <- readLines("Makefile", 12L) # currently it's the 5-th lines
            srcdir <- sub(" +$", "", # trailing blanks
                          sub("^srcdir *= *", "", grep("^srcdir", lns, value=TRUE)))
            if(dir.exists(srcdir))
                return(srcdir)
        }
        ## give up
        odir
    }
    comparePdf <- function(fnam) {
        ff <- paste0(fnam, ".pdf")
        fsv <- paste0(ff, ".save")
        if(srcDiffers) fsv <- file.path(testSrcdir, fsv)
        message("  comparing '",ff,"' to '",fsv,"' ...", appendLF = FALSE, domain = NA)
        res <- Rdiff(ff, fsv, TRUE)
        message(if(res != 0L) "DIFFERED" else "OK")
    }
    srcDiffers <- (normalizePath(testSrcdir) != normalizePath(outDir))
    SRCDIR <- if(srcDiffers) testSrcdir else "."
    extra0 <- paste("LANGUAGE=en", "LC_COLLATE=C", "LC_TIME=C",
                    "R_DEFAULT_PACKAGES=", paste0("SRCDIR=",SRCDIR))
    if (scope %in% c("basic", "both", "all")) {
        message("running strict specific tests", domain = NA)
        for (f in tests1) if (runone(f)) return(1L)
        message("running sloppy specific tests", domain = NA)
        for (f in tests2) runone(f, TRUE)
        message("running regression tests", domain = NA)
        for (f in tests3) {
            if (runone(f)) return(invisible(1L))
            if (f == "reg-plot") {
                comparePdf(f)
            }
        }
        runone("reg-encodings", inC=FALSE)
        runone("reg-translation", inC=FALSE)
        runone("reg-tests-3", TRUE)
        runone("reg-examples3", TRUE)
        message("running tests of plotting Latin-1", domain = NA)
        message("  expect failure or some differences if not in a Latin or UTF-8 locale", domain = NA)

        if (runone("reg-plot-latin1", TRUE, inC=FALSE) == 0L) {
            comparePdf("reg-plot-latin1")
        }
    }

    if (scope %in% c("devel", "both", "all")) {
        message("running tests of date-time printing\n expect platform-specific differences", domain = NA)
        ## "datetime" and "datetime3" are in "basic" above
        runone("datetime2")
        runone("datetime4")
        runone("datetime5")
        message("running tests of consistency of as/is.*", domain = NA)
        runone("isas-tests")
        message("running tests of random deviate generation (should no longer ever fail)")
        runone("p-r-random-tests", TRUE)
        message("running miscellaneous strict devel checks", domain = NA)
        if (runone("misc-devel")) return(invisible(1L))
        message("running tests demos from base and stats", domain = NA)
        if (runone("demos")) return(invisible(1L))
        if (runone("demos2")) return(invisible(1L))
        message("running tests of primitives", domain = NA)
        if (runone("primitives")) return(invisible(1L))
        message("running regexp regression tests", domain = NA)
        if (runone("utf8-regex", inC = FALSE)) return(invisible(1L))
        if (runone("PCRE")) return(invisible(1L))
        message("running tests on encodings & iconv() - first with C, then current locale", domain = NA)
        if (runone("iconv"             )) return(invisible(1L))
        if (runone("iconv", inC = FALSE)) return(invisible(1L))
        message("running tests of CRAN tools", domain = NA)
        if (runone("CRANtools")) return(invisible(1L))
        message("running tests to possibly trigger segfaults", domain = NA)
        if (runone("no-segfault")) return(invisible(1L))
    }
    if (scope %in% c("internet", "all")) {
        message("running tests of Internet functions", domain = NA)
        runone("internet")
        message("running more Internet and socket tests", domain = NA)
        runone("internet2")
        runone("libcurl")
    }
    invisible(0L)
}

detachPackages <- function(pkgs, verbose = TRUE)
{
    pkgs <- pkgs[pkgs %in% search()]
    if(!length(pkgs)) return()
    if(verbose){
        msg <- paste("detaching", paste(sQuote(pkgs), collapse = ", "))
        cat("", strwrap(msg, exdent = 2L), "", sep = "\n")
    }

    ## Normally 'pkgs' will be in reverse order of attachment (latest first)
    ## but not always (e.g. BioC package CMA attaches at the end).

    ## The items need not all be packages
    ## and non-packages can be on the list multiple times.
    isPkg <- startsWith(pkgs,"package:")
    for(item in pkgs[!isPkg]) {
        pos <- match(item, search())
        if(!is.na(pos)) .detach(pos)
    }

    pkgs <- pkgs[isPkg]
    if(!length(pkgs)) return()

    deps <- lapply(pkgs, function(x) if(exists(".Depends", x, inherits = FALSE)) get(".Depends", x) else character())
    names(deps) <- pkgs

    unload <- nzchar(Sys.getenv("_R_CHECK_UNLOAD_NAMESPACES_"))
    ## unloading 'grid' kills all devices
    ## tcltk is unhappy to have its DLL unloaded repeatedly
    exclusions <- c("grid", "tcltk")
    exclusions <- paste0("package:", exclusions)
    while(length(deps)) {
        unl <- unlist(deps)
        for(i in seq_along(deps)) {
            this <- names(deps)[i]
	    if(.rmpkg(this) %in% unl) next else break
        }
        ## hopefully force = TRUE is never needed, but it does ensure
        ## that progress gets made
        try(detach(this, character.only = TRUE,
                   unload = unload && (this %notin% exclusions),
                   force = TRUE))
        deps <- deps[-i]
    }
}

## Wrapper for  R CMD Rdiff   based on Rdiff() above :
.Rdiff <- function(no.q = FALSE)
{
    options(showErrorCalls=FALSE)

    Usage <- function() {
        cat("Usage: R CMD Rdiff [options] FROM-file TO-file EXITSTATUS",
            "",
            "Diff R output files FROM-FILE and TO-FILE discarding the R startup message,",
            "where FROM-FILE equal to '-' means stdin.",
            "",
            "Options:",
            "  -e, --forEx    uses 'forEx = TRUE' in Rdiff()",
            "  -h, --help     print this help message and exit",
            "  -v, --version  print version info and exit",
            "",
            "Report bugs at <https://bugs.R-project.org>.",
            sep = "\n")
    }

    do_exit <-
	if(no.q)
	    function(status = 0L) (if(status) stop else message)(
		".Rdiff() exit status ", status)
	else
	    function(status = 0L) q("no", status = status, runLast = FALSE)

    args <- commandArgs(TRUE)
    if (!length(args)) {
        Usage()
        do_exit(1L)
    }
    args <- paste(args, collapse=" ")
    args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    if (length(args) == 1L) {
        if(args[1L] %in% c("-h", "--help")) { Usage(); do_exit(0) }
        if(args[1L] %in% c("-v", "--version")) {
            cat("R output diff: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                .R_copyright_msg(2000),
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            do_exit(0)
        }
        Usage()
        do_exit(1L)
    }
    if (length(args) == 0L) {
        Usage()
        do_exit(1L)
    }
    ## options before file args {potentially allow multiple}:
    forEx <- any(is.ex <- args %in% c("-e", "--forEx"))
    if(forEx) args <- args[!is.ex]
    exitstatus <- as.integer(args[3L])
    if(is.na(exitstatus)) # default, also if length(args) == 2
        exitstatus <- 0L
    left <- args[1L]
    if(left == "-") left <- "stdin"
    status <- Rdiff(left, args[2L], useDiff = TRUE, forEx = forEx)
    if(status) status <- exitstatus
    do_exit(status)
} ## .Rdiff()

