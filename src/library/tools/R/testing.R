#  File src/library/tools/R/testing.R
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

## functions principally for testing R and packages

massageExamples <- function(pkg, files, outFile = stdout(), addTiming = FALSE)
{
    if(file_test("-d", files[1L]))
        files <- sort(Sys.glob(file.path(files, "*.R")))

    if(is.character(outFile)) {
        out <- file(outFile, "wt")
        on.exit(close(out))
    } else out <- outFile

#    lines <- readLines(file.path(R.home("share"), "R", "examples-header.R"))
#    cat(sub("@PKG@", pkg, lines), sep = "\n", file = out)
    lines <- c(paste('pkgname <- "', pkg, '"', sep =""),
               'source(file.path(R.home("share"), "R", "examples-header.R"))',
               "options(warn = 1)")
    cat(lines, sep = "\n", file = out)
    if(.Platform$OS.type == "windows")
        cat("options(pager = \"console\")\n", file = out)

    if(pkg == "tcltk") {
        if(capabilities("tcltk")) cat("require('tcltk')\n\n", file = out)
        else cat("q()\n\n", file = out)
    } else if(pkg != "base")
        cat("library('", pkg, "')\n\n", sep = "", file = out)

    cat("assign(\".oldSearch\", search(), pos = 'CheckExEnv')\n", file = out)
    ## cat("assign(\".oldNS\", loadedNamespaces(), pos = 'CheckExEnv')\n", file = out)
    if(addTiming) {
        ## adding timings
        cat("assign(\".ExTimings\", \"", pkg,
            "-Ex.timings\", pos = 'CheckExEnv')\n", sep="", file = out)
        cat("cat(\"name\\tuser\\tsystem\\telapsed\\n\", file=get(\".ExTimings\", pos = 'CheckExEnv'))\n", file = out)
        cat("assign(\".format_ptime\",",
            "function(x) {",
            "  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]",
            "  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]",
            "  format(x[1L:3L])",
            "},",
            "pos = 'CheckExEnv')\n", sep = "\n", file = out)
    }
    for(file in files) {
        nm <- sub("\\.R$", "", basename(file))
        ## make a syntactic name out of the filename
        nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE, useBytes = TRUE)
        if (pkg == "graphics" && nm == "text") next
        if(!file.exists(file)) stop("file ", file, " cannot be opened")
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
        dont_test <- FALSE
        if(addTiming)
            cat("assign(\".ptime\", proc.time(), pos = \"CheckExEnv\")\n",
                file = out)
        for (line in lines) {
            if(any(grepl("^[[:space:]]*## No test:", line, perl = TRUE, useBytes = TRUE)))
                dont_test <- TRUE
            if(!dont_test) cat(line, "\n", sep = "", file = out)
            if(any(grepl("^[[:space:]]*## End\\(No test\\)",
                         line, perl = TRUE, useBytes = TRUE)))
                dont_test <- FALSE
        }

        if(addTiming) {
            cat("\nassign(\".dptime\", (proc.time() - get(\".ptime\", pos = \"CheckExEnv\")), pos = \"CheckExEnv\")\n", file = out)
            cat("cat(\"", nm, "\", get(\".format_ptime\", pos = 'CheckExEnv')(get(\".dptime\", pos = \"CheckExEnv\")), \"\\n\", file=get(\".ExTimings\", pos = 'CheckExEnv'), append=TRUE, sep=\"\\t\")\n", sep = "", file = out)
        }
        if(have_par)
            cat("graphics::par(get(\"par.postscript\", pos = 'CheckExEnv'))\n", file = out)
        if(have_contrasts)
            cat("options(contrasts = c(unordered = \"contr.treatment\",",
                "ordered = \"contr.poly\"))\n", sep="", file = out)
    }

    cat(readLines(file.path(R.home("share"), "R", "examples-footer.R")),
        sep="\n", file = out)
}

## compares 2 files
Rdiff <- function(from, to, useDiff = FALSE, forEx = FALSE)
{
    clean <- function(txt)
    {
        ## remove R header
        if(length(top <- grep("^(R version|R : Copyright)", txt,
                              perl = TRUE, useBytes = TRUE)) &&
           length(bot <- grep("quit R.$", txt, perl = TRUE, useBytes = TRUE)))
            txt <- txt[-(top[1L]:bot[1L])]
        ## remove BATCH footer
        nl <- length(txt)
        if(grepl("^> proc.time()", txt[nl-2L])) txt <- txt[1:(nl-3L)]
        ## regularize fancy quotes.
        txt <- gsub("(\xe2\x80\x98|\xe2\x80\x99)", "'", txt,
                      perl = TRUE, useBytes = TRUE)
        if(.Platform$OS.type == "windows") # not entirely safe ...
            txt <- gsub("(\x93|\x94)", "'", txt, perl = TRUE, useBytes = TRUE)
        pat <- '(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded|^<(environment|promise|pointer): )'
        txt[!grepl(pat, txt, perl = TRUE, useBytes = TRUE)]
    }
    clean2 <- function(txt)
    {
        eoh <- grep("^> options\\(warn = 1\\)$", txt)
        if(length(eoh)) txt[-(1:eoh[1])] else txt
    }

    left <- clean(readLines(from))
    right <- clean(readLines(to))
    if (forEx) {
        left <- clean2(left)
        right <- clean2(right)
    }
    if (!useDiff && (length(left) == length(right))) {
        bleft <- gsub("[[:space:]]+", " ", left)
        bright <- gsub("[[:space:]]+", " ", right)
        if(all(bleft == bright)) return(0L)
        cat("\n")
        diff <- bleft != bright
        ## FIXME do run lengths here
        for(i in which(diff)) {
            cat(i,"c", i, "\n< ", left[i], "\n", "---\n> ", right[i], "\n",
                sep = "")
        }
        return(1L)
    } else {
        ## FIXME: use C code, or something like merge?
        ## The files can be very big.
        if(!useDiff) cat("\nfiles differ in number of lines:\n")
        a <- tempfile()
        b <- tempfile()
        writeLines(left, a)
        writeLines(right, b)
        return(system(paste("diff -bw", shQuote(a), shQuote(b))))
    }
}

testInstalledPackages <-
    function(outDir = ".", errorsAreFatal = TRUE,
             scope = c("both", "base", "recommended"),
             types = c("examples", "tests", "vignettes"))
{
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
    ## It *should* be an error if any of these are missing
    for (pkg in pkgs) {
        res <- testInstalledPackage(pkg, .Library, outDir, types)
        if (res) {
            status <- 1L
            msg <- gettextf("testing '%s' failed", pkg)
            if (errorsAreFatal) stop(msg, domain=NA, call.=FALSE)
            else warning(msg, domain=NA, call.=FALSE, immediate.=TRUE)
        }
    }
    return(invisible(status))
}

testInstalledPackage <-
    function(pkg, lib.loc = NULL, outDir = ".",
             types = c("examples", "tests", "vignettes"))
{
    types <- pmatch(types, c("examples", "tests", "vignettes"))
    pkgdir <- .find.package(pkg, lib.loc)
    exdir <- file.path(pkgdir, "R-ex")
    owd <- setwd(outDir)
    on.exit(setwd(owd))

    if (1 %in% types) { # && file_test("-d", exdir)) {
        message("\nCollecting examples for package ", sQuote(pkg))
        Rfile <- .createExdotR(pkg, pkgdir)
        if (length(Rfile)) {
            outfile <- paste(pkg, "-Ex.Rout", sep = "")
            failfile <- paste(outfile, "fail", sep = "." )
            savefile <- paste(outfile, "prev", sep = "." )
            if (file.exists(outfile)) file.rename(outfile, savefile)
            unlink(failfile)
            message("Running examples in package ", sQuote(pkg))
            ## Create as .fail in case this R session gets killed
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --vanilla --no-timing",
                         shQuote(Rfile), shQuote(failfile))
            if (.Platform$OS.type == "windows") Sys.setenv(R_LIBS="")
            else cmd <- paste("R_LIBS=", cmd)
            res <- system(cmd)
            if (res) return(invisible(1L)) else file.rename(failfile, outfile)

            savefile <- paste(outfile, "prev", sep = "." )
            if (file.exists(savefile)) {
                message("  Comparing ", sQuote(outfile), " to ",
                        sQuote(basename(savefile)), " ...", appendLF = FALSE)
                res <- Rdiff(outfile, savefile)
                if (!res) message(" OK")
            }
        } else warning("no examples found")
    }

    ## FIXME merge with code in .runPackageTests
    if (2 %in% types && file_test("-d", d <- file.path(pkgdir, "tests"))) {
        this <- paste(pkg, "tests", sep="-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        ## system(paste("cp -pr", file.path(d, "*"), this))
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message("Running specific tests for package ", sQuote(pkg))
        Rfiles <- dir(".", pattern="\\.R$")
        for(f in Rfiles) {
            message("  Running ", sQuote(f))
            outfile <- paste(f, "out", sep = "")
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --vanilla --no-timing",
                         shQuote(f), shQuote(outfile))
            cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C")
            else paste("LANGUAGE=C", cmd)
           res <- system(cmd)
            if (res) {
                file.rename(outfile, paste(outfile, "fail", sep="."))
                return(invisible(1L))
            }
            savefile <- paste(outfile, "save", sep = "." )
            if (file.exists(savefile)) {
                message("  Comparing ", sQuote(outfile), " to ",
                        sQuote(savefile), " ...", appendLF = FALSE)
                res <- Rdiff(outfile, savefile)
                if (!res) message(" OK")
            }
        }
        setwd(owd)
    }

    if (3 %in% types && file_test("-d", d <- file.path(pkgdir, "doc"))) {
        message("Running vignettes for package ", sQuote(pkg))
        checkVignettes(pkg, lib.loc, latex = FALSE, weave =TRUE)
    }

    invisible(0L)
}

## run all the tests in a directory: for use by R CMD check.
## trackObjs has .Rin files

.runPackageTestsR <- function(...)
{
    cat("\n");
    status <- .runPackageTests(...)
    q("no", status = status)
}

## used by R CMD check
.runPackageTests <- function(use_gct = FALSE, use_valgrind = FALSE)
{
    runone <- function(f)
    {
        message("  Running ", sQuote(f))
        outfile <- paste(f, "out", sep = "")
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --vanilla --no-timing",
                     if(use_valgrind) "--debugger=valgrind",
                     shQuote(f), shQuote(outfile))
        if (.Platform$OS.type == "windows") {
            Sys.setenv(LANGUAGE="C")
            Sys.setenv(R_TESTS="startup.Rs")
        } else
            cmd <- paste("LANGUAGE=C", "R_TESTS=startup.Rs", cmd)
        res <- system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            return(1L)
        }
        savefile <- paste(outfile, "save", sep = "." )
        if (file.exists(savefile)) {
            message("  Comparing ", sQuote(outfile), " to ",
                    sQuote(savefile), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile, TRUE)
            if (!res) message(" OK")
        }
        0L
    }

    file.copy(file.path(R.home("share"), "R", "tests-startup.R"), "startup.Rs")
    if (use_gct) cat("gctorture(TRUE)" , file = "startup.Rs", append = TRUE)
    nfail <- 0L ## allow for later running all tests even if some fail.
    Rinfiles <- dir(".", pattern="\\.Rin$")
    for(f in Rinfiles) {
        Rfile <- sub("\\.Rin$", ".R", f)
        message("  Creating ", sQuote(Rfile), domain = NA)
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --no-timing --vanilla --slave", f)
        if (system(cmd))
            warning("creation of ", sQuote(Rfile), " failed")
        else if (file.exists(Rfile)) nfail <- nfail + runone(Rfile)
        if (nfail > 0) return(nfail)
    }

    Rfiles <- dir(".", pattern="\\.R$")
    for(f in Rfiles) {
        nfail <- nfail + runone(f)
        if (nfail > 0) return(nfail)
    }
    return(nfail)
}

.createExdotR <- function(pkg, pkgdir, silent = FALSE, addTiming = FALSE)
{
    Rfile <- paste(pkg, "-Ex.R", sep = "")
    ## might be zipped:
    exdir <- file.path(pkgdir, "R-ex")
    if (file_test("-d", exdir)) {
        if (file.exists(fzip <- file.path(exdir, "Rex.zip"))) {
            filedir <- tempfile()
            unzip(fzip, exdir = filedir)
            on.exit(unlink(filedir, recursive = TRUE))
        } else filedir <- exdir
    } else {
        db <- Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
        if (!length(db)) {
            message("no parsed files found")
            return(invisible(NULL))
        }
        if (!silent) message("  Extracting from parsed Rd's ",
                             appendLF = FALSE, domain = NA)
        files <- names(db)
        filedir <- tempfile()
        dir.create(filedir)
        on.exit(unlink(filedir, recursive = TRUE))
        cnt <- 0L
        for(f in files) {
            ## names are 'fullpath.Rd' if from 'man' dir, 'topic' if from RdDB
            nm <- sub("\\.[Rr]d$", "", basename(f))
            Rd2ex(db[[f]],
                  file.path(filedir, paste(nm, "R", sep = ".")),
                  defines = NULL)
            cnt <- cnt + 1L
            if(!silent && cnt %% 10L == 0L)
                message(".", appendLF = FALSE, domain = NA)
        }
        if (!silent) message()
        nof <- length(Sys.glob(file.path(filedir, "*.R")))
        if(!nof) return(invisible(NULL))
    }
    massageExamples(pkg, filedir, Rfile, addTiming)
    invisible(Rfile)
}

testInstalledBasic <- function(scope = c("basic", "devel", "both"))
{
    scope <- match.arg(scope)

    ## We need to force C collation: might not work
    Sys.setlocale("LC_COLLATE", "C")
    tests1 <- c("eval-etc", "simple-true", "arith-true", "lm-tests",
                "ok-errors", "method-dispatch", "d-p-q-r-tests")
    tests2 <- c("complex", "print-tests", "lapack", "datasets")
    tests3 <- c("reg-tests-1", "reg-tests-2", "reg-IO", "reg-IO2", "reg-S4")

    runone <- function(f, diffOK = FALSE, inC = TRUE)
    {
        f <- paste(f, "R", sep = ".")
        if (!file.exists(f)) {
            if (!file.exists(fin <- paste(f, "in", sep = "")))
                stop("file ", sQuote(f), " not found", domain = NA)
            message("creating ", sQuote(f))
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --no-timing --vanilla --slave", fin)
            if (system(cmd))
                stop("creation of ", sQuote(f), " failed")
            on.exit(unlink(f))
        }
        message("  running code in ", sQuote(f))
        outfile <- paste(f, "out", sep = "")
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                     "CMD BATCH --vanilla --no-timing",
                     shQuote(f), shQuote(outfile))
        extra <- paste("LANGUAGE=C", "R_DEFAULT_PACKAGES=", "SRCDIR=.")
        if (inC) extra <- paste(extra,  "LC_ALL=C")
        if (.Platform$OS.type == "windows") {
            Sys.setenv(LANGUAGE="C")
            Sys.setenv(R_DEFAULT_PACKAGES="")
            Sys.setenv(SRCDIR=".")
            ## ignore inC and hope
        } else cmd <- paste(extra, cmd)
        res <- system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            message("FAILED")
            return(1L)
        }
        savefile <- paste(outfile, "save", sep = "." )
        if (file.exists(savefile)) {
            message("  comparing ", sQuote(outfile), " to ",
                    sQuote(savefile), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile, TRUE)
            if (!res) message(" OK")
            else if (!diffOK) return(1L)
        }
        0L
    }
    owd <- setwd(file.path(R.home(), "tests"))
    on.exit(setwd(owd))

    if (scope %in% c("basic", "both")) {
        message("running strict specific tests")
        for (f in tests1) if (runone(f)) return(1L)
        message("running sloppy specific tests")
        for (f in tests2) runone(f, TRUE)
        message("running regression tests")
        for (f in tests3) {
            if (runone(f)) return(invisible(1L))
            if (f == "reg-plot") {
                message("  comparing 'reg-plot.ps' to 'reg-plot.ps.save' ...",
                        appendLF = FALSE)
                system("diff reg-plot.ps reg-plot.ps.save")
                message("OK")
            }
        }
        runone("reg-tests-3", TRUE)
        message("running tests of plotting Latin-1")
        message("  expect failure or some differences if not in a Latin or UTF-8 locale")

        runone("reg-plot-latin1", TRUE, FALSE)
        message("  comparing 'reg-plot-latin1.ps' to 'reg-plot-latin1.ps.save' ...",
                appendLF = FALSE)
        system("diff reg-plot-latin1.ps reg-plot-latin1.ps.save")
        message("OK")
    }

    if (scope %in% c("devel", "both")) {
        message("running tests of consistency of as/is.*")
        runone("isas-tests")
        message("running tests of random deviate generation -- fails occasionally")
        runone("p-r-random-tests", TRUE)
        message("running tests of primitives")
        if (runone("primitives")) return(invisible(1L))
        message("running regexp regression tests")
        if (runone("utf8-regex", inC = FALSE)) return(invisible(1L))
        message("running tests to possibly trigger segfaults")
        if (runone("no-segfault")) return(invisible(1L))
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
    isPkg <- grepl("^package:", pkgs)
    for(item in pkgs[!isPkg]) {
        pos <- match(item, search())
        if(!is.na(pos)) .Internal(detach(pos))
    }

    pkgs <- pkgs[isPkg]
    if(!length(pkgs)) return()

    deps <- lapply(pkgs, function(x) if(exists(".Depends", x, inherits = FALSE)) get(".Depends", x) else character())
    names(deps) <- pkgs

    unload <- nzchar(Sys.getenv("_R_CHECK_UNLOAD_NAMESPACES_"))
    ## unloading 'grid' kills all devices
    ## tcltk is unhappy to have its DLL unloaded repeatedly
    exclusions <- c("grid", "tcltk")
    exclusions <- paste("package", exclusions, sep=":")
    while(length(deps)) {
        unl <- unlist(deps)
        for(i in seq_along(deps)) {
            this <- names(deps)[i]
            if(sub("^package:", "", this) %in% unl) next else break
        }
        ## hopefully force = TRUE is never needed, but it does ensure
        ## that progress gets made
        try(detach(this, character.only = TRUE,
                   unload = unload && !(this %in% exclusions),
                   force = TRUE))
        deps <- deps[-i]
    }
}
