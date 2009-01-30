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

## functions principally for testing R

massageExamples <- function(pkg, files, outFile = stdout())
{
    if(utils::file_test("-d", files[1]))
        files <- sort(Sys.glob(file.path(files, "*.R")))

    if(is.character(outFile)) {
        out <- file(outFile, "wt")
        on.exit(close(out))
    } else out <- outFile

    lines <- readLines(file.path(R.home("share"), "R", "examples-header.R"))
    cat(sub("@PKG@", pkg, lines), sep = "\n", file = out)
    if(.Platform$OS.type == "windows")
        cat("options(pager = \"console\")\n", file = out)

    if(pkg == "tcltk") {
        if(capabilities("tcltk")) cat("require('tcltk')\n\n", file = out)
        else cat("q()\n\n", file = out)
    } else if(pkg != "base")
        cat("library('", pkg, "')\n\n", sep = "", file = out)

    cat("assign(\".oldSearch\", search(), pos = 'CheckExEnv')\n", file = out)
    cat("assign(\".oldNS\", loadedNamespaces(), pos = 'CheckExEnv')\n",
        file = out)

    for(file in files) {
        nm <- sub("\\.R$", "", basename(file))
        ## make a syntactic name out of the filename
        nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE)
        if (pkg == "graphics" && nm == "text") next
        if(!file.exists(file)) stop("file ", file, " cannot be opened")
        lines <- readLines(file)
        have_examples <- any(grepl("_ Examples _|### \\*+ Examples", lines))
        ## skip comment lines
        com <- grep("^#", lines)
        lines1 <- if(length(com)) lines[-com] else lines
        have_par <- length(grep("[^a-zA-Z0-9.]par\\(|^par\\(", lines1)) > 0L
        have_contrasts <- length(grep("options\\(contrasts", lines1)) > 0L

        if(have_examples)
            cat("cleanEx(); nameEx(\"", nm, "\")\n", sep = "", file = out)

        cat("### * ", nm, "\n\n", sep = "", file = out)
        cat("flush(stderr()); flush(stdout())\n\n", file = out)
        dont_test <- FALSE
        for (line in lines) {
            if(length(grep("^[[:space:]]*## No test:", line)))
                dont_test <- TRUE
            if(!dont_test) cat(line, "\n", sep = "", file = out)
            if(length(grep("^[[:space:]]*## End\\(No test\\)", line)))
                dont_test <- FALSE
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
Rdiff <- function(from, to, useDiff = FALSE)
{
    clean <- function(txt)
    {
        ## remove R header
        if(length(top <- grep("^(R version|R : Copyright)", txt)) &&
           length(bot <- grep("quit R.$", txt)))
            txt <- txt[-(top[1]:bot[1])]
        ## remove BATCH footer
        nl <- length(txt)
        if(grepl("^> proc.time()", txt[nl-2])) txt <- txt[1:(nl-3)]
        ## regularize fancy quotes.
        txt <- gsub("(\xe2\x80\x98|\xe2\x80\x99)", "'", txt,
                      perl = TRUE, useBytes = TRUE)
        if(.Platform$OS.type == "windows") # not entirely safe ...
            txt <- gsub("(\x93|\x94)", "'", txt, perl = TRUE, useBytes = TRUE)
        pat <- '(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded|^<(environment|promise|pointer): )'
        txt[!grepl(pat, txt)]
    }

    left <- clean(readLines(from))
    right <- clean(readLines(to))
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
    } else {
        ## FIXME: use C code, or something like merge?
        ## The files can be very big.
        if(!useDiff) cat("\nfiles differ in number of lines:\n")
        a <- tempfile()
        b <- tempfile()
        writeLines(left, a)
        writeLines(right, b)
        system(paste("diff -bw", shQuote(a), shQuote(b)))
    }
    return(1L)
}

testInstalledPackages <-
    function(outDir = ".", errorsAreFatal = TRUE,
             scope = c("both", "base", "recommended"),
             types = c("examples", "tests", "vignettes"))
{
    scope <- match.arg(scope)
    status <- 0L
    pkgs <- character()
    known_packages <- .get_standard_package_names()
    if(scope %in% c("both", "base"))
        pkgs <- known_packages$base
    if(scope %in% c("both", "recommended"))
        pkgs <- c(pkgs, known_packages$recommended)
    ## It *should* be an error if any of these are missing
    for (pkg in pkgs) {
        res <- testInstalledPackage(pkg, .Library, outDir, types)
        if(res) {
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
    if (1 %in% types && file_test("-d", exdir)) {
        Rfile <- paste(pkg, "-Ex.R", sep = "")
        ## might be zipped:
        if(file.exists(fzip <- file.path(exdir, "Rex.zip"))) {
            files <- tempfile()
            system(paste("unzip -q", fzip, "-d", files))
        } else files <- exdir
        massageExamples(pkg, files, Rfile)
        outfile <- paste(pkg, "-Ex.Rout", sep = "")
        savefile <- paste(outfile, "prev", sep = "." )
        if (file.exists(outfile)) file.rename(outfile, savefile)
        message("Running examples in package ", sQuote(pkg))
        cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                     "CMD BATCH --vanilla", shQuote(Rfile), shQuote(outfile))
        if(.Platform$OS.type == "windows") Sys.setenv(R_LIBS = "")
        else cmd <- paste("R_LIBS=", cmd)
        res <- system(cmd)
        if(res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            return(invisible(1L))
        }
        savefile <- paste(outfile, "prev", sep = "." )
        if (file.exists(savefile)) {
            message("  Comparing ", sQuote(outfile), " to ",
                    sQuote(basename(savefile)), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile)
            if(!res) message(" OK")
        }
    }

    if (2 %in% types && file_test("-d", d <- file.path(pkgdir, "tests"))) {
        this <- paste(pkg, "tests", sep="-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        ## system(paste("cp -pr", file.path(d, "*"), this))
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message("Running specific tests for package ", sQuote(pkg))
        Rfiles <- dir(d, pattern="\\.R$")
        for(f in Rfiles) {
            message("  Running ", sQuote(f))
            outfile <- paste(f, "out", sep = "")
            cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                         "CMD BATCH --vanilla",
                         shQuote(file.path(d, f)), shQuote(outfile))
            if(.Platform$OS.type == "windows") Sys.setenv(LANGUAGE = "C")
            else cmd <- paste("LANGUAGE=C", cmd)
           res <- system(cmd)
            if(res) {
                file.rename(outfile, paste(outfile, "fail", sep="."))
                return(invisible(1L))
            }
            savefile <- file.path(d, paste(outfile, "save", sep = "." ))
            if (file.exists(savefile)) {
                message("  Comparing ", sQuote(outfile), " to ",
                        sQuote(basename(savefile)), " ...", appendLF = FALSE)
                res <- Rdiff(outfile, savefile)
                if(!res) message(" OK")
            }
        }
    }

    if (3 %in% types && file_test("-d", d <- file.path(pkgdir, "doc"))) {
        message("Running vignettes for package ", sQuote(pkg))
        checkVignettes(pkg, lib.loc, latex = FALSE, weave =TRUE)
    }

    invisible(0L)
}

## run all the tests in a directory: for use by R CMD check.
## trackObjs has .Rin files

.runPackageTestsR <- function()
{
    cat("\n");
    status <- .runPackageTests()
    q("no", status = status)
}

.runPackageTests <- function(dir=".")
{
    runone <- function(f)
    {
        message("  Running ", sQuote(f))
        outfile <- paste(f, "out", sep = "")
        cmd <- paste(shQuote(file.path(R.home(), "bin", "R")),
                     "CMD BATCH --vanilla",
                     shQuote(file.path(dir, f)), shQuote(outfile))
        if(.Platform$OS.type == "windows")
            Sys.setenv(LANGUAGE = "C")
        else {
            startup <- file.path(R.home("share"), "R", "tests-startup.R")
            cmd <- paste("LANGUAGE=C ", "R_TESTS=", shQuote(startup), " ",
                         cmd, sep = "")
        }
        res <- system(cmd)
        if(res) {
            file.rename(outfile, paste(outfile, "fail", sep="."))
            return(1L)
        }
        savefile <- file.path(dir, paste(outfile, "save", sep = "." ))
        if (file.exists(savefile)) {
            message("  Comparing ", sQuote(outfile), " to ",
                    sQuote(basename(savefile)), " ...", appendLF = FALSE)
            res <- Rdiff(outfile, savefile, TRUE)
            if(!res) message(" OK")
        }
        0L
    }

    nfail <- 0L ## allow for later running all tests even if some fail.
    Rinfiles <- dir(dir, pattern="\\.Rin$")
    for(f in Rinfiles) {
        Rfile <- sub("\\.Rin$", ".R", f)
        message("  Creating ", sQuote(Rfile))
        cmd <- paste(shQuote(file.path(R.home(), "bin", "Rscript")),
                     "--vanilla", f)
        if(system(cmd))
            warning("creation of ", sQuote(Rfile), " failed")
        else if(file.exists(Rfile)) nfail <- nfail + runone(Rfile)
        if (nfail > 0) return(nfail)
    }

    Rfiles <- dir(dir, pattern="\\.R$")
    for(f in Rfiles) {
        nfail <- nfail + runone(f)
        if (nfail > 0) return(nfail)
    }
    return(nfail)
}
