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
Rdiff <- function(from, to)
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
        pat1 <- '(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded)'
        pat2 <- if (any(grepl("primitive-funs", from))) '^\\[1\\] [19][0-9][0-9])'
        else '^<(environment|promise|pointer):'
        txt <- txt[! (grepl(pat1, txt) | grepl(pat2, txt))]
        txt
    }

    left <- clean(readLines(from))
    right <- clean(readLines(to))
    if (length(left) == length(right)) {
        if(all(left == right)) return(0L)
        cat("\n")
        diff <- left != right
        ## FIXME do run lengths here
        for(i in which(diff)) {
            cat(i,"c", i, "\n< ", left[i], "\n", "---\n> ", right[i], "\n",
                sep = "")
        }
    } else {
        ## FIXME: use C code, or something like merge?
        cat("\nfiles differ in number of lines:\n")
        a <- tempfile()
        b <- tempfile()
        writeLines(left, a)
        writeLines(right, b)
        system(paste("diff", shQuote(a), shQuote(b)))
    }
    return(1L)
}

testInstalledPackages <- function(outDir = ".", errorsAreFatal = TRUE)
{
    status <- 0L
    base_pkgs <- rownames(installed.packages(.Library, priority = "base"))
    recommended_pkgs <-
        rownames(installed.packages(.Library, priority = "recommended"))
    for (pkg in c(base_pkgs, recommended_pkgs)) {
        res <- testInstalledPackage(pkg, .Library, outDir)
        if(res) {
            status <- 1L
            msg <- gettextf("testing '%s' failed", pkg)
            if (errorsAreFatal) stop(msg, domain=NA, call.=FALSE)
            else warning(msg, domain=NA, call.=FALSE, immediate.=TRUE)
        }
    }
    return(invisible(status))
}

testInstalledPackage <- function(pkg, lib.loc = NULL, outDir = ".")
{
    pkgdir <- .find.package(pkg, lib.loc)
    exdir <- file.path(pkgdir, "R-ex")
    owd <- setwd(outDir)
    on.exit(setwd(owd))
    if (file_test("-d", exdir)) {
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
        cmd <- paste(file.path(R.home(), "bin", "R"),
                     "CMD BATCH --vanilla", Rfile, outfile)
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

    if (file_test("-d", d <- file.path(pkgdir, "tests"))) {
        this <- paste(pkg, "tests", sep="-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        system(paste("cp -pr", file.path(d, "*"), this)) # FIXME: avoid
        setwd(this)
        message("Running specific tests for package ", sQuote(pkg))
        Rfiles <- dir(d, pattern="\\.R$")
        for(f in Rfiles) {
            message("  Running ", sQuote(f))
            outfile <- paste(f, "out", sep = "")
            cmd <- paste(file.path(R.home(), "bin", "R"),
                         "CMD BATCH --vanilla", file.path(d, f), outfile)
            if(.Platform$OS.type == "windows") Sys.setenv(LANGUAGE = "C")
            else cmd <- paste("R_LIBS=", cmd)
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
    invisible(0L)
}
