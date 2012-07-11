#  File src/library/tools/R/translations.R
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

#### R based engine for managing translations

en_quote <- function(lines)
{
    ## msgfilter with quot.sed did
    ## s/"\([^"]*\)"/“\1”/g
    ## s/`\([^`']*\)'/‘\1’/g
    ## s/ '\([^`']*\)' / ‘\1’ /g
    ## s/ '\([^`']*\)'$/ ‘\1’/g
    ## s/^'\([^`']*\)' /‘\1’ /g
    ## s/“”/""/g
    good <- grepl("^msgstr", lines)
    tmp <- lines[good]
    ## This needs to be improved: strip msgstr: "",
    ## and deal with continuation lines.
#    tmp <- gsub('"([^"]*)"', '“\\1”', tmp)
#    tmp <- gsub("`([^`']*)'",'‘\\1', tmp)
#    tmp <- gsub(" '([^`']*)' ", ' ‘\\1’ ', tmp)
#    tmp <- gsub(" '([^`']*)'$", ' ‘\\1’', tmp)
#    tmp <- gsub("^'([^`']*)' ", '‘\\1’ ', tmp)
#    tmp <- gsub("“”", '""', tmp)
    tmp <- gsub("'([^`']*)'",'‘\\1’', tmp)
    lines[good] <- tmp
    lines
}

update_pkg_po <- function(pkgdir, pkg = NULL, version = NULL)
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- grep("^POT-Creation-Date: .*$", tmpa, invert = TRUE, value = TRUE)
        tmpb <- grep("^POT-Creation-Date: .*$", tmpb, invert = TRUE, value = TRUE)
        identical(tmpa, tmpb)
    }
    ## Follow previous version by always collating in C.
    pwd <- getwd()
    coll <- Sys.getlocale("LC_COLLATE")
    on.exit({Sys.setlocale("LC_COLLATE", coll); setwd(pwd)})
    Sys.setlocale("LC_COLLATE", "C")
    setwd(pkgdir)
    dir.create("po", FALSE)
    files <- dir("po")
    have_src <- paste0(pkg, ".pot") %in% files

    desc <- file.path(pkgdir, "DESCRIPTION")
    if(file.exists(desc)) {
        desc <- read.dcf(desc, fields = c("Package", "Version"))
        pkg <- desc[1L]
        version <- desc[2L]
    } else { # A base package
        pkg <- basename(pkgdir)
        version <- as.character(getRversion())
    }
    ## do R-pkg domain first
    ofile <- tempfile()
    xgettext2pot(".", ofile)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if(file.exists(potfile) && same(potfile, ofile)) {
    } else file.copy(ofile, potfile)
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "R-en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        cat("  ", lang, ":", sep = "")
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) stop("running msgmerge failed")
        res <- checkPoFile(f, TRUE)
        if(nrow(res)) print(res)
    }
    ## do en@quot
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L) stop("running msginit failed")
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) stop("running msgconv failed")
    lines <- readLines(tfile2)
    lines <- en_quote(lines)
    writeLines(lines, "po/R-en@quot.po", useBytes = TRUE)
    pofiles <- c(pofiles, "R-en@quot.po")

    for (f in pofiles) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE)
        dest <- file.path(dest, sprintf("R-%s.mo", lang))
        if (file_test("-ot", dest, f)) {
            lang <- sub("[.]po", "", basename(f))
            cat("  ", lang, ":", sep = "")
            cmd <- paste("msgfmt -c --statistics -o", pkg, f)
            if(system(cmd) != 0L)
                warning(sprintf("running msgfmt on %s failed", basename(f)),
                        domain = NA, immediate. = TRUE)
        }
    }


    if(!have_src) return()
    cfiles <- dir("src", pattern = "[.]c$", full.names = TRUE)
    potfile <- file.path("po", paste0(pkg, ".pot"))
    ofile <- tempfile()
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed")
    ## compare ofile and po/pkg.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile)
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        cat("  ", lang, ":", sep = "")
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) stop("running msgmerge failed")
        res <- checkPoFile(f, TRUE)
        if(nrow(res)) print(res)
    }
    ## do en@quot
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L) stop("running msginit failed")
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) stop("running msgconv failed")
    lines <- readLines(tfile2)
    lines <- sub("Project-Id-Version: PACKAGE VERSION",
                 ## FIXME: use package version
                 paste("Project-Id-Version: R", getRversion()),
                 lines, useBytes = TRUE)
    lines <- en_quote(lines)
    writeLines(lines, "po/en@quot.po", useBytes = TRUE)
    pofiles <- c(pofiles, "en@quot.po")

    for (f in pofiles) {
        lang <- sub("[.]po", "", basename(f))
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE)
        dest <- file.path(dest, sprintf("%s.mo", pkg))
        if (file_test("-ot", dest, f)) {
            cat("  ", lang, ":", sep = "")
            cmd <- paste("msgfmt -c --statistics -o", dest, f)
            if(system(cmd) != 0L)
                warning(sprintf("running msgfmt on %s failed", basename(f)),
                        domain = NA)
        }
    }

    invisible()
}
