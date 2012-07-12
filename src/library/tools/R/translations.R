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

## FIXME: this needs to set the copyright holder, the bugs address ....
update_pkg_po <- function(pkgdir, pkg = NULL, version = NULL)
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- grep('^"POT-Creation-Date:', tmpa, invert = TRUE, value = TRUE)
        tmpb <- grep('^"POT-Creation-Date:', tmpb, invert = TRUE, value = TRUE)
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

    desc <- "DESCRIPTION"
    if(file.exists(desc)) {
        desc <- read.dcf(desc, fields = c("Package", "Version"))
        pkg <- name <- desc[1L]
        version <- desc[2L]
    } else { # A base package
        pkg <- basename(pkgdir)
        name <- "R"
        version <- as.character(getRversion())
    }
    have_src <- paste0(pkg, ".pot") %in% files

    ## do R-pkg domain first
    ofile <- tempfile()
    xgettext2pot(".", ofile, name, version)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if(file.exists(potfile) && same(potfile, ofile)) {
    } else file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/R-en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        cat("  R-", lang, ":", sep = "")
        ## This seems not to update the file dates.
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge on ", sQuote(f), " failed")
            next
        }
        res <- checkPoFile(f, TRUE)
        if(nrow(res)) {
            print(res)
            next
        }
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        if(file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA, immediate. = TRUE)
    }

    ## do en@quot
    lang <- "en@quot"
    cat("  R-", lang, ":\n", sep = "")
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L) stop("running msginit failed")
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) stop("running msgconv failed")
    lines <- readLines(tfile2)
    lines <- en_quote(lines)
    writeLines(lines, "po/R-en@quot.po", useBytes = TRUE)
    dest <- file.path("inst", "po", lang, "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    dest <- file.path(dest, sprintf("R-%s.mo", pkg))
    cmd <- paste("msgfmt -c --statistics -o", dest, f)
    if(system(cmd) != 0L)
        warning(sprintf("running msgfmt on %s failed", basename(f)),
                domain = NA, immediate. = TRUE)

    if(!have_src) return(invisible())

    setwd("src")
    cfiles <- dir(".", pattern = "[.]c$")
    ofile <- tempfile()
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, paste("--package-name", name, sep = "="),
             paste("--package-version", version, sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed")
    setwd("..")

    ## compare ofile and po/pkg.pot, ignoring dates.
    potfile <- file.path("po", paste0(pkg, ".pot"))
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        cat("  ", lang, ":", sep = "")
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge on ",  f, " failed")
            next
        }
        res <- checkPoFile(f, TRUE)
        if(nrow(res)) {
            print(res)
            next
        }
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", pkg))
        if(file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }
    ## do en@quot
    lang <- "en@quot"
    cat("  ", lang, ":\n", sep = "")
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L) {
        warning("running msginit failed")
        next
    }
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) {
        warning("running msgconv failed")
        next
    }
    lines <- readLines(tfile2)
    lines <- en_quote(lines)
    writeLines(lines, "po/en@quot.po", useBytes = TRUE)
    dest <- file.path("inst", "po", lang, "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    dest <- file.path(dest, sprintf("%s.mo", pkg))
    cmd <- paste("msgfmt -c --statistics -o", dest, f)
    if(system(cmd) != 0L)
        warning(sprintf("running msgfmt on %s failed", basename(f)),
                domain = NA)

    invisible()
}

update_po <- function(srcdir)
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- grep('^"POT-Creation-Date:', tmpa, invert = TRUE, value = TRUE)
        tmpb <- grep('^"POT-Creation-Date:', tmpb, invert = TRUE, value = TRUE)
        identical(tmpa, tmpb)
    }
    ## Follow previous version by always collating in C.
    pwd <- getwd()
    coll <- Sys.getlocale("LC_COLLATE")
    on.exit({Sys.setlocale("LC_COLLATE", coll); setwd(pwd)})
    Sys.setlocale("LC_COLLATE", "C")
    setwd(srcdir)
    potfile <- "po/R.pot"
    ofile <- tempfile()
    cfiles <- readLines("po/POTFILES.in")
    cfiles <- grep("^#", cfiles, value = TRUE, invert = TRUE)
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste("--package-version", getRversion(), sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed")
    ## compare ofile and po/R.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R]*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        cat("  ", lang, ":", sep = "")
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge failed")
            next
        }
        res <- checkPoFile(f, FALSE)
        if(nrow(res)) {
            print(res)
            next
        }
        dest <- sprintf("po/%s.gmo", lang)
        if (file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

    ## do en@quot
    lang <- "en@quot"
    cat("  ", lang, ":\n", sep = "")
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L) {
        warning("running msginit failed")
        next
    }
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) {
        warning("running msgconv failed")
        next
    }
    lines <- readLines(tfile2)
    lines <- en_quote(lines)
    writeLines(lines, "po/en@quot.po", useBytes = TRUE)
    dest <- file.path("inst", "po", lang, "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    dest <- sprintf("po/%s.gmo", lang)
    cmd <- paste("msgfmt -c --statistics -o", dest, f)
    if(system(cmd) != 0L)
        warning(sprintf("running msgfmt on %s failed", basename(f)),
                domain = NA)

    invisible()
}

update_RGui_po <- function(srcdir)
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- grep('^"POT-Creation-Date:', tmpa, invert = TRUE, value = TRUE)
        tmpb <- grep('^"POT-Creation-Date:', tmpb, invert = TRUE, value = TRUE)
        identical(tmpa, tmpb)
    }
    ## Follow previous version by always collating in C.
    pwd <- getwd()
    coll <- Sys.getlocale("LC_COLLATE")
    on.exit({Sys.setlocale("LC_COLLATE", coll); setwd(pwd)})
    Sys.setlocale("LC_COLLATE", "C")
    setwd(srcdir)
    cfiles <- c(file.path("src/gnuwin32",
                          c("console.c", "dataentry.c",  "editor.c",  "extra.c",
                            "pager.c", "preferences.c", "rui.c", "system.c")),
                file.path("src/extra/graphapp",
                          c("clipboard.c", "dialogs.c", "gmenus.c",
                            "metafile.c", "printer.c")),
                "src/library/grDevices/src/devWindows.c")
    potfile <- "po/RGui.pot"
    ofile <- tempfile()
    ofile <- '/tmp/foo.pot'
    cmd <- sprintf("xgettext --keyword= --keyword=G_ --keyword=GN_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste("--package-version", getRversion(), sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed")
    ## compare ofile and po/RGui.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^RGui-.*[.]po$", full.names = TRUE)
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        cat("  ", lang, ":", sep = "")
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge failed")
            next
        }
        res <- checkPoFile(f, FALSE)
        if(nrow(res)) {
            print(res)
            next
        }
        dest <- sprintf("po/%s.gmo", lang)
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
   }

    invisible()
}
