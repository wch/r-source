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

## This only works in a UTF-8 locale: specifically substr needs to count
## UTF-8 chars
en_quote <- function(potfile, outfile)
{
    tfile <- tempfile()
    cmd <- paste("msginit -i", potfile, "--no-translator -l en -o", tfile)
    if(system(cmd, ignore.stderr = TRUE) != 0L)
        stop("running msginit failed", domain = NA)
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", tfile2, tfile)
    if(system(cmd) != 0L) stop("running msgconv failed", domain = NA)
    lines <- readLines(tfile2) # will be in UTF-8
    starts <- grep("^msgstr", lines)
    current <- 1L; out <- character()
    for (s in starts) {
        if (current < s)
            out <- c(out, lines[seq.int(current, s-1L, 1L)])
        start <- sub('([^"]*)"(.*)"$', "\\1", lines[s])
        this <- sub('([^"]*)"(.*)"$', "\\2", lines[s])
        current <- s+1L
        while(grepl('^"', lines[current])) {
            this <- c(this, sub('^"(.*)"$', "\\1", lines[current]))
            current <- current + 1L
        }
        nc <- nchar(this); n <- length(nc)
        this <- paste0(this, collapse="")
        ## This is the fixup: need to avoid apostrophes, which follow alnum
        this <- gsub("^'([^`']*)'",'‘\\1’', this)
#        this <- gsub("( |\\(|\\\\n|/)'([^`']*)'",'\\1‘\\2’', this)
        this <- gsub("([^[:alpha:]]|\\\\\n)'([^`']*)'",'\\1‘\\2’', this)
        out <- if (n > 1L) {
            ## now split where it was before
            this1 <- character()
            sc <- c(0, cumsum(nc))
            for(i in seq_along(nc)) {
                if(!nc[i]) this1 <- c(this1, "")
                else {
                    this1 <- c(this1, substr(this, sc[i]+1L, sc[i+1]))
                }
            }
            c(out,
              paste0(start, '"', this1[1L] , '"'),
              paste0('"', this1[-1L] , '"'))
        } else
            c(out, paste0(start, '"', this , '"'))
    }
    if(current <= length(lines))
        out <- c(out, lines[seq.int(current, length(lines), 1L)])
    ## in case this is done on Windows, force LF line endings
    con <- file(outfile, "wb")
    writeLines(out, con, useBytes = TRUE)
    close(con)
}

## But for now
en_quote0 <- function(potfile, out)
{
    SED <- Sys.getenv("SED", "sed") # but needs to be GNU sed on my Mac
#    SED <- "gnused"
    tfile <- tempfile()
    cmd <- paste("msginit -i", shQuote(potfile),
                 "--no-translator -l en -o", shQuote(tfile))
    if(system(cmd, ignore.stderr = TRUE) != 0L)
        stop("running msginit failed", domain = NA)
    tfile2 <- tempfile()
    cmd <- paste("msgconv -t UTF-8 -o", shQuote(tfile2), shQuote(tfile))
    if(system(cmd) != 0L) stop("running msgconv failed", domain = NA)
    cmd <- paste("msgfilter -i", shQuote(tfile2), "-o", out, SED,
                 "-f", shQuote(system.file(package="tools", "po","quot.sed")))
    if(system(cmd, ignore.stderr = TRUE) != 0L)
        stop("running msgfilter failed", domain = NA)
}


update_pkg_po <- function(pkgdir, pkg = NULL, version = NULL, copyright, bugs)
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
        if (missing(copyright)) copyright <- NULL
        if (missing(bugs)) bugs <- NULL
    } else { # A base package
        pkg <- basename(pkgdir)
        name <- "R"
        version <- as.character(getRversion())
        copyright <- "The R Core Team"
        bugs <- "bugs.r-project.org"
    }
    have_src <- paste0(pkg, ".pot") %in% files

    ## do R-pkg domain first
    ofile <- tempfile()
    xgettext2pot(".", ofile, name, version, bugs)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if(file.exists(potfile) && same(potfile, ofile)) {
    } else file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/R-en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        message("  R-", lang, ":", domain = NA, appendLF = FALSE)
        ## This seems not to update the file dates.
        cmd <- paste("msgmerge --update", f, shQuote(potfile))
        if(system(cmd) != 0L) {
            warning("running msgmerge on ", sQuote(f), " failed", domain = NA)
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
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA, immediate. = TRUE)
    }

    ## do en@quot
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  R-", lang, ":", domain = NA)
        f <- "po/R-en@quot.po"
        en_quote(potfile, f)
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA, immediate. = TRUE)
    }

    if(!have_src) return(invisible())

    setwd("src")
    cfiles <- dir(".", pattern = "[.]c$")
    ofile <- tempfile()
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, paste("--package-name", name, sep = "="),
             paste("--package-version", version, sep = "="),
             "--add-comments=TRANSLATORS:",
             if(!is.null(copyright))
                 sprintf('--copyright-holder="%s"', copyright),
             if(!is.null(bugs))
                 sprintf('--msgid-bugs-address="%s"', bugs))
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    setwd("..")

    ## compare ofile and po/pkg.pot, ignoring dates.
    potfile <- file.path("po", paste0(pkg, ".pot"))
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        message("  ", lang, ":", appendLF = FALSE, domain = NA)
        cmd <- paste("msgmerge --update", shQuote(f), shQuote(potfile))
        if(system(cmd) != 0L) {
            warning("running msgmerge on ",  f, " failed", domain = NA)
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
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }
    ## do en@quot
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  ", lang, ":", domain = NA)
        f <- "po/en@quot.po"
        en_quote(potfile, f)
        dest <- file.path("inst", "po", lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

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
    cfiles <- readLines("po/POTFILES")
    cfiles <- grep("^#", cfiles, value = TRUE, invert = TRUE)
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste("--package-version", getRversion(), sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    ## compare ofile and po/R.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R]*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/en@quot.po"]
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        message("  ", lang, ":", domain = NA, appendLF = FALSE)
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge failed", domain = NA)
            next
        }
##         res <- checkPoFile(f, FALSE)
##         if(nrow(res)) {
##             print(res)
##             next
##         }
        dest <- sprintf("po/%s.gmo", lang)
        if (file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

    ## do en@quot: assume UTF-8 locale
    if (!l10n_info()[["UTF-8"]])
        warning("UTF-8 locale required", domain = NA,
                immediate. = TRUE, call. = FALSE)

    lang <- "en@quot"
    message("  ", lang, ":", domain = NA)
    f <- "po/en@quot.po"
    en_quote(potfile, f)
    dest <- sprintf("po/%s.gmo", lang)
    cmd <- paste("msgfmt -c --statistics -o", dest, f)
    if(system(cmd) != 0L)
        warning(sprintf("running msgfmt on %s failed", basename(f)),
                domain = NA)

    invisible()
}

install_po <- function(srcdir, Rlocaledir)
{
    podir <- file.path(srcdir, "po")
    message("installing translations:", domain = NA)
    langs <- dir(podir, pattern = "^[^R].*.gmo")
    langs <- sub("[.]gmo$", "", langs)
    for(lang in langs) {
        dest <- file.path(Rlocaledir, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        file.copy(file.path(srcdir, "po", paste0(lang, ".gmo")),
                  file.path(dest, "R.mo"))
    }
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
    cmd <- sprintf("xgettext --keyword --keyword=G_ --keyword=GN_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste("--package-version", getRversion(), sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Foundation"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    ## compare ofile and po/RGui.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^RGui-.*[.]po$", full.names = TRUE)
    newer <- file_test("-nt", potfile, pofiles)
    for (f in pofiles[newer]) {
        lang <- sub("[.]po", "", basename(f))
        message("  ", lang, ":", appendLF = FALSE, domain = NA)
        cmd <- paste("msgmerge --update", f, potfile)
        if(system(cmd) != 0L) {
            warning("running msgmerge failed", domain = NA)
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
