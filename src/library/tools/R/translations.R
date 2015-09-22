#  File src/library/tools/R/translations.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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
        this <- gsub("^'([^`']*)'",'\u2018\\1\u2019', this)
        this <- gsub("([^[:alpha:]]|\\\\n)'([^`']*)'",'\\1\u2018\\2\u2019', this)
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
        stem <- file.path("inst", "po")
    } else { # A base package
        pkg <- basename(pkgdir)
        name <- "R"
        version <- as.character(getRversion())
        copyright <- "The R Core Team"
        bugs <- "bugs.r-project.org"
        stem <- file.path("..", "translations", "inst")
    }

    ## The interpreter is 'src' for the base package.
    is_base <- (pkg == "base")
    have_src <- paste0(pkg, ".pot") %in% files

    ## do R-pkg domain first
    ofile <- tempfile()
    xgettext2pot(".", ofile, name, version, bugs)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if(file.exists(potfile) && same(potfile, ofile)) {
    } else file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/R-en@quot.po"]
    ## .po file might be newer than .mo
    for (f in pofiles) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        message("  R-", lang, ":", appendLF = FALSE, domain = NA)
        ## This seems not to update the file dates.
        cmd <- paste("msgmerge --update", f, shQuote(potfile))
        if(system(cmd) != 0L) {
            warning("running msgmerge on ", sQuote(f), " failed", domain = NA)
            next
        }
        res <- checkPoFile(f, TRUE)
        if(nrow(res)) {
            print(res)
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
 #       if(file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA, immediate. = TRUE)
    }

    ## do en@quot
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  R-", lang, ":", domain = NA)
        # f <- "po/R-en@quot.po"
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA, immediate. = TRUE)
    }

    if(!(is_base || have_src)) return(invisible())

    ofile <- tempfile()
    if (!is_base) {
        dom <- pkg
        od <- setwd("src")
        exts <- "[.](c|cc|cpp|m|mm)$"
        cfiles <- dir(".", pattern = exts)
        if (file.exists("windows"))
            cfiles <- c(cfiles,
                        dir("windows", pattern = exts, full.names = TRUE))
    } else {
        dom <- "R"
        od <- setwd("../../..")
        cfiles <- grep("^#", readLines("po/POTFILES"),
                       value = TRUE, invert = TRUE)
    }
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, paste("--package-name", name, sep = "="),
             paste("--package-version", version, sep = "="),
             "--add-comments=TRANSLATORS:",
             if(!is.null(copyright))
                 sprintf('--copyright-holder="%s"', copyright),
             if(!is.null(bugs))
                 sprintf('--msgid-bugs-address="%s"', bugs),
             if(is_base) "-C") # avoid messages about .y
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    setwd(od)

    ## compare ofile and po/dom.pot, ignoring dates.
    potfile <- file.path("po", paste0(dom, ".pot"))
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/en@quot.po"]
    for (f in pofiles) {
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
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
#        if(file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }
    ## do en@quot
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  ", lang, ":", domain = NA)
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

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
                          c("console.c", "editor.c",  "extra.c",
                            "pager.c", "preferences.c", "rui.c", "system.c")),
                file.path("src/extra/graphapp",
                          c("clipboard.c", "dialogs.c", "gmenus.c",
                            "metafile.c", "printer.c")),
                "src/library/utils/src/windows/dataentry.c",
                "src/library/utils/src/windows/widgets.c",
                "src/library/grDevices/src/devWindows.c")
    potfile <- "src/library/base/po/RGui.pot"
    ofile <- tempfile()
    cmd <- sprintf("xgettext --keyword --keyword=G_ --keyword=GN_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste("--package-version", getRversion(), sep = "="),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Core Team"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    ## compare ofile and po/RGui.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("src/library/base/po", pattern = "^RGui-.*[.]po$", full.names = TRUE)
    for (f in pofiles) {
        lang <- sub("^RGui-(.*)[.]po$", "\\1", basename(f))
        lang2 <- sub("[.]po", "", basename(f))
        message("  ", lang2, ":", appendLF = FALSE, domain = NA)
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
        dest <- file.path("src/library/translations/inst", lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, "RGui.mo")
        if (file_test("-ot", f, dest)) next
        cmd <- paste("msgfmt -c --statistics -o", dest, f)
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
   }

    invisible()
}

## make package out of current translations.
make_translations_pkg <- function(srcdir, outDir = ".", append = "-1")
{
    src <- file.path(srcdir, "src/library/translations")
    dest <- file.path(tempdir(), "translations")
    dir.create(dest, FALSE)
    file.copy(file.path(src, "inst"),  dest, recursive = TRUE)
    lines <- readLines(file.path(src, "DESCRIPTION.in"))
    ver <- getRversion()
    lines <- gsub("@VERSION@", ver, lines, fixed = TRUE)
    lines[2] <- paste0(lines[2], append)
    ver <- unclass(getRversion())[[1]]
    deps <- sprintf("Depends: R (>= %s.%d.0), R (< %d.%d.0)",
                    ver[1], ver[2], ver[1], ver[2] + 1)
    lines <- c(lines, deps)
    writeLines(lines, file.path(dest, "DESCRIPTION"))
    cmd <- file.path(R.home(), "bin", "R")
    cmd <- paste(cmd, "CMD", "build", shQuote(dest))
    if(system(cmd) != 0L) stop("R CMD build failed")
    tarball <- Sys.glob(file.path(tempdir(), "translations_*.tar.gz"))
    file.rename(tarball, file.path(outDir, basename(tarball)))
    invisible()
}
