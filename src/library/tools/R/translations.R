#  File src/library/tools/R/translations.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

processPoFile <- function (f, potfile,
                           localedir = file.path("inst", "po"),
                           mergeOpts = "", # in addition to --update
                           verbose = getOption("verbose"))
{
    poname <- sub("[.]po$", "", basename(f))
    lang <- sub("^(R|RGui)-", "", poname)
    dom <- sub("[.]pot$", "", basename(potfile))
    mo_make <- !is.null(localedir)

    ## Will not update PO file if already in sync; keeps PO-Revision-Date.
    cmd <- paste("msgmerge",
                 if (is.character(mergeOpts)) paste("--update", mergeOpts),
                 shQuote(f), shQuote(potfile))
    if (verbose) cat("Running cmd", cmd, ":\n")
    else message("  ", poname, ":", appendLF = FALSE, domain = NA)
    if (system(cmd) != 0L)
        return(warning(sprintf("running msgmerge on %s failed", sQuote(f)),
                       call. = FALSE, domain = NA))

    res <- checkPoFile(f, TRUE)
    if (nrow(res)) {
        print(res)
        if (mo_make) message("not installing", domain = NA)
        ## the msgfmt -c step below would also fail (includes --check-format)
        return(warning(sprintf("inconsistent format strings in %s", sQuote(f)),
                       call. = FALSE, domain = NA))
    }

    if (!mo_make) return(invisible())
    dest <- file.path(localedir, lang, "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    dest <- file.path(dest, sprintf("%s.mo", dom))
    ## Skip compilation if PO is unchanged since last run / checkout?
    #if (file.exists(dest) && file_test("-ot", f, dest)) return(invisible())
    cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
    if (verbose) cat("Running cmd", cmd, ":\n")
    if (system(cmd) != 0L)
        return(warning(sprintf("running msgfmt on %s failed", sQuote(f)),
                       call. = FALSE, domain = NA))
}

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
    starts <- which(startsWith(lines, "msgstr"))
    current <- 1L; out <- character()
    for (s in starts) {
        if (current < s)
            out <- c(out, lines[seq.int(current, s-1L, 1L)])
        start <- sub('([^"]*)"(.*)"$', "\\1", lines[s])
        this <- sub('([^"]*)"(.*)"$', "\\2", lines[s])
        current <- s+1L
        while(!is.na(line <- lines[current]) && startsWith(line, '"')) {
            this <- c(this, sub('^"(.*)"$', "\\1", line))
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

update_pkg_po <- function(pkgdir, pkg = NULL, version = NULL,
                          pot_make = TRUE, mo_make = TRUE,
                          verbose = getOption("verbose"),
                          mergeOpts = "", # only those *in addition* to --update
                          copyright, bugs)
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- filtergrep('^"POT-Creation-Date:', tmpa)
        tmpb <- filtergrep('^"POT-Creation-Date:', tmpb)
        identical(tmpa, tmpb)
    }

    ## Follow previous version by always collating in C.
    pwd <- getwd()
    coll <-  Sys.getlocale("LC_COLLATE")
    on.exit({Sys.setlocale("LC_COLLATE", coll); setwd(pwd)})
    Sys.setlocale("LC_COLLATE", "C")
    setwd(pkgdir)
    dir.create("po", FALSE)
    files <- dir("po")

    desc <- "DESCRIPTION"
    if(file.exists(desc)) {
        desc <- read.dcf(desc, fields = c("Package", "Version"))
        name <- desc[1L]
        if (is.null(pkg))	pkg <- name
        if (is.null(version))	version <- desc[2L]
        if (missing(copyright)) copyright <- NULL
        if (missing(bugs))	bugs <- NULL
        stem <- file.path("inst", "po")
    }
    if (is.null(pkg) || pkg %in% .get_standard_package_names()$base) { # A base package
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
  if(pot_make) {
    ofile <- tempfile()
    if(verbose) cat("Creating pot: .. ")
    xgettext2pot(".", ofile, name, version, bugs)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if(file.exists(potfile) && same(potfile, ofile)) {
        if(verbose) cat("the same() as previous: not copying.\n")
    } else {
        if(verbose) cat("copying to potfile", potfile, "\n")
        file.copy(ofile, potfile, overwrite = TRUE)
    }
  } else {
        if(!file.exists(potfile <- file.path("po", paste0("R-", pkg, ".pot"))))
            stop(gettextf("file '%s' does not exist", potfile), domain = NA)
    }
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/R-en@quot.po"]
    for (f in pofiles) {
        processPoFile(f, potfile, localedir = if(mo_make) stem,
                      mergeOpts = mergeOpts, verbose = verbose)
    }

    ## do en@quot
    if (l10n_info()[["UTF-8"]] && mo_make) {
        lang <- "en@quot"
        message("  R-", lang, ":", domain = NA)
        # f <- "po/R-en@quot.po"
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(verbose) cat("Running cmd", cmd, ":\n")
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

    if(!(is_base || have_src)) return(invisible())

  if(pot_make) {
    ofile <- tempfile()
    if (!is_base) {
        dom <- pkg
        od <- setwd("src")
        exts <- "[.](c|cc|cpp|m|mm)$"
        cfiles <- dir(".", pattern = exts)
        for (subdir in c("windows", "cairo")) { # only grDevices/src/cairo
          if(dir.exists(subdir))
            cfiles <- c(cfiles,
                        dir(subdir, pattern = exts, full.names = TRUE))
        }
    } else {
        dom <- "R"
        od <- setwd("../../..")
        cfiles <- filtergrep("^#", readLines("po/POTFILES"))
    }
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", shQuote(ofile))
    cmd <- c(cmd, paste0("--package-name=", name),
             paste0("--package-version=", version),
             "--add-comments=TRANSLATORS:",
             if(!is.null(copyright))
                 sprintf('--copyright-holder="%s"', copyright),
             if(!is.null(bugs))
                 sprintf('--msgid-bugs-address="%s"', bugs),
             if(is_base) "-C") # avoid messages about .y
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(verbose) cat("Running cmd", cmd, ":\n")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    setwd(od)

    ## compare ofile and po/dom.pot, ignoring dates.
    potfile <- file.path("po", paste0(dom, ".pot"))
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)

  } else { # not pot_make
        dom <- if(is_base) "R" else pkg
        if(!file.exists(potfile <- file.path("po", paste0(dom, ".pot"))))
            stop(gettextf("file '%s' does not exist", potfile), domain = NA)
    }
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/en@quot.po"]
    for (f in pofiles) {
        processPoFile(f, potfile, localedir = if(mo_make) stem,
                      mergeOpts = mergeOpts, verbose = verbose)
    }
    ## do en@quot
    if (l10n_info()[["UTF-8"]] && mo_make) {
        lang <- "en@quot"
        message("  ", lang, ":", domain = NA)
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), shQuote(f))
        if(verbose) cat("Running cmd", cmd, ":\n")
        if(system(cmd) != 0L)
            warning(sprintf("running msgfmt on %s failed", basename(f)),
                    domain = NA)
    }

    invisible()
}

# (not exported)
update_RGui_po <- function(srcdir,
                           pot_make = TRUE, mo_make = TRUE,
                           mergeOpts = "")
{
    same <- function(a, b)
    {
        tmpa <- readLines(a); tmpb <- readLines(b)
        tmpa <- filtergrep('^"POT-Creation-Date:', tmpa)
        tmpb <- filtergrep('^"POT-Creation-Date:', tmpb)
        identical(tmpa, tmpb)
    }
    ## Follow previous version by always collating in C.
    pwd <- getwd()
    coll <-  Sys.getlocale("LC_COLLATE")
    on.exit({Sys.setlocale("LC_COLLATE", coll); setwd(pwd)})
    Sys.setlocale("LC_COLLATE", "C")
    setwd(srcdir)
    potfile <- "src/library/base/po/RGui.pot"
  if(pot_make) {
    cfiles <- c(file.path("src/gnuwin32",
                          c("console.c", "editor.c",  "extra.c",
                            "pager.c", "preferences.c", "rui.c", "system.c")),
                file.path("src/extra/graphapp",
                          c("clipboard.c", "dialogs.c", "gmenus.c",
                            "metafile.c", "printer.c")),
                "src/library/utils/src/windows/dataentry.c",
                "src/library/utils/src/windows/widgets.c",
                "src/library/grDevices/src/devWindows.c")
    ofile <- tempfile()
    cmd <- sprintf("xgettext --keyword --keyword=G_ --keyword=GN_ -o %s", shQuote(ofile))
    cmd <- c(cmd, "--package-name=R",
             paste0("--package-version=", getRversion()),
             "--add-comments=TRANSLATORS:",
             '--copyright-holder="The R Core Team"',
             '--msgid-bugs-address="bugs.r-project.org"')
    cmd <- paste(c(cmd, cfiles), collapse=" ")
    if(system(cmd) != 0L) stop("running xgettext failed", domain = NA)
    ## compare ofile and po/RGui.pot, ignoring dates.
    if(!same(potfile, ofile)) file.copy(ofile, potfile, overwrite = TRUE)
  }
    pofiles <- dir("src/library/base/po", pattern = "^RGui-.*[.]po$", full.names = TRUE)
    for (f in pofiles) {
        processPoFile(f, potfile,
                      localedir = if(mo_make) "src/library/translations/inst",
                      mergeOpts = mergeOpts)
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
    cmd <- shQuote(file.path(R.home(), "bin", "R"))
    cmd <- paste(cmd, "CMD", "build", shQuote(dest))
    if(system(cmd) != 0L) stop("R CMD build failed")
    tarball <- Sys.glob(file.path(tempdir(), "translations_*.tar.gz"))
    file.rename(tarball, file.path(outDir, basename(tarball)))
    invisible()
}
