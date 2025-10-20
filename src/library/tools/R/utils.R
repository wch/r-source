#  File src/library/tools/R/utils.R
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

### * File utilities.

### ** file_ext

file_ext <-
function(x)
{
    ## Return the file extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

### ** file_path_as_absolute

file_path_as_absolute <-
function(x)
{
    ## Turn a possibly relative file path absolute, performing tilde
    ## expansion if necessary.
    if(length(x) != 1L)
        stop(gettextf("'%s' must be a character string", "x"), domain=NA)
    if(!file.exists(epath <- path.expand(x)))
        stop(gettextf("file '%s' does not exist", x),
             domain = NA)
    normalizePath(epath, "/", TRUE)
}

### ** file_path_relative_to

file_path_relative_to <-
function(x, start = getwd(), parent = TRUE)
{
    x <- normalizePath(x, "/", mustWork = FALSE)
    if(!parent) {
        p <- normalizePath(start[1L], "/", mustWork = TRUE)
        if(any(i <- startsWith(x, p))) {
            ## Assume .Platform$file.sep is a single character.
            x[i] <- substring(x[i], nchar(p) + 2L)
        }
        x
    } else {
        p <- strsplit(normalizePath(start, "/", mustWork = FALSE),
                      "/", fixed = TRUE)[[1L]]
        y <- strsplit(x, "/", fixed = TRUE)
        f <- function(u, v) {
            i <- 1L
            while(i <= min(length(v), length(p))) {
                if(v[i] == p[i])
                    i <- i + 1L
                else
                    break
            }
            if(i == 1L) {
                ## Paths start differently, so relative cannot work
                u
            } else {
                i <- i - 1L
                paste(c(rep_len("..", length(p) - i), v[-seq_len(i)]),
                      collapse = .Platform$file.sep)
            }
        }
        unlist(Map(f, x, y, USE.NAMES = FALSE))
    }
}

### ** file_path_sans_ext

file_path_sans_ext <-
function(x, compression = FALSE)
{
    ## Return the file paths without extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    if(compression)
        x <- sub("[.](gz|bz2|xz)$", "", x)
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

### ** file_test

## exported/documented copy is in utils.

file_test <-
function(op, x, y)
{
    ## Provide shell-style '-f', '-d', '-h'/'-L', '-x', '-w', '-r',
    ## '-nt' and '-ot' tests.
    ## Note that file.exists() only tests existence ('test -e' on some
    ## systems), and that our '-f' tests for existence and not being a
    ## directory (the GNU variant tests for being a regular file).
    ## Note: vectorized in x and y.
    switch(op,
           "-f" = !is.na(isdir <- file.info(x, extra_cols = FALSE)$isdir) & !isdir,
           "-d" = dir.exists(x),
           "-h" = (!is.na(y <- Sys.readlink(x)) & nzchar(y)),
           "-L" = (!is.na(y <- Sys.readlink(x)) & nzchar(y)),
           "-nt" = (!is.na(mt.x <- file.mtime(x))
                    & !is.na(mt.y <- file.mtime(y))
                    & (mt.x > mt.y)),
           "-ot" = (!is.na(mt.x <- file.mtime(x))
                    & !is.na(mt.y <- file.mtime(y))
                    & (mt.x < mt.y)),
           "-x" = (file.access(x, 1L) == 0L),
           "-w" = (file.access(x, 2L) == 0L),
           "-r" = (file.access(x, 4L) == 0L),
           stop(gettextf("test '%s' is not available", op),
                domain = NA))
}

### ** list_files_with_exts

list_files_with_exts <-
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.

    files <- list.files(dir, all.files = all.files)
    ## does not cope with exts with '.' in.
    ## files <- files[sub(".*\\.", "", files) %in% exts]
    patt <- paste0("\\.(", paste(exts, collapse="|"), ")$")
    files <- grep(patt, files, value = TRUE)
    if(full.names)
        files <- if(length(files))
            file.path(dir, files)
        else
            character()
    files
}

### ** list_files_with_type

list_files_with_type <-
function(dir, type, all.files = FALSE, full.names = TRUE,
         OS_subdirs = .OStype())
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .make_file_exts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included (if present) according to the value
    ## of @code{OS_subdirs}.

    exts <- .make_file_exts(type)
    files <-
        list_files_with_exts(dir, exts, all.files = all.files,
                             full.names = full.names)

    if(type %in% c("code", "docs")) {
        for(os in OS_subdirs) {
            os_dir <- file.path(dir, os)
            if(dir.exists(os_dir)) {
                os_files <- list_files_with_exts(os_dir, exts,
                                                 all.files = all.files,
                                                 full.names = FALSE)
                os_files <- file.path(if(full.names) os_dir else os,
                                      os_files)
                files <- c(files, os_files)
            }
        }
    }
    ## avoid ranges since they depend on the collation order in the locale.
    if(type %in% c("code", "docs")) { # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]", basename(files))]
    }
    if(type %in% "demo") {           # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", basename(files))]
    }
    files
}

### ** list_code_files_in_package

list_code_files_in_package <-
function(dir) {
    dir <- normalizePath(dir)
    code_dir <- file.path(dir, "R")
    code_files <- list_files_with_type(code_dir, "code")
    if(!length(code_files)) return(code_files)
    meta <- .get_package_metadata(dir)
    collate_fields <- c(paste0("Collate.", .OStype()), "Collate")
    if(any(i <- (collate_fields %in% names(meta)))) {
        collate <-
            file.path(code_dir,
                      .read_collate_field(meta[collate_fields[i][1L]]))
        ## Note that matching code files and collate spec really only is
        ## appropriate after having run configure as part of installing,
        ## as this can create code files (e.g., from a .R.in code file).
        ## Note also that using set ops is not appropriate here, as
        ## these re-sort according to the current locale.
        code_files <- collate[collate %in% code_files]
    }
    code_files
}


### ** reQuote

reQuote <-
function(x)
{
    escape <- function(s) paste0("\\", s)
    re <- "[.*?+^$\\()[]"
    m <- gregexpr(re, x)
    regmatches(x, m) <- lapply(regmatches(x, m), escape)
    x
}

### ** showNonASCII

showNonASCII <-
function(x)
{
    ind <- .Call(C_nonASCII, x)
    if(any(ind)) {
        message(paste0(which(ind), ": ",
                       ## iconv will usually substitute,
                       ## but inplementations including macOS 14
                       ## may translate to ASCII.
                       iconv(x[ind], "", "ASCII", sub = "byte"),
                       collapse = "\n"), domain = NA)
    }
    invisible(x[ind])
}

showNonASCIIfile <-
function(file)
    showNonASCII(readLines(file, warn = FALSE))

env_path <- function(...) file.path(..., fsep = .Platform$path.sep)

### * Text utilities.

### ** delimMatch
delimMatch <-
function(x, delim = c("{", "}"), syntax = "Rd")
{
    if(!is.character(x))
        stop("argument 'x' must be a character vector")
    ## FIXME: bytes or chars?
    if((length(delim) != 2L) || any(nchar(delim) != 1L))
        stop("argument 'delim' must specify two characters")
    if(syntax != "Rd")
        stop("only Rd syntax is currently supported")

    .Call(C_delim_match, x, delim)
}

### ** lines2str
lines2str <-
function(txt, sep = "") {
    bytes <- gsub("\n", sep, paste(txt, collapse = sep),
                  fixed = TRUE, useBytes = TRUE)
    trimws(iconv(bytes, to = "UTF-8", sub = "byte"))
}


### * LaTeX utilities

### ** texi2pdf
texi2pdf <-
function(file, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"),
         texinputs = NULL, index = TRUE)
    texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,
             texi2dvi = texi2dvi, texinputs = texinputs, index = index)

### ** texi2dvi

texi2dvi <-
function(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"),
         texinputs = NULL, index = TRUE)
{
    if (clean) pre_files <- list.files(all.files = TRUE)
    do_cleanup <- function(clean)
        if(clean) {
            ## output file will be created in the current directory
            out_file <- paste(basename(file_path_sans_ext(file)),
                              if(pdf) "pdf" else "dvi", sep = ".")
            files <- setdiff(list.files(all.files = TRUE),
                             c(".", "..", out_file, pre_files))
            file.remove(files)
        }

    ## Run texi2dvi on a latex file, or emulate it.

    if(identical(texi2dvi, "emulation")) texi2dvi <- ""
    else {
        if(is.null(texi2dvi) || !nzchar(texi2dvi) || texi2dvi == "texi2dvi") {
            texi2dvi <- Sys.which("texi2dvi")
            if(.Platform$OS.type == "windows" && !nzchar(texi2dvi))
                texi2dvi <- Sys.which("texify")
        } else if (!nzchar(Sys.which(texi2dvi))) { # check provided path
            warning("texi2dvi script/program not available, using emulation")
            texi2dvi <- ""
        } # else the provided one should work
    }

    paths2env <- function(x) paste(x, collapse = .Platform$path.sep)
    ## not clear if this is needed, but works
    if(.Platform$OS.type == "windows")
        texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
    Rtexmf <- file.path(R.home("share"), "texmf", fsep = "/")
    Rtexinputs <- file.path(Rtexmf, "tex", "latex", fsep = "/")
    Rbibinputs <- file.path(Rtexmf, "bibtex", "bib", fsep = "/")
    Rbstinputs <- file.path(Rtexmf, "bibtex", "bst", fsep = "/")

    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA_character_)
    if(is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    } else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    ## "" below represents system paths
    Sys.setenv(TEXINPUTS = paths2env(c(texinputs, otexinputs, Rtexinputs, "")))
    obibinputs <- Sys.getenv("BIBINPUTS", unset = NA_character_)
    if(is.na(obibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        obibinputs <- "."
    } else on.exit(Sys.setenv(BIBINPUTS = obibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paths2env(c(texinputs, obibinputs, Rbibinputs, "")))
    obstinputs <- Sys.getenv("BSTINPUTS", unset = NA_character_)
    if(is.na(obstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        obstinputs <- "."
    } else on.exit(Sys.setenv(BSTINPUTS = obstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paths2env(c(texinputs, obstinputs, Rbstinputs, "")))

    if(index && nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        ## switch off the use of texindy in texi2dvi >= 1.157
        Sys.setenv(TEXINDY = "false")
        on.exit(Sys.unsetenv("TEXINDY"), add = TRUE)
        opt_pdf <- if(pdf) "--pdf" else ""
        opt_quiet <- if(quiet) "--quiet" else ""
        opt_extra <- ""

        ## the current heuristics for finding error messages in log files
        ## have better coverage with the default '!' error indicator, but
        ## texi2dvi enables the file:line:error style, so:
        out <- .system_with_capture(texi2dvi, "--help")
        if(length(grep("--no-line-error", out$stdout)))
            opt_extra <- "--no-line-error"

        ## and work around a bug in texi2dvi
        ## https://stat.ethz.ch/pipermail/r-devel/2011-March/060262.html
        ## That has [A-Za-z], earlier versions [A-z], both of which may be
        ## invalid in some locales.
        env0 <- "LC_COLLATE=C"
        ## texi2dvi, at least on macOS (4.8) does not accept TMPDIR with spaces.
        if (grepl(" ", Sys.getenv("TMPDIR")))
            env0 <- paste(env0,  "TMPDIR=/tmp")
        out <- .system_with_capture(texi2dvi,
                                    c(opt_pdf, opt_quiet, opt_extra,
                                      shQuote(file)),
                                    env = env0)

        log <- paste0(file_path_sans_ext(file), ".log")

        ## With Texinfo 6.1 (precisely, c6637), texi2dvi may not rerun
        ## often enough and give a non-zero status value when it should
        ## have continued iterating.
        ## Try to catch and correct cases seen on CRAN ...
        ## (Note that texi2dvi may have been run quietly, in which case
        ## diagnostics will only be in the log file.)
        if(out$status &&
           file_test("-f", log) &&
           any(grepl("(Rerun to get|biblatex.*\\(re\\)run)",
                     readLines(log, warn = FALSE), useBytes = TRUE))) {
            out <- .system_with_capture(texi2dvi,
                                        c(opt_pdf, opt_quiet, opt_extra,
                                          shQuote(file)),
                                        env = env0)
        }

        ## We cannot necessarily rely on out$status, hence let us
        ## analyze the log files in any case.
        errors <- character()
        ## (La)TeX errors.
        log <- paste0(file_path_sans_ext(file), ".log")
        if(file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                errors <- paste0("LaTeX errors:\n",
                                 paste(lines, collapse = "\n"))
        }
        ## BibTeX errors.
        log <- paste0(file_path_sans_ext(file), ".blg")
        if(file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                errors <- paste0("BibTeX errors:\n",
                                 paste(lines, collapse = "\n"))
        }

        msg <- ""
        if(out$status) {
            ## <NOTE>
            ## If we cannot rely on out$status, we could test for
            ##   if(out$status || length(errors))
            ## But shouldn't we be able to rely on out$status on Unix?
            ## </NOTE>
            msg <- gettextf("Running 'texi2dvi' on '%s' failed.", file)
            ## Error messages from GNU texi2dvi are rather terse, so
            ## only use them in case no additional diagnostics are
            ## available (e.g, makeindex errors).
            if(length(errors))
                msg <- paste(msg, errors, sep = "\n")
            else if(length(out$stderr))
                msg <- paste(msg, "Messages:",
                             paste(out$stderr, collapse = "\n"),
                             sep = "\n")
            if(!quiet)
                msg <- paste(msg, "Output:",
                             paste(out$stdout, collapse = "\n"),
                             sep = "\n")
        }

        do_cleanup(clean)

        if(nzchar(msg))
            stop(msg, domain = NA)
        else if(!quiet)
            message(paste(paste(out$stderr, collapse = "\n"),
                          paste(out$stdout, collapse = "\n"),
                          sep = "\n"))
    } else if(index && nzchar(texi2dvi)) { # MiKTeX on Windows
        extra <- ""

        ## look for MiKTeX (which this almost certainly is)
        ## http://docs.miktex.org/manual/texify.html
        ## ver <- system(paste(shQuote(texi2dvi), "--version"), intern = TRUE)
        ## if(length(grep("MiKTeX", ver[1L]))) {
        ##     extra <- "--max-iterations=20"
        ## }

        ## 'file' could be a file path
        base <- basename(file_path_sans_ext(file))
        ## this only gives a failure in some cases, e.g. not for bibtex errors.
        system(paste(shQuote(texi2dvi),
                     if(quiet) "--quiet" else "",
                     if(pdf) "--pdf" else "",
                     shQuote(file), extra),
               intern=TRUE, ignore.stderr=TRUE)
        msg <- ""
        ## (La)TeX errors.
        logfile <- paste0(base, ".log")
        if(file_test("-f", logfile)) {
            lines <- .get_LaTeX_errors_from_log_file(logfile)
            if(length(lines))
                msg <- paste(msg, "LaTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }
        ## BibTeX errors.
        logfile <- paste0(base, ".blg")
        if(file_test("-f", logfile)) {
            lines <- .get_BibTeX_errors_from_blg_file(logfile)
            if(length(lines))
                msg <- paste(msg, "BibTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }

        do_cleanup(clean)
        if(nzchar(msg)) {
            msg <- paste(gettextf("running 'texi2dvi' on '%s' failed", file),
                         msg, "", sep = "\n")
            stop(msg, call. = FALSE, domain = NA)
        }
    } else {
        ## Do not have texi2dvi or don't want to index
        ## Needed on Windows except for MiKTeX (prior to Sept 2015)

        texfile <- shQuote(file)
        ## 'file' could be a file path
        base <- basename(file_path_sans_ext(file))
        idxfile <- paste0(base, ".idx")
        latex <- if(pdf) Sys.getenv("PDFLATEX", "pdflatex")
        else  Sys.getenv("LATEX", "latex")
        if(!nzchar(Sys.which(latex)))
            stop(if(pdf) "pdflatex" else "latex", " is not available",
                 domain = NA)

        sys2 <- if(quiet)
            function(...) system2(..., stdout = FALSE, stderr = FALSE)
        else system2
        bibtex <- Sys.getenv("BIBTEX", "bibtex")
        makeindex <- Sys.getenv("MAKEINDEX", "makeindex")
        ltxargs <- c("-interaction=nonstopmode", texfile)
        if(sys2(latex, ltxargs)) {
            lines <- .get_LaTeX_errors_from_log_file(paste0(base, ".log"))
            errors <- if(length(lines))
                          paste0("LaTeX errors:\n",
                                 paste(lines, collapse = "\n"))
                      else character()
            stop(paste(gettextf("unable to run %s on '%s'", latex, file),
                       errors, sep = "\n"),
                 domain = NA)
        }
        nmiss <- length(grep("Warning:.*Citation.*undefined",
                             readLines(paste0(base, ".log")),
                             useBytes = TRUE))
        for(iter in 1L:10L) { ## safety check
            ## This might fail as the citations have been included in the Rnw
            if(nmiss) sys2(bibtex, shQuote(base))
            nmiss_prev <- nmiss
            if(index && file.exists(idxfile)) {
                if(sys2(makeindex, shQuote(idxfile)))
                    stop(gettextf("unable to run '%s' on '%s'",
                                  makeindex, idxfile),
                         domain = NA)
            }
            if(sys2(latex, ltxargs)) {
                lines <- .get_LaTeX_errors_from_log_file(paste0(base, ".log"))
                errors <- if(length(lines))
                              paste0("LaTeX errors:\n",
                                     paste(lines, collapse = "\n"))
                else character()
                stop(paste(gettextf("unable to run %s on '%s'", latex, file),
                           errors, sep = "\n"),
                     domain = NA)
            }
            Log <- readLines(paste0(base, ".log"))
            nmiss <- length(grep("Warning:.*Citation.*undefined", Log,
                                 useBytes = TRUE))
            if(nmiss == nmiss_prev &&
               !any(grepl("(Rerun to get|biblatex.*\\(re\\)run)", Log,
                          useBytes = TRUE)) ) break
        }
        do_cleanup(clean)
    }
    invisible(NULL)
}

### * Internal utility variables.

### ** .vc_dir_names

## Version control directory names: CVS, .svn (Subversion), .arch-ids
## (arch), .bzr, .git, .hg (mercurial) and _darcs (Darcs)
## And it seems .metadata (eclipse) is in the same category.

.vc_dir_names <-
    c("CVS", ".svn", ".arch-ids", ".bzr", ".git", ".hg", "_darcs", ".metadata")

## and RE version (beware of the need for escapes if amending)

.vc_dir_names_re <-
    "/(CVS|\\.svn|\\.arch-ids|\\.bzr|\\.git|\\.hg|_darcs|\\.metadata)(/|$)"

## We are told
## .Rproj.user is Rstudio
## .cproject .project .settings are Eclipse
## .exrc is for vi
## .tm_properties is Mac's TextMate
.hidden_file_exclusions <-
    c(".Renviron", ".Rprofile", ".Rproj.user",
      ".Rhistory", ".Rapp.history",
      ".tex", ".log", ".aux", ".pdf", ".png",
      ".backups", ".cvsignore", ".cproject", ".directory",
      ".dropbox", ".exrc", ".gdb.history",
      ".gitattributes", ".gitignore", ".gitmodules",
      ".hgignore", ".hgtags",
      ".htaccess",
      ".latex2html-init",
      ".project", ".seed", ".settings", ".tm_properties")

### * Internal utility functions.

### ** filtergrep

filtergrep <-
function(pattern, x, ...)
    grep(pattern, x, invert = TRUE, value = TRUE, ...)

### ** %notin%

`%notin%` <-
function(x, y)
    is.na(match(x, y))

### ** %w/o%

## x without y, as in the examples of ?match.
`%w/o%` <-
function(x, y)
    x[is.na(match(x, y))]

### ** .OStype

.OStype <- function() {
    Sys.getenv("R_OSTYPE", unset = .Platform$OS.type, names = FALSE)
}

### ** .R_copyright_msg

.R_copyright_msg <-
function(year)
    sprintf("Copyright (C) %s-%s The R Core Team.",
            year, R.version$year)

### ** .R_top_srcdir

## Find the root directory of the source tree used for building this
## version of R (corresponding to Unix configure @top_srcdir@).
## Seems this is not recorded anywhere, but we can find our way ...

.R_top_srcdir_from_Rd <-
function() {
    filebase <-
        file_path_sans_ext(system.file("help", "tools.rdb",
                                       package = "tools"))
    path <- attr(fetchRdDB(filebase, "QC"), "Rdfile")
    ## We could use 5 dirname() calls, but perhaps more easily:
    substr(path, 1L, nchar(path) - 28L)
}

## Unfortunately,
##   .R_top_srcdir <- .R_top_srcdir_from_Rd()
## does not work because when tools is installed there are no Rd pages
## yet ...

### ** config_val_to_logical

config_val_to_logical <-
function(val, na.ok=TRUE) utils:::str2logical(val, na.ok=na.ok)

### ** .canonicalize_doi

.canonicalize_doi <-
function(x)
{
    x <- sub("^((doi|DOI):)?[[:space:]]*https?://(dx[.])?doi[.]org/", "",
             x)
    sub("^(doi|DOI):", "", x)
}

### ** .canonicalize_quotes

.canonicalize_quotes <-
function(txt)
{
    txt <- as.character(txt)
    if(!length(txt)) return(txt)
    enc <- Encoding(txt)
    txt <- gsub("(\u2018|\u2019)", "'", txt, perl = TRUE, useBytes = TRUE)
    txt <- gsub("(\u201c|\u201d)", '"', txt, perl = TRUE, useBytes = TRUE)
    Encoding(txt) <- enc
    txt
}

### ** .enc2latin1

.enc2latin1 <-
function(x)
{
    if(length(pos <- which(Encoding(x) == "UTF-8")))
        x[pos] <- iconv(x[pos], "UTF-8", "latin1", sub = "byte")
    x
}

### ** .eval_with_capture

.eval_with_capture <-
function(expr, type = NULL)
{
    ## Evaluate the given expression and return a list with elements
    ## 'value', 'output' and 'message' (with obvious meanings).

    ## <NOTE>
    ## The current implementation gives character() if capturing was not
    ## attempted of gave nothing.  If desired, one could modify the code
    ## to return NULL in the former case.
    ## </NOTE>

    if(is.null(type))
        capture_output <- capture_message <- TRUE
    else {
        type <- match.arg(type, c("output", "message"))
        capture_output <- type == "output"
        capture_message <- !capture_output
    }

    outcon <- file(open = "w+", encoding = "UTF-8")
    msgcon <- file(open = "w+", encoding = "UTF-8")
    if(capture_output) {
        sink(outcon, type = "output")
        on.exit(sink(type = "output"))
    }
    if(capture_message) {
        sink(msgcon, type = "message")
        on.exit(sink(type = "message"), add = capture_output)
    }
    on.exit({ close(outcon) ; close(msgcon) }, add = TRUE)

    value <- eval(expr)
    list(value = value,
         output = readLines(outcon, warn = FALSE),
         message = readLines(msgcon, warn = FALSE))
}

### ** .expand_anchored_Rd_xrefs

.expand_anchored_Rd_xrefs <-
function(db)
{
    ## db should have columns Target and Anchor.
    db <- db[, c("Target", "Anchor"), drop = FALSE]
    ## See .check_Rd_xrefs().
    anchor <- db[, 2L]
    have_equals <- startsWith(anchor, "=")
    if(any(have_equals))
        db[have_equals, ] <-
            cbind(sub("^=", "", anchor[have_equals]), "")
    anchor <- db[, 2L]
    have_colon <- grepl(":", anchor, fixed = TRUE)
    y <- cbind(T_Package = anchor, T_File = db[, 1L])
    y[have_colon, ] <-
        cbind(sub("([^:]*):(.*)", "\\1", anchor[have_colon]),
              sub("([^:]*):(.*)", "\\2", anchor[have_colon]))
    y
}

### ** .file_append_ensuring_LFs

.file_append_ensuring_LFs <-
function(file1, file2)
{
    ## Use a fast version of file.append() that ensures LF between
    ## files.
    .Call(C_codeFilesAppend, file1, file2)
}

### ** .file_path_to_LaTeX_graphicspath

.file_path_to_LaTeX_graphicspath <-
function(x)
{
    x <- normalizePath(x, "/")
    ## Older versions of (PDF)LaTeX need double quotes in case of spaces
    ## etc.  Newer versions of XeLaTeX and LuaLaTeX cannot handle these.
    ## Argh ...
    sprintf(paste(c("\\makeatletter",
                    "\\ifthenelse",
                    "{\\boolean{Rd@graphicspath@needs@quotes}}",
                    "{\\graphicspath{{\"%s/\"}}}",
                    "{\\graphicspath{{%s/}}}",
                    "\\makeatother"),
                  collapse = ""),
            x, x)
}

### ** .file_path_relative_to_dir

.file_path_relative_to_dir <-
function(x, dir, add = FALSE)
{
    if(any(ind <- startsWith(x, dir))) {
        ## Assume .Platform$file.sep is a single character.
        x[ind] <- if(add)
            file.path(basename(dir), substring(x[ind], nchar(dir) + 2L))
        else
            substring(x[ind], nchar(dir) + 2L)
    }
    x
}

### ** .find_calls

.find_calls <-
function(x, predicate = NULL, recursive = FALSE)
{
    calls <- list()

    if(!is.recursive(x) || isS4(x)) return(calls)

    x <- if(is.call(x))
             list(x)
         else {
             if(is.object(x))
                 class(x) <- NULL
             as.list(x)
         }

    f <- if(is.null(predicate))
        function(e) is.call(e)
    else ## no check predicate returns a scalar, so any() added for 4.2.0
        function(e) is.call(e) && any(predicate(e))

    if(!recursive) return(Filter(f, x))

    gatherer <- function(e) {
        if(f(e)) calls <<- c(calls, list(e))
        if(is.recursive(e) && !is.environment(e) && !isS4(e)) {
            if(is.object(e))
                class(e) <- NULL
            e <- as.list(e)
            for(i in seq_along(e)) gatherer(e[[i]])
        }
    }

    gatherer(x)

    calls
}

### ** .find_calls_in_file

.find_calls_in_file <-
function(file, encoding = NA, predicate = NULL, recursive = FALSE)
{
    .find_calls(.parse_code_file(file, encoding), predicate, recursive)
}

### ** .find_calls_in_package_code

.find_calls_in_package_code <-
function(dir, predicate = NULL, recursive = FALSE, .worker = NULL,
         which = "code")
{
    dir <- file_path_as_absolute(dir)

    dfile <- file.path(dir, "DESCRIPTION")
    encoding <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    if(is.null(.worker))
        .worker <- function(file, encoding)
            .find_calls_in_file(file, encoding, predicate, recursive)

    which <- match.arg(which,
                       c("code", "vignettes", "tests",
                         "NAMESPACE", "CITATION", "docs"),
                       several.ok = TRUE)
    code_files <-
        c(character(),
          if("code" %in% which)
              list_files_with_type(file.path(dir, "R"), "code",
                                   OS_subdirs = c("unix", "windows")),
          if(("vignettes" %in% which) &&
             dir.exists(file.path(dir, "vignettes")) &&
             dir.exists(fp <- file.path(dir, "inst", "doc")))
              list_files_with_type(fp, "code"),
          ## cf. .check_packages_used_in_tests() ...
          if(("tests" %in% which) &&
             dir.exists(fp <- file.path(dir, "tests")))
              c(list.files(fp, pattern = "\\.[rR]$",
                           full.names = TRUE),
                if(dir.exists(fp <- file.path(fp, "testthat")))
                    list.files(fp, pattern = "\\.[rR]$",
                               full.names = TRUE)),
          if(("NAMESPACE" %in% which) &&
             file.exists(fp <- file.path(dir, "NAMESPACE")))
              fp,
          if(("CITATION" %in% which) &&
             file.exists(fp <- file.path(dir, "inst", "CITATION")))
              fp)

    calls <- lapply(code_files, .worker, encoding)
    names(calls) <-
        .file_path_relative_to_dir(code_files, dirname(dir))

    if("docs" %in% which) {
        db <- Rd_db(dir = dir)
        names(db) <- file.path(basename(dir), "man", names(db))
        calls <-
            c(calls,
              Filter(length,
                     lapply(db,
                            function(e) {
                                f <- tempfile()
                                on.exit(unlink(f))
                                Rd2ex(e, f)
                                if(file.exists(f))
                                    .worker(f, "UTF-8")
                            })))
    }
    
    calls
}

### ** .predicate_for_calls_with_names

.predicate_for_calls_with_names <-
function(funnames, pkgnames = character(), colons = c("::", ":::"))
{
    ## Use pkgnames = NA_character_ to match *any* PKG::FUN call with
    ## FUN in funnames.  Strange but why not?  Or better to use "*"?
    function(e) {
        (is.call(e) &&        
         ((is.name(x <- e[[1L]]) &&
           as.character(x) %in% funnames)) ||
         ((is.call(x <- e[[1L]]) &&
           is.name(x[[1L]]) &&
           (as.character(x[[1L]]) %in% colons) &&
           (((length(pkgnames) == 1L) && is.na(pkgnames)) ||
            as.character(x[[2L]]) %in% pkgnames) &&
           as.character(x[[3L]]) %in% funnames)))
    }
}

### ** .find_owner_env

.find_owner_env <-
function(v, env, last = NA, default = NA) {
    while(!identical(env, last))
        if(exists(v, envir = env, inherits = FALSE))
            return(env)
        else
            env <- parent.env(env)
    default
}

### ** .find_tidy_cmd

.find_tidy_cmd <-
function(Tidy = Sys.getenv("R_TIDYCMD", "tidy"))
{
    ## Require a recent enough version of HTML Tidy.
    ## We really need HTML Tidy 5.0.0 or later, and all these versions
    ## should have tidy --version match
    ##   ^HTML Tidy .*version (\\d+\\.\\d+\\.\\d+)
    ## See
    ## <https://github.com/htacg/tidy-html5/blob/next/README/VERSION.md>
    ## and
    ## <https://bugs.r-project.org/show_bug.cgi?id=18731>.
    msg <- ""
    OK <- nzchar(Sys.which(Tidy))
    if(OK) {
        ver <- system2(Tidy, "--version", stdout = TRUE)
        ## Argh.  We used to match with
        ##   ^HTML Tidy .*version (\\d+\\.\\d+\\.\\d+)$
        ## but HTML Tidy 5.8.0 has added l10n to its version info.  For
        ## now, this always seems to match
        ##   ^HTML Tidy .* (\\d+\\.\\d+\\.\\d+)$
        ## if this changes, we could try getting the version info with
        ## LC_MESSAGES= (set to empty) which seems to get the English
        ## default.
        mat <- regexec("^HTML Tidy .* (\\d+\\.\\d+\\.\\d+)$", ver)
        ver <- regmatches(ver, mat)[[1L]][2L]
        OK <- !is.na(ver)
        if(OK) {
            ## Minimum version requirement.
            req <- "5.0.0"
            OK <- numeric_version(ver) >= req
            if(!OK)
                msg <-
                    sprintf("'%s' is too old: need version %s, found %s",
                            Tidy, req, ver)
        } else
            msg <-
                sprintf("'%s' doesn't look like recent enough HTML Tidy",
                        Tidy)
    } else msg <- sprintf("no command '%s' found", Tidy)
    if(nzchar(msg)) {
        Tidy <- ""
        attr(Tidy, "msg") <- msg
    }
    Tidy
}

### ** .get_BibTeX_errors_from_blg_file

.get_BibTeX_errors_from_blg_file <-
function(con)
{
    ## Get BibTeX error info, using non-header lines until the first
    ## warning or summary, hoping for the best ...
    lines <- readLines(con, warn = FALSE)
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE))))
        lines[ind] <- iconv(lines[ind], "", "", sub = "byte")

    ## How can we find out for sure that there were errors?  Try
    ## guessing ... and peeking at tex-buf.el from AUCTeX.
    really_has_errors <-
        (any(startsWith(lines, "---")) ||
         regexpr("There (was|were) ([0123456789]+) error messages?",
                 lines[length(lines)]) > -1L)
    ## (Note that warnings are ignored for now.)
    ## MiKTeX does not give usage, so '(There were n error messages)' is
    ## last.
    pos <- grep("^(Warning|You|\\(There)", lines)
    if(!really_has_errors || !length(pos) ) return(character())
    ind <- seq.int(from = 3L, length.out = pos[1L] - 3L)
    lines[ind]
}

### ** .get_LaTeX_errors_from_log_file

.get_LaTeX_errors_from_log_file <-
function(con, n = 4L)
{
    ## Get (La)TeX lines with error plus n (default 4) lines of trailing
    ## context.
    lines <- readLines(con, warn = FALSE)
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE))))
        lines[ind] <- iconv(lines[ind], "", "", sub = "byte")

    ## Try matching both the regular error indicator ('!') as well as
    ## the file line error indicator ('file:line:').
    pos <- grep("(^! |^!pdfTeX error:|:[0123456789]+:.*[Ee]rror)", lines)
    ## the latter will miss some error messages like "Missing $ inserted"
    ## (a more general pattern caught false positives)
    ## Errors are typically of the form
    ## ! LaTeX Error:
    ## !pdfTeX error:
    ## ! Emergency stop
    ## !  ==> Fatal error occurred, no output PDF file produced!
    ## .../pegas.Rcheck/inst/doc/ReadingFiles.tex:395: Package inputenc Error:
    if(!length(pos)) return(character())
    ## Error chunk extends to at most the next error line.
    mapply(function(from, to) paste(lines[from : to], collapse = "\n"),
           pos, pmin(pos + n, c(pos[-1L], length(lines))))
}

### ** .get_internal_S3_generics

.get_internal_S3_generics <-
function(primitive = TRUE) # primitive means 'include primitives'
{
    c(.internalGenerics,
      if(primitive)
          c("[", "[[", "$", "[<-", "[[<-", "$<-", "@", "@<-",
            ## The above are actually primitive but not listed in
            ## base::.S3PrimitiveGenerics et al: not sure why?
            .get_S3_primitive_generics()
            ## ^^^^^^^ now contains the members of the group generics
            ## from groupGeneric.Rd.
            )
      )
}

### ** .get_namespace_package_depends

.get_namespace_package_depends <-
function(dir, selective_only = FALSE)
{
    nsInfo <- .check_namespace(dir)
    getter <- if(selective_only) {
        function(e) {
            if(is.list(e) && length(e[[2L]])) e[[1L]] else character()
        }
    } else {
        function(e) e[[1L]]
    }
    depends <- c(lapply(nsInfo$imports, getter),
                 lapply(nsInfo$importClasses, getter),
                 lapply(nsInfo$importMethods, getter))
    unique(sort(as.character(unlist(depends, use.names = FALSE))))
}

### ** .get_namespace_S3_methods_db

.get_namespace_S3_methods_db <-
function(nsInfo)
{
    ## Get the registered S3 methods for an 'nsInfo' object returned by
    ## parseNamespaceFile(), as a 3-column character matrix with the
    ## names of the generic, class and method (as a function).
    S3_methods_db <- nsInfo$S3methods
    if(!length(S3_methods_db))
        return(matrix(character(), ncol = 4L))
    idx <- is.na(S3_methods_db[, 3L])
    S3_methods_db[idx, 3L] <-
        paste(S3_methods_db[idx, 1L],
              S3_methods_db[idx, 2L],
              sep = ".")
    S3_methods_db
}

### ** .get_namespace_S3_methods_with_homes

.get_namespace_S3_methods_with_homes <-
function(package, lib.loc = NULL)
{
    ## Get the registered S3 methods with the 'homes' of the generics
    ## they are registered for.
    ## Original code provided by Luke Tierney.
    path <- system.file(package = package, lib.loc = lib.loc)
    if(!nzchar(path)) return(NULL)
    if(package == "base") {
        len <- nrow(.S3_methods_table)
        return(list2DF(list(generic = .S3_methods_table[, 1L],
                            home = rep_len("base", len),
                            class = .S3_methods_table[, 2L],
                            delayed = rep_len(FALSE, len))))
    }
    lib.loc <- dirname(path)
    nsinfo <- parseNamespaceFile(package, lib.loc)
    S3methods <- nsinfo$S3methods
    if(!length(S3methods)) return(NULL)
    tab <- NULL
    ind <- is.na(S3methods[, 4L])
    if(!all(ind)) {
        ## Delayed registrations can be handled directly.
        pos <- which(!ind)
        tab <- list2DF(list(generic = S3methods[pos, 1L],
                            home = S3methods[pos, 4L],
                            class = S3methods[pos, 2L],
                            delayed = rep_len(TRUE, length(pos))))
        S3methods <- S3methods[ind, , drop = FALSE]
    }
    generic <- S3methods[, 1L]
    nsenv <- loadNamespace(package, lib.loc)
    ## Possibly speed things up by only looking up the unique generics.
    generics <- unique(generic)
    homes <- character(length(generics))
    ind <- is.na(match(generics, .get_S3_group_generics()))
    homes[ind] <-
        unlist(lapply(generics[ind],
                      function(f) {
                          f <- get(f, nsenv)
                          getNamespaceName(topenv(environment(f)))
                      }),
               use.names = FALSE)
    ## S3 group generics belong to base.
    homes[!ind] <- "base"
    rbind(list2DF(list(generic = generic,
                       home = homes[match(generic, generics)],
                       class = S3methods[, 2L],
                       delayed = rep_len(FALSE, length(generic)))),
          tab)
}

### ** .get_package_metadata

.get_package_metadata <-
function(dir, installed = FALSE)
{
    ## Get the package DESCRIPTION metadata for a package with root
    ## directory 'dir'.  If an unpacked source (uninstalled) package,
    ## base packages (have only a DESCRIPTION.in file with priority
    ## "base") need special attention.
    dir <- file_path_as_absolute(dir)
    dfile <- file.path(dir, "DESCRIPTION")
    if(file_test("-f", dfile)) return(.read_description(dfile))
    if(installed) stop("File 'DESCRIPTION' is missing.")
    dfile <- file.path(dir, "DESCRIPTION.in")
    if(file_test("-f", dfile))
        meta <- .read_description(dfile)
    else
        stop("Files 'DESCRIPTION' and 'DESCRIPTION.in' are missing.")
    if(identical(as.character(meta["Priority"]), "base")) return(meta)
    stop("invalid package layout")
}

### ** .get_requires_from_package_db

.get_requires_from_package_db <-
function(db,
         category = c("Depends", "Imports", "LinkingTo", "VignetteBuilder",
         "Suggests", "Enhances", "RdMacros"))
{
    category <- match.arg(category)
    if(category %in% names(db)) {
        requires <- unlist(strsplit(db[category], ","))
        requires <-
            sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1", requires)
        if(category == "Depends")
            requires <- requires[requires != "R"]
    }
    else
        requires <- character()
    requires
}

### ** .get_requires_with_version_from_package_db

.get_requires_with_version_from_package_db <-
function(db,
         category = c("Depends", "Imports", "LinkingTo", "VignetteBuilder",
         "Suggests", "Enhances"))
{
    category <- match.arg(category)
    if(category %in% names(db)) {
        res <- .split_dependencies(db[category])
        if(category == "Depends") res[names(res) != "R"] else res
    } else list()
}

### ** .get_S3_generics_as_seen_from_package

## .get_S3_generics_as_seen_from_package <-
## function(dir, installed = TRUE, primitive = FALSE)
## {
##     ## Get the S3 generics "as seen from a package" rooted at
##     ## @code{dir}.  Tricky ...
##     if(basename(dir) == "base")
##         env_list <- list()
##     else {
##         ## Always look for generics in the whole of the former base.
##         ## (Not right, but we do not perform run time analyses when
##         ## working off package sources.)  Maybe change this eventually,
##         ## but we still cannot rely on packages to fully declare their
##         ## dependencies on base packages.
##         env_list <-
##             list(baseenv(),
##                  as.environment("package:graphics"),
##                  as.environment("package:stats"),
##                  as.environment("package:utils"))
##         if(installed) {
##             ## Also use the loaded namespaces and attached packages
##             ## listed in the DESCRIPTION Depends and Imports fields.
##             ## Not sure if this is the best approach: we could also try
##             ## to determine which namespaces/packages were made
##             ## available by loading the package (which should work at
##             ## least when run from R CMD check), or we could simply
##             ## attach every package listed as a dependency ... or
##             ## perhaps do both.
##             db <- .read_description(file.path(dir, "DESCRIPTION"))
##             depends <- .get_requires_from_package_db(db, "Depends")
##             imports <- .get_requires_from_package_db(db, "Imports")
##             reqs <- intersect(c(depends, imports), loadedNamespaces())
##             if(length(reqs))
##                 env_list <- c(env_list, lapply(reqs, getNamespace))
##             reqs <- intersect(setdiff(depends, loadedNamespaces()),
##                               .packages())
##             if(length(reqs))
##                 env_list <- c(env_list, lapply(reqs, .package_env))
##             env_list <- unique(env_list)
##         }
##     }
##     ## some BioC packages warn here
##     suppressWarnings(
##     unique(c(.get_internal_S3_generics(primitive),
##              unlist(lapply(env_list, .get_S3_generics_in_env))))
##     )
## }

### ** .get_S3_generics_in_base

.get_S3_generics_in_base <-
function()
{
    ## .get_S3_generics_in_env(.BaseNamespaceEnv) gets all UseMethod
    ## generics.
    ## .get_internal_S3_generics() gets the internal S3 generics.  By
    ## default this also adds the primitive generics.
    ## .get_S3_group_generics() gets the S3 group generics.
    ## Note that
    ##   .make_S3_group_generic_env()
    ## generates an env with the group generics and appropriate
    ## signatures, so we should always have
    ##    identical(sort(.get_S3_group_generics()),
    ##              sort(names(.make_S3_group_generic_env())))
    ## and that
    ##    .make_S3_primitive_generic_env()
    ##  generates and env with the primitive generics and appropriate
    ##  signatures (in turn using base::.GenericArgsEnv), so we should
    ##  always have
    ##    identical(sort(.get_S3_primitive_generics()),
    ##              sort(names(.make_S3_primitive_generic_env())))
    c(.get_S3_generics_in_env(.BaseNamespaceEnv),
      .get_internal_S3_generics(),
      .get_S3_group_generics())
}

### ** .get_S3_generics_in_env

.get_S3_generics_in_env <-
function(env, nms = NULL)
{
    if(is.null(nms))
        nms <- sort(names(env))
    if(".no_S3_generics" %in% nms)
        character()
    else
        Filter(function(f) .is_S3_generic(f, envir = env), nms)
}

### ** .get_S3_group_generics

.get_S3_group_generics <-
function()
    c("Ops", "Math", "Summary", "Complex", "matrixOps")

### ** .get_S3_primitive_generics

.get_S3_primitive_generics <-
function(include_group_generics = TRUE)
{
    if(include_group_generics)
        c(base::.S3PrimitiveGenerics,
          ## Keep this in sync with ? groupGeneric:
          ## Group 'Math':
          "abs", "sign", "sqrt",
          "floor", "ceiling", "trunc",
          "round", "signif",
          "exp", "log", "expm1", "log1p",
          "cos", "sin", "tan",
          "cospi", "sinpi", "tanpi",
          "acos", "asin", "atan",
          "cosh", "sinh", "tanh",
          "acosh", "asinh", "atanh",
          "lgamma", "gamma", "digamma", "trigamma",
          "cumsum", "cumprod", "cummax", "cummin",
          ## Group 'Ops':
          "+", "-", "*", "/",
          "^", "%%", "%/%",
          "&", "|", "!",
          "==", "!=",
          "<", "<=", ">=", ">",
          ## Group 'Summary':
          "all", "any", "sum", "prod", "max", "min", "range",
          ## Group 'Complex':
          "Arg", "Conj", "Im", "Mod", "Re",
          ## Group 'matrixOps'
          "%*%")
    else
        base::.S3PrimitiveGenerics
}

### ** .get_standard_Rd_keywords

.get_standard_Rd_keywords <-
function()
{
    lines <- readLines(file.path(R.home("doc"), "KEYWORDS.db"))
    lines <- grep("^.*\\|([^:]*):.*", lines, value = TRUE)
    lines <- sub( "^.*\\|([^:]*):.*", "\\1", lines)
    lines
}

### ** .get_standard_package_names

standard_package_names <-
.get_standard_package_names <-
local({
    lines <- readLines(file.path(R.home("share"), "make", "vars.mk"))
    lines <- grep("^R_PKGS_[[:upper:]]+ *=", lines, value = TRUE)
    out <- strsplit(sub("^R_PKGS_[[:upper:]]+ *= *", "", lines), " +")
    names(out) <-
        tolower(sub("^R_PKGS_([[:upper:]]+) *=.*", "\\1", lines))
    eval(substitute(function() {out}, list(out=out)), envir = topenv())
    })


### ** .get_standard_package_dependencies

.get_standard_package_dependencies <-
function(reverse = FALSE, recursive = FALSE)
{
    names <- unlist(.get_standard_package_names())
    paths <- file.path(.Library, names, "DESCRIPTION")
    ## Be nice ...
    paths <- paths[file.exists(paths)]
    which <- c("Depends", "Imports")
    fields <- c("Package", which)
    ## Create a minimal available packages db.
    a <- do.call(rbind,
                 lapply(paths,
                        function(p) .read_description(p)[fields]))
    colnames(a) <- fields
    package_dependencies(names, a, which = which,
                         reverse = reverse, recursive = recursive)
}

### ** .get_standard_repository_URLs

## Usage in e.g. CRAN_baseurl_for_web_area assumes this returns a
## valid CRAN mirror as its first element.
## That used not to be guaranteed, and it is still unchecked.
.get_standard_repository_URLs <-
function(ForXrefs = FALSE)
 {
     if(ForXrefs &&
        nzchar(repos <- Sys.getenv("_R_CHECK_XREFS_REPOSITORIES_", "")))
         return(utils:::.expand_BioC_repository_URLs(strsplit(repos, " +")[[1L]]))

     nms <- c("CRAN", "BioCsoft", "BioCann", "BioCexp")
     repos <- getOption("repos")
     ## This is set by utils:::.onLoad(), hence may be NULL.
     if(!is.null(repos) && !anyNA(repos[nms]) && (repos["CRAN"] != "@CRAN@"))
         repos <- repos[nms]
     else {
         repos <- utils:::.get_repositories()[nms, "URL"]
         names(repos) <- nms
         ## That might not contain an entry for CRAN
         if(is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@")
             repos["CRAN"] <- "https://CRAN.R-project.org"
     }
     repos
}

.get_CRAN_repository_URL <-
function()
 {
     repos <- getOption("repos")
     if(!is.null(repos) && !is.na(cr <- repos["CRAN"]) && (cr != "@CRAN@"))
         return(cr)
     cr <- utils:::.get_repositories()["CRAN", "URL"]
     ## That might not contain an entry for CRAN
     if(is.na(cr) || cr == "@CRAN@") cr <- "https://CRAN.R-project.org"
     cr
 }

### ** .get_standard_repository_db_fields

.get_standard_repository_db_fields <-
function(type = c("source", "mac.binary", "win.binary")) {
    type <- match.arg(type)
    c("Package", "Version", "Priority",
      "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
      "License", "License_is_FOSS", "License_restricts_use",
      "OS_type", "Archs", "MD5sum",
      if(type == "source") "NeedsCompilation"
      )
}

### ** .get_standard_DESCRIPTION_fields

.get_standard_DESCRIPTION_fields <-
function()
{
    unique(c(.get_standard_repository_db_fields(),
             ## Extract from R-exts via
             ## .get_DESCRIPTION_fields_in_R_exts():
             c("Additional_repositories",
               "Author",
               "Authors@R",
               "Biarch",
               "BugReports",
               "BuildKeepEmpty",
               "BuildManual",
               "BuildResaveData",
               "BuildVignettes",
               "Built",
               "ByteCompile",
               "Classification/ACM",
               "Classification/ACM-2012",
               "Classification/JEL",
               "Classification/MSC",
               "Classification/MSC-2010",
               "Collate",
               "Collate.unix",
               "Collate.windows",
               "Contact",
               "Copyright",
               "Date",
               "Depends",
               "Description",
               "Encoding",
               "Enhances",
               "Imports",
               "KeepSource",
               "Language",
               "LazyData",
               "LazyDataCompression",
               "LazyLoad",
               "License",
               "LinkingTo",
               "MailingList",
               "Maintainer",
               "Note",
               "OS_type",
               "Package",
               "Packaged",
               "Priority",
               "RdMacros",
               "Suggests",
               "StagedInstall",
               "SysDataCompression",
               "SystemRequirements",
               "Title",
               "Type",
               "URL",
               "UseLTO",
               "Version",
               "VignetteBuilder",
               "ZipData"),
             ## Others: adjust as needed.
             c("Repository",
               "Path",
               "Date/Publication",
               "LastChangedDate",
               "LastChangedRevision",
               "Revision",
               "RcmdrModels",
               "RcppModules",
               "Roxygen",
               "Acknowledgements",
               "Acknowledgments", # USA/Canadian usage.
               "biocViews")
             ))
}

### ** .get_DESCRIPTION_fields_in_R_exts

.get_DESCRIPTION_fields_in_R_exts <-
function(texi = NULL)
{
    if(is.null(texi))
        texi <- file.path(.R_top_srcdir_from_Rd(),
                          "doc", "manual", "R-exts.texi")
    lines <- readLines(texi)
    re <- "^@c DESCRIPTION field "
    sort(unique(sub(re, "", lines[grepl(re, lines)])))
}

### ** .get_top_call_in_fun

.get_top_call_in_fun <-
function(f)
{
    b <- body(f)
    repeat {
        if(!is.call(b)) return(NULL)
        if((length(b) > 1L) && (b[[1L]] == as.name("{")))
            b <- b[[2L]]
        else
            break
    }
    b
}

### ** .gregexec_at_pos

.gregexec_at_pos <-
function(pattern, x, m, pos)
{
    unlist(lapply(regmatches(x, m),
                  function(e)
                      do.call(rbind,
                              regmatches(e,
                                         regexec(pattern, e)))[, pos]
                  ),
           use.names = FALSE)
}

### ** .gsub_with_transformed_matches

.gsub_with_transformed_matches <-
function(pattern, replacement, x, trafo, count, ...)
{
    ## gsub() with replacements featuring transformations of matches.
    ##
    ## Character string (%s) conversion specifications in 'replacement'
    ## will be replaced by applying the respective transformations in
    ## 'trafo' to the respective matches (parenthesized subexpressions of
    ## 'pattern') specified by 'count'.
    ##
    ## Argument 'trafo' should be a single unary function, or a list of
    ## such functions.
    ## Argument 'count' should be a vector of with the numbers of
    ## parenthesized subexpressions to be transformed (0 gives the whole
    ## match).

    replace <- function(yi) {
        do.call(sprintf,
                c(list(replacement),
                  Map(function(tr, co) fsub("\\", "\\\\", tr(yi[co])),
                      trafo, count + 1L)))
    }

    if(!is.list(trafo)) trafo <- list(trafo)
    m <- gregexpr(pattern, x, ...)
    v <- lapply(regmatches(x, m),
                function(e) {
                    y <- regmatches(e, regexec(pattern, e, ...))
                    unlist(Map(function(ei, yi) {
                        sub(pattern, replace(yi), ei, ...)
                    },
                               e,
                               y))
                })
    regmatches(x, m) <- v
    x
}

### imports_for_undefined_globals

imports_for_undefined_globals <-
function(txt, lst, selective = TRUE)
{
    if(!missing(txt))
        lst <- scan(what = character(), text = txt, quiet = TRUE)
    lst <- sort(unique(lst))
    nms <- lapply(lst, utils::find)
    ind <- lengths(nms) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
        sprintf("importFrom(%s)",
                vapply(Map(c, names(imp), imp),
                       function(e)
                           paste0("\"", e, "\"", collapse = ", "),
                       ""))
    } else {
        sprintf("import(\"%s\")", names(imp))
    }
}

### ** .is_ASCII

.is_ASCII <-
function(x)
{
    ## Determine whether the strings in a character vector are ASCII or
    ## not.
    vapply(as.character(x),
           function(txt) all(charToRaw(txt) <= as.raw(127)),
           NA)
}

### ** .is_ISO_8859

.is_ISO_8859 <-
function(x)
{
    ## Determine whether the strings in a character vector could be in
    ## some ISO 8859 character set or not.
    raw_ub <- as.raw(0x7f)
    raw_lb <- as.raw(0xa0)
    vapply(as.character(x),
           function(txt) {
               raw <- charToRaw(txt)
               all(raw <= raw_ub | raw >= raw_lb)
           },
           NA)
}

### ** .is_primitive_in_base

.is_primitive_in_base <-
function(fname)
{
    ## Determine whether object named 'fname' found in the base
    ## environment is a primitive function.
    is.primitive(baseenv()[[fname]])
}

### ** .is_S3_generic

.is_S3_generic <-
function(fname, envir, mustMatch = TRUE)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is (to be considered) an S3 generic function.  Note,
    ## found *in* not found *from*, so envir does not have a default.
    ##
    ## If it is, does it dispatch methods of fname?  We need that to
    ## look for possible methods as functions named fname.* ....
    ##
    ## Provided by LT with the following comments:
    ##
    ## This is tricky.  Figuring out what could possibly dispatch
    ## successfully some of the time is pretty much impossible given R's
    ## semantics.  Something containing a literal call to UseMethod is
    ## too broad in the sense that a UseMethod call in a local function
    ## doesn't produce a dispatch on the outer function ...
    ##
    ## If we use something like: a generic has to be
    ##      function(e) <UME>  # UME = UseMethod Expression
    ## with
    ##      <UME> = UseMethod(...) |
    ##             if (...) <UME> [else ...] |
    ##             if (...) ... else <UME>
    ##             { ... <UME> ... }
    ## then a recognizer for UME might be as follows.

    f <- suppressMessages(get(fname, envir = envir, inherits = FALSE))
    if(!is.function(f)) return(FALSE)
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1L])) if (nzchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3L) isUME(e[[3L]])
        else {
            if (nzchar(res <- isUME(e[[3L]]))) res
            else if (nzchar(res <- isUME(e[[4L]]))) res
            else ""
        }

    }
    isUME <- function(e) {
        if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
            switch(as.character(e[[1L]]),
                   UseMethod = as.character(e[[2L]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    res <- isUME(body(f))
    if(mustMatch) res == fname else nzchar(res)
}

### ** .load_namespace_quietly

.load_namespace_quietly <-
function(package, lib.loc) {
    if(package != "base")
        .try_quietly(loadNamespace(package, lib.loc))
}

### ** .load_namespace_rather_quietly

.load_namespace_rather_quietly <-
function(package)
{
    ## Suppress messages and warnings from loading namespace
    ## dependencies.
    .whandler <- function(e) {
        calls <- sys.calls()
        if(sum(.call_names(calls) == "loadNamespace") == 1L)
            signalCondition(e)
        else
            tryInvokeRestart("muffleWarning")
    }
    expr <- substitute(loadNamespace(package), list(package = package))
    invisible(withCallingHandlers(suppressMessages(eval(expr)),
                                  warning = .whandler))
}

### ** .load_package_quietly

.load_package_quietly <-
function(package, lib.loc)
{
    ## Quietly ensure that package @code{package} is loaded and
    ## attached.
    ## If not yet loaded, look for the package in @code{lib.loc}.
    ## Otherwise, we do not attempt reloading: previously we tried at
    ## least when attached, but reloading namespaces invalidates DLLs
    ## and S3 registries, see e.g. PR#18130
    ## <https://bugs.r-project.org/show_bug.cgi?id=18130>.
    ## Hence if already loaded, we can neither ensure that the package
    ## came from @code{lib.loc}, nor that we used the currently
    ## installed versions.
    ## Don't do anything for base.
    ##
    ## All QC functions use this for loading packages because R CMD
    ## check interprets all output as indicating a problem.
    if(package != "base")
        .try_quietly({
            pos <- match(paste0("package:", package), search())
            if(!is.na(pos)) {
                detach(pos = pos)
                ## Presumably this should use
                ## <CODE>
                ##   detach(pos, force = TRUE)
                ## </CODE>
                ## to always detach?
                ## Or perhaps simply leave things as they are?
            }
            library(package, lib.loc = lib.loc, character.only = TRUE,
                    verbose = FALSE)
        })
}

### ** .make_file_exts

## <FIXME>
## Remove support for type "vignette" eventually ...
## </FIXME>

.make_file_exts <-
function(type = c("code", "data", "demo", "docs", "vignette"))
{
    ## Return a character vector with the possible/recognized file
    ## extensions for a given file type.
    switch(type,
           code = c("R", "r", "S", "s", "q"),
           ## Keep in sync with the order given in base's data.Rd.
           data = c("R", "r",
                    "RData", "rdata", "rda",
                    "tab", "txt", "TXT",
                    "tab.gz", "txt.gz",
                    "tab.bz2", "txt.bz2",
                    "tab.xz", "txt.xz",
                    "csv", "CSV",
                    "csv.gz", "csv.bz2", "csv.xz"),
           demo = c("R", "r"),
           docs = c("Rd", "rd"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste0),
                        "Rmd"))
}

### ** .make_S3_group_generic_env

.make_S3_group_generic_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions for the S3 group
    ## methods.
    env <- new.env(parent = parent) # small
    assign("Math", function(x, ...) UseMethod("Math"),
           envir = env)
    assign("Ops", function(e1, e2) UseMethod("Ops"),
           envir = env)
    assign("matrixOps", function(x, y) UseMethod("matrixOps"),
           envir = env)
    assign("Summary", function(..., na.rm = FALSE) UseMethod("Summary"),
           envir = env)
    assign("Complex", function(z) UseMethod("Complex"),
           envir = env)
    env
}

### ** .make_S3_primitive_generic_env

.make_S3_primitive_generic_env <-
function(parent = parent.frame(), fixup = FALSE)
{
    ## Create an environment with pseudo-definitions for the S3 primitive
    ## generics
    env <- list2env(as.list(base::.GenericArgsEnv, all.names=TRUE),
                    hash=TRUE, parent=parent)
    if(fixup) {
        ## now fixup the operators from (e1,e2) to (x,y)
        for(f in c('+', '-', '*', '/', '^', '%%', '%/%', '&', '|',
                   '==', '!=', '<', '<=', '>=', '>')) {
            fx <- get(f, envir = env)
            formals(fx) <- alist(x=, y=)
            assign(f, fx, envir = env)
        }
    }
    env
}

### ** .make_S3_primitive_nongeneric_env

## why not just use  base::.ArgsEnv -- is the parent really important if(is_base)?
.make_S3_primitive_nongeneric_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions
    ## for the S3 primitive non-generics
    list2env(as.list(base::.ArgsEnv, all.names=TRUE),
             hash=TRUE, parent=parent)
}

### ** .make_KaTeX_checker

.make_KaTeX_checker <- local({
    fun <- NULL
    ctx <- NULL
    function() {
        if(is.null(fun) && requireNamespace("V8", quietly = TRUE)) {
            dir <- file.path(R.home("doc"), "html")
            ctx <<- V8::v8("window")
            ctx$source(file.path(dir, "katex", "katex.js"))
            ## Provides additional macros:
            ctx$source(file.path(dir, "katex-config.js"))
            ## Provides checkTex():
            ctx$source(file.path(dir, "katex-check.js"))
            fun <<- function(tex) ctx$call('checkTex', tex)
        }
        fun
    }
})

### ** .make_RFC4646_langtag_regexp

.make_RFC4646_langtag_regexp <-
function()
{
    ## See <https://www.ietf.org/rfc/rfc4646.html>.
    ## Language tags can be of the form (in ABNF, see
    ## <https://tools.ietf.org/rfc/rfc4234.txt>): 
    ##   langtag / privateuse / grandfathered
    ## where
    ##   privateuse    = ("x"/"X") 1*("-" (1*8alphanum))
    ##   grandfathered = 1*3ALPHA 1*2("-" (2*8alphanum))
    ## We only allow langtag, for which in turn we have
    ##   (language
    ##    ["-" script]
    ##    ["-" region]
    ##    *(["-" variant])
    ##    *(["-" extension])
    ##    ["-" privateuse]
    ## where
    ##   language    = (2*3ALPHA [-extlang])  ; shortest ISO 639 code
    ##                  / 4ALPHA              ; reserved for future use
    ##                  / 5*8ALPHA            ; registered language subtag
    ##   extlang     = *3("-" 3*ALPHA)        ; reserved for future use
    ##   script      = 4ALPHA                 ; ISO 15924 code
    ##   region      = 2ALPHA                 ; ISO 3166 code
    ##                 / 3DIGIT               ; UN M.49 code
    ##   variant     = 5*8alphanum            ; registered variants
    ##                 / (DIGIT 3alphanum)
    ##   extension   = singleton 1*("-" (2*8alphanum))
    ##   singleton   = %x41-57 / %x59-5A / %x61-77 / %x79-7A / DIGIT
    ##               ; "a"-"w" / "y"-"z" / "A"-"W" / "Y"-"Z" / "0"-"9"
    ##   alphanum    = (ALPHA / DIGIT)        ; letters and numbers

    re_extlang <- "[[:alpha:]]{3}"
    re_language <-
        sprintf("[[:alpha:]]{2,3}(-%s){0,3}|[[:alpha:]]{4,8}", re_extlang)
    re_script <- "[[:alpha:]]{4}"
    re_region <- "[[:alpha:]]{2}|[[:digit:]]{3}"
    re_variant <- "[[:alnum:]]{5,8}|[[:digit:]][[:alnum:]]{3}"
    re_singleton <- "[abcdefghijklmnopqrstuvwyzABCDEFGHIJKLMNOPQRSTUVWYZ0123456789]"
    re_extension <- sprintf("(%s)(-[[:alnum:]]{2,8}){1,}", re_singleton)

    sprintf("(%s)((-%s)?)((-%s)?)((-%s)*)((-%s)*)",
            re_language, re_script, re_region, re_variant, re_extension)
}
    
### ** nonS3methods [was .make_S3_methods_stop_list ]

nonS3methods <- function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    ## Using package = NULL returns all known examples

    stopList <-
        list(base = c("all.equal", "all.names", "all.vars",
             "as.data.frame.vector",
             "format.info", "format.pval",
             "max.col",
             ## the next two only exist in *-defunct.Rd.
             ## "print.atomic", "print.coefmat",
             "qr.Q", "qr.R", "qr.X", "qr.coef", "qr.fitted", "qr.qty",
             "qr.qy", "qr.resid", "qr.solve",
             "rep.int", "seq.int", "sort.int", "sort.list"),
             AMORE = "sim.MLPnet",
             BSDA = "sign.test",
             BiocGenerics = "rep.int",
             ChemometricsWithR = "lda.loofun",
             ElectoGraph = "plot.wedding.cake",
             FrF2 = "all.2fis.clear.catlg",
             GLDEX = c("hist.su", "pretty.su"),
             Hmisc = c("abs.error.pred", "all.digits", "all.is.numeric",
                       "format.df", "format.pval", "t.test.cluster"),
             HyperbolicDist = "log.hist",
             MASS = c("frequency.polygon", "gamma.dispersion", "gamma.shape",
                      "hist.FD", "hist.scott"),
             LinearizedSVR = "sigma.est",
             ## FIXME: since these are already listed with 'base',
             ##        they should not need to be repeated here:
             Matrix = c("qr.Q", "qr.R", "qr.coef", "qr.fitted",
                        "qr.qty", "qr.qy", "qr.resid"),
             PerformanceAnalytics = c("mean.LCL", "mean.UCL",
                                      "mean.geometric", "mean.stderr"),
             RCurl = "merge.list",
             RNetCDF = c("close.nc", "dim.def.nc", "dim.inq.nc",
                         "dim.rename.nc", "open.nc", "print.nc"),
             Rmpfr = c("mpfr.is.0", "mpfr.is.integer"),
             SMPracticals = "exp.gibbs",
             SparseM = c("as.matrix.csc","as.matrix.csr", "as.matrix.ssc", "as.matrix.ssr", "as.matrix.coo",
                         "is.matrix.csc","is.matrix.csr", "is.matrix.ssc", "is.matrix.ssr", "is.matrix.coo"),
             TANOVA = "sigma.hat",
             TeachingDemos = "sigma.test",
             XML = "text.SAX",
             ape = "sort.index",
             arm = "sigma.hat", # lme4 has sigma()
             assist = "chol.new",
             boot = "exp.tilt",
             car = "scatterplot.matrix",
             calibrator = "t.fun",
             clusterfly = "ggobi.som",
             coda = "as.mcmc.list",
             crossdes = "all.combn",
             ctv = "update.views",
             deSolve = "plot.1D",
             effects = "all.effects", # already deprecated
             elliptic = "sigma.laurent",
             equivalence = "sign.boot",
             fields = c("qr.q2ty", "qr.yq2"),
             gbm = c("pretty.gbm.tree", "quantile.rug"),
             genetics = "diseq.ci",
             gpclib = "scale.poly",
             grDevices = "boxplot.stats",
             graphics = c("close.screen", "plot.design", "plot.new",
                          "plot.window", "plot.xy", "split.screen"),
             ic.infer = "all.R2",
             hier.part = "all.regs",
             lasso2 = "qr.rtr.inv",
             latticeExtra = "xyplot.list",
             locfit = c("density.lf", "plot.eval"),
             moments = c("all.cumulants", "all.moments"),
             mosaic = "t.test",
             mratios = c("t.test.ration", "t.test.ratio.default",
                         "t.test.ratio.formula"),
             ncdf = c("open.ncdf", "close.ncdf",
                      "dim.create.ncdf", "dim.def.ncdf",
                      "dim.inq.ncdf", "dim.same.ncdf"),
             plyr = c("rbind.fill", "rbind.fill.matrix"),
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             reshape = "all.vars.character",
             rgeos = "scale.poly",
             rowr = "cbind.fill",
             sac = "cumsum.test",
             sfsmisc = "cumsum.test",
             sm = "print.graph",
             spatstat = "lengths.psp",
             splusTimeDate = "sort.list",
             splusTimeSeries = "sort.list",
	     stats = c("anova.lmlist", "expand.model.frame", "fitted.values",
		       "influence.measures", "lag.plot", "qr.influence", "t.test",
                       "plot.spec.phase", "plot.spec.coherency"),
             stremo = "sigma.hat",
             supclust = c("sign.change", "sign.flip"),
             tensorA = "chol.tensor",
             utils = c("close.socket", "flush.console", "update.packages"),
             wavelets = "plot.dwt.multiple"
             )
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if(!length(thisPkg)) character() else thisPkg
}

### ** .make_S3_methods_table_for_base

.make_S3_methods_table_for_base <-
function()
{
    env <- baseenv()
    objects <- ls(env, all.names = TRUE)
    ind <- vapply(objects,
                  function(o) .is_S3_generic(o, env),
                  FALSE)
    generics <- sort(unique(c(objects[ind],
                              .get_S3_group_generics(),
                              .get_internal_S3_generics())))
    ind <- grepl("^[[:alpha:]]", generics)
    generics <- c(generics[!ind], generics[ind])
    ## The foo.bar objects in base:
    objects <- grep("[^.]+[.][[:alpha:]]", objects, value = TRUE)
    ## Make our lives easier ...
    objects <- setdiff(objects, nonS3methods("base"))
    ## Find the ones matching GENERIC.CLASS from the list of generics.
    methods <-
        lapply(generics,
               function(e) objects[startsWith(objects, paste0(e, "."))])
    names(methods) <- generics
    ## Need to separate all from all.equal:
    methods$all <- methods$all[!startsWith(methods$all, "all.equal")]
    methods <- Filter(length, methods)
    classes <- Map(substring, methods, nchar(names(methods)) + 2L)

    cbind(generic = rep.int(names(classes), lengths(classes)),
          class = unlist(classes, use.names = FALSE))
}

.deparse_S3_methods_table_for_base <-
function()
{
    if(!identical("C", Sys.getlocale("LC_COLLATE")))
        warning("*not* using 'C' for LC_COLLATE locale")
    mdb <- .make_S3_methods_table_for_base()
    n <- nrow(mdb)
    c(sprintf("%s\"%s\", \"%s\"%s",
              c("matrix(c(", rep.int("         ", n - 1L)),
              mdb[, 1L],
              mdb[, 2L],
              c(rep.int(",", n - 1L), "),")),
      "       ncol = 2L, byrow = TRUE,",
      "       dimnames = list(NULL, c(\"generic\", \"class\")))")
}

### ** .package_apply

.package_apply <-
function(packages = NULL, FUN, ..., pattern = NULL, verbose = TRUE,
         Ncpus = getOption("Ncpus", 1L))
{
    ## Apply FUN and extra '...' args to all given packages.
    ## The default corresponds to all installed packages with high
    ## priority.
    if(is.null(packages))
        packages <-
            unique(utils::installed.packages(priority = "high")[ , 1L])

    if(!is.null(pattern))
        packages <- grepv(pattern, packages)

    ## Keep in sync with .unpacked_source_repository_apply().
    ## <FIXME>
    ## Should we really catch errors?
    one <- function(p) {
        if(verbose)
            message(sprintf("processing %s", p))
        tryCatch(FUN(p, ...), error = identity)
    }
    ## </FIXME>

    ## Would be good to have a common wrapper ...
    if(Ncpus > 1L) {
        if(.Platform$OS.type != "windows") {
            out <- parallel::mclapply(packages, one, mc.cores = Ncpus)
        } else {
            cl <- parallel::makeCluster(Ncpus)
            args <- list(FUN, ...)      # Eval promises.
            out <- parallel::parLapply(cl, packages, one)
            parallel::stopCluster(cl)
        }
    } else {
        out <- lapply(packages, one)
    }

    names(out) <- packages
    out
}

### ** .package_code_using_R_4.x_syntax

.package_code_using_R_4.x_syntax <-
function(dir)
{
    dir <- file_path_as_absolute(dir)
    wrk <- function(p, f) {
        x <- utils::getParseData(parse(p, keep.source = TRUE))
        i1 <- which(x$token %in% c("PIPE", "'\\\\'"))
        i2 <- which(x$token == "PLACEHOLDER")
        if(length(i1) || length(i2)) {
            xi <- x$id
            xp <- x$parent
            n1 <- rep_len("4.1.0", length(i1))
            ## Detect experimental placeholder feature as the head of a
            ## chain of extractions by looking at the first child of the
            ## grandparent of the placeholder: if it is the placeholder
            ## expression then we have the 4.3.0 syntax.
            n2 <- ifelse(vapply(i2,
                                function(j) {
                                    u <- xp[j]
                                    v <- xp[xi %in% u]
                                    min(xi[xp %in% v]) == u
                                },
                                NA),
                         "4.3.0",
                         "4.2.0")
            i <- c(i1, i2)
            data.frame(token = x$token[i],
                       needs = c(n1, n2),
                       text = utils::getParseText(x, xp[i]),
                       file = rep_len(f, length(i)))
        } else
            NULL
    }

    files <- list_files_with_type(file.path(dir, "R"), "code",
                                  full.names = FALSE,
                                  OS_subdirs = c("unix", "windows"))
    ## As of 2025-03, packages
    ##   gmailr httr2 purrr
    ## use configure code to drop the pipe using examples for R < 4.1.
    db <- if(basename(dir) %in% c("gmailr", "httr2", "purrr"))
              list()
          else
              Rd_db(dir = dir)

    do.call(rbind,
            c(Map(function(u, v) {
                      tryCatch({
                          wrk(u, v)
                      }, error = function(e) NULL)
                  },
                  file.path(dir, "R", files),
                  files,
                  USE.NAMES = FALSE),
              Map(function(u, v) {
                      tryCatch({
                          p <- tempfile()
                          on.exit(unlink(p))
                          ## Need to extract the code in the examples.
                          ## Rd2ex() does that and more, but provides no
                          ## output if there are no examples ...
                          Rd2ex(u, p)
                          if(file.exists(p))
                              wrk(p, v)
                      }, error = function(e) NULL)
                  },
                  db,
                  names(db),
                  USE.NAMES = FALSE)))
}

## ** .package_depends_on_R_at_least

.package_depends_on_R_at_least <-
function(dir, v)
{
    .package_metadata_has_depends_on_R_at_least(.get_package_metadata(dir),
                                                v)
}

### ** .package_metadata_has_depends_on_R_at_least

.package_metadata_has_depends_on_R_at_least <-
function(meta, v)
{
    for(dep in .split_description(meta)$Rdepends2) {
        if((dep$op == '>=') && (dep$version >= v)) return(TRUE)
    }
    FALSE
}
    
### ** .package_vignettes_via_call_to_R

.package_vignettes_via_call_to_R <-
function(dir, ..., libpaths = .libPaths()) {
    ## pkgVignettes() needs to load the namespaces of the vignette
    ## builders in order to find the vignette engines, and cannot unload
    ## again, which may be undesirable (e.g., when calling from the
    ## master check process *before* installing the package checked.
    ## pkgVignettes() has a lib.loc argument but that is not passed
    ## through to loadVignetteBuilder(), so we use .libPaths() instead.
    fun <- function(dir, ..., libpaths) {
        .libPaths(libpaths)
        pkgVignettes(dir = dir, ...)
    }
    R(fun, list(dir, ..., libpaths = libpaths), "--vanilla")
}

### ** .pandoc_md_for_CRAN

.pandoc_md_for_CRAN <-
function(ifile, ofile)
{
    .system_with_capture("pandoc",
                         paste(shQuote(normalizePath(ifile)),
                               "-s", "--mathjax",
                               "--email-obfuscation=references",
                               "-o", shQuote(ofile)))
}

### ** .parse_code_file

.parse_code_file <-
function(file, encoding = NA, keep.source = getOption("keep.source"))
{
    if(!file.exists(file) || !file.size(file)) return()
    suppressWarnings({
        if(!is.na(encoding) &&
           (encoding != "unknown") &&
           (Sys.getlocale("LC_CTYPE") %notin% c("C", "POSIX"))) {
            ## Previous use of con <- file(file, encoding = encoding)
            ## was intolerant so do something similar to what
            ## .install_package_code_files() does.  Do not use a #line
            ## directive though as this will confuse getParseData().
            lines <- iconv(readLines(file, warn = FALSE),
                           from = encoding, to = "", sub = "byte")
            parse(text = lines, srcfile = srcfile(file),
                  keep.source = keep.source)
        } else
            parse(file,
                  keep.source = keep.source)
    })
}

### ** .persons_from_metadata

.persons_from_metadata <- function(dir, meta = NULL) {
    if(is.null(meta))
        meta <- .get_package_metadata(dir)
    if(!is.na(aar <- meta["Authors@R"])) {
        aar <- tryCatch(utils:::.read_authors_at_R_field(aar),
                        error = identity)
        if(inherits(aar, "person"))
            return(aar)
    }
    NULL
}

### ** .persons_from_citation

.persons_from_citation <- function(dir, installed = FALSE) {
    meta <- .get_package_metadata(dir, installed = installed)
    path <- if(installed)
                "CITATION"
            else
                file.path("inst", "CITATION")
    cfile <- file.path(dir, path)
    cinfo <- .read_citation_quietly(cfile, meta)
    if(!inherits(cinfo, "error")) {
        aut <- do.call(c, lapply(unclass(cinfo), `[[`, "author"))
        if(inherits(aut, "person"))
            return(aut)
    }
    NULL
}

### ** .read_additional_repositories_field

.read_additional_repositories_field <-
function(txt)
    unique(unlist(strsplit(txt, ",[[:space:]]*")))

### ** .read_citation_quietly

.read_citation_quietly <-
function(cfile, meta)
{
    tryCatch(suppressMessages(suppressWarnings(utils::readCitationFile(cfile,
                                                                       meta))),
             error = identity)
}

### ** .read_collate_field

.read_collate_field <-
function(txt)
{
    ## Read Collate specifications in DESCRIPTION files.
    ## These consist of file paths relative to the R code directory,
    ## separated by white space, possibly quoted.  Note that we could
    ## have newlines in DCF entries but do not allow them in file names,
    ## hence we gsub() them out.
    con <- textConnection(gsub("\n", " ", txt, fixed=TRUE))
    on.exit(close(con))
    scan(con, what = character(), strip.white = TRUE, quiet = TRUE)
}

### ** .read_description

.keep_white_description_fields <-
    c("Description", "Authors@R", "Author", "Built", "Packaged")

.read_description <-
function(dfile, keep.white = .keep_white_description_fields)
{
    ## Try reading in package metadata from a DESCRIPTION file.
    ## (Never clear whether this should work on the path of the file
    ## itself, or on that of the directory containing it.)
    ## <NOTE>
    ## As we do not have character "frames", we return a named character
    ## vector.
    ## </NOTE>
    if(!file_test("-f", dfile))
        stop(gettextf("file '%s' does not exist", dfile), domain = NA)
    out <- tryCatch(read.dcf(dfile, keep.white = keep.white),
                    error = function(e)
                    stop(gettextf("file '%s' is not in valid DCF format",
                                  dfile),
                         domain = NA, call. = FALSE))
    if (nrow(out) != 1L)
        stop("contains a blank line", call. = FALSE)
    out <- out[1L, ]
    if(!is.na(encoding <- out["Encoding"])) {
        ## could convert everything (valid) to UTF-8
        if(encoding == "UTF-8") {
            Encoding(out) <- "UTF-8"
            ind <- validUTF8(out)
            if(!all(ind)) {
                pos <- which(!ind)
                ## Be as nice as for the other cases ...
                ## Could also throw an error along the lines of
                ##   stop(sprintf(ngettext(length(pos),
                ##                         "field %s is not valid UTF-8",
                ##                         "fields %s are not valid UTF-8"),
                ##                paste(sQuote(names(out)[pos]),
                ##                             collapse = ", ")),
                ##        call. = FALSE, domain = NA)
                out[pos] <-
                    iconv(out[pos], "UTF-8", "UTF-8", sub = "byte")
            }
        }
        else if(encoding == "latin1")
            Encoding(out) <- "latin1"
        else
            out <- iconv(out, encoding, "", sub = "byte")
    }
    out
}

.write_description <-
function(x, dfile)
{
    ## Invert how .read_description() handles package encodings.
    if(!is.na(encoding <- x["Encoding"])) {
        ## For UTF-8 or latin1 encodings, .read_description() would
        ## simply have marked the encoding.  But we might have added
        ## fields encoded differently ...
        ind <- is.na(match(Encoding(x), c(encoding, "unknown")))
        if(any(ind))
            x[ind] <- mapply(iconv, x[ind], Encoding(x)[ind], encoding,
                             sub = "byte")
    } else {
        ## If there is no declared encoding, we cannot have non-ASCII
        ## content.
        ## Cf. tools::showNonASCII():
        asc <- iconv(x, "latin1", "ASCII")
        ## fields might have been NA to start with, so use identical.
        if(!identical(asc, x)) {
            warning("Unknown encoding with non-ASCII data: converting to ASCII")
	    ind <- is.na(asc) | (asc != x)
            x[ind] <- iconv(x[ind], "latin1", "ASCII", sub = "byte")
        }
    }
    ## Avoid folding for fields where we keep whitespace when reading,
    ## plus two more fields where legacy code does not strip whitespace
    ## and so we should not wrap.
    ## Unfortunately, wrapping may destroy declared encodings: for the
    ## fields where we do not keep whitespace, write.dcf() calls
    ## formatDL() which in turn calls paste() on the results of
    ## strwrap(), and paste() may change the (common) encoding.
    ## In particular, pasting a latin1 string comes out in UTF-8 in a
    ## UTF-8 locale, and with unknown encoding in a C locale.
    ## Hence, when we have a declared non-UTF-8 encoding, we convert
    ## to UTF-8 before formatting, and convert back to the declared
    ## encoding when writing out.
    if(!is.na(encoding) && (encoding != "UTF-8")) {
        x <- iconv(x, from = encoding, to = "UTF-8")
        tfile <- tempfile()
        write.dcf(rbind(x), tfile,
                  keep.white = c(.keep_white_description_fields,
                                 "Maintainer", "BugReports"),
                  useBytes = TRUE)
        writeLines(iconv(readLines(tfile),
                         from = "UTF-8", to = encoding),
                   dfile, useBytes = TRUE)
    } else {
        write.dcf(rbind(x), dfile,
                  keep.white = c(.keep_white_description_fields,
                                 "Maintainer", "BugReports"),
                  useBytes = TRUE)
    }
}

.expand_package_description_db_R_fields <-
function(x)
{
    enc <- x["Encoding"]
    y <- character()
    if(!is.na(aar <- x["Authors@R"])) {
        aar <- utils:::.read_authors_at_R_field(aar)
        lat <- identical(enc, "latin1")
        if(is.na(x["Author"])) {
            tmp <- utils:::.format_authors_at_R_field_for_author(aar)
            if(lat) tmp <- .enc2latin1(tmp)
            y["Author"] <- tmp
        }
        if(is.na(x["Maintainer"])) {
            tmp <- utils:::.format_authors_at_R_field_for_maintainer(aar)
            if(lat) tmp <- .enc2latin1(tmp)
            y["Maintainer"] <- tmp
        }
    }
    y
}

### ** .replace_chars_by_hex_subs

.replace_chars_by_hex_subs <-
function(x, re) {
    char_to_hex_sub <- function(s) {
        paste0("<", charToRaw(s), ">", collapse = "")
    }
    vapply(strsplit(x, ""),
           function(e) {
               pos <- grep(re, e, perl = TRUE)
               if(length(pos))
                   e[pos] <- vapply(e[pos], char_to_hex_sub, "")
               paste(e, collapse = "")
           },
           "")
}

### ** .source_assignments

.source_assignments <-
function(file, envir, enc = NA)
{
    ## Read and parse expressions from @code{file}, and then
    ## successively evaluate the top-level assignments in @code{envir}.
    ## Apart from only dealing with assignments, basically does the same
    ## as @code{sys.source(file, envir, keep.source = FALSE)}.
    oop <- options(topLevelEnvironment = envir, keep.source = FALSE)
    on.exit(options(oop))

### <FIXME> for S4, setClass() .. are assignments, but must be called
    ##         with correct 'where = envir'!
    ## Possible solution: modified versions of these functions with changed
    ##                    'where = ...' (default arg) in formals(.)
    ## stopifnot(require(methods, quietly=TRUE))
    ## assignmentSymbols <- c(c("<-", "="),
    ##                        ls(pattern = "^set[A-Z]", pos = "package:methods"))
    assignmentSymbols <- c("<-", "=")
### </FIXME>
    con <- if(!is.na(enc) &&
              (Sys.getlocale("LC_CTYPE") %notin% c("C", "POSIX"))) {
               on.exit(close(con), add = TRUE)
               file(file, encoding = enc)
           } else file
    exprs <- parse(n = -1L, file = con)
    exprs <- exprs[lengths(exprs) > 0L]
    for(e in exprs) {
	if(is.call(e) &&
           as.character(e[[1L]])[1L] %in% assignmentSymbols)
            tryCatch(eval(e, envir), error = identity)
    }
    invisible()
}

### .source_assignments_in_code_dir

.source_assignments_in_code_dir <-
function(dir, envir, meta = character())
{
    ## Combine all code files in @code{dir}, read and parse expressions,
    ## and successively evaluate the top-level assignments in @code{envir}.
    con <- tempfile("Rcode")
    on.exit(unlink(con))
    if(!file.create(con))
        stop("unable to create ", con)
    ## If the (DESCRIPTION) metadata contain a Collate specification,
    ## use this for determining the code files and their order.
    txt <- meta[c(paste0("Collate.", .OStype()), "Collate")]
    ind <- which(!is.na(txt))
    files <- if(any(ind))
        Filter(function(x) file_test("-f", x),
               file.path(dir, .read_collate_field(txt[ind[1L]])))
    else
        list_files_with_type(dir, "code")
    if(!all(.file_append_ensuring_LFs(con, files)))
        stop("unable to write code files")
    if(!is.na(package <- meta["Package"]))
        envir$.packageName <- package
    tryCatch(.source_assignments(con, envir, enc = meta["Encoding"]),
             error = function(e)
                 stop("cannot source package code:\n", conditionMessage(e),
                      call. = FALSE))
}

### ** .split_dependencies

.split_dependencies <-
function(x)
{
    ## given one or more Depends: or Suggests: fields from DESCRIPTION
    ## return a named list of list (name, [op, version])
    if(!length(x)) return(list())
    x <- unlist(strsplit(x, ","))
    ## some have had space before ,
    x <- sub('[[:space:]]+$', '', x)
    x <- unique(sub("^[[:space:]]*(.*)", "\\1" , x))
    names(x) <- sub("^([[:alnum:].]+).*$", "\\1" , x)
    lapply(x, .split_op_version)
}

### ** .split_op_version

.split_op_version <-
function(x)
{
    ## given a single piece of dependency
    ## return a list of components (name, [op, version])
    ## NB this relies on trailing space having been removed
    pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
    x1 <- sub(pat, "\\1", x)
    x2 <- sub(pat, "\\2", x)
    if(x2 != x1) {
        pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
        version <- sub(pat, "\\2", x2)
        if (!startsWith(version, "r")) version <- package_version(version)
        list(name = x1, op = sub(pat, "\\1", x2), version = version)
    } else list(name = x1)
}

### ** .system_with_capture

.system_with_capture <-
function(command, args = character(), env = character(),
         stdin = "", input = NULL, timeout = 0)
{
    ## Invoke a system command and capture its status, stdout and stderr
    ## into separate components.

    outfile <- tempfile("xshell")
    errfile <- tempfile("xshell")
    on.exit(unlink(c(outfile, errfile)))
    status <- system2(command, args, env = env,
                      stdout = outfile, stderr = errfile,
                      stdin = stdin, input = input,
                      timeout = timeout)
    list(status = status,
         stdout = readLines(outfile, warn = FALSE),
         stderr = readLines(errfile, warn = FALSE))
}

### ** .trim_common_leading_whitespace

.trim_common_leading_whitespace <-
function(x)
{
    y <- sub("^([ \t]*).*", "\\1", x)
    n <- nchar(y)
    if(any(n == 0))
        return(x)
    i <- grep("\t", y, fixed = TRUE)
    if(length(i)) {
        ## Need to convert tabs to spaces.
        ## Ideally nchar(y, "width") would do things for us ...
        wids <- vapply(strsplit(y[i], ""),
                       function(e) {
                           p <- which(e == "\t")
                           d <- diff(c(0, p))
                           sum(d + 8 - (d %% 8)) + length(e) -
                               p[length(p)]
                       },
                       0)
        x[i] <- paste0(strrep(" ", wids), substring(x[i], n[i] + 1L))
        n[i] <- wids
    }
    substring(x, min(n) + 1L)
}

### ** .try_quietly

.try_quietly <-
function(expr)
{
    ## Try to run an expression, suppressing all 'output'.  In case of
    ## failure, stop with the error message and a "traceback" ...

    oop <- options(warn = 1)
    on.exit(options(oop))
    outConn <- file(open = "w+")         # anonymous tempfile
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    yy <- tryCatch(withRestarts(withCallingHandlers(expr, error = {
        function(e) invokeRestart("grmbl", e, sys.calls())
    }),
                                grmbl = function(e, calls) {
                                    n <- length(sys.calls())
                                    ## Chop things off as needed ...
                                    calls <- calls[-seq.int(length.out = n - 1L)]
                                    calls <- rev(calls)[-c(1L, 2L)]
                                    tb <- lapply(calls, deparse)
                                    stop(conditionMessage(e),
                                         "\nCall sequence:\n",
                                         paste(.eval_with_capture(traceback(tb))$output,
                                               collapse = "\n"),
                                         call. = FALSE)
                                }),
                   error = identity,
                   finally = {
                       sink(type = "message")
                       sink(type = "output")
                       close(outConn)
                   })
    if(inherits(yy, "error"))
        stop(yy)
    yy
}

### ** .unpacked_source_repository_apply

.unpacked_source_repository_apply <-
function(dir, FUN, ..., pattern = NULL, verbose = FALSE,
         Ncpus = getOption("Ncpus", 1L))
{
    dir <- file_path_as_absolute(dir)

    dfiles <- Sys.glob(file.path(dir, "*", "DESCRIPTION"))
    if(!is.null(pattern))
        dfiles <- dfiles[grepl(pattern, basename(dirname(dfiles)))]
    paths <- dirname(dfiles)

    ## Keep in sync with .package_apply().
    ## <FIXME>
    ## Should we really catch errors?
    one <- function(p) {
        if(verbose)
            message(sprintf("processing %s", basename(p)))
        tryCatch(FUN(p, ...), error = identity)
    }
    ## </FIXME>

    ## Would be good to have a common wrapper ...
    if(Ncpus > 1L) {
        if(.Platform$OS.type != "windows") {
            out <- parallel::mclapply(paths, one, mc.cores = Ncpus)
        } else {
            cl <- parallel::makeCluster(Ncpus)
            args <- list(FUN, ...)      # Eval promises.
            out <- parallel::parLapply(cl, paths, one)
            parallel::stopCluster(cl)
        }
    } else {
        out <- lapply(paths, one)
    }

    names(out) <- basename(paths)
    out
}

### ** .wrong_args

.wrong_args <-
function(args, msg)
{
    len <- length(args)
    if(!len)
        character()
    else if(len == 1L)
        paste("argument", sQuote(args), msg)
    else
        paste("arguments",
              paste0(c(rep.int("", len - 1L), "and "),
                     sQuote(args),
                     c(rep.int(", ", len - 1L), ""),
                     collapse = ""),
              msg)
}

### * Miscellania

### ** R

R <-
function(fun, args = list(), opts = "--no-save --no-restore",
         env = character(), arch = "", drop = TRUE, timeout = 0)
{
    .safe_repositories <- function() {
        x <- getOption("repos")
        y <- .get_standard_repository_URLs()
        i <- which(names(x) == "CRAN")[1L]
        if(is.na(i) || x[i] == "@CRAN@")
            x[i] <- y["CRAN"]
        c(x, y[match(names(y), names(x), 0L) == 0L])
    }

    ## escape issue if we use backslashes in paths, hence convert to "/"
    tfi <- normalizePath(tempfile("runri"), winslash="/", mustWork=FALSE)
    tfo <- normalizePath(tempfile("runro"), winslash="/", mustWork=FALSE)

    wrk <- c(sprintf("x <- readRDS(\"%s\")", tfi),
             "options(repos = x$repos)",
             ## need quote = TRUE in case some of args are not self-evaluating
             ## could catch other conditions also
             "y <- tryCatch(list(do.call(x$fun, x$args, quote = TRUE)), error = identity)",
             sprintf("saveRDS(y, \"%s\")", tfo))
    saveRDS(list(fun = fun, args = args, repos = .safe_repositories()),
            tfi)
    cmd <- if(.Platform$OS.type == "windows") {
               if(nzchar(arch))
                   ## R.home("bin") might be better, but Windows
                   ## installation is monolithic
                   file.path(R.home(), "bin", arch, "Rterm.exe")
               else
                   file.path(R.home("bin"), "Rterm.exe")
           } else {
               if(nzchar(arch))
                   opts <- c(paste0("--arch=", arch), opts)
               file.path(R.home("bin"), "R")
           }
    res <- .system_with_capture(cmd, opts, env, input = wrk,
                                timeout = timeout)
    ## FIXME: what should the "value" be in case of error?
    if(file.exists(tfo)) {
        val <- readRDS(tfo)
        if (inherits(val, "condition")) {
            ## maybe wrap in a classed error and include some of res
            msg <- paste0("error in inferior call:\n  ", conditionMessage(val))
            stop(do.call(errorCondition,
                         c(list(message = msg, 
                                class = "inferiorCallError",
                                value = val),
                           res)))
        }
        else {
            val <- val[[1L]]
            if(drop)
                val
            else
                c(list(value = val), res)
        }
    }
    else
        ## again maybe wrap in a classed error  and include some of res
        ## might want to distinguish two errors by sub-classes
        stop(do.call(errorCondition,
                     c(list(message = "inferior call failed",
                            class = "inferiorCallError"),
                       res = res)))
}

### ** Rcmd

Rcmd <-
function(args, ...)
{
    if(.Platform$OS.type == "windows")
        system2(file.path(R.home("bin"), "Rcmd.exe"), args, ...)
    else
        system2(file.path(R.home("bin"), "R"), c("CMD", args), ...)
}

### ** Sys.setenv1

##' Sys.setenv() *one* variable unless it's set (to non-empty) already - export/move to base?
Sys.setenv1 <- function(var, value) {
    if(!nzchar(Sys.getenv(var)))
        .Internal(Sys.setenv(var, as.character(value)))
}

### ** pskill

pskill <-
function(pid, signal = SIGTERM)
    invisible(.Call(C_ps_kill, pid, signal))

### ** psnice

psnice <-
function(pid = Sys.getpid(), value = NA_integer_)
{
    res <- .Call(C_ps_priority, pid, value)
    if(is.na(value)) res else invisible(res)
}

### ** toTitleCase

## original version based on http://daringfireball.net/2008/05/title_case
## but much altered before release.
toTitleCase <-
function(text)
{
    ## leave these alone: the internal caps rule would do that
    ## in some cases.  We could insist on this exact capitalization.
    alone <- c("2D", "3D", "AIC", "BayesX", "GoF", "HTML", "LaTeX",
               "MonetDB", "OpenBUGS", "TeX", "U.S.", "U.S.A.", "WinBUGS",
               "aka", "et", "al.", "ggplot2", "i.e.", "jar", "jars",
               "ncdf", "netCDF", "rgl", "rpart", "xls", "xlsx")
    ## These should be lower case except at the beginning (and after :)
    lpat <- "^(a|an|and|are|as|at|be|but|by|en|for|if|in|is|nor|not|of|on|or|per|so|the|to|v[.]?|via|vs[.]?|from|into|than|that|with)$"
    ## These we don't care about
    either <- c("all", "above", "after", "along", "also", "among",
                "any", "both", "can", "few", "it", "less", "log",
                "many", "may", "more", "over", "some", "their",
                "then", "this", "under", "until", "using", "von",
                "when", "where", "which", "will", "without",
                "yet", "you", "your")
    titleCase1 <- function(x) {
        ## A quote might be prepended.
        do1 <- function(x) {
            x1 <- substr(x, 1L, 1L)
            if(nchar(x) >= 3L && x1 %in% c("'", '"'))
                paste0(x1, toupper(substr(x, 2L, 2L)),
                       tolower(substring(x, 3L)))
            else paste0(toupper(x1), tolower(substring(x, 2L)))
        }
        if(is.na(x)) return(NA_character_)
        xx <- .Call(C_splitString, x, ' -/"()\n\t')
        ## for 'alone' we could insist on that exact capitalization
        alone <- xx %in% c(alone, either)
        alone <- alone | grepl("^'.*'$", xx)
        havecaps <- grepl("^[[:alpha:]].*[[:upper:]]+", xx)
        l <- grepl(lpat, xx, ignore.case = TRUE)
        l[1L] <- FALSE
        ## do not remove capitalization immediately after ": " or "- "
        ind <- grep("[-:]$", xx); ind <- ind[ind + 2L <= length(l)]
        ind <- ind[(xx[ind + 1L] == " ") & grepl("^['[:alnum:]]", xx[ind + 2L])]
        # don't capitalize lpat words after hyphenation
        ind <- ind[!(xx[ind] == "-" & grepl(lpat, xx[ind + 2L]))]
        l[ind + 2L] <- FALSE
        ## Also after " (e.g. "A Book Title")
        ind <- which(xx == '"'); ind <- ind[ind + 1L <= length(l)]
        l[ind + 1L] <- FALSE
        xx[l] <- tolower(xx[l])
        keep <- havecaps | l | (nchar(xx) == 1L) | alone
        xx[!keep] <- vapply(xx[!keep], do1, "<chr>")
        paste(xx, collapse = "")
    }
    if(typeof(text) != "character")
        stop("'text' must be a character vector")
    vapply(text, titleCase1, "<chr>", USE.NAMES = FALSE)
}

### ** path_and_libPath

##' Typically the union of R_LIBS and current .libPaths(); may differ e.g. via R_PROFILE
path_and_libPath <-
function(...)
{
    lP <- .libPaths()
    ## don't call normalizePath on paths which do not exist: allowed in R_LIBS!
    ep0 <- c(strsplit(env_path(...), .Platform$path.sep, fixed = TRUE)[[1L]], lP[-length(lP)])
    ep0 <- ep0[dir.exists(ep0)]
    paste(unique(normalizePath(ep0)), collapse = .Platform$path.sep)
}

### ** str_parse_logic

##' @param otherwise: can be call, such as quote(errmesg(...))
str_parse_logic <-
function(ch, default = TRUE, otherwise = default, n = 1L)
{
    if(is.na(ch)) default
    else switch(tolower(ch),
                "1" =, "yes" =, "true" = TRUE,
                "0" =, "no" =, "false" = FALSE,
                eval.parent(otherwise, n = n))
}

### ** str_parse

str_parse <-
function(ch, default = TRUE, logical = TRUE, otherwise = default, n = 2L)
{
    if(logical)
        str_parse_logic(ch, default=default, otherwise=otherwise, n = n)
    else if(is.na(ch))
        default
    else
        ch
}

### **

namespace_loads_from_file_load <-
function(f, verbose = FALSE)
{
    if(verbose) message(sprintf("processing %s", f))

    fun <- local({
        make_namespace_load_tracer <- function() {
            local({
                .packages <- character()
                .nframes <- integer()
                function(p, n) {
                    .packages <<- c(.packages, p)
                    .nframes <<- c(.nframes, n)
                }
            })
        }
        trace_namespace_loads <- function(expr, tracer) {
            ..namespace_load_tracer <- tracer
            suppressMessages({
                trace(base::loadNamespace,
                      function() {
                          pkg <- as.character(parent.frame()$package)
                          dynGet("..namespace_load_tracer")(pkg[[1L]],
                              sys.nframe())
                      },
                      print = FALSE)
            })
            on.exit(suppressMessages(untrace(base::loadNamespace)))
            expr
        }
        function(file) {
            tracer <- make_namespace_load_tracer()
            tmpenv <- new.env()
            trace_namespace_loads(load(file, tmpenv), tracer)
            with(environment(tracer),
                 .packages[.nframes == min(.nframes)])
        }
    })

    R(fun, list(f))
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
