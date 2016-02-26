#  File src/library/tools/R/utils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
        stop("'x' must be a single character string")
    if(!file.exists(epath <- path.expand(x)))
        stop(gettextf("file '%s' does not exist", x),
             domain = NA)
    normalizePath(epath, "/", TRUE)
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
    ## Provide shell-style '-f', '-d', '-x', '-nt' and '-ot' tests.
    ## Note that file.exists() only tests existence ('test -e' on some
    ## systems), and that our '-f' tests for existence and not being a
    ## directory (the GNU variant tests for being a regular file).
    ## Note: vectorized in x and y.
    switch(op,
           "-f" = !is.na(isdir <- file.info(x, extra_cols = FALSE)$isdir) & !isdir,
           "-d" = dir.exists(x),
           "-nt" = (!is.na(mt.x <- file.mtime(x))
                    & !is.na(mt.y <- file.mtime(y))
                    & (mt.x > mt.y)),
           "-ot" = (!is.na(mt.x <- file.mtime(x))
                    & !is.na(mt.y <- file.mtime(y))
                    & (mt.x < mt.y)),
           "-x" = (file.access(x, 1L) == 0L),
           stop(gettextf("test '%s' is not available", op),
                domain = NA))
}

### ** list_files_with_exts

list_files_with_exts <-
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    ## Might be in a zipped dir on Windows.
    if(file.exists(file.path(dir, "filelist")) &&
       any(file.exists(file.path(dir, c("Rdata.zip", "Rex.zip", "Rhelp.zip")))))
    {
        files <- readLines(file.path(dir, "filelist"))
        if(!all.files)
            files <- grep("^[^.]", files, value = TRUE)
    } else {
        files <- list.files(dir, all.files = all.files)
    }
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
    ## in particular, Estonian sorts Z after S.
    if(type %in% c("code", "docs")) { # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]", basename(files))]
    }
    if(type %in% "demo") {           # only certain filenames are valid.
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", basename(files))]
    }
    files
}

### ** reQuote

## <FIXME>
## Move into base eventually ...
reQuote <-
function(x)
{
    escape <- function(s) paste0("\\", s)
    re <- "[.*?+^$\\[]"
    m <- gregexpr(re, x)
    regmatches(x, m) <- lapply(regmatches(x, m), escape)
    x
}
## </FIXME>

### ** showNonASCII

showNonASCII <-
function(x)
{
    ## All that is needed here is an 8-bit encoding that includes ASCII.
    ## The only one we guarantee to exist is 'latin1'.
    ## The default sub=NA is faster, but on some platforms
    ## some characters used just to lose their accents, so two tests.
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | asc != x
    if(any(ind))
        message(paste0(which(ind), ": ",
                       iconv(x[ind], "latin1", "ASCII", sub = "byte"),
                       collapse = "\n"), domain = NA)
    invisible(x[ind])
}

showNonASCIIfile <-
function(file)
    showNonASCII(readLines(file, warn = FALSE))

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

    .Call(delim_match, x, delim)
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
        if(is.null(texi2dvi) || !nzchar(texi2dvi) || texi2dvi == "texi2dvi")
            texi2dvi <- Sys.which("texi2dvi")
        if(.Platform$OS.type == "windows" && !nzchar(texi2dvi))
            texi2dvi <- Sys.which("texify")
    }

    envSep <- .Platform$path.sep
    texinputs0 <- texinputs
    Rtexmf <- file.path(R.home("share"), "texmf")
    Rtexinputs <- file.path(Rtexmf, "tex", "latex")
    ## "" forces use of default paths.
    texinputs <- paste(c(texinputs0, Rtexinputs, ""),
                       collapse = envSep)
    ## not clear if this is needed, but works
    if(.Platform$OS.type == "windows")
        texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
    Rbibinputs <- file.path(Rtexmf, "bibtex", "bib")
    bibinputs <- paste(c(texinputs0, Rbibinputs, ""),
                       collapse = envSep)
    Rbstinputs <- file.path(Rtexmf, "bibtex", "bst")
    bstinputs <- paste(c(texinputs0, Rbstinputs, ""),
                       collapse = envSep)

    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA_character_)
    if(is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    } else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    Sys.setenv(TEXINPUTS = paste(otexinputs, texinputs, sep = envSep))
    obibinputs <- Sys.getenv("BIBINPUTS", unset = NA_character_)
    if(is.na(obibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        obibinputs <- "."
    } else on.exit(Sys.setenv(BIBINPUTS = obibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paste(obibinputs, bibinputs, sep = envSep))
    obstinputs <- Sys.getenv("BSTINPUTS", unset = NA_character_)
    if(is.na(obstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        obstinputs <- "."
    } else on.exit(Sys.setenv(BSTINPUTS = obstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paste(obstinputs, bstinputs, sep = envSep))

    if(index && nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        ## switch off the use of texindy in texi2dvi >= 1.157
        Sys.setenv(TEXINDY = "false")
        on.exit(Sys.unsetenv("TEXINDY"), add = TRUE)
        opt_pdf <- if(pdf) "--pdf" else ""
        opt_quiet <- if(quiet) "--quiet" else ""
        opt_extra <- ""
        out <- .system_with_capture(texi2dvi, "--help")

        if(length(grep("--no-line-error", out$stdout)))
            opt_extra <- "--no-line-error"
        ## (Maybe change eventually: the current heuristics for finding
        ## error messages in log files should work for both regular and
        ## file line error indicators.)

        ## This is present in texinfo after late 2009, so really >= 5.0.
        if(any(grepl("--max-iterations=N", out$stdout)))
            opt_extra <- c(opt_extra, "--max-iterations=20")

        ## and work around a bug in texi2dvi
        ## https://stat.ethz.ch/pipermail/r-devel/2011-March/060262.html
        ## That has [A-Za-z], earlier versions [A-z], both of which may be
        ## invalid in some locales.
        env0 <- "LC_COLLATE=C"
        ## texi2dvi, at least on OS X (4.8) does not accept TMPDIR with spaces.
        if (grepl(" ", Sys.getenv("TMPDIR")))
            env0 <- paste(env0,  "TMPDIR=/tmp")
        out <- .system_with_capture(texi2dvi,
                                    c(opt_pdf, opt_quiet, opt_extra,
                                      shQuote(file)),
                                    env = env0)

        log <- paste(file_path_sans_ext(file), "log", sep = ".")

        ## With Texinfo 6.1 (precisely, c6637), texi2dvi may not rerun
        ## often enough and give a non-zero status value when it should
        ## have continued iterating.
        ## Try to catch and correct cases seen on CRAN ...
        ## (Note that texi2dvi may have been run quietly, in which case
        ## diagnostics will only be in the log file.)
        if(out$status &&
           file_test("-f", log) &&
           any(grepl("(Rerun to get|biblatex.*\\(re\\)run)",
                     readLines(log, warn = FALSE)))) {
            out <- .system_with_capture(texi2dvi,
                                        c(opt_pdf, opt_quiet, opt_extra,
                                          shQuote(file)),
                                        env = env0)
        }

        ## We cannot necessarily rely on out$status, hence let us
        ## analyze the log files in any case.
        errors <- character()
        ## (La)TeX errors.
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if(length(lines))
                errors <- paste("LaTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
        }
        ## BibTeX errors.
        log <- paste(file_path_sans_ext(file), "blg", sep = ".")
        if(file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if(length(lines))
                errors <- paste("BibTeX errors:",
                                paste(lines, collapse = "\n"),
                                sep = "\n")
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
        ## and set the path to R's style files.
        ## -I works in MiKTeX >= 2.4, at least
        ## http://docs.miktex.org/manual/texify.html
        ver <- system(paste(shQuote(texi2dvi), "--version"), intern = TRUE)
        if(length(grep("MiKTeX", ver[1L]))) {
            ## AFAICS need separate -I for each element of texinputs.
            texinputs <- c(texinputs0, Rtexinputs, Rbstinputs)
            texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
            paths <- paste ("-I", shQuote(texinputs))
            extra <- "--max-iterations=20"
            extra <- paste(extra, paste(paths, collapse = " "))
        }
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
        logfile <- paste(base, "log", sep = ".")
        if(file_test("-f", logfile)) {
            lines <- .get_LaTeX_errors_from_log_file(logfile)
            if(length(lines))
                msg <- paste(msg, "LaTeX errors:",
                             paste(lines, collapse = "\n"),
                             sep = "\n")
        }
        ## BibTeX errors.
        logfile <- paste(base, "blg", sep = ".")
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
        if(sys2(latex, ltxargs))
            stop(gettextf("unable to run '%s' on '%s'", latex, file),
                 domain = NA)
        nmiss <- length(grep("Warning:.*Citation.*undefined",
                             readLines(paste0(base, ".log"))))
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
                    paste("LaTeX errors:",
                          paste(lines, collapse = "\n"), sep = "\n")
                else character()
                stop(paste(gettextf("unable to run %s on '%s'", latex, file),
                           errors, sep = "\n"),
                     domain = NA)
            }
            Log <- readLines(paste0(base, ".log"))
            nmiss <- length(grep("Warning:.*Citation.*undefined", Log))
            if(nmiss == nmiss_prev &&
               !any(grepl("(Rerun to get|biblatex.*\\(re\\)run)", Log)) ) break
        }
        do_cleanup(clean)
    }
    invisible(NULL)
}

### * Internal utility variables.

### ** .BioC_version_associated_with_R_version

.BioC_version_associated_with_R_version <-
    function() numeric_version(Sys.getenv("R_BIOC_VERSION", "3.3"))
## Things are more complicated from R-2.15.x with still two BioC
## releases a year, so we do need to set this manually.
## Wierdly, 3.0 is the second version (after 2.14) for the 3.1.x series.

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

### ** %w/o%

## x without y, as in the examples of ?match.
`%w/o%` <-
function(x, y)
    x[!x %in% y]

### ** .OStype

.OStype <-
function()
{
    OS <- Sys.getenv("R_OSTYPE")
    if(nzchar(OS)) OS else .Platform$OS.type
}

### .R_top_srcdir

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
    substring(path, 1L, nchar(path) - 28L)
}

## Unfortunately,
##   .R_top_srcdir <- .R_top_srcdir_from_Rd()
## does not work because when tools is installed there are no Rd pages
## yet ...

### ** config_val_to_logical

config_val_to_logical <-
function(val) {
    v <- tolower(val)
    if (v %in% c("1", "yes", "true")) TRUE
    else if (v %in% c("0", "no", "false")) FALSE
    else {
        warning(gettextf("cannot coerce %s to logical", sQuote(val)),
                domain = NA)
        NA
    }
}

### ** .canonicalize_doi

.canonicalize_doi <-
function(x)    
{
    x <- sub("^((doi|DOI):)?[[:space:]]*http://(dx[.])?doi[.]org/", "",
             x)
    sub("^(doi|DOI):", "", x)
}

### ** .canonicalize_quotes

.canonicalize_quotes <-
function(txt)
{
    txt <- gsub("(\xe2\x80\x98|\xe2\x80\x99)", "'", txt,
                perl = TRUE, useBytes = TRUE)
    txt <- gsub("(\xe2\x80\x9c|\xe2\x80\x9d)", '"', txt,
                perl = TRUE, useBytes = TRUE)
    txt
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
         output = readLines(outcon, encoding = "UTF-8", warn = FALSE),
         message = readLines(msgcon, encoding = "UTF-8", warn = FALSE))
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
    .Call(codeFilesAppend, file1, file2)
}

### ** .file_path_relative_to_dir

.file_path_relative_to_dir <-
function(x, dir, add = FALSE)
{
    if(any(ind <- (substring(x, 1L, nchar(dir)) == dir))) {
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
    x <- if(is.call(x)) list(x) else as.list(x)

    f <- if(is.null(predicate))
        function(e) is.call(e)
    else
        function(e) is.call(e) && predicate(e)

    if(!recursive) return(Filter(f, x))

    calls <- list()
    gatherer <- function(e) {
        if(f(e)) calls <<- c(calls, list(e))
        if(is.recursive(e))
            for(i in seq_along(e)) gatherer(e[[i]])
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
function(dir, predicate = NULL, recursive = FALSE, .worker = NULL)
{
    dir <- file_path_as_absolute(dir)

    dfile <- file.path(dir, "DESCRIPTION")
    encoding <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    if(is.null(.worker))
        .worker <- function(file, encoding)
            .find_calls_in_file(file, encoding, predicate, recursive)

    code_files <-
        list_files_with_type(file.path(dir, "R"), "code",
                             OS_subdirs = c("unix", "windows"))
    calls <- lapply(code_files, .worker, encoding)
    names(calls) <-
        .file_path_relative_to_dir(code_files, dirname(dir))

    calls
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
        (length(grep("^---", lines)) ||
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
    ## unforunately that was too general and caught false positives
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
    out <-
        ## Get the names of R internal S3 generics (via DispatchOrEval(),
        ## cf. zMethods.Rd).
        c("[", "[[", "$", "[<-", "[[<-", "$<-",
          "as.vector", "unlist",
          .get_S3_primitive_generics()
          ## ^^^^^^^ now contains the members of the group generics from
          ## groupGeneric.Rd.
          )
    if(!primitive)
        out <- out[!vapply(out, .is_primitive_in_base, NA)]
    out
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
    S3_methods_list <- nsInfo$S3methods
    if(!length(S3_methods_list)) return(matrix(character(), ncol = 3L))
    idx <- is.na(S3_methods_list[, 3L])
    S3_methods_list[idx, 3L] <-
        paste(S3_methods_list[idx, 1L],
              S3_methods_list[idx, 2L],
              sep = ".")
    S3_methods_list
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

### ** .get_repositories

.get_repositories <-
function()
{
    rfile <- Sys.getenv("R_REPOSITORIES", unset = NA_character_)
    if(is.na(rfile) || !file_test("-f", rfile)) {
        rfile <- file.path(Sys.getenv("HOME"), ".R", "repositories")
        if(!file_test("-f", rfile))
            rfile <- file.path(R.home("etc"), "repositories")
    }
    .read_repositories(rfile)
}

### ** .get_requires_from_package_db

.get_requires_from_package_db <-
function(db,
         category = c("Depends", "Imports", "LinkingTo", "VignetteBuilder",
         "Suggests", "Enhances"))
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

.get_S3_generics_as_seen_from_package <-
function(dir, installed = TRUE, primitive = FALSE)
{
    ## Get the S3 generics "as seen from a package" rooted at
    ## @code{dir}.  Tricky ...
    if(basename(dir) == "base")
        env_list <- list()
    else {
        ## Always look for generics in the whole of the former base.
        ## (Not right, but we do not perform run time analyses when
        ## working off package sources.)  Maybe change this eventually,
        ## but we still cannot rely on packages to fully declare their
        ## dependencies on base packages.
        env_list <-
            list(baseenv(),
                 as.environment("package:graphics"),
                 as.environment("package:stats"),
                 as.environment("package:utils"))
        if(installed) {
            ## Also use the loaded namespaces and attached packages
            ## listed in the DESCRIPTION Depends and Imports fields.
            ## Not sure if this is the best approach: we could also try
            ## to determine which namespaces/packages were made
            ## available by loading the package (which should work at
            ## least when run from R CMD check), or we could simply
            ## attach every package listed as a dependency ... or
            ## perhaps do both.
            db <- .read_description(file.path(dir, "DESCRIPTION"))
            depends <- .get_requires_from_package_db(db, "Depends")
            imports <- .get_requires_from_package_db(db, "Imports")
            reqs <- intersect(c(depends, imports), loadedNamespaces())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, getNamespace))
            reqs <- intersect(setdiff(depends, loadedNamespaces()),
                              .packages())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, .package_env))
            env_list <- unique(env_list)
        }
    }
    ## some BioC packages warn here
    suppressWarnings(
    unique(c(.get_internal_S3_generics(primitive),
             unlist(lapply(env_list,
                           function(env) {
                               nms <- sort(names(env))
                               if(".no_S3_generics" %in% nms)
                                   character()
                               else Filter(function(f)
                                           .is_S3_generic(f, envir = env),
                                           nms)
                           })))))
}

### ** .get_S3_group_generics

.get_S3_group_generics <-
function()
    c("Ops", "Math", "Summary", "Complex")

### ** .get_S3_primitive_generics

.get_S3_primitive_generics <-
function(include_group_generics = TRUE)
{
    if(include_group_generics)
        c(base::.S3PrimitiveGenerics,
          "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round",
          "signif", "exp", "log", "expm1", "log1p",
          "cos", "sin", "tan", "acos", "asin", "atan",
          "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
          "lgamma", "gamma", "digamma", "trigamma",
          "cumsum", "cumprod", "cummax", "cummin",
          "+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==",
          "!=", "<", "<=", ">=", ">",
          "all", "any", "sum", "prod", "max", "min", "range",
          "Arg", "Conj", "Im", "Mod", "Re")
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

## we cannot assume that file.path(R.home("share"), "make", "vars.mk")
## is installed, as it is not on Windows
.get_standard_package_names <-
local({
    lines <- readLines(file.path(R.home("share"), "make", "vars.mk"))
    lines <- grep("^R_PKGS_[[:upper:]]+ *=", lines, value = TRUE)
    out <- strsplit(sub("^R_PKGS_[[:upper:]]+ *= *", "", lines), " +")
    names(out) <-
        tolower(sub("^R_PKGS_([[:upper:]]+) *=.*", "\\1", lines))
    eval(substitute(function() {out}, list(out=out)), envir=NULL)
})

### ** .get_standard_repository_URLs

.get_standard_repository_URLs <-
function()
{
    repos <- Sys.getenv("_R_CHECK_XREFS_REPOSITORIES_", "")
    if(nzchar(repos)) {
        repos <-
            .expand_BioC_repository_URLs(strsplit(repos, " +")[[1L]])
    } else {
        nms <- c("CRAN", "BioCsoft", "BioCann", "BioCexp")
        repos <- getOption("repos")
        ## This is set by utils:::.onLoad(), hence may be NULL.
        if(!is.null(repos) &&
           !any(is.na(repos[nms])) &&
           (repos["CRAN"] != "@CRAN@"))
            repos <- repos[nms]
        else {
            repos <- .get_repositories()[nms, "URL"]
            names(repos) <- nms
            if(repos["CRAN"] == "@CRAN@")
                repos["CRAN"] <- "http://CRAN.R-project.org"
        }
    }
    repos
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
               "Suggests",
               "SysDataCompression",
               "SystemRequirements",
               "Title",
               "Type",
               "URL",
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
                  Map(function(tr, co) tr(yi[co]),
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
    ind <- sapply(nms, length) > 0L
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
    vapply(as.character(x), function(txt)
           all(charToRaw(txt) <= as.raw(127)), NA)
}

### ** .is_ISO_8859

.is_ISO_8859 <-
function(x)
{
    ## Determine whether the strings in a character vector could be in
    ## some ISO 8859 character set or not.
    raw_ub <- charToRaw("\x7f")
    raw_lb <- charToRaw("\xa0")
    vapply(as.character(x), function(txt) {
        raw <- charToRaw(txt)
        all(raw <= raw_ub | raw >= raw_lb)
    }, NA)
}

### ** .is_primitive_in_base

.is_primitive_in_base <-
function(fname)
{
    ## Determine whether object named 'fname' found in the base
    ## environment is a primitive function.
    is.primitive(get(fname, envir = baseenv(), inherits = FALSE))
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
            invokeRestart("muffleWarning")
    }
    expr <- substitute(loadNamespace(package), list(package = package))
    invisible(withCallingHandlers(suppressMessages(eval(expr)),
                                  warning = .whandler))
}

### ** .load_package_quietly

.load_package_quietly <-
function(package, lib.loc)
{
    ## Load (reload if already loaded) @code{package} from
    ## @code{lib.loc}, capturing all output and messages.
    ## Don't do anything for base.
    ## Earlier versions did not attempt reloading methods as this used
    ## to cause trouble, but this now (2009-03-19) seems ok.
    ## Otoh, it seems that unloading tcltk is a bad idea ...
    ## Also, do not unload ourselves (but shouldn't we be "in use"?).
    ##
    ## All QC functions use this for loading packages because R CMD
    ## check interprets all output as indicating a problem.
    if(package != "base")
        .try_quietly({
            pos <- match(paste("package", package, sep = ":"), search())
            if(!is.na(pos)) {
                detach(pos = pos,
                       unload = ! package %in% c("tcltk", "tools"))
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
           docs = c("Rd", "rd", "Rd.gz", "rd.gz"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste, sep = ""), "Rmd"))
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

### ** nonS3methods [was .make_S3_methods_stop_list ]

nonS3methods <- function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    ## Using package = NULL returns all known examples

    stopList <-
        list(base = c("all.equal", "all.names", "all.vars", "expand.grid",
             "format.char", "format.info", "format.pval",
             "max.col",
             ## the next two only exist in *-defunct.Rd.
             "print.atomic", "print.coefmat",
             "qr.Q", "qr.R", "qr.X", "qr.coef", "qr.fitted", "qr.qty",
             "qr.qy", "qr.resid", "qr.solve",
             "rep.int", "seq.int", "sort.int", "sort.list"),
             AMORE = "sim.MLPnet",
             BSDA = "sign.test",
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
             RCurl = "merge.list",
             RNetCDF = c("close.nc", "dim.def.nc", "dim.inq.nc",
                         "dim.rename.nc", "open.nc", "print.nc"),
             Rmpfr = c("mpfr.is.0", "mpfr.is.integer"),
             SMPracticals = "exp.gibbs",
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
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             rgeos = "scale.poly",
             sac = "cumsum.test",
             sm = "print.graph",
             splusTimeDate = "sort.list",
             splusTimeSeries = "sort.list",
	     stats = c("anova.lmlist", "expand.model.frame", "fitted.values",
		       "influence.measures", "lag.plot", "t.test",
                       "plot.spec.phase", "plot.spec.coherency"),
             stremo = "sigma.hat",
             supclust = c("sign.change", "sign.flip"),
             tensorA = "chol.tensor",
             utils = c("close.socket", "flush.console", "update.packages")
             )
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if(!length(thisPkg)) character() else thisPkg
}

### ** .package_apply

.package_apply <-
function(packages = NULL, FUN, ...)
{
    ## Apply FUN and extra '...' args to all given packages.
    ## The default corresponds to all installed packages with high
    ## priority.
    if(is.null(packages))
        packages <-
            unique(utils::installed.packages(priority = "high")[ , 1L])
    out <- lapply(packages, function(p)
                  tryCatch(FUN(p, ...),
                           error = function(e)
                           noquote(paste("Error:",
                                         conditionMessage(e)))))
    ## (Just don't throw the error ...)
    names(out) <- packages
    out
}

### ** .pandoc_md_for_CRAN

.pandoc_md_for_CRAN <-
function(ifile, ofile)
{
    .system_with_capture("pandoc",
                         paste(shQuote(ifile), "-s",
                               "--email-obfuscation=references",
                               "--css=../../CRAN_web.css",
                               "-o", shQuote(ofile)))
}

### ** .parse_code_file

.parse_code_file <-
function(file, encoding = NA, keep.source = getOption("keep.source"))
{
    if(!file.size(file)) return()
    suppressWarnings({
        if(!is.na(encoding) &&
           (encoding != "unknown") &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
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


### ** .read_Rd_lines_quietly

.read_Rd_lines_quietly <-
function(con)
{
    ## Read lines from a connection to an Rd file, trying to suppress
    ## "incomplete final line found by readLines" warnings.
    if(is.character(con)) {
        con <- if(length(grep("\\.gz$", con))) gzfile(con, "r") else file(con, "r")
        on.exit(close(con))
    }
    .try_quietly(readLines(con, warn=FALSE))
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
    con <- textConnection(gsub("\n", " ", txt))
    on.exit(close(con))
    scan(con, what = character(), strip.white = TRUE, quiet = TRUE)
}

### ** .read_description

.keep_white_description_fields <-
    c("Description", "Authors@R", "Author", "Built", "Packaged")

.read_description <-
function(dfile)
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
    out <- tryCatch(read.dcf(dfile,
                             keep.white =
                             .keep_white_description_fields),
                    error = function(e)
                    stop(gettextf("file '%s' is not in valid DCF format",
                                  dfile),
                         domain = NA, call. = FALSE))
    if (nrow(out) != 1)
        stop("contains a blank line", call. = FALSE)
    out <- out[1,]
    if(!is.na(encoding <- out["Encoding"])) {
        ## could convert everything to UTF-8
        if (encoding %in% c("latin1", "UTF-8"))
            Encoding(out) <- encoding
        else out <- iconv(out, encoding, "", sub = "byte")
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
        ind <- is.na(asc) | (asc != x)
        if(any(ind)) {
            warning(gettext("Unknown encoding with non-ASCII data: converting to ASCII"),
                    domain = NA)
            x[ind] <- iconv(x[ind], "latin1", "ASCII", sub = "byte")
        }
    }
    ## Avoid declared encodings when writing out.
    Encoding(x) <- "unknown"
    ## Avoid folding for fields where we keep whitespace when reading.
    write.dcf(rbind(x), dfile,
              keep.white = c(.keep_white_description_fields, "Maintainer"))
}

### ** .read_repositories

.read_repositories <-
function(file)
{
    db <- utils::read.delim(file, header = TRUE, comment.char = "#",
                            colClasses =
                            c(rep.int("character", 3L),
                              rep.int("logical", 4L))) # allow for win64.binary
    db[, "URL"] <- .expand_BioC_repository_URLs(db[, "URL"])
    db
}

### default changed to https: for R 3.3.0
.expand_BioC_repository_URLs <-
function(x)
{
    x <- sub("%bm",
             as.character(getOption("BioC_mirror",
                                    "https://bioconductor.org")),
             x, fixed = TRUE)
    sub("%v",
        as.character(.BioC_version_associated_with_R_version()),
        x, fixed = TRUE)
}

.expand_package_description_db_R_fields <-
function(x)
{
    enc <- x["Encoding"]
    y <- character()
    if(!is.na(aar <- x["Authors@R"])) {
        aar <- utils:::.read_authors_at_R_field(aar)
        if(is.na(x["Author"])) {
            tmp <- utils:::.format_authors_at_R_field_for_author(aar)
            ## uses strwrap, so will be in current locale
            if(!is.na(enc)) tmp <- iconv(tmp, "", enc)
            y["Author"] <- tmp
        }
        if(is.na(x["Maintainer"]))
            y["Maintainer"] <-
                utils:::.format_authors_at_R_field_for_maintainer(aar)
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
    oop <- options(keep.source = FALSE)
    on.exit(options(oop))
    assignmentSymbolLM <- as.symbol("<-")
    assignmentSymbolEq <- as.symbol("=")
    if(!is.na(enc) &&
       !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        con <- file(file, encoding = enc)
        on.exit(close(con))
    } else con <- file
    exprs <- parse(n = -1L, file = con)
    if(!length(exprs))
        return(invisible())
    for(e in Filter(length, exprs)) {
        if(is.call(e) &&
           (e[[1L]] == assignmentSymbolLM ||
            e[[1L]] == assignmentSymbolEq))
            eval(e, envir)
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
    txt <- meta[c(paste("Collate", .OStype(), sep = "."), "Collate")]
    ind <- which(!is.na(txt))
    files <- if(any(ind))
        Filter(function(x) file_test("-f", x),
               file.path(dir, .read_collate_field(txt[ind[1L]])))
    else
        list_files_with_type(dir, "code")
    if(!all(.file_append_ensuring_LFs(con, files)))
        stop("unable to write code files")
    tryCatch(.source_assignments(con, envir, enc = meta["Encoding"]),
             error =
             function(e)
             stop("cannot source package code\n",
                  conditionMessage(e),
                  call. = FALSE))
}

### * .split_dependencies

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

### * .split_op_version

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

## <FIXME>
## We now have base::trimws(), so this is no longer needed.
## Remove eventually.

### ** .strip_whitespace

## <NOTE>
## Other languages have this as strtrim() (or variants for left or right
## trimming only), but R has a different strtrim().
## So perhaps strstrip()?
## Could more generally do
##   strstrip(x, pattern, which = c("both", "left", "right"))
## </NOTE>

.strip_whitespace <-
function(x)
{
    ## Strip leading and trailing whitespace.
    x <- sub("^[[:space:]]+", "", x)
    x <- sub("[[:space:]]+$", "", x)
    x
}

## </FIXME>

### ** .system_with_capture

.system_with_capture <-
function(command, args = character(), env = character(),
         stdin = "", input = NULL)
{
    ## Invoke a system command and capture its status, stdout and stderr
    ## into separate components.

    outfile <- tempfile("xshell")
    errfile <- tempfile("xshell")
    on.exit(unlink(c(outfile, errfile)))
    status <- system2(command, args, env = env,
                      stdout = outfile, stderr = errfile,
                      stdin = stdin, input = input)
    list(status = status,
         stdout = readLines(outfile, warn = FALSE),
         stderr = readLines(errfile, warn = FALSE))
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
function(dir, fun, ..., pattern = "*", verbose = FALSE)
{
    dir <- file_path_as_absolute(dir)

    dfiles <- Sys.glob(file.path(dir, pattern, "DESCRIPTION"))

    results <-
        lapply(dirname(dfiles),
               function(dir) {
                   if(verbose)
                       message(sprintf("processing %s", basename(dir)))
                   fun(dir, ...)
               })
    names(results) <- basename(dirname(dfiles))
    results
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
              paste(c(rep.int("", len - 1L), "and "),
                    sQuote(args),
                    c(rep.int(", ", len - 1L), ""),
                    sep = "", collapse = ""),
              msg)
}


### ** pskill

pskill <- function(pid, signal = SIGTERM)
    invisible(.Call(ps_kill, pid, signal))

### ** psnice

psnice <- function(pid = Sys.getpid(), value = NA_integer_)
{
    res <- .Call(ps_priority, pid, value)
    if(is.na(value)) res else invisible(res)
}

### ** toTitleCase

## original version based on http://daringfireball.net/2008/05/title_case
## but much altered before release.
toTitleCase <- function(text)
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
            x1 <- substring(x, 1L, 1L)
            if(nchar(x) >= 3L && x1 %in% c("'", '"'))
                paste0(x1, toupper(substring(x, 2L, 2L)),
                       tolower(substring(x, 3L)))
            else paste0(toupper(x1), tolower(substring(x, 2L)))
        }
        xx <- .Call(splitString, x, ' -/"()\n')
        ## for 'alone' we could insist on that exact capitalization
        alone <- xx %in% c(alone, either)
        alone <- alone | grepl("^'.*'$", xx)
        havecaps <- grepl("^[[:alpha:]].*[[:upper:]]+", xx)
        l <- grepl(lpat, xx, ignore.case = TRUE)
        l[1L] <- FALSE
        ## do not remove capitalization immediately after ": " or "- "
        ind <- grep("[-:]$", xx); ind <- ind[ind + 2L <= length(l)]
        ind <- ind[(xx[ind + 1L] == " ") & grepl("^['[:alnum:]]", xx[ind + 2L])]
        l[ind + 2L] <- FALSE
        ## Also after " (e.g. "A Book Title")
        ind <- which(xx == '"'); ind <- ind[ind + 1L <= length(l)]
        l[ind + 1L] <- FALSE
        xx[l] <- tolower(xx[l])
        keep <- havecaps | l | (nchar(xx) == 1L) | alone
        xx[!keep] <- sapply(xx[!keep], do1)
        paste(xx, collapse = "")
    }
    if(typeof(text) != "character")
        stop("'text' must be a character vector")
    sapply(text, titleCase1, USE.NAMES = FALSE)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
