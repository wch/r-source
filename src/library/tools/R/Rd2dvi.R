#  File src/library/tools/R/Rd2dvi.R
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

#### R based engine for  R CMD Rdconv Rd2dvi
####

##' @param args

##' @return ...

## base packages do not have versions and this is called on
## DESCRIPTION.in
## encodings are tricky: this may be done in a foreign encoding
## (e.g., Latin-1 in UTF-8)
.DESCRIPTION_to_latex <- function(descfile, outfile, version = "Unknown")
{
    desc <- read.dcf(descfile)[1, ]
    if (is.character(outfile)) {
        out <- file(outfile, "a")
        on.exit(close(out))
    } else out <- outfile
    cat("\\begin{description}", "\\raggedright{}", sep="\n", file=out)
    fields <- names(desc)
    fields <- fields[! fields %in% c("Package", "Packaged", "Built")]
    if ("Encoding" %in% fields)
        cat("\\inputencoding{", latex_canonical_encoding(desc["Encoding"]),
            "}\n", sep = "", file = out)
    for (f in fields) {
        text <- desc[f]
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Rd2dvi, then added to.
        ## KH: the LaTeX special characters are
        ##   # $ % & _ ^ ~ { } \
        ## \Rd@AsIs@dospecials in Rd.sty handles the first seven, so
        ## braces and backslashes need explicit handling.
        text <- gsub('"([^"]*)"', "\\`\\`\\1''", text, useBytes = TRUE)
        text <- gsub("\\", "\\textbackslash{}", text,
                     fixed = TRUE, useBytes = TRUE)
        text <- gsub("([{}$#_])", "\\\\\\1", text, useBytes = TRUE)
        text <- gsub("@VERSION@", version, text, fixed = TRUE, useBytes = TRUE)
        ## text can have paras, and digest/DESCRIPTION does.
        ## \AsIs is per-para.
        text <- strsplit(text, "\n\n", fixed = TRUE, useBytes = TRUE)[[1L]]
        Encoding(text) <- "unknown"
        wrap <- paste("\\AsIs{", text, "}", sep = "")
        if(f %in% c("Author", "Maintainer"))
            wrap <- gsub("<([^@]+)@([^>]+)>", "\\\\email{\\1@\\2}",
                         wrap, useBytes = TRUE)
        if(f == "URL")
            wrap <- gsub("(http://|ftp://)([^[:space:]]+)",
                         "\\\\url{\\1\\2}", wrap, useBytes = TRUE)
        ## Not entirely safe: in theory, tags could contain \ ~ ^.
        cat("\\item[", gsub("([#$%&_{}])", "\\\\\\1", f),
            "]", paste(wrap, collapse = "\n\n"),  "\n", sep = "", file=out)
    }
    cat("\\end{description}\n", file = out)
}

## workhorse of .Rd2dvi
.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", outputEncoding = "UTF-8",
             append = FALSE, extraDirs = NULL, internals = FALSE,
             silent = FALSE)
{
    if (file_test("-d", files))
        .pkg2tex(files, outfile, encoding = encoding, append = append,
                 asChapter = FALSE, extraDirs = extraDirs,
                 internals = internals, silent = silent)
    else {
        files <- strsplit(files, "[[:space:]]+")[[1L]]
        latexdir <- tempfile("ltx")
        dir.create(latexdir)
        if (!silent) message("Converting Rd files to LaTeX ...")
        if (is.character(outfile)) {
            outfile <- file(outfile, if (append) "at" else "wt")
            on.exit(close(outfile))
        }
        latexEncodings <- character()
        for(f in files) {
            cat("  ", basename(f), "\n", sep="")
            if (!internals) {
                lines <- readLines(f)
                if (any(grepl("\\\\keyword\\{\\s*internal\\s*\\}",
                         lines, perl = TRUE))) next
            }
            out <-  file.path(latexdir, sub("\\.[Rr]d$", ".tex", basename(f)))
            ## people have file names with quotes in them.
            latexEncodings <- c(latexEncodings,
                                attr(Rd2latex(f, out, encoding=encoding,
                                              outputEncoding=outputEncoding),
                                     "latexEncoding"))
            writeLines(readLines(out), outfile)
        }
        unique(latexEncodings[!is.na(latexEncodings)])
    }
}

## used for the refman (from doc/manual/Makefile*)
## and for directories from .Rdfiles2tex  (with asChapter = FALSE)
.pkg2tex <-
    function(pkgdir, outfile, internals = FALSE, asChapter = TRUE,
             encoding = "unknown", outputEncoding = "UTF-8",
             extraDirs = NULL, append = FALSE, silent = FALSE)
{
    ## sort order for topics, a little tricky
    re <- function(x) x[order(toupper(x), x)]

    ## given an installed package with a latex dir or a source package
    ## with a man dir, make a single file for use in the refman.

    options(warn = 1)
    if (missing(outfile))
        outfile <- paste(basename(pkgdir), "-pkg.tex", sep="")

    latexEncodings <- character() # Record any encodings used in the output

    ## First check for a latex dir.
    ## Second guess is this is a >= 2.10.0 package with stored .rds files.
    ## If it does not exist, guess this is a source package.
    latexdir <- file.path(pkgdir, "latex")
    if (!file_test("-d", latexdir)) {
        if (file_test("-d", file.path(pkgdir, "help"))) {
            ## So convert it
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            if (!silent) message("Converting parsed Rd's to LaTeX ",
                                 appendLF = FALSE, domain = NA)
            Rd <- Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
            if (!length(Rd)) {
                if (is.character(outfile))
                    close(file(outfile, if (append) "at" else "wt"))
                return(invisible(character()))
            }
            cnt <- 0L
            for(f in names(Rd)) {
                bf <- basename(f)
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("[Rr]d$", "tex", basename(f))
                latexEncodings <- c(latexEncodings,
                                    attr(Rd2latex(Rd[[f]],
                                                  file.path(latexdir, out),
                                                  encoding = encoding,
                                                  outputEncoding = outputEncoding,
                                                  defines = NULL,
                                                  writeEncoding = !asChapter),
                                         "latexEncoding"))
            }
            if (!silent) message(domain = NA)
        } else {
            files <- c(Sys.glob(file.path(pkgdir, "*.Rd")),
                       Sys.glob(file.path(pkgdir, "*.rd")))
            if (!length(files)) {
                ## is this a source package?  That has man/*.Rd files.
                files <- c(Sys.glob(file.path(pkgdir, "man", "*.Rd")),
                           Sys.glob(file.path(pkgdir, "man", "*.rd")))
                if (!length(files))
                    stop("this package does not have either a ", sQuote("latex"),
                         " or a (source) ", sQuote("man"), " directory",
                         domain = NA)
                if (is.null(extraDirs)) extraDirs <- .Platform$OS.type
                for(e in extraDirs)
                    files <- c(files,
                               Sys.glob(file.path(pkgdir, "man", e, "*.Rd")),
                               Sys.glob(file.path(pkgdir, "man", e, "*.rd")))
            }
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            if (!silent) message("Converting Rd files to LaTeX ",
                                 appendLF = FALSE, domain = NA)
            cnt <- 0L
            for(f in files) {
#                cat("  ", basename(f), "\n", sep="")
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("\\.[Rr]d$", ".tex", basename(f))
                latexEncodings <-
                    c(latexEncodings,
                      attr(Rd2latex(f, file.path(latexdir, out),
                                    encoding = encoding,
                                    outputEncoding = outputEncoding),
                           "latexEncoding"))
            }
            if (!silent) message(domain = NA)
        }
    }
    ## they might be zipped up
    if (file.exists(f <- file.path(latexdir, "Rhelp.zip"))) {
        dir.create(newdir <- tempfile("latex"))
        unzip(f, exdir = newdir)
        ## res <- system(paste("unzip -q", f, "-d", newdir))
        ## if (res) stop("unzipping latex files failed")
        latexdir <- newdir
    }
    ## There are some restrictions, but the former "[[:alnum:]]+\\.tex$" was
    ## too strict.
    files <- dir(latexdir, pattern = "\\.tex$", full.names = TRUE)
    if (!length(files))
        stop("no validly-named files in the ", sQuote("latex"), " directory",
             domain = NA)

    if (is.character(outfile)) {
        outcon <- file(outfile, if (append) "at" else "wt")
        on.exit(close(outcon))
    } else outcon <- outfile

    if (asChapter)
        cat("\n\\chapter{The \\texttt{", basename(pkgdir), "} package}\n",
            sep = "", file = outcon)
    topics <- rep.int("", length(files)); names(topics) <- files
    scanForEncoding <- !length(latexEncodings)
    for (f in files) {
        lines <- readLines(f)  # This reads as "unknown", no re-encoding done
        hd <- grep("^\\\\HeaderA", lines, value = TRUE,
                   perl = TRUE, useBytes = TRUE)
        if (!length(hd)) {
            warning("file ", sQuote(f), " lacks a header: skipping",
                    domain = NA)
            next
        }
        this <- sub("\\\\HeaderA\\{\\s*([^}]*)\\}.*", "\\1", hd[1L], perl = TRUE)
        if (!internals &&
           any(grepl("\\\\keyword\\{\\s*internal\\s*\\}", lines, perl = TRUE)))
            next
        if (scanForEncoding) {
	    enc <- lines[grepl('^\\\\inputencoding', lines, perl = TRUE)]
	    latexEncodings <- c(latexEncodings,
	                        sub("^\\\\inputencoding\\{(.*)\\}", "\\1", enc))
	}
        topics[f] <- this
    }

    topics <- topics[nzchar(topics)]
    summ <- grep("-package$", topics, perl = TRUE)
    topics <- if (length(summ)) c(topics[summ], re(topics[-summ])) else re(topics)
    for (f in names(topics)) writeLines(readLines(f), outcon)

    if (asChapter)
        cat("\\clearpage\n", file = outcon)

    invisible(latexEncodings)
}


### * .Rdconv

## replacement R code for Perl-based R CMD Rdconv

.Rdconv <- function(args = NULL)
{
    Usage <- function() {
        cat("Usage: R CMD Rdconv [options] FILE",
            "",
            "Convert R documentation in FILE to other formats such as plain text,",
            "HTML or LaTeX.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -t, --type=TYPE	convert to format TYPE",
            "  --encoding=enc        use 'enc' as the output encoding",
            "  --package=pkg         use 'pkg' as the package name",
            "  -o, --output=OUT	use 'OUT' as the output file",
            "      --os=NAME		assume OS 'NAME' (unix or windows)",
            "      --OS=NAME		the same as '--os'",
            "",
            "Possible format specifications are 'txt' (plain text), 'html', 'latex',",
            "and 'example' (extract R code in the examples).",
            "",
            "The default is to send output to stdout, which is also given by '-o -'.",
            "Using '-o \"\"' will choose an output filename by removing a '.Rd'",
            "extension from FILE and adding a suitable extension.",
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep = "\n")
    }

    options(showErrorCalls = FALSE, warn = 1)
    files <- character(0L)
    type <- "unknown"
    enc <- ""
    pkg <- ""
    out <- NULL
    os <- ""

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        }
        else if (a %in% c("-v", "--version")) {
            cat("Rdconv: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 1997-2009 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "-t") {
            if (length(args) >= 2L) {type <- args[2L]; args <- args[-1L]}
            else stop("-t option without value", call. = FALSE)
        } else if (substr(a, 1, 7) == "--type=") {
            type <- substr(a, 8, 1000)
        } else if (substr(a, 1, 10) == "--encoding=") {
            enc <- substr(a, 11, 1000)
        } else if (substr(a, 1, 10) == "--package=") {
            pkg <- substr(a, 11, 1000)
        } else if (a == "-o") {
            if (length(args) >= 2L) {out <- args[2L]; args <- args[-1L]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            out <- substr(a, 10, 1000)
        } else if (substr(a, 1, 5) %in% c("--os=", "--OS=")) {
            os <- substr(a, 6, 1000)
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else files <- c(files, a)
        args <- args[-1L]
    }
    if (length(files) != 1L)
        stop("exactly one Rd file must be specified", call. = FALSE)
    if (is.character(out) && !nzchar(out)) {
        ## choose 'out' from filename
        bf <- sub("\\.[Rr]d$", "", file)
        exts <- c(txt=".txt", html=".html", latex=".tex", exmaple=".R")
        out <- paste(bf,  exts[type], sep = "")
    } else if (is.null(out)) out <- ""
    if (!nzchar(os)) os <- .Platform$OS.type
    switch(type,
           "txt" = {
               Rd2txt(files, out, package=pkg, defines=os,
                      outputEncoding = enc)
           },
           "html" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2HTML(files, out, package = pkg, defines = os,
                       outputEncoding = enc, no_links = TRUE)
           },
           "latex" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2latex(files, out, defines = os,
                        outputEncoding = enc)
           },
           "example" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2ex(files, out, defines = os, outputEncoding = enc)
           },
           "unknown" = stop("no 'type' specified", call. = FALSE),
           stop("'type' must be one of 'txt', 'html', 'latex' or 'example'",
                call. = FALSE)
           )
    invisible()
}

### * .Rd2dvi

.Rd2dvi <-
function(pkgdir, outfile, title, batch = FALSE,
         description = TRUE, only_meta = FALSE,
         enc = "unknown", outputEncoding = "UTF-8", files_or_dir, OSdir,
         internals = "no", index = "true")
{
    ## Write directly to the final location.  Encodings may mean we need
    ## to make edits, but for most files one pass should be enough.
    out <- file(outfile, "wt")
    if (!nzchar(enc)) enc <- "unknown"
    description <- description == "true"
    only_meta <- only_meta == "true"
    internals <- internals != "no"
    index <- index != "false"

    desc <- NULL
    if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
        desc <- read.dcf(f)[1,]
        if (enc == "unknown") {
            pkg_enc <- desc["Encoding"]
            if (!is.na(pkg_enc)) {
            	enc <- pkg_enc
            	outputEncoding <- pkg_enc
            }
        }
    }

    ## Rd2.tex part 1: header
    if (batch == "true") writeLines("\\nonstopmode{}", out)
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4DVI", "ae"), "]{Rd}\n",
        sep = "", file = out)
    if (index) writeLines("\\usepackage{makeidx}", out)
    ## this needs to be canonical, e.g. 'utf8'
    setEncoding <- paste("\\usepackage[", latex_canonical_encoding(outputEncoding), "]{inputenc} % @SET ENCODING@", sep="")
    writeLines(c(setEncoding,
                 if (index) "\\makeindex{}",
                 "\\begin{document}"), out)
    if (!nzchar(title)) {
        if (is.character(desc))
            title <- paste("Package `", desc["Package"], "'", sep = "")
        else if (file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            desc <- read.dcf(f)[1,]
            title <- paste("Package `", desc["Package"], "'", sep = "")
        } else {
            if (file_test("-d", pkgdir)) {
                subj <- paste("all in \\file{", pkgdir, "}", sep ="")
            } else {
                files <- strsplit(files_or_dir, "[[:space:]]+")[[1L]]
                subj1 <- if (length(files) > 1L) " etc." else ""
                subj <- paste("\\file{", pkgdir, "}", subj1, sep = "")
            }
            subj <- gsub("[_$]", "\\\\1", subj)
            title <- paste("\\R{} documentation}} \\par\\bigskip{{\\Large of", subj)
        }
    }
    cat("\\chapter*{}\n",
        "\\begin{center}\n",
        "{\\textbf{\\huge ", title, "}}\n",
        "\\par\\bigskip{\\large \\today}\n",
        "\\end{center}\n", sep = "", file = out)
    if (description && file.exists(f <- file.path(pkgdir, "DESCRIPTION")))
        .DESCRIPTION_to_latex(f, out)
    ## running on the sources of a base package will have DESCRIPTION.in,
    ## only.
    if (description &&
        file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
        version <- readLines(file.path(pkgdir, "../../../VERSION"))
        .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION.in"),
                              out, version)
    }

    ## Rd2.tex part 2: body
    toc <- if (file_test("-d", files_or_dir)) {
        "\\Rdcontents{\\R{} topics documented:}"
    } else ""

    latexEncodings <- character(0)
    ## if this looks like a package with no man pages, skip body
    if (file.exists(file.path(pkgdir, "DESCRIPTION")) &&
        !(file_test("-d", file.path(pkgdir, "man")) ||
          file_test("-d", file.path(pkgdir, "help")) ||
          file_test("-d", file.path(pkgdir, "latex")))) only_meta <- TRUE
    if (!only_meta) {
        if (nzchar(toc)) writeLines(toc, out)
        latexEncodings <-
            .Rdfiles2tex(files_or_dir, out, encoding = enc, append = TRUE,
                         extraDirs = OSdir, internals = internals,
                         silent = (batch == "true"))
    }

    ## Rd2.tex part 3: footer
    if (index) writeLines("\\printindex{}", out)
    writeLines("\\end{document}", out)
    close(out)

    ## Fix up encodings
    ## FIXME cyrillic probably only works with times, not ae.
    latexEncodings <- unique(latexEncodings)
    latexEncodings <- latexEncodings[!is.na(latexEncodings)]
    cyrillic <- if (nzchar(Sys.getenv("_R_CYRILLIC_TEX_"))) "utf8" %in% latexEncodings else FALSE
    latex_outputEncoding <- latex_canonical_encoding(outputEncoding)
    encs <- latexEncodings[latexEncodings != latex_outputEncoding]
    if (length(encs) || cyrillic) {
        lines <- readLines(outfile)
	encs <- paste(encs, latex_outputEncoding, collapse=",", sep=",")

	if (!cyrillic) {
	    lines[lines == setEncoding] <-
		paste("\\usepackage[", encs, "]{inputenc}", sep = "")
	} else {
	    lines[lines == setEncoding] <-
		paste(
"\\usepackage[", encs, "]{inputenc}
\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}", sep = "")
	}
	writeLines(lines, outfile)
    }

    invisible(NULL)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
