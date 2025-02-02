#  File src/library/tools/R/Rd2pdf.R
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

#### R based engine for  R CMD Rdconv|Rd2pdf
####

## base packages do not have versions and this is called on
## DESCRIPTION.in
## encodings are tricky: this may be done in a foreign encoding
## (e.g., Latin-1 in UTF-8)
.DESCRIPTION_to_latex <- function(descfile, outfile, version = "Unknown",
                                  writeEncoding = TRUE)
{
    mytrfm <- .gsub_with_transformed_matches
    mygsub <- function(pattern, replacement, x)
        .Internal(gsub(pattern, replacement, x, FALSE, FALSE, FALSE, FALSE))
    ## Unlike tools:::psub and tools:::fsub, don't use useBytes = TRUE:
    mypsub <- function(pattern, replacement, x)
        .Internal(gsub(pattern, replacement, x, FALSE, TRUE,  FALSE, FALSE))
    myfsub <- function(pattern, replacement, x)
        .Internal(gsub(pattern, replacement, x, FALSE, FALSE,  TRUE, FALSE))
    texify <- function(x, one = TRUE, two = FALSE) {
        ## Handle LaTeX special characters.
        ## one: handle # $ % & _ ^ ~
        ##      backslash escape the first five
        ##      replace ^ by \textasciicircum{}
        ##      replace ~ by \textasciitilde{}
        ## two: handle { } \
        ##      backslash escape the first two
        ##      replace \ by \textbackslash{}
        if(two)
            x <- myfsub("\\", "\\textbackslash", x)
        if(one) {
            x <- mypsub("([#$%&_])", "\\\\\\1", x)
            x <- myfsub("^", "\\textasciicircum", x)
            x <- myfsub("~", "\\textasciitilde", x)
        }
        if(two) {
            x <- mypsub("([{}])", "\\\\\\1", x)
            x <- myfsub("\\textbackslash", "\\textbackslash{}", x)
        }
        if(one) {
            x <- myfsub("\\textasciicircum", "\\textasciicircum{}", x)
            x <- myfsub("\\textasciitilde", "\\textasciitilde{}", x)
        }
        x
    }
    mytrim <- function(x) {
        y <- unlist(strsplit(x, "\n", fixed = TRUE))
        lines2trim <- setdiff(which(nzchar(y)), 1L)
        if(!length(lines2trim))
            x
        else
            paste(replace(y, lines2trim,
                          .trim_common_leading_whitespace(y[lines2trim])),
                  collapse = "\n")
    }

    desc <- enc2utf8(.read_description(descfile))
    ## Drop empty fields: these are usually taken as missing.    
    desc <- desc[nzchar(desc)]
    if (is.character(outfile)) {
        out <- file(outfile, "a")
        on.exit(close(out))
    } else out <- outfile
    fields <- names(desc)
    fields <- fields %w/o% c("Package", "Packaged", "Built")
    if(writeEncoding && !is.na(desc["Encoding"])) {
        cat("\\inputencoding{utf8}\n", file = out)
    }
    ## Also try adding PDF title and author metadata.
    tit <- desc["Title"]
    tit <- paste0(desc["Package"], ": ",
                  texify(mygsub("[[:space:]]+", " ", tit), two = TRUE))
    tit <- paste0("\\ifthenelse{\\boolean{Rd@use@hyper}}",
                  "{\\hypersetup{pdftitle = {", tit, "}}}{}")
    writeLines(tit, con = out, useBytes = TRUE)
    ## Only try author from Authors@R.
    if(!is.na(aar <- desc["Authors@R"])) {
        aar <- tryCatch(utils:::.read_authors_at_R_field(aar),
                        error = identity)
        if(!inherits(aar, "error")) {
            aar <- Filter(utils:::.person_has_author_role, aar)
            aut <- format(aar, include = c("given", "family"))
            aut <- paste(aut[nzchar(aut)], collapse = "; ")
            aut <- texify(mygsub("[[:space:]]+", " ", aut), two = TRUE)
            if(nzchar(aut)) {
                aut <- paste0("\\ifthenelse{\\boolean{Rd@use@hyper}}",
                              "{\\hypersetup{pdfauthor = {", aut, "}}}{}")
                writeLines(aut, con = out, useBytes = TRUE)
            }
        }
    }
    ## And now the actual content.
    cat("\\begin{description}", "\\raggedright{}", sep="\n", file=out)
    for (f in fields) {
        ## Drop 'Authors@R' for now: this is formatted badly by \AsIs,
        ## and ideally was used for auto-generating the Author and
        ## Maintainer fields anyways ...
        if(f == "Authors@R") next
        text <- desc[f]
        if(f %in% c("Author", "Description"))
            text <- mytrim(text)
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Perl Rd2dvi, then added to.
        ## KH: the LaTeX special characters are
        ##   # $ % & _ ^ ~ { } \
        ## \Rd@AsIs@dospecials in Rd.sty handles the first seven, so
        ## braces and backslashes need explicit handling.
        text <- mygsub('"([^"]*)"', "\\`\\`\\1''", text)
        text <- texify(text, one = FALSE, two = TRUE)
        text <- myfsub("@VERSION@", version, text)
        if(f %in% c("Author", "Maintainer", "Contact"))
            text <- mytrfm("<([^@ ]+)@([^> ]+)>",
                           "}\\\\email{%s@%s}\\\\AsIs{",
                           text,
                           list(texify, texify),
                           c(1L, 2L))
        if(f %in% c("URL", "BugReports", "Additional_repositories"))
            text <- mygsub("(http://|ftp://|https://)([^[:space:],]+)",
                           "}\\\\url{\\1\\2}\\\\AsIs{",
                           text)
        if(f %in% c("Author",       # possibly with ORCID URLs inside <>
                    "Description")) {
            text <- mygsub("<(http://|ftp://|https://)([^[:space:],>]+)>",
                           "<}\\\\url{\\1\\2}\\\\AsIs{>",
                           text)
        }
        if(f == "Description") {   # DOI and arXiv identifiers inside <>
            text <- mytrfm("<(DOI:|doi:)([[:space:]]*)([^[:space:]]+)>",
                           "<}\\\\Rhref{https://doi.org/%s}{\\1%s}\\\\AsIs{>",
                           text,
                           list(identity, texify),
                           c(3L, 3L))
            ## Fancy escaping should not be needed for arXiv ids.
            text <- mygsub("<(arXiv|arxiv):([[:alnum:]/.-]+)([[:space:]]*\\[[^]]+\\])?>",
                           "<}\\\\Rhref{https://doi.org/10.48550/arXiv.\\2}{doi:10.48550/arXiv.\\2}\\\\AsIs{>",
                           text)
        }
        text <- paste0("\\AsIs{", text, "}")
        writeLines(paste0("\\item[", texify(f, TRUE, TRUE), "]",
                          text),
                   con = out, useBytes = TRUE)
    }
    cat("\\end{description}\n", file = out)
}

## workhorse of .Rd2pdf
.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", outputEncoding = "UTF-8",
             append = FALSE, extraDirs = NULL, internals = FALSE,
             silent = FALSE, pkglist = NULL)
{
    if (dir.exists(files)) {
        .pkg2tex(files, outfile, internals = internals, asChapter = FALSE, 
                 encoding = encoding, outputEncoding = outputEncoding,
                 extraDirs = extraDirs, append = append,
                 silent = silent, pkglist = pkglist)
    } else {
        files <- strsplit(files, "[[:space:]]+")[[1L]]
        if (!silent) message("Converting Rd files to LaTeX ...")
        if (is.character(outfile)) {
            outfile <- file(outfile, if (append) "at" else "wt")
            on.exit(close(outfile))
        }
        hasFigures <- FALSE
        macros <- initialRdMacros(pkglist = pkglist)
        for(f in files) {
            if (!silent) cat("  ", basename(f), "\n", sep="")
            rd <- parse_Rd(f, encoding = encoding, macros = macros)
            if (!internals &&
                any(.Rd_get_metadata(rd, "keyword") == "internal"))
                next
            lines <- character()
            con <- textConnection("lines", "w", local = TRUE)
            res <- Rd2latex(rd, con,
                            outputEncoding = outputEncoding,
                            writeEncoding = FALSE,
                            stages = c("build", "install", "render"))
            close(con) # ensure final line is output
            if (attr(res, "hasFigures")) {
                graphicspath <- file.path(dirname(f), "figures")
                lines <- c(.file_path_to_LaTeX_graphicspath(graphicspath),
                           lines)
            	hasFigures <- TRUE
            }
            writeLines(lines, outfile)
        }
        list(hasFigures = hasFigures)
    }
}

## used for the refman (from doc/manual/Makefile*)
## and for directories from .Rdfiles2tex  (with asChapter = FALSE)
.pkg2tex <-
    function(pkgdir, outfile, internals = FALSE, asChapter = TRUE,
             encoding = "unknown", outputEncoding = "UTF-8",
             extraDirs = NULL, append = FALSE, silent = FALSE,
             pkglist = NULL)
{
    ## For Rd \packageFOO macro expansion:
    path <- normalizePath(pkgdir)
    if(file.exists(file.path(path, "DESCRIPTION")))
        Sys.setenv("_R_RD_MACROS_PACKAGE_DIR_" = path)
    else if((basename(path) == "man") &&
            file.exists(file.path(dirname(path), "DESCRIPTION")))
        Sys.setenv("_R_RD_MACROS_PACKAGE_DIR_" = dirname(path))

    ## sort order for topics, a little tricky
    re <- function(x) x[order(toupper(x), x)]

    ## given an installed package with a latex dir or a source package
    ## with a man dir, make a single file for use in the refman.

    options(warn = 1)
    if (missing(outfile))
        outfile <- paste0(basename(pkgdir), "-pkg.tex")

    hasFigures <- FALSE
    graphicspath <- NULL

    ## First check for a latex dir (from R CMD INSTALL --latex).
    ## Second guess is this is a >= 2.10.0 package with stored .rds files.
    ## If it does not exist, guess this is a source package.
    latexdir <- file.path(pkgdir, "latex")
    if (!dir.exists(latexdir)) {
        if (dir.exists(file.path(pkgdir, "help"))) {
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
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("[Rr]d$", "tex", basename(f))
                outfilename <- file.path(latexdir, out)
                res <- Rd2latex(Rd[[f]],
				  outfilename,
				  outputEncoding = outputEncoding,
				  defines = NULL, # already processed
				  writeEncoding = FALSE)
                hasFigures <- hasFigures || attr(res, "hasFigures")
            }
            if (hasFigures)
                graphicspath <- file.path(pkgdir, "help", "figures")
            if (!silent) message(domain = NA)
        } else {
            ## As from R 2.15.3, give priority to a man dir.
            mandir <- file.path(pkgdir, "man")
            if (dir.exists(mandir)) {
                files <- c(Sys.glob(file.path(mandir, "*.Rd")),
                           Sys.glob(file.path(mandir, "*.rd")))
                if (is.null(extraDirs)) extraDirs <- .Platform$OS.type
                for(e in extraDirs)
                    files <- c(files,
                               Sys.glob(file.path(mandir, e, "*.Rd")),
                               Sys.glob(file.path(mandir, e, "*.rd")))
                if (!length(files))
                    stop("this package has a ", sQuote("man"), " directory but no .Rd files",
                         domain = NA)
                macros <- loadPkgRdMacros(pkgdir)
                macros <- initialRdMacros(pkglist, macros)
            } else {
                ## (Be nice and find Rd files & system macros also when 'pkgdir' is
                ## not a package root directory.)
                mandir <- pkgdir
                files <- c(Sys.glob(file.path(mandir, "*.Rd")),
                           Sys.glob(file.path(mandir, "*.rd")))
                if (!length(files))
                    stop("this package does not have either a ", sQuote("latex"),
                         " or a (source) ", sQuote("man"), " directory",
                         domain = NA)
                macros <- initialRdMacros(pkglist)
            }
            paths <- files
            ## Use a partial Rd db if there is one.
            ## In this case, files will become a list of paths or
            ## preprocessed Rd objects to be passed to Rd2latex(), and
            ## paths will contain the corresponding paths.
            built_file <- file.path(pkgdir, "build", "partial.rdb")
            if(file_test("-f", built_file)) {
                db <- readRDS(built_file)
                pos <- match(names(db), basename(paths), nomatch = 0L)
                files <- as.list(files)
                files[pos] <- db[pos > 0L]
            }
            ## Use a stage23 Rd db if there is one and we were asked to
            ## use it.
            built_file <- file.path(pkgdir, "build", "stage23.rdb")
            if(file_test("-f", built_file)) {
                use <- Sys.getenv("_RD2PDF_USE_BUILT_STAGE23_RD_DB_IF_AVAILABLE_",
                                  "FALSE")
                if(isTRUE(config_val_to_logical(use))) {
                    db <- readRDS(built_file)
                    pos <- match(names(db), basename(paths), nomatch = 0L)
                    files <- as.list(files)
                    files[pos] <- db[pos > 0L]
                }
            }
            latexdir <- tempfile("ltx")
            dir.create(latexdir)
            if (!silent) message("Converting Rd files to LaTeX ",
                                 appendLF = FALSE, domain = NA)
            cnt <- 0L
            for(i in seq_along(paths)) {
                cnt <- cnt + 1L
                if(!silent && cnt %% 10L == 0L)
                    message(".", appendLF = FALSE, domain = NA)
                out <-  sub("\\.[Rr]d$", ".tex", basename(paths[i]))
                outfilename <- file.path(latexdir, out)
                res <- Rd2latex(files[[i]], outfilename,
                                stages = c("build", "install", "render"),
                                encoding = encoding,
                                outputEncoding = outputEncoding,
                                writeEncoding = FALSE,
                                macros = macros)
                hasFigures <- hasFigures || attr(res, "hasFigures")
            }
            if (hasFigures)
                graphicspath <- file.path(mandir, "figures")
            if (!silent) message(domain = NA)
        }
    } else {
        graphicspath <- file.path(pkgdir, "help", "figures")
        hasFigures <- dir.exists(graphicspath)
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

    if (hasFigures && !is.null(graphicspath))
        cat(.file_path_to_LaTeX_graphicspath(graphicspath), "\n",
            sep = "", file = outcon)

    ## Extract (LaTeX-escaped, ASCII) \name for sorting.
    topics <- rep("", length(files))
    names(topics) <- files
    for (f in files) {
        lines <- readLines(f, encoding = "bytes") # possibly latin1, still
        hd <- lines[startsWith(lines, "\\HeaderA")]
        if (!length(hd)) {
            warning("file ", sQuote(f), " lacks a header: skipping",
                    domain = NA)
            next
        }
        this <- sub("\\\\HeaderA\\{\\s*([^}]*)\\}.*", "\\1", hd[1L], perl = TRUE)
        if (!internals &&
           any(startsWith(lines, "\\keyword{internal}")))
            next
        topics[f] <- this
    }

    topics <- topics[nzchar(topics)]
    ## <FIXME>
    ## these 'topics' come from Rd \name, not \alias entries, but we should
    ## (and WRE says) put the page aliased to the pkgname-package *topic* first
    ## which for >1500 CRAN packages is in a differently named file (90% pkg.Rd)
    ## </FIXME>
    summ <- which(endsWith(topics, "-package"))
    topics <- if (length(summ)) c(topics[summ], re(topics[-summ])) else re(topics)
    for (f in names(topics)) writeLines(readLines(f), outcon)

    if (asChapter)
        cat("\\clearpage\n", file = outcon)

    invisible(list(hasFigures = hasFigures))
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
            "  --RdMacros=pkglist",
            "             		packages from which to get Rd macros",
            "",
            "Possible format specifications are 'txt' (plain text), 'html', 'latex',",
            "and 'example' (extract R code in the examples).",
            "",
            "The default is to send output to stdout, which is also given by '-o -'.",
            "Using '-o \"\"' will choose an output filename by removing a '.Rd'",
            "extension from FILE and adding a suitable extension.",
            "",
            "Report bugs at <https://bugs.R-project.org>.", sep = "\n")
    }

    options(showErrorCalls = FALSE, warn = 1)
    files <- character(0L)
    type <- "unknown"
    enc <- ""
    pkg <- ""
    out <- NULL
    os <- ""
    pkglist <- NULL

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
                .R_copyright_msg(1997),
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "-t") {
            if (length(args) >= 2L) {type <- args[2L]; args <- args[-1L]}
            else stop("-t option without value", call. = FALSE)
        } else if (substr(a, 1, 7) == "--type=") {
            type <- substr(a, 8, 1000)
        } else if (substr(a, 1, 11) == "--encoding=") {
            enc <- substr(a, 12, 1000)
        } else if (substr(a, 1, 10) == "--package=") {
            pkg <- substr(a, 11, 1000)
        } else if (a == "-o") {
            if (length(args) >= 2L) {out <- args[2L]; args <- args[-1L]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            out <- substr(a, 10, 1000)
        } else if (substr(a, 1, 5) %in% c("--os=", "--OS=")) {
            os <- substr(a, 6, 1000)
        } else if (substr(a, 1, 11) == "--RdMacros=") {
            pkglist <- substr(a, 12, 1000)
        } else if (startsWith(a, "-")) {
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
        out <- paste0(bf,  exts[type])
    } else if (is.null(out)) out <- ""
    if (!nzchar(os)) os <- .Platform$OS.type
    macros <- initialRdMacros(pkglist = pkglist)
    switch(type,
           "txt" = {
               Rd2txt(files, out, package=pkg, defines=os,
                      outputEncoding = enc,
                      stages = c("build", "install", "render"),
                      macros = macros)
           },
           "html" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2HTML(files, out, package = pkg, defines = os,
                       outputEncoding = enc, no_links = TRUE,
                       stages = c("build", "install", "render"),
                       macros = macros)
           },
           "latex" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2latex(files, out, defines = os,
                        outputEncoding = enc,
                        stages = c("build", "install", "render"),
                        macros = macros)
           },
           "example" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2ex(files, out, defines = os, outputEncoding = enc,
                     stages = c("build", "install", "render"),
                     macros = macros)
           },
           "unknown" = stop("no 'type' specified", call. = FALSE),
           stop("'type' must be one of 'txt', 'html', 'latex' or 'example'",
                call. = FALSE)
           )
    invisible()
}

### * .Rd2pdf

.Rd2pdf <-
function(pkgdir, outfile, title, silent = FALSE,
         description = TRUE, only_meta = FALSE,
         enc = "unknown", outputEncoding = "UTF-8", files_or_dir, OSdir,
         internals = FALSE, index = TRUE, pkglist = NULL)
{
    ## Write directly to the final location.  Encodings and figures
    ## may mean we need to make edits, but for most files one pass
    ## should be enough.
    out <- file(outfile, "wt")
    if (!nzchar(enc)) enc <- "unknown"

    desc <- NULL
    preconverted <- FALSE
    if (file.exists(f <- file.path(pkgdir, "DESCRIPTION"))) {
        desc <- read.dcf(f)[1,]
        if (enc == "unknown") {
            pkg_enc <- desc["Encoding"]
            if (!is.na(pkg_enc)) {
            	enc <- pkg_enc
            }
        }
        ## 'outputEncoding' is irrelevant when pkgdir contains a package
        ## installed with --latex: tex files were written using pkg_enc
        ## and specify their \inputencoding, so we need inputenc.
        preconverted <- dir.exists(file.path(pkgdir, "latex"))
    }

    ## Rd2.tex part 1: header
    writeLines("\\nonstopmode{}", out)  # for texinfo < 6.7 and MikTeX's texify
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4PDF", "times,inconsolata,hyper"), "]{Rd}\n",
        sep = "", file = out)
    if (index) writeLines("\\usepackage{makeidx}", out)
    inputenc <- Sys.getenv("RD2PDF_INPUTENC", "inputenc")
    ## this needs to be canonical, e.g. 'utf8'
    ## trailer is for detection if we want to edit it later.
    latex_outputEncoding <- latex_canonical_encoding(outputEncoding)
    asUTF8 <- latex_outputEncoding == "utf8"
    setEncoding <-
        if (!preconverted && asUTF8 && inputenc == "inputenc") {
            paste0("\\makeatletter\\@ifl@t@r\\fmtversion{2018/04/01}{}{",
                   "\\usepackage[utf8]{inputenc}}",
                   "\\makeatother")
        } else
        paste0("\\usepackage[",
               if (asUTF8) "utf8"
               else paste0(c(if (description) "utf8", latex_outputEncoding), collapse=","),
               "]{", inputenc, "} % @SET ENCODING@")
    useGraphicx <- "% \\usepackage{graphicx} % @USE GRAPHICX@"
    writeLines(c(
        setEncoding,
        if (inputenc == "inputenx" && asUTF8) {
            "\\IfFileExists{ix-utf8enc.dfu}{\\input{ix-utf8enc.dfu}}{}"
        },
        if (nzchar(Sys.getenv("_R_CYRILLIC_TEX_")) && asUTF8) {
            "\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}"
        },
        useGraphicx,
        if (index) "\\makeindex{}",
        "\\begin{document}"
    ), out)
    if (!nzchar(title)) {
        if (is.character(desc))
            title <- paste0("Package `", desc["Package"], "'")
        else if (file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            desc <- read.dcf(f)[1,]
            title <- paste0("Package `", desc["Package"], "'")
        } else {
            if (dir.exists(pkgdir)) {
                subj <- paste0("all in \\file{", pkgdir, "}")
            } else {
                files <- strsplit(files_or_dir, "[[:space:]]+")[[1L]]
                subj1 <- if (length(files) > 1L) " etc." else ""
                subj <- paste0("\\file{", pkgdir, "}", subj1)
            }
            subj <- gsub("([_$])", "\\\\\\1", subj)
            title <- paste("\\R{} documentation}} \\par\\bigskip{{\\Large of", subj)
        }
    }
    cat("\\chapter*{}\n",
        "\\begin{center}\n",
        "{\\textbf{\\huge ", title, "}}\n",
        "\\par\\bigskip{\\large \\today}\n",
        "\\end{center}\n", sep = "", file = out)
    if(description) {
        if(file.exists(f <- file.path(pkgdir, "DESCRIPTION")))
            .DESCRIPTION_to_latex(f, out, writeEncoding = !asUTF8)
        else if(file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            ## running on the sources of a base package will have
            ## DESCRIPTION.in, only.
            version <- readLines(file.path(pkgdir, "../../../VERSION"))
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION.in"),
                                  out, version, writeEncoding = !asUTF8)
        }
    }

    ## Rd2.tex part 2: body
    hasFigures <- FALSE
    ## if this looks like a package with no man pages, skip body
    if (file.exists(file.path(pkgdir, "DESCRIPTION")) &&
        !(dir.exists(file.path(pkgdir, "man")) ||
          dir.exists(file.path(pkgdir, "help")) ||
          dir.exists(file.path(pkgdir, "latex")))) only_meta <- TRUE
    if (!only_meta) {
        if (dir.exists(files_or_dir))
            writeLines(c(
                "\\Rdcontents{Contents}",
                if (!asUTF8) paste0("\\inputencoding{", latex_outputEncoding, "}")
            ), out)
        res <- .Rdfiles2tex(files_or_dir, out, encoding = enc,
                            outputEncoding = outputEncoding,
                            append = TRUE, extraDirs = OSdir, 
                            internals = internals, silent = silent,
                            pkglist = pkglist)
        if(length(res)) {
            hasFigures <- res$hasFigures
        }
    }

    ## Rd2.tex part 3: footer
    if (index) writeLines("\\printindex{}", out)
    writeLines("\\end{document}", out)
    close(out)

    ## enable graphicx only if needed
    if (hasFigures) {
        lines <- readLines(outfile)
        lines[lines == useGraphicx] <-
            "\\usepackage{graphicx}\\setkeys{Gin}{width=0.7\\textwidth}"
	writeLines(lines, outfile)
    }

    invisible(NULL)
}

### * .Rdnewer

## replacement for tools/Rdnewer.pl,
## called from doc/manual/Makefile
.Rdnewer <- function(dir, file)
    q("no", status = ..Rdnewer(dir, file), runLast = FALSE)

..Rdnewer <- function(dir, file, OS = .Platform$OS.type)
{
    ## Test whether any Rd file in the 'man' and 'man/$OS'
    ## subdirectories of directory DIR is newer than a given FILE.
    ## Return 0 if such a file is found (i.e., in the case of
    ## 'success'), and 1 otherwise, so that the return value can be used
    ## for shell 'if' tests.

    if (!file.exists(file)) return(0L)
    age <- file.mtime(file)

    if (any(file.mtime(c(Sys.glob(file.path(dir, "man", "*.Rd")),
                         Sys.glob(file.path(dir, "man", "*.rd"))))
            > age))
        return(0L)

    if (dir.exists(file.path(dir, OS))) {
        if (any(file.mtime(c(Sys.glob(file.path(dir, "man", OS, "*.Rd")),
                             Sys.glob(file.path(dir, "man", OS, "*.rd"))))
                > age))
            return(0L)
    }

    1L
}

### * ..Rd2pdf

## Driver called from R CMD Rd2pdf
## See the comments in install.R as to how this can be called directly.

..Rd2pdf <- function(args = NULL, quit = TRUE)
{
    do_cleanup <- function(quiet = FALSE) {
        if(clean) {
            setwd(startdir)
            unlink(build_dir, recursive = TRUE)
        } else if (!quiet) {
            cat("You may want to clean up by 'rm -Rf ", build_dir, "'\n", sep="")
        }
    }

    Usage <- function() {
        cat("Usage: R CMD Rd2pdf [options] files",
            "",
            "Generate PDF output from the Rd sources specified by files, by",
            "either giving the paths to the files, or the path to a directory with",
            "the sources of a package, or an installed package.",
            "",
            "Unless specified via option '--output', the basename of the output file",
            "equals the basename of argument 'files' if this specifies a package",
            "or a single file, and 'Rd2' otherwise.",
            "",
            "The Rd sources are assumed to be ASCII unless they contain \\encoding",
            "declarations (which take priority) or --encoding is supplied or if using",
            "package sources, if the package DESCRIPTION file has an Encoding field.",
            "The output encoding defaults to 'UTF-8'.",
            "",
            "Files are listed in the order given: for a package they are in alphabetic",
            "order of the \\name sections.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "  -q, --quiet		no output unless errors",
            "      --no-clean	do not remove created temporary files",
            "      --no-preview	do not preview generated PDF file",
            "      --encoding=enc    use 'enc' as the default input encoding",
            "      --outputEncoding=outenc",
            "                        use 'outenc' as the default output encoding",
            "      --os=NAME		use OS subdir 'NAME' (unix or windows)",
            "      --OS=NAME		the same as '--os'",
            "  -o, --output=FILE	write output to FILE",
            "      --force		overwrite output file if it exists",
            "      --title=NAME	use NAME as the title of the document",
            "      --no-index	do not index output",
            "      --no-description	do not typeset the description of a package",
            "      --internals	typeset 'internal' documentation (usually skipped)",
            "      --build-dir=DIR	use DIR as the working directory",
            "      --RdMacros=pkglist",
            "             		packages from which to get Rd macros",
            "",
            "The output papersize is set by the environment variable R_PAPERSIZE.",
            "The PDF previewer is set by the environment variable R_PDFVIEWER.",
            "",
            "Report bugs at <https://bugs.R-project.org>.",
            sep = "\n")
    }

    options(showErrorCalls = FALSE, warn = 1)

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    startdir <- getwd()
    if (is.null(startdir))
        stop("current working directory cannot be ascertained")
    build_dir <- paste0(".Rd2pdf", Sys.getpid())
    title <- ""
    quiet <- FALSE
    clean <- TRUE
    only_meta <- FALSE
    out_ext <- "pdf"
    output <- ""
    enc <- "unknown"
    outenc <- "UTF-8"
    index <- TRUE
    description <- TRUE
    internals <- FALSE
    files <- character()
    dir <- ""
    force <- FALSE
    pkglist <- NULL

    WINDOWS <- .Platform$OS.type == "windows"

    preview <- Sys.getenv("R_PDFVIEWER", if(WINDOWS) "open" else "false")
    OSdir <- if (WINDOWS) "windows" else "unix"

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            q("no", runLast = FALSE)
        } else if (a %in% c("-v", "--version")) {
            cat("Rd2pdf: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                .R_copyright_msg(2000),
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "--batch") {
            # ignore for back-compatibility (now always use batch)
        } else if (a %in% c("-q", "--quiet")) {
            quiet <- TRUE
        } else if (a == "--no-clean") {
            clean <- FALSE
        } else if (a == "--no-preview") {
            preview <- "false"
        } else if (a == "--pdf") {
            # ignore for back-compatibility
        } else if (substr(a, 1, 8) == "--title=") {
            title <- substr(a, 9, 1000)
        } else if (a == "-o") {
            if (length(args) >= 2L) {output <- args[2L]; args <- args[-1L]}
            else stop("-o option without value", call. = FALSE)
        } else if (substr(a, 1, 9) == "--output=") {
            output <- substr(a, 10, 1000)
        } else if (a == "--force") {
            force <- TRUE
        } else if (a == "--only-meta") {
            only_meta <- TRUE
        } else if (substr(a, 1, 5) %in% c("--os=", "--OS=")) {
            OSdir <- substr(a, 6, 1000)
        } else if (substr(a, 1, 11) == "--encoding=") {
            enc <- substr(a, 12, 1000)
        } else if (substr(a, 1, 17) == "--outputEncoding=") {
            outenc <- substr(a, 18, 1000)
        } else if (substr(a, 1, 12) == "--build-dir=") {
            build_dir <- substr(a, 13, 1000)
        } else if (a == "--no-index") {
            index <- FALSE
        } else if (a == "--no-description") {
            description <- FALSE
        } else if (a == "--internals") {
            internals <- TRUE
        } else if (substr(a, 1, 11) == "--RdMacros=") {
            pkglist <- substr(a, 12, 1000)
        } else if (startsWith(a, "-")) {
            message("Warning: unknown option ", sQuote(a))
        } else files <- c(files, a)
        args <- args[-1L]
    }

    if(!length(files)) {
        message("no inputs")
        q("no", status = 1L, runLast = FALSE)
    }

    ## Windows does not allow .../man/, say, for a directory
    if(WINDOWS) files[1L] <- sub("[\\/]$", "", files[1L])
    if(dir.exists(files[1L])) {
        if(file.exists(file.path(files[1L], "DESCRIPTION"))) {
            if (!quiet) cat("Hmm ... looks like a package\n")
            dir <- files[1L]
            if(!nzchar(output)) output <- paste(basename(dir), out_ext, sep = ".")
        } else if (file.exists(f <- file.path(files[1L], "DESCRIPTION.in"))
                   && any(grepl("^Priority: *base", readLines(f)))) {
            if (!quiet) cat("Hmm ... looks like a package from the R distribution\n")
            dir <- files[1L]
            if(!nzchar(output)) output <- paste(basename(dir), out_ext, sep = ".")
            if(index && basename(dir) == "base") {
                index <- FALSE
                if (!quiet) cat("_not_ indexing 'base' package\n")
            }
        } else {
            dir <- if(dir.exists(d <- file.path(files[1L], "man"))) d else files[1L]
        }
    } else {
        description <- FALSE
        if(length(files) == 1L && !nzchar(output))
            output <- paste(sub("[.][Rr]d$", "", basename(files)), out_ext, sep = ".")
    }

    if(!nzchar(dir)) dir <- paste(files, collapse = " ")

    ## Prepare for building the documentation.
    if(!nzchar(output)) output <- paste0("Rd2.", out_ext)
    if(file.exists(output) && !force) {
        cat("file", sQuote(output), "exists; please remove it first\n")
        q("no", status = 1L, runLast = FALSE)
    }
    if(dir.exists(build_dir) && unlink(build_dir, recursive = TRUE)) {
        cat("cannot write to build dir\n")
        q("no", status = 2L, runLast = FALSE)
    }
    dir.create(build_dir, FALSE)

    res <-
        try(.Rd2pdf(files[1L], file.path(build_dir, "Rd2.tex"),
                    title, quiet, description, only_meta,
                    enc, outenc, dir, OSdir, internals, index,
                    pkglist))
    if (inherits(res, "try-error"))
        q("no", status = 11L, runLast = FALSE)

    if (!quiet)  cat("Creating", out_ext, "output from LaTeX ...\n")
    setwd(build_dir)

    ## R CMD _appends_ R's texmf tree to environmental TEXINPUTS, which could
    ## list another R version, so ensure Rd2pdf finds _this_ R's Rd.sty
    texinputs <- file.path(R.home("share"), "texmf", "tex", "latex")
    res <- try(texi2pdf('Rd2.tex', quiet = quiet, index = index, texinputs = texinputs))
    if(inherits(res, "try-error")) {
        res <- try(texi2pdf('Rd2.tex', quiet = quiet, index = index, texinputs = texinputs))
        if(inherits(res, "try-error")) {
            message("Error in running tools::texi2pdf()")
            do_cleanup()
            q("no", status = 1L, runLast = FALSE)
        }
    }

    setwd(startdir)
    if (!quiet)  cat("Saving output to", sQuote(output), "...\n")
    file.copy(file.path(build_dir, paste0("Rd2.", out_ext)), output,
              overwrite = force)
    if (!quiet)  cat("Done\n")

    do_cleanup(quiet)
    if(preview != "false") system(paste(preview, output))
    if (quit)
    	q("no", runLast = FALSE)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
