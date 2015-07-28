#  File src/library/tools/R/Rd2pdf.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

#### R based engine for  R CMD Rdconv|Rd2pdf
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
    ## Using
    ##   desc <- .read_description(descfile)
    ## would preserve leading white space in Description and Author ...
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
        ## Drop 'Authors@R' for now: this is formatted badly by \AsIs,
        ## and ideally was used for auto-generating the Author and
        ## Maintainer fields anyways ...
        if(f == "Authors@R") next
        text <- desc[f]
        ## munge 'text' appropriately (\\, {, }, "...")
        ## not sure why just these: copied from Perl Rd2dvi, then added to.
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
        if(f %in% c("Author", "Maintainer", "Contact"))
            text <- gsub("<([^@ ]+)@([^> ]+)>",
                         "}\\\\email{\\1@\\2}\\\\AsIs{",
                         text, useBytes = TRUE)
        if(f %in% c("URL", "BugReports", "Contact"))
            text <- gsub("(http://|ftp://|https://)([^[:space:],]+)",
                         "}\\\\url{\\1\\2}\\\\AsIs{",
                         text, useBytes = TRUE)
        text <- paste0("\\AsIs{", text, "}")
        ## Not entirely safe: in theory, tags could contain \ ~ ^.
        cat("\\item[", gsub("([#$%&_{}])", "\\\\\\1", f),
            "]", paste(text, collapse = "\n\n"),  "\n", sep = "", file=out)
    }
    cat("\\end{description}\n", file = out)
}

## workhorse of .Rd2pdf
.Rdfiles2tex <-
    function(files, outfile, encoding = "unknown", outputEncoding = "UTF-8",
             append = FALSE, extraDirs = NULL, internals = FALSE,
             silent = FALSE)
{
    if (dir.exists(files))
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
        hasFigures <- FALSE
        for(f in files) {
            if (!silent) cat("  ", basename(f), "\n", sep="")
            if (!internals) {
                lines <- readLines(f)
                if (any(grepl("\\\\keyword\\{\\s*internal\\s*\\}",
                         lines, perl = TRUE))) next
            }
            out <-  file.path(latexdir, sub("\\.[Rr]d$", ".tex", basename(f)))
            ## people have file names with quotes in them.
            res <- Rd2latex(f, out, encoding = encoding,
                            outputEncoding = outputEncoding,
                            stages = c("build", "install", "render"))
            latexEncodings <- c(latexEncodings,
                                attr(res,"latexEncoding"))
            lines <- readLines(out)
            if (attr(res, "hasFigures")) {
                graphicspath <- paste("\\graphicspath{{",
                                      normalizePath(file.path(dirname(f), "figures"), "/"),
                                      "/}}", sep="")
            	lines <- c(graphicspath, lines)
            	hasFigures <- TRUE
            }
            writeLines(lines, outfile)
        }
        list(latexEncodings = unique(latexEncodings[!is.na(latexEncodings)]),
             hasFigures = hasFigures)
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
        outfile <- paste0(basename(pkgdir), "-pkg.tex")

    latexEncodings <- character() # Record any encodings used in the output
    hasFigures <- FALSE           # and whether graphics is used

    ## First check for a latex dir.
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
##                bf <- basename(f)
                cnt <- cnt + 1L
                if (!silent && cnt %% 10L == 0L)
                    message(".", appendLF=FALSE, domain=NA)
                out <-  sub("[Rr]d$", "tex", basename(f))
                outfilename <- file.path(latexdir, out)
                res <- Rd2latex(Rd[[f]],
				  outfilename,
				  encoding = encoding,
				  outputEncoding = outputEncoding,
				  defines = NULL,
				  writeEncoding = !asChapter)
                latexEncodings <- c(latexEncodings,
                                    attr(res, "latexEncoding"))
                if (attr(res, "hasFigures")) {
                    lines <- readLines(outfilename)
                    graphicspath <- paste("\\graphicspath{{",
                    		    normalizePath(file.path(pkgdir, "help", "figures"), "/"),
                    		    "/}}", sep="")
                    writeLines(c(graphicspath, lines), outfilename)
                    hasFigures <- TRUE
                }
            }
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
           } else {
                files <- c(Sys.glob(file.path(pkgdir, "*.Rd")),
                           Sys.glob(file.path(pkgdir, "*.rd")))
                if (!length(files))
                    stop("this package does not have either a ", sQuote("latex"),
                         " or a (source) ", sQuote("man"), " directory",
                         domain = NA)
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
                                outputEncoding = outputEncoding)
                latexEncodings <-
                    c(latexEncodings, attr(res, "latexEncoding"))
                if (attr(res, "hasFigures")) {
                    lines <- readLines(outfilename)
                    graphicspath <-
                        paste("\\graphicspath{{",
                              normalizePath(file.path(dirname(paths[i]),
                                                      "figures"),
                                            "/"),
                              "/}}",
                              sep = "")
                    writeLines(c(graphicspath, lines), outfilename)
                    hasFigures <- TRUE
                }
            }
            if (!silent) message(domain = NA)
        }
    }
    ## they might be zipped up
    if (file.exists(f <- file.path(latexdir, "Rhelp.zip"))) {
        dir.create(newdir <- tempfile("latex"))
        utils::unzip(f, exdir = newdir)
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

    invisible(list(latexEncodings = latexEncodings, hasFigures = hasFigures))
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
            "Report bugs at bugs.r-project.org .", sep = "\n")
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
                "Copyright (C) 1997-2009 The R Core Team.",
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
        out <- paste0(bf,  exts[type])
    } else if (is.null(out)) out <- ""
    if (!nzchar(os)) os <- .Platform$OS.type
    switch(type,
           "txt" = {
               Rd2txt(files, out, package=pkg, defines=os,
                      outputEncoding = enc,
                      stages = c("build", "install", "render"))
           },
           "html" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2HTML(files, out, package = pkg, defines = os,
                       outputEncoding = enc, no_links = TRUE,
                       stages = c("build", "install", "render"))
           },
           "latex" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2latex(files, out, defines = os,
                        outputEncoding = enc,
                        stages = c("build", "install", "render"))
           },
           "example" = {
               if (!nzchar(enc)) enc <- "UTF-8"
               Rd2ex(files, out, defines = os, outputEncoding = enc,
                     stages = c("build", "install", "render"))
           },
           "unknown" = stop("no 'type' specified", call. = FALSE),
           stop("'type' must be one of 'txt', 'html', 'latex' or 'example'",
                call. = FALSE)
           )
    invisible()
}

### * .Rd2pdf

.Rd2pdf <-
function(pkgdir, outfile, title, batch = FALSE,
         description = TRUE, only_meta = FALSE,
         enc = "unknown", outputEncoding = "UTF-8", files_or_dir, OSdir,
         internals = FALSE, index = TRUE)
{
    ## Write directly to the final location.  Encodings and figures
    ## may mean we need to make edits, but for most files one pass
    ## should be enough.
    out <- file(outfile, "wt")
    if (!nzchar(enc)) enc <- "unknown"

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
    if (batch) writeLines("\\nonstopmode{}", out)
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[", Sys.getenv("R_RD4PDF", "times,inconsolata,hyper"), "]{Rd}\n",
        sep = "", file = out)
    if (index) writeLines("\\usepackage{makeidx}", out)
    inputenc <- Sys.getenv("RD2PDF_INPUTENC", "inputenc")
    ## this needs to be canonical, e.g. 'utf8'
    ## trailer is for detection if we want to edit it later.
    latex_outputEncoding <- latex_canonical_encoding(outputEncoding)
    setEncoding <-
        paste("\\usepackage[",
              latex_outputEncoding, "]{",
              inputenc, "} % @SET ENCODING@", sep="")
    useGraphicx <- "% \\usepackage{graphicx} % @USE GRAPHICX@"
    writeLines(c(setEncoding,
                 if (inputenc == "inputenx" &&
                     latex_outputEncoding == "utf8") {
                     "\\IfFileExists{ix-utf8enc.dfu}{\\input{ix-utf8enc.dfu}}{}"
                 },
    		 useGraphicx,
                 if (index) "\\makeindex{}",
                 "\\begin{document}"), out)
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
            .DESCRIPTION_to_latex(f, out)
        else if(file.exists(f <- file.path(pkgdir, "DESCRIPTION.in"))) {
            ## running on the sources of a base package will have
            ## DESCRIPTION.in, only.
            version <- readLines(file.path(pkgdir, "../../../VERSION"))
            .DESCRIPTION_to_latex(file.path(pkgdir, "DESCRIPTION.in"),
                                  out, version)
        }
    }

    ## Rd2.tex part 2: body
    toc <- if (dir.exists(files_or_dir)) {
        "\\Rdcontents{\\R{} topics documented:}"
    } else ""

    latexEncodings <- character()
    hasFigures <- FALSE
    ## if this looks like a package with no man pages, skip body
    if (file.exists(file.path(pkgdir, "DESCRIPTION")) &&
        !(dir.exists(file.path(pkgdir, "man")) ||
          dir.exists(file.path(pkgdir, "help")) ||
          dir.exists(file.path(pkgdir, "latex")))) only_meta <- TRUE
    if (!only_meta) {
        if (nzchar(toc)) writeLines(toc, out)
        res <- .Rdfiles2tex(files_or_dir, out, encoding = enc, append = TRUE,
                         extraDirs = OSdir, internals = internals,
                         silent = batch)
        if(length(res)) {
            latexEncodings <- res$latexEncodings
            hasFigures <- res$hasFigures
        } else {
            latexEncodings <- character()
            hasFigures <- FALSE
        }
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
    encs <- latexEncodings[latexEncodings != latex_outputEncoding]
    if (length(encs) || hasFigures || cyrillic) {
        lines <- readLines(outfile)
        moreUnicode <- inputenc == "inputenx" && "utf8" %in% encs
	encs <- paste(encs, latex_outputEncoding, collapse=",", sep=",")

	if (!cyrillic) {
	    setEncoding2 <-
		paste0("\\usepackage[", encs, "]{", inputenc, "}")
	} else {
	    setEncoding2 <-
		paste(
"\\usepackage[", encs, "]{", inputenc, "}
\\IfFileExists{t2aenc.def}{\\usepackage[T2A]{fontenc}}{}", sep = "")
	}
	if (moreUnicode) {
	    setEncoding2 <-
		paste0(
setEncoding2, "
\\IfFileExists{ix-utf8enc.dfu}{\\input{ix-utf8enc.dfu}}{}")
        }
        lines[lines == setEncoding] <- setEncoding2
	if (hasFigures)
	    lines[lines == useGraphicx] <- "\\usepackage{graphicx}\\setkeys{Gin}{width=0.7\\textwidth}"
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

    ## <NOTE>
    ## For now only used for the R sources (/doc/manual/Makefile.in)
    ## hence no need to also look for Rd files with '.rd' extension.
    ## </NOTE>

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
    do_cleanup <- function() {
        if(clean) {
            setwd(startdir)
            unlink(build_dir, recursive = TRUE)
        } else {
            cat("You may want to clean up by 'rm -rf ", build_dir, "'\n", sep="")
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
            "The output encoding defaults to the package encoding then to 'UTF-8'.",
            "",
            "Files are listed in the order given: for a package they are in alphabetic",
            "order of the \\name sections.",
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "      --batch		no interaction",
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
            "      --build_dir=DIR	use DIR as the working directory",
            "",
            "The output papersize is set by the environment variable R_PAPERSIZE.",
            "The PDF previewer is set by the environment variable R_PDFVIEWER.",
            "",
            "Report bugs at bugs.r-project.org .",
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
    batch <- FALSE
    clean <- TRUE
    only_meta <- FALSE
    out_ext <- "pdf"
    output <- ""
    enc <- "unknown"
    outenc <- "latin1"
    index <- TRUE
    description <- TRUE
    internals <- FALSE
    files <- character()
    dir <- ""
    force <- FALSE

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
                "Copyright (C) 2000-2011 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            q("no", runLast = FALSE)
        } else if (a == "--batch") {
            batch <- TRUE
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
        } else if (substr(a, 1, 5) == "--OS=" || substr(a, 1, 5) == "--OS=") {
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
        } else if (substr(a, 1, 1) == "-") {
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
            cat("Hmm ... looks like a package\n")
            dir <- files[1L]
            if(!nzchar(output)) output <- paste(basename(dir), out_ext, sep = ".")
        } else if (file.exists(f <- file.path(files[1L], "DESCRIPTION.in"))
                   && any(grepl("^Priority: *base", readLines(f)))) {
            cat("Hmm ... looks like a package from the R distribution\n")
            dir <- files[1L]
            if(!nzchar(output)) output <- paste(basename(dir), out_ext, sep = ".")
            if(index && basename(dir) == "base") {
                index <- FALSE
                cat("_not_ indexing 'base' package\n")
            }
        } else {
            dir <- if(dir.exists(d <- file.path(files[1L], "man"))) d else files[1L]
        }
    } else {
        if(length(files) == 1L && !nzchar(output))
            output <- paste(sub("[.][Rr]d$", "", basename(files)), out_ext, sep = ".")
    }

    if(!nzchar(dir)) dir <- paste(files, collapse = " ")

    ## Prepare for building the documentation.
    if(dir.exists(build_dir) && unlink(build_dir, recursive = TRUE)) {
        cat("cannot write to build dir\n")
        q("no", status = 2L, runLast = FALSE)
    }
    dir.create(build_dir, FALSE)
    if(!nzchar(output)) output <- paste("Rd2", out_ext, sep = ".")
    if(file.exists(output) && !force) {
        cat("file", sQuote(output), "exists; please remove it first\n")
        q("no", status = 1L, runLast = FALSE)
    }

    res <-
        try(.Rd2pdf(files[1L], file.path(build_dir, "Rd2.tex"),
                    title, batch, description, only_meta,
                    enc, outenc, dir, OSdir, internals, index))
    if (inherits(res, "try-error"))
        q("no", status = 11L, runLast = FALSE)

    if (!batch)  cat("Creating", out_ext, "output from LaTeX ...\n")
    setwd(build_dir)

    res <- try(texi2pdf('Rd2.tex', quiet = FALSE, index = index))
    if(inherits(res, "try-error")) {
        res <- try(texi2pdf('Rd2.tex', quiet = FALSE, index = index))
        if(inherits(res, "try-error")) {
            message("Error in running tools::texi2pdf()")
            do_cleanup()
            q("no", status = 1L, runLast = FALSE)
        }
    }

    setwd(startdir)
    cat("Saving output to", sQuote(output), "...\n")
    file.copy(file.path(build_dir, paste("Rd2", out_ext, sep = ".")), output,
              overwrite = force)
    cat("Done\n")

    do_cleanup()
    if(preview != "false") system(paste(preview, output))
    if (quit)
    	q("no", runLast = FALSE)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
