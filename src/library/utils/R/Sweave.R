#   File src/library/utils/R/Sweave.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

### The drivers are now in SweaveDrivers.R

### FIXMEs
### b) It would be nice to allow multiple 'grdevice' options

### Encodings (currently, different from 2.13.0)
###
### SweaveReadFile figures out an encoding, uses it (not currently for
### \SweaveInclude files) and returns it as an attribute.  This is
### then passed as an attribute of 'file' to the driver's setup
### routine.  Unless it is "" or "ASCII", the RweaveLatex driver
### re-encodes the output back to 'encoding': the Rtangle driver
### leaves it in the encoding of the current locale and records what
### that is in a comment.  The "UTF-8" encoding is preserved on
### both input and output in RweaveLatex, but is handled like
### other encodings in Rtangle.
###
### SweaveReadFile first looks for a call to one of the LaTeX packages
### inputen[cx] and deduces the vignette encoding from that, falling
### back to the package encoding, then Latin-1 (with a warning).  This
### should work OK provided the package encoding is Latin-1: it is
### UTF-8 then LaTeX needs to be told what to do.  It also assumes
### that R output is in the current locale: a package with a different
### encoding from the current one might have data in that package's
### encoding.


### Correspondence between input and output is maintained in two
### places: Each chunk has a srclines attribute, recording the input
### lines it corresponds to.  Each code chunk will have attached
### srcrefs that duplicate the srclines.  We don't need srclines for
### code, but we do need it for doc chunks, and it's easiest to just
### keep it for everything.



Stangle <- function(file, driver = Rtangle(),
                    syntax = getOption("SweaveSyntax"),
                    encoding = "", ...)
    Sweave(file = file, driver = driver, encoding = encoding, ...)

Sweave <- function(file, driver = RweaveLatex(),
                   syntax = getOption("SweaveSyntax"),
                   encoding = "", ...)
{
    if (is.character(driver)) driver <- get(driver, mode = "function")()
    else if (is.function(driver)) driver <- driver()

    if (is.null(syntax)) syntax <- SweaveGetSyntax(file) # from the extension
    if (is.character(syntax)) syntax <- get(syntax, mode = "list")

    if (.Platform$OS.type == "windows") file <- chartr("\\", "/", file)

    text <- SweaveReadFile(file, syntax, encoding = encoding)
    attr(file, "encoding") <- encoding <- attr(text, "encoding")
    srcFilenames <- attr(text, "files")
    srcFilenum <- attr(text, "srcFilenum")
    srcLinenum <- attr(text, "srcLinenum")

    ## drobj$options is the current set of options for this file.
    drobj <- driver$setup(file = file, syntax = syntax, ...)
    on.exit(driver$finish(drobj, error = TRUE))

    syntax <- attr(text, "syntax") # this is from the file commands.

    if (!is.na(envopts <- Sys.getenv("SWEAVE_OPTIONS", NA)))
        drobj$options <-
            SweaveParseOptions(envopts, drobj$options, driver$checkopts)

    drobj$filename <- file

    mode <- "doc"
    chunknr <- 0L
    chunk <- NULL
    chunkopts <- NULL

    namedchunks <- list()
    prevfilenum <- 0L
    prevlinediff <- 0L
    for (linenum in seq_along(text)) {
    	line <- text[linenum]
    	filenum <- srcFilenum[linenum]
    	linediff <- srcLinenum[linenum] - linenum
	if(nzchar(Sys.getenv("R_DEBUG_Sweave"))) {
	    ## Extensive logging for debugging, needs 'ls' (unix-like or Rtools):
	    cat(sprintf("l.%3d: %30s -'%4s'- ", linenum, substr(line,1,30), mode))
	    cat(sprintf("%16s\n", system(paste("ls -s",
				   summary(drobj$output)$description), intern=TRUE)))
	}
        if (length(grep(syntax$doc, line))) { # start new documentation chunk
            if (mode == "doc") {
                if (!is.null(chunk)) drobj <- driver$writedoc(drobj, chunk)
            } else {
                if (!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        } else if (length(grep(syntax$code, line))) { # start new code chunk
            if (mode == "doc") {
                if (!is.null(chunk)) drobj <- driver$writedoc(drobj, chunk)
            } else {
                if (!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
            }
            mode <- "code"
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts,
                                            drobj$options,
                                            driver$checkopts)
            ## these #line directives are used for error messages when parsing
            file <- srcFilenames[filenum]
            chunk <- paste0("#line ", linenum+linediff+1L, ' "', basename(file), '"')
            attr(chunk, "srclines") <- linenum + linediff
            attr(chunk, "srcFilenum") <- filenum
            attr(chunk, "srcFilenames") <- srcFilenames
            chunknr <- chunknr + 1L  # this is really 'code chunk number'
            chunkopts$chunknr <- chunknr
        } else {  # continuation of current chunk
            if (mode == "code" && length(grep(syntax$coderef, line))) {
                chunkref <- sub(syntax$coderef, "\\1", line)
                if (!(chunkref %in% names(namedchunks))) {
                    ## omit unknown references
                    warning(gettextf("reference to unknown chunk %s",
                                     sQuote(chunkref)),
                            call. = TRUE,domain = NA)
                    next
                } else {
                    ## these #line directives are used for error messages
                    ## when parsing
                    file <- srcFilenames[filenum]
                    line <- c(namedchunks[[chunkref]],
			      paste0("#line ", linenum+linediff+1L,
				     ' "', basename(file), '"'))
                }
            }
            if (mode == "code" &&
                (prevfilenum != filenum ||
                 prevlinediff != linediff)) {
                file <- srcFilenames[filenum]
                line <- c(paste0("#line ", linenum+linediff, ' "', basename(file), '"'),
                          line)
            }
            srclines <- c(attr(chunk, "srclines"), rep.int(linenum+linediff, length(line)))
            srcfilenum <- c(attr(chunk, "srcFilenum"), rep.int(filenum, length(line)))
	    chunk <- c(chunk, line)
            attr(chunk, "srclines") <- srclines
            attr(chunk, "srcFilenum") <- srcfilenum
            attr(chunk, "srcFilenames") <- srcFilenames
	}
	prevfilenum <- filenum
	prevlinediff <- linediff
    }
    if (!is.null(chunk)) { # write out final chunk
	drobj <-
	    if (mode == "doc") driver$writedoc(drobj, chunk)
	    else driver$runcode(drobj, chunk, chunkopts)
    }

    on.exit() # clear action to finish with error = TRUE
    drobj$srcFilenames <- srcFilenames
    driver$finish(drobj)
}

SweaveReadFile <- function(file, syntax, encoding = "")
{
    ## file can be a vector to keep track of recursive calls to
    ## SweaveReadFile.  In this case only the first element is
    ## tried to read in, the rest are forbidden names for further
    ## SweaveInput
    f <- file[1L]

    bf <- basename(f)
    df <- dirname(f)
    if (!file.exists(f)) {
        f <- list.files(df, full.names = TRUE,
                        pattern = paste0(bf, syntax$extension))

        if (length(f) == 0L)
            stop(gettextf("no Sweave file with name %s found",
                          sQuote(file[1L])), domain = NA)
        else if (length(f) > 1L)
            stop(paste(sprintf(ngettext(length(f), "%d Sweave file for basename %s found",
                                        "%d Sweave files for basename %s found",

                                domain = "R-utils"),
                                length(f), sQuote(file[1L])), paste(":\n         ", f, collapse = "")),
                 domain = NA)
    }

    ## An incomplete last line is not a real problem.
    text <- readLines(f[1L], warn = FALSE)
    srcLinenum <- seq_along(text)

    if (encoding != "bytes")  {
        ## now sort out an encoding, if needed.
        enc <- tools:::.getVignetteEncoding(text,
			    default = if (identical(encoding, "")) NA else encoding)
        if (enc == "non-ASCII") {
            enc <- if (nzchar(encoding)) {
                encoding
            } else {
                stop(sQuote(basename(file)),
                        " is not ASCII and does not declare an encoding",
                        domain = NA, call. = FALSE)
            }
        } else if (enc == "unknown") {
            stop(sQuote(basename(file)),
                 " declares an encoding that Sweave does not know about",
                 domain = NA, call. = FALSE)
        }
        if (enc == "UTF-8")
            Encoding(text) <- enc
        else {
            if (nzchar(enc)) text <- iconv(text, enc, "") else enc <- "ASCII"
        }
    } else enc <- "bytes"

    pos <- grep(syntax$syntaxname, text)

    if (length(pos) > 1L)
        warning(gettextf("more than one syntax specification found, using the first one"),
		domain = NA)

    if (length(pos) > 0L) {
        sname <- sub(syntax$syntaxname, "\\1", text[pos[1L]])
        syntax <- get(sname, mode = "list")
        if (!identical(class(syntax), "SweaveSyntax"))
            stop(gettextf("object %s does not have class \"SweaveSyntax\"",
                          sQuote(sname)), domain = NA)
        text <- text[-pos]
        srcLinenum <- srcLinenum[-pos]
    }
    srcFilenum <- rep_len(1, length(srcLinenum))

    if (!is.null(syntax$input)) {
        while(length(pos <- grep(syntax$input, text))) {
            pos <- pos[1L]
            ifile <- file.path(df, sub(syntax$input, "\\1", text[pos]))
            if (any(ifile == file)) {
                stop(paste(gettextf("recursive Sweave input %s in stack",
                                    sQuote(ifile)),
                           paste("\n         ", seq_len(file), ": ",
                                 rev(file), collapse="")),
                 domain = NA)
            }
            itext <- SweaveReadFile(c(ifile, file), syntax, encoding = encoding)

	    pre <- seq_len(pos-1L)
	    post <- seq_len(length(text) - pos) + pos
	    text <- c(text[pre], itext, text[post])

	    srcLinenum <- c(srcLinenum[pre], attr(itext, "srcLinenum"),
	    		    srcLinenum[post])
	    srcFilenum <- c(srcFilenum[pre], attr(itext, "srcFilenum")+length(f),
	    		    srcFilenum[post])
	    f <- c(f, attr(itext, "files"))
        }
    }

    attr(text, "syntax") <- syntax
    attr(text, "files") <- f
    attr(text, "encoding") <- enc
    attr(text, "srcLinenum") <- srcLinenum
    attr(text, "srcFilenum") <- srcFilenum
    text
}



###**********************************************************

## NB: } should not be escaped in [] .
SweaveSyntaxNoweb <-
    list(doc = "^@",
         code = "^<<(.*)>>=.*",
         coderef = "^<<(.*)>>.*",
         docopt = "^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}",
         docexpr = "\\\\Sexpr\\{([^}]*)\\}",
         extension = "\\.[rsRS]?nw$",
         syntaxname = "^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}",
         input = "^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}",
         trans = list(
             doc = "@",
             code = "<<\\1>>=",
             coderef = "<<\\1>>",
             docopt = "\\\\SweaveOpts{\\1}",
             docexpr = "\\\\Sexpr{\\1}",
             extension = ".Snw",
             syntaxname = "\\\\SweaveSyntax{SweaveSyntaxNoweb}",
             input = "\\\\SweaveInput{\\1}")
         )

class(SweaveSyntaxNoweb) <- "SweaveSyntax"

SweaveSyntaxLatex <- SweaveSyntaxNoweb
SweaveSyntaxLatex$doc <-  "^[[:space:]]*\\\\end\\{Scode\\}"
SweaveSyntaxLatex$code <- "^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^}]*)\\}?.*"
SweaveSyntaxLatex$coderef <- "^[[:space:]]*\\\\Scoderef\\{([^}]*)\\}.*"
SweaveSyntaxLatex$extension <- "\\.[rsRS]tex$"

SweaveSyntaxLatex$trans$doc <-  "\\\\end{Scode}"
SweaveSyntaxLatex$trans$code <- "\\\\begin{Scode}{\\1}"
SweaveSyntaxLatex$trans$coderef <- "\\\\Scoderef{\\1}"
SweaveSyntaxLatex$trans$syntaxname <- "\\\\SweaveSyntax{SweaveSyntaxLatex}"
SweaveSyntaxLatex$trans$extension <- ".Stex"

SweaveGetSyntax <- function(file)
{
    synt <- apropos("SweaveSyntax", mode = "list")
    for (sname in synt) {
        s <- get(sname, mode = "list")
        if (!identical(class(s), "SweaveSyntax")) next
        if (length(grep(s$extension, file))) return(s)
    }
    SweaveSyntaxNoweb
}


SweaveSyntConv <- function(file, syntax, output=NULL)
{
    if (is.character(syntax)) syntax <- get(syntax)

    if (!identical(class(syntax), "SweaveSyntax"))
        stop(gettextf("target syntax not of class %s",
                      dQuote("SweaveSyntax")),
             domain = NA)
    if (is.null(syntax$trans))
        stop("target syntax contains no translation table")

    insynt <- SweaveGetSyntax(file)
    text <- readLines(file)
    if (is.null(output))
        output <- sub(insynt$extension, syntax$trans$extension, basename(file))

    TN <- names(syntax$trans)

    for (n in TN)
        if (n != "extension") text <- gsub(insynt[[n]], syntax$trans[[n]], text)

    cat(text, file = output, sep = "\n")
    cat("Wrote file", output, "\n")
}


###**********************************************************

## parses an option string, from
## - the header of a code chunk
## - an \SweaveOpts{} statement (strangely, left to the drivers)
## - the value of environment variable SWEAVE_OPTIONS
##
## The format is name=value pairs with whitespace being discarded
## (and could have been done all at once).
SweaveParseOptions <- function(text, defaults = list(), check = NULL)
{
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")

    ## only the first option may have no name: the chunk label
    if (length(x)) {
        if (length(x[[1L]]) == 1L) x[[1L]] <- c("label", x[[1L]])
    } else return(defaults)

    if (any(lengths(x) != 2L))
        stop(gettextf("parse error or empty option in\n%s", text), domain = NA)

    options <- defaults
    for (k in seq_along(x)) options[[ x[[k]][1L] ]] <- x[[k]][2L]

    ## This is undocumented
    if (!is.null(options[["label"]]) && !is.null(options[["engine"]]))
        options[["label"]] <-
            sub(paste0("\\.", options[["engine"]], "$"),
                "", options[["label"]])

    if (!is.null(check)) check(options) else options
}

## really part of the RweaveLatex and Rtangle drivers
SweaveHooks <- function(options, run = FALSE, envir = .GlobalEnv)
{
    if (is.null(SweaveHooks <- getOption("SweaveHooks"))) return(NULL)

    z <- character()
    for (k in names(SweaveHooks))
        if (nzchar(k) && is.logical(options[[k]]) && options[[k]])
            if (is.function(SweaveHooks[[k]])) {
                z <- c(z, k)
                if (run) eval(SweaveHooks[[k]](), envir=envir)
            }
    z # a character vector.
}

### For R CMD xxxx ------------------------------------------
.Sweave <- function(args = NULL, no.q = interactive())
{
    options(warn = 1)
    if (is.null(args)) {
        args <- commandArgs(TRUE)
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    Usage <- function() {
        cat("Usage: R CMD Sweave [options] file",
            "",
            "A front-end for Sweave and other vignette engines, via buildVignette()",
            "",
            "Options:",
            "  -h, --help      print this help message and exit",
            "  -v, --version   print version info and exit",
            "  --driver=name   use named Sweave driver",
            "  --engine=pkg::engine  use named vignette engine",
            "  --encoding=enc  default encoding 'enc' for file",
	    "  --clean         corresponds to --clean=default",
	    "  --clean=        remove some of the created files:",
            '                  "default" removes those the same initial name;',
            '                  "keepOuts" keeps e.g. *.tex even when PDF is produced',
            "  --options=      comma-separated list of Sweave/engine options",
            "  --pdf           convert to PDF document",
            "  --compact=      try to compact PDF document:",
            '                  "no" (default), "qpdf", "gs", "gs+qpdf", "both"',
            "  --compact       same as --compact=qpdf",
            "",
            "Report bugs at <https://bugs.R-project.org>.",
            sep = "\n")
    }
    do_exit <-
	if(no.q)
	    function(status = 0L) (if(status) stop else message)(
		".Sweave() exit status ", status)
	else
	    function(status = 0L) q("no", status = status, runLast = FALSE)

    if (!length(args)) {
        Usage()
        do_exit(1L)
    }
    file <- character()
    driver <- encoding <- options <- ""
    engine <- NULL
    toPDF <- FALSE
    compact <- Sys.getenv("_R_SWEAVE_COMPACT_PDF_", "no")
    clean <- FALSE ## default!
    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            do_exit()
        }
        else if (a %in% c("-v", "--version")) {
            cat("Sweave front-end: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2006-2014 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            do_exit()
        } else if (substr(a, 1, 9) == "--driver=") {
            driver <- substr(a, 10, 1000)
        } else if (substr(a, 1, 9) == "--engine=") {
            engine <- substr(a, 10, 1000)
        } else if (substr(a, 1, 11) == "--encoding=") {
            encoding <- substr(a, 12, 1000)
	} else if (a == "--clean") {
	    clean <- TRUE
	} else if (substr(a, 1, 8) == "--clean=") {
	    clean. <- substr(a, 9, 1000)
	    clean <- switch(clean.,
			    "default" = TRUE,
			    "keepOuts" = NA,
			    message(gettextf("Warning: unknown option '--clean='%s",
					     clean.), domain = NA))
        } else if (substr(a, 1, 10) == "--options=") {
            options <- substr(a, 11, 1000)
        } else if (a == "--pdf") {
            toPDF <- TRUE
        } else if (substr(a, 1, 10) == "--compact=") {
            compact <- substr(a, 11, 1000)
        } else if (a == "--compact") {
            compact <- "qpdf"
        } else if (startsWith(a, "-")) {
            message(gettextf("Warning: unknown option %s", sQuote(a)),
                    domain = NA)
        } else file <- c(file, a)
       args <- args[-1L]
    }
    if(length(file) != 1L) {
        Usage()
        do_exit(1L)
    }
    args <- list(file=file, tangle=FALSE, latex=toPDF, engine=engine, clean=clean)
    if(nzchar(driver)) args <- c(args, driver)
    args <- c(args, encoding = encoding)
    if(nzchar(options)) {
        opts <- eval(str2expression(paste0("list(", options, ")")))
        args <- c(args, opts)
    }
    output <- do.call(tools::buildVignette, args)
    message("Output file:  ", output)
    if (toPDF && compact != "no"
        && length(output) == 1 && grepl(".pdf$", output, ignore.case=TRUE)) {
	## <NOTE>
	## Same code as used for --compact-vignettes in
	## .build_packages() ...
	message("Compacting PDF document")
	if(compact %in% c("gs", "gs+qpdf", "both")) {
	    gs_cmd <- tools::find_gs_cmd(Sys.getenv("R_GSCMD", ""))
	    gs_quality <- "ebook"
	} else {
	    gs_cmd <- ""
	    gs_quality <- "none"
	}
	qpdf <- if(compact %in% c("qpdf", "gs+qpdf", "both"))
	    Sys.which(Sys.getenv("R_QPDF", "qpdf"))
	else ""
	res <- tools::compactPDF(output, qpdf = qpdf,
				 gs_cmd = gs_cmd,
				 gs_quality = gs_quality)
	res <- format(res, diff = 1e5)
	if(length(res))
	    message(paste(format(res), collapse = "\n"))
    }
    do_exit()
}

.Stangle <- function(args = NULL, no.q = interactive())
{
    options(warn = 1)
    if (is.null(args)) {
        args <- commandArgs(TRUE)
        args <- paste(args, collapse=" ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    Usage <- function() {
        cat("Usage: R CMD Stangle file",
            "",
            "A front-end for Stangle and other vignette engines",
            "",
            "Options:",
            "  -h, --help     print this help message and exit",
            "  -v, --version  print version info and exit",
	    "  --engine=pkg::engine  use named vignette engine",
            "  --encoding=enc  assume encoding 'enc' for file",
            "  --options=      comma-separated list of Stangle options",
            "",
            "Report bugs at <https://bugs.R-project.org>.",
            sep = "\n")
    }
    do_exit <-
	if(no.q)
	    function(status = 0L) (if(status) stop else message)(
		".Stangle() exit status ", status)
	else
	    function(status = 0L) q("no", status = status, runLast = FALSE)

    if (!length(args)) {
        Usage()
        do_exit(1L)
    }
    file <- character()
    encoding <- options <- ""
    engine <- NULL
    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            do_exit()
        }
        else if (a %in% c("-v", "--version")) {
            cat("Stangle front-end: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 2006-2011 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            do_exit()
        } else if (substr(a, 1, 9) == "--engine=") {
            engine <- substr(a, 10, 1000)
        } else if (substr(a, 1, 11) == "--encoding=") {
            encoding <- substr(a, 12, 1000)
        } else if (substr(a, 1, 10) == "--options=") {
            options <- substr(a, 11, 1000)
        } else if (startsWith(a, "-")) {
            message(gettextf("Warning: unknown option %s", sQuote(a)),
                    domain = NA)
        } else file <- c(file, a)
        args <- args[-1L]
    }
    if(length(file) != 1L) {
        Usage()
        do_exit(1L)
    }
    args <- list(file=file, tangle=TRUE, weave=FALSE, engine=engine,
                 encoding=encoding)
    if(nzchar(options)) {
        opts <- eval(str2expression(paste0("list(", options, ")")))
        args <- c(args, opts)
    }
    output <- do.call(tools::buildVignette, args)
    message("Output file:  ", output)
    do_exit()
}
