#  File src/library/utils/R/SweaveDrivers.R
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

RweaveLatex <- function()
{
    list(setup = RweaveLatexSetup,
         runcode = RweaveLatexRuncode,
         writedoc = RweaveLatexWritedoc,
         finish = RweaveLatexFinish,
         checkopts = RweaveLatexOptions)
}

## We definitely do not want '.' in here, to avoid misidentification
## of file extensions.
.SweaveValidFilenameRegexp <- "^[[:alnum:]#+-_]+$"

RweaveLatexSetup <-
    function(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
             stylepath, ...)
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep=".")
    } else prefix.string <- basename(sub("\\.tex$", "", output))

    if (!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks with options ...\n", sep = "")
    encoding <- attr(file, "encoding")
    if (encoding %in% c("ASCII", "bytes")) encoding <- ""
    output <- file(output, open = "w", encoding = encoding)

    if (missing(stylepath)) {
        p <- Sys.getenv("SWEAVE_STYLEPATH_DEFAULT")
        stylepath <-
            if (length(p) >= 1L && nzchar(p[1L])) identical(p, "TRUE") else FALSE
    }
    if (stylepath) {
        styfile <- file.path(R.home("share"), "texmf", "tex", "latex", "Sweave")
        if (.Platform$OS.type == "windows")
            styfile <- chartr("\\", "/", styfile)
        if (length(grep(" ", styfile)))
            warning(gettextf("path to '%s' contains spaces,\n", styfile),
                    gettext("this may cause problems when running LaTeX"),
                    domain = NA)
    } else styfile <- "Sweave"

    options <- list(prefix = TRUE, prefix.string = prefix.string,
                    engine = "R", print = FALSE, eval = TRUE, fig = FALSE,
                    pdf = TRUE, eps = FALSE, png = FALSE, jpeg = FALSE,
                    width = 6, height = 6, resolution = 300, term = TRUE,
                    echo = TRUE, keep.source = FALSE, results = "verbatim",
                    split = FALSE, strip.white = "true", include = TRUE,
                    pdf.version = grDevices::pdf.options()$version,
                    pdf.encoding = grDevices::pdf.options()$encoding,
                    concordance = FALSE, expand = TRUE, figs.only = FALSE)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveLatexOptions(options)

    list(output = output, styfile = styfile, havesty = FALSE,
         haveconcordance = FALSE, debug = debug, quiet = quiet,
         syntax = syntax, options = options,
         chunkout = list(), # a list of open connections
         srclines = integer())
}

makeRweaveLatexCodeRunner <- function(evalFunc = RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    ## FIXME: well, actually not for the figures.
    ## If there were just one figure option set, we could eval the chunk
    ## only once.
    function(object, chunk, options) {
        pdf.Swd <- function(name, width, height, ...)
            grDevices::pdf(file = paste(chunkprefix, "pdf", sep = "."),
                           width = width, height = height,
                           version = options$pdf.version,
                           encoding = options$pdf.encoding)
        eps.Swd <- function(name, width, height, ...)
            grDevices::postscript(file = paste(name, "eps", sep = "."),
                                  width = width, height = height,
                                  paper = "special", horizontal = FALSE)
        png.Swd <- function(name, width, height, options, ...)
            grDevices::png(filename = paste(chunkprefix, "png", sep = "."),
                           width = width, height = height,
                           res = options$resolution, units = "in")
        jpeg.Swd <- function(name, width, height, options, ...)
            grDevices::jpeg(filename = paste(chunkprefix, "png", sep = "."),
                            width = width, height = height,
                            res = options$resolution, units = "in")

        if (!(options$engine %in% c("R", "S"))) return(object)

        devs <- devoffs <- list()
        if (options$fig && options$eval) {
            if (options$pdf) {
                devs <- c(devs, list(pdf.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$eps) {
                devs <- c(devs, list(eps.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$png) {
                devs <- c(devs, list(png.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$jpeg) {
                devs <- c(devs, list(jpeg.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (!is.null(grd <- options$grdevice)) {
                devs <- c(devs, list(get(grd, envir = .GlobalEnv)))
                grdo <- paste(grd, "off", sep = ".")
                devoffs <- c(devoffs,
                             if (exists(grdo, envir = .GlobalEnv))
                                 list(get(grdo, envir = .GlobalEnv))
                             else list(grDevices::dev.off))
            }
        }
        if (!object$quiet) {
            cat(formatC(options$chunknr, width = 2), ":")
            if (options$echo) cat(" echo")
            if (options$keep.source) cat(" keep.source")
            if (options$eval) {
                if (options$print) cat(" print")
                if (options$term) cat(" term")
                cat("", options$results)
                if (options$fig) {
                    if (options$eps) cat(" eps")
                    if (options$pdf) cat(" pdf")
                    if (options$png) cat(" png")
                    if (options$jpeg) cat(" jpeg")
                    if (!is.null(options$grdevice)) cat("", options$grdevice)
                }
            }
            if (!is.null(options$label))
                cat(" (label = ", options$label, ")", sep = "")
            cat("\n")
        }

        chunkprefix <- RweaveChunkPrefix(options)

        if (options$split) {
            ## [x][[1L]] avoids partial matching of x
            chunkout <- object$chunkout[chunkprefix][[1L]]
            if (is.null(chunkout)) {
                chunkout <- file(paste(chunkprefix, "tex", sep = "."), "w")
                if (!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
                if(!grepl(.SweaveValidFilenameRegexp, chunkout))
                    warning("file name ", sQuote(chunkout), " is not portable",
                            call. = FALSE, domain = NA)
            }
        } else chunkout <- object$output

        srcfile <- srcfilecopy(object$filename, chunk)

        ## Note that we edit the error message below, so change both
        ## if you change this line:
        chunkexps <- try(parse(text = chunk, srcfile = srcfile), silent = TRUE)

        if (inherits(chunkexps, "try-error"))
            chunkexps[1L] <- sub(" parse(text = chunk, srcfile = srcfile) : \n ",
                                 "", chunkexps[1L], fixed = TRUE)

        RweaveTryStop(chunkexps, options)

        ## Some worker functions used below...
        putSinput <- function(dce, leading) {
            if (!openSinput) {
                if (!openSchunk) {
                    cat("\\begin{Schunk}\n", file = chunkout)
                    linesout[thisline + 1L] <<- srcline
                    thisline <<- thisline + 1L
                    openSchunk <<- TRUE
                }
                cat("\\begin{Sinput}", file = chunkout)
                openSinput <<- TRUE
            }
            leading <- max(leading, 1L) # safety check
            cat("\n", paste(getOption("prompt"), dce[seq_len(leading)],
                            sep = "", collapse = "\n"),
                file = chunkout, sep = "")
            if (length(dce) > leading)
                cat("\n", paste(getOption("continue"), dce[-seq_len(leading)],
                                sep = "", collapse = "\n"),
                    file = chunkout, sep = "")
            linesout[thisline + seq_along(dce)] <<- srcline
            thisline <<- thisline + length(dce)
        }

        trySrcLines <- function(srcfile, showfrom, showto, ce) {
            lines <- try(suppressWarnings(getSrcLines(srcfile, showfrom, showto)),
                         silent = TRUE)
            if (inherits(lines, "try-error")) {
                if (is.null(ce)) lines <- character()
                else lines <- deparse(ce, width.cutoff = 0.75*getOption("width"))
            }
            lines
        }

        echoComments <- function(showto) {
            if (options$echo && !is.na(lastshown) && lastshown < showto) {
                dce <- trySrcLines(srcfile, lastshown + 1L, showto, NULL)
                linedirs <- grepl("^#line ", dce)
		dce <- dce[!linedirs]
                putSinput(dce, length(dce)) # These are all trailing comments
                lastshown <<- showto
            }
        }

        openSinput <- FALSE
        openSchunk <- FALSE

        srclines <- attr(chunk, "srclines")
        linesout <- integer()      # maintains concordance
        srcline <- srclines[1L]    # current input line
        thisline <- 0L             # current output line
        lastshown <- 0L            # last line already displayed;

        refline <- NA    # line containing the current named chunk ref
        leading <- 1L    # How many lines get the user prompt

        srcrefs <- attr(chunkexps, "srcref")

        if (length(devs)) {
            if(!grepl(.SweaveValidFilenameRegexp, chunkprefix))
                warning("file name ", sQuote(chunkprefix), " is not portable",
                        call. = FALSE, domain = NA)
            if (options$figs.only)
                devs[[1L]](name = chunkprefix,
                           width = options$width, height = options$height,
                           options)
        }
        SweaveHooks(options, run = TRUE)

        for (nce in seq_along(chunkexps)) {
            ce <- chunkexps[[nce]]
            if (options$keep.source && nce <= length(srcrefs) &&
                !is.null(srcref <- srcrefs[[nce]])) {
                showfrom <- srcref[7L]
                showto <- srcref[8L]

                dce <- trySrcLines(srcfile, lastshown+1L, showto, ce)
                leading <- showfrom - lastshown

                lastshown <- showto
                srcline <- srcref[3L]

                linedirs <- grepl("^#line ", dce)
                dce <- dce[!linedirs]
                # Need to reduce leading lines if some were just removed
                leading <- leading - sum(linedirs[seq_len(leading)])

                while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
                    dce <- dce[-1L]
                    leading <- leading - 1L
                }
            } else {
                dce <- deparse(ce, width.cutoff = 0.75*getOption("width"))
                leading <- 1L
            }
            if (object$debug)
                cat("\nRnw> ", paste(dce, collapse = "\n+  "),"\n")

            if (options$echo && length(dce)) putSinput(dce, leading)

            ## avoid the limitations (and overhead) of output text connections
            if (options$eval) {
                tmpcon <- file()
                sink(file = tmpcon)
                err <- evalFunc(ce, options)
                cat("\n")           # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if (length(output) == 1L && !nzchar(output[1L])) output <- NULL
                RweaveTryStop(err, options)
            } else output <- NULL

            ## or writeLines(output)
            if (length(output) && object$debug)
                cat(paste(output, collapse = "\n"))

            if (length(output) && (options$results != "hide")) {
                if (openSinput) {
                    cat("\n\\end{Sinput}\n", file = chunkout)
                    linesout[thisline + 1L:2L] <- srcline
                    thisline <- thisline + 2L
                    openSinput <- FALSE
                }
                if (options$results == "verbatim") {
                    if (!openSchunk) {
                        cat("\\begin{Schunk}\n", file = chunkout)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                        openSchunk <- TRUE
                    }
                    cat("\\begin{Soutput}\n", file = chunkout)
                    linesout[thisline + 1L] <- srcline
                    thisline <- thisline + 1L
                }

                output <- paste(output, collapse = "\n")
                if (options$strip.white %in% c("all", "true")) {
                    output <- sub("^[[:space:]]*\n", "", output)
                    output <- sub("\n[[:space:]]*$", "", output)
                    if (options$strip.white == "all")
                        output <- sub("\n[[:space:]]*\n", "\n", output)
                }
                cat(output, file = chunkout)
                count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                if (count > 0L) {
                    linesout[thisline + 1L:count] <- srcline
                    thisline <- thisline + count
                }

                remove(output)

                if (options$results == "verbatim") {
                    cat("\n\\end{Soutput}\n", file = chunkout)
                    linesout[thisline + 1L:2L] <- srcline
                    thisline <- thisline + 2L
                }
            }
        } # end of loop over chunkexps.

        ## Echo remaining comments if necessary
        if (options$keep.source) echoComments(length(srcfile$lines))

        if (openSinput) {
            cat("\n\\end{Sinput}\n", file = chunkout)
            linesout[thisline + 1L:2L] <- srcline
            thisline <- thisline + 2L
        }

        if (openSchunk) {
            cat("\\end{Schunk}\n", file = chunkout)
            linesout[thisline + 1L] <- srcline
            thisline <- thisline + 1L
        }

        if (is.null(options$label) && options$split) close(chunkout)

        if (options$split && options$include) {
            cat("\\input{", chunkprefix, "}\n", sep = "",
                file = object$output)
            linesout[thisline + 1L] <- srcline
            thisline <- thisline + 1L
        }

        if (length(devs)) {
            if (options$figs.only) devoffs[[1L]]()
            for (i in seq_along(devs)) {
                if (options$figs.only && i == 1) next
                devs[[i]](name = chunkprefix, width = options$width,
                          height = options$height, options)
                err <- tryCatch({
                    SweaveHooks(options, run = TRUE)
                    eval(chunkexps, envir = .GlobalEnv)
                }, error = function(e) {
                    devoffs[[i]]()
                    stop(conditionMessage(e), call. = FALSE, domain = NA)
                })
                devoffs[[i]]()
            }

            if (options$include) {
                cat("\\includegraphics{", chunkprefix, "}\n", sep = "",
                    file = object$output)
                linesout[thisline + 1L] <- srcline
                thisline <- thisline + 1L
            }
        }
        object$linesout <- c(object$linesout, linesout)
        object
    }
}

RweaveLatexRuncode <- makeRweaveLatexCodeRunner()

RweaveLatexWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

    if (length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE

    if (!object$havesty) {
 	begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
 	which <- grep(begindoc, chunk)
 	if (length(which)) {
            chunk[which] <- sub(begindoc,
                                paste("\\\\usepackage{",
                                      object$styfile,
                                      "}\n\\\\begin{document}", sep = ""),
                                chunk[which])
            linesout <- linesout[c(1L:which, which,
                                   seq(from = which+1L,
                                       length.out = length(linesout)-which))]
            object$havesty <- TRUE
        }
    }

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc + attr(cmdloc, "match.length") - 1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
            ## protect against character(), because sub() will fail
            if (length(val) == 0L) val <- ""
        } else val <- paste("\\\\verb{<<", cmd, ">>{", sep = "")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }

    ## Process \SweaveOpts{} or similar
    ## Since they are only supposed to affect code chunks, it is OK
    ## to process all such in a doc chunk at once.
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)

        if (isTRUE(object$options$concordance)
            && !object$haveconcordance) {
            if (isTRUE(object$hasSweaveInput))
            	warning("\\SweaveInput is not compatible with concordances.",
            	        immediate. = TRUE)
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep = ".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                  paste("\\\\input{", prefix, "}", sep = ""),
                                  chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep = "\n", file = object$output)
    object$linesout <- c(object$linesout, linesout)

    object
}

RweaveLatexFinish <- function(object, error = FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$filename
    if (!object$quiet && !error)
        cat("\n",
            sprintf("You can now run (pdf)latex on '%s'", outputname),
            "\n", sep = "")
    close(object$output)
    if (length(object$chunkout))
        for (con in object$chunkout) close(con)
    if (object$haveconcordance) {
    	## This output format is subject to change.  Currently it contains
    	## three parts, separated by colons:
    	## 1.  The output .tex filename
    	## 2.  The input .Rnw filename
    	## 3.  The input line numbers corresponding to each output line.
    	##     This are compressed using the following simple scheme:
    	##     The first line number, followed by
    	##     a run-length encoded diff of the rest of the line numbers.
        linesout <- object$linesout
        vals <- rle(diff(linesout))
        vals <- c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
    	concordance <- paste(strwrap(paste(vals, collapse = " ")), collapse = " %\n")
    	special <- paste("\\Sconcordance{concordance:", outputname, ":",
                         inputname, ":%\n", concordance,"}\n", sep = "")
    	cat(special, file = object$concordfile)
    }
    invisible(outputname)
}

## This is the check function for both RweaveLatex and Rtangle drivers
RweaveLatexOptions <- function(options)
{
    ## ATTENTION: Changes in this function have to be reflected in the
    ## defaults in the initialization in RweaveLatexSetup

    ## convert a character string to logical
    c2l <- function(x)
        if (is.null(x)) FALSE else as.logical(toupper(as.character(x)))

    ## numeric
    NUMOPTS <- c("width", "height", "resolution")

    ## not logical
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string", "engine",
                   "label", "strip.white", "pdf.version", "pdf.encoding",
                   "grdevice")

    for (opt in names(options)) {
        if (! (opt %in% NOLOGOPTS)) {
            oldval <- options[[opt]]
            if (!is.logical(options[[opt]]))
                options[[opt]] <- c2l(options[[opt]])
            if (is.na(options[[opt]]))
                stop(gettextf("invalid value for '%s' : %s", opt, oldval),
                     domain = NA)
        } else if (opt %in% NUMOPTS)
            options[[opt]] <- as.numeric(options[[opt]])
    }

    if (!is.null(options$results)) {
        res <- as.character(options$results)
        if(tolower(res) != res) # documented as lower-case
            warning("value of 'results' option should be lowercase",
                    call. = FALSE)
        options$results <- tolower(res)
    }
    options$results <- match.arg(options$results, c("verbatim", "tex", "hide"))

    if (!is.null(options$strip.white)) {
        res <- as.character(options$strip.white)
        if(tolower(res) != res)
            warning("value of 'strip.white' option should be lowercase",
                    call. = FALSE)
        options$strip.white <- tolower(res)
    }
    options$strip.white <-
        match.arg(options$strip.white, c("true", "false", "all"))
    options
}


RweaveChunkPrefix <- function(options)
{
    if (!is.null(options$label)) {
        if (options$prefix)
            chunkprefix <- paste(options$prefix.string, "-",
                                 options$label, sep = "")
        else
            chunkprefix <- options$label
    } else
        chunkprefix <- paste(options$prefix.string, "-",
                             formatC(options$chunknr, flag = "0", width = 3),
                             sep = "")
    chunkprefix
}

RweaveEvalWithOpt <- function (expr, options)
{
    if (options$eval) {
        res <- try(withVisible(eval(expr, .GlobalEnv)), silent = TRUE)
        if (inherits(res, "try-error")) return(res)
        if (options$print || (options$term && res$visible)) print(res$value)
    }
    res
}

RweaveTryStop <- function(err, options)
{
    if (inherits(err, "try-error")) {
        cat("\n")
        msg <- paste(" chunk", options$chunknr)
        if (!is.null(options$label))
            msg <- paste(msg, " (label = ", options$label, ")", sep = "")
        msg <- paste(msg, "\n")
        stop(msg, err, call. = FALSE)
    }
}

###------------------------------------------------------------------------

Rtangle <-  function()
{
    list(setup = RtangleSetup,
         runcode = RtangleRuncode,
         writedoc = RtangleWritedoc,
         finish = RtangleFinish,
         checkopts = RweaveLatexOptions)
}


RtangleSetup <-
    function(file, syntax, output = NULL, annotate = TRUE, split = FALSE,
             quiet = FALSE, ...)
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        ## This is odd, since for split = TRUE it uses the engine name.
        output <- paste(prefix.string, "R", sep = ".")
    } else
        prefix.string <- basename(sub("\\.[rsRS]$", "", output))

    if (!split) {
        if (identical(output, "stdout")) output <- stdout()
        else if (identical(output, "stderr")) output <- stderr()
        else {
            if (!quiet) cat("Writing to file", output, "\n")
            ## We could at some future point try to write the file in
            ## 'encoding'.
            output <- file(output, open = "w")
        }
        lines <- c(sprintf("R code from vignette source '%s'", file),
                   if(attr(file, "encoding") != "ASCII")
                   sprintf("Encoding: %s", localeToCharset()[1L])
                   )
        lines <- c(paste("###", lines), "")
        writeLines(lines, output)
    } else {
        if (!quiet) cat("Writing chunks to files ...\n")
        output <- NULL
    }

    options <- list(split = split, prefix = TRUE,
                    prefix.string = prefix.string,
                    engine = "R", eval = TRUE,
                    show.line.nos = FALSE)
    options[names(dots)] <- dots

    list(output = output, annotate = annotate, options = options,
         chunkout = list(), quiet = quiet, syntax = syntax)
}


RtangleRuncode <-  function(object, chunk, options)
{
    if (!(options$engine %in% c("R", "S"))) return(object)

    chunkprefix <- RweaveChunkPrefix(options)

    if (options$split) {
        if(!grepl(.SweaveValidFilenameRegexp, chunkprefix))
            warning("file name ", sQuote(chunkprefix), " is not portable",
                    call. = FALSE, domain = NA)
        outfile <- paste(chunkprefix, options$engine, sep = ".")
        if (!object$quiet) cat(options$chunknr, ":", outfile,"\n")
        ## [x][[1L]] avoids partial matching of x
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if (is.null(chunkout)) {
            chunkout <- file(outfile, "w")
            if (!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    } else
        chunkout <- object$output

    if (object$annotate) {
        lnos <- grep("^#line ", chunk, value = TRUE)
        if(length(lnos)) {
            srclines <- attr(chunk, "srclines")
            ## this currently includes the chunk header
            lno <- if (length(srclines)) paste(min(srclines), max(srclines), sep="-") else srclines
            fn <- sub('[^"]*"([^"]+).*', "\\1", lnos[1L])
        }
        cat("###################################################\n",
            "### code chunk number ", options$chunknr,
            ": ",
            if(!is.null(options$label)) options$label
            else paste(fn, lno, sep = ":"),
            ifelse(options$eval, "", " (eval = FALSE)"), "\n",
            "###################################################\n",
            file = chunkout, sep = "")
    }

    ## The next returns a character vector of the logical options
    ## which are true and have hooks set.
    hooks <- SweaveHooks(options, run = FALSE)
    for (k in hooks)
        cat("getOption(\"SweaveHooks\")[[\"", k, "\"]]()\n",
            file = chunkout, sep = "")

    if (!options$show.line.nos)
        chunk <- grep("^#line ", chunk, value = TRUE, invert = TRUE)
    if (!options$eval) chunk <- paste("##", chunk)
    cat(chunk, "\n", file = chunkout, sep = "\n")
    if (is.null(options$label) && options$split) close(chunkout)
    object
}

RtangleWritedoc <- function(object, chunk)
{
    while(length(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    object
}


RtangleFinish <- function(object, error = FALSE)
{
    ## might be stdout() or stderr()
    if (!is.null(object$output) && object$output >= 3)
        close(object$output)

    if (length(object$chunkout))
        for (con in object$chunkout) close(con)
}
