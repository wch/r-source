#  File src/library/utils/R/Sweave.R
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

Sweave <- function(file, driver=RweaveLatex(),
                   syntax=getOption("SweaveSyntax"), ...)
{
    if(is.character(driver))
        driver <- get(driver, mode="function")()
    else if(is.function(driver))
        driver <- driver()


    if(is.null(syntax))
        syntax <- SweaveGetSyntax(file)
    if(is.character(syntax))
        syntax <- get(syntax, mode="list")

    drobj <- driver$setup(file=file, syntax=syntax, ...)
    on.exit(driver$finish(drobj, error=TRUE))

    text <- SweaveReadFile(file, syntax)
    syntax <- attr(text, "syntax")

    mode <- "doc"
    chunknr <- 0L
    chunk <- NULL

    namedchunks <- list()
    for(linenum in seq_along(text)) {
    	line <- text[linenum]
        if(length(grep(syntax$doc, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "doc"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        }
        else if(length(grep(syntax$code, line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "code"
            }
            else{
                if(!is.null(chunkopts$label))
                    namedchunks[[chunkopts$label]] <- chunk
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "code"
            }
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts,
                                            drobj$options,
                                            driver$checkopts)
            chunk <- NULL
            chunknr <- chunknr+1
            chunkopts$chunknr <- chunknr
        }
        else{
            if(mode=="code" && length(grep(syntax$coderef, line))){
                chunkref <- sub(syntax$coderef, "\\1", line)
                if(!(chunkref %in% names(namedchunks)))
                    warning(gettextf("reference to unknown chunk '%s'",
                                     chunkref), domain = NA)
                line <- namedchunks[[chunkref]]
            }
            srclines <- c(attr(chunk, "srclines"), rep(linenum, length(line)))
            if(is.null(chunk))
                chunk <- line
            else
                chunk <- c(chunk, line)
            attr(chunk, "srclines") <- srclines
        }
    }
    if(!is.null(chunk)){
        if(mode=="doc") drobj <- driver$writedoc(drobj, chunk)
        else drobj <- driver$runcode(drobj, chunk, chunkopts)
    }

    on.exit()
    driver$finish(drobj)
}

SweaveReadFile <- function(file, syntax)
{
    ## file can be a vector to keep track of recursive calls to
    ## SweaveReadFile.  In this case only the first element is
    ## tried to read in, the rest are forbidden names for further
    ## SweaveInput
    f <- file[1L]

    bf <- basename(f)
    df <- dirname(f)
    if(!file.exists(f)) {
        f <- list.files(df, full.names=TRUE,
                        pattern=paste(bf, syntax$extension, sep=""))

        if(length(f) == 0L)
            stop(gettextf("no Sweave file with name '%s' found", file[1L]),
                 domain = NA)
        else if(length(f) > 1L)
            stop(paste(gettextf("%d Sweave files for basename '%s' found:",
                                length(f), file),
                       paste("\n         ", f, collapse="")),
                 domain = NA)
    }

    ## An incomplete last line is not a real problem.
    text <- readLines(f[1L], warn = FALSE)

    ## <FIXME>
    ## This needs to be more refined eventually ...
    if(any(is.na(nchar(text, "c", TRUE)))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        text <- iconv(text, "latin1", "")
    }
    ## </FIXME>

    pos <- grep(syntax$syntaxname, text)

    if(length(pos) > 1L)
        warning(gettextf("more than one syntax specification found, using the first one"), domain = NA)

    if(length(pos) > 0L) {
        sname <- sub(syntax$syntaxname, "\\1", text[pos[1L]])
        syntax <- get(sname, mode = "list")
        if(class(syntax) != "SweaveSyntax")
            stop(gettextf("object '%s' does not have class \"SweaveSyntax\"",
                          sname), domain = NA)
        text <- text[-pos]
    }

    if(!is.null(syntax$input)) {
        while(length(pos <- grep(syntax$input, text))) {
            pos <- pos[1L]
            ifile <- file.path(df, sub(syntax$input, "\\1", text[pos]))
            if(any(ifile == file)){
                stop(paste(gettextf("recursive Sweave input '%s' in stack",
                                    ifile),
                           paste("\n         ", seq_len(file), ": ",
                                 rev(file), collapse="")),
                 domain = NA)
            }
            itext <- SweaveReadFile(c(ifile, file), syntax)

            if(pos == 1L)
                text <- c(itext, text[-pos])
            else if(pos == length(text))
                text <- c(text[-pos], itext)
            else
                text <- c(text[seq_len(pos-1L)], itext, text[(pos+1L):length(text)])
        }
    }

    attr(text, "syntax") <- syntax
    text
}



###**********************************************************

SweaveSyntaxNoweb <-
    list(doc = "^@",
         code = "^<<(.*)>>=.*",
         coderef = "^<<(.*)>>.*",
         docopt = "^[[:space:]]*\\\\SweaveOpts\\{([^\\}]*)\\}",
         docexpr = "\\\\Sexpr\\{([^\\}]*)\\}",
         extension = "\\.[rsRS]?nw$",
         syntaxname = "^[[:space:]]*\\\\SweaveSyntax\\{([^\\}]*)\\}",
         input = "^[[:space:]]*\\\\SweaveInput\\{([^\\}]*)\\}",
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
SweaveSyntaxLatex$code <- "^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^\\}]*)\\}?.*"
SweaveSyntaxLatex$coderef <- "^[[:space:]]*\\\\Scoderef\\{([^\\}]*)\\}.*"
SweaveSyntaxLatex$extension <- "\\.[rsRS]tex$"

SweaveSyntaxLatex$trans$doc <-  "\\\\end{Scode}"
SweaveSyntaxLatex$trans$code <- "\\\\begin{Scode}{\\1}"
SweaveSyntaxLatex$trans$coderef <- "\\\\Scoderef{\\1}"
SweaveSyntaxLatex$trans$syntaxname <- "\\\\SweaveSyntax{SweaveSyntaxLatex}"
SweaveSyntaxLatex$trans$extension <- ".Stex"

###**********************************************************

SweaveGetSyntax <- function(file){

    synt <- apropos("SweaveSyntax", mode="list")
    for(sname in synt){
        s <- get(sname, mode="list")
        if(class(s) != "SweaveSyntax") next
        if(length(grep(s$extension, file))) return(s)
    }
    return(SweaveSyntaxNoweb)
}


SweaveSyntConv <- function(file, syntax, output=NULL)
{
    if(is.character(syntax))
        syntax <- get(syntax)

    if(class(syntax) != "SweaveSyntax")
        stop("target syntax not of class \"SweaveSyntax\"")

    if(is.null(syntax$trans))
        stop("target syntax contains no translation table")

    insynt <- SweaveGetSyntax(file)
    text <- readLines(file)
    if(is.null(output))
        output <- sub(insynt$extension, syntax$trans$extension, basename(file))

    TN <- names(syntax$trans)

    for(n in TN){
        if(n != "extension")
            text <- gsub(insynt[[n]], syntax$trans[[n]], text)
    }

    cat(text, file=output, sep="\n")
    cat("Wrote file", output, "\n")
}




###**********************************************************

SweaveParseOptions <- function(text, defaults=list(), check=NULL)
{
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")

    ## only the first option may have no name: the chunk label
    if(length(x)){
        if(length(x[[1L]]) == 1L) {
            x[[1L]] <- c("label", x[[1L]])
        }
    }
    else
        return(defaults)

    if(any(sapply(x, length) != 2L))
        stop(gettextf("parse error or empty option in\n%s", text), domain = NA)

    options <- defaults

    for(k in seq_along(x))
        options[[ x[[k]][1L] ]] <- x[[k]][2L]

    if(!is.null(options[["label"]]) && !is.null(options[["engine"]]))
        options[["label"]] <- sub(paste("\\.", options[["engine"]], "$",
                                        sep=""),
                                  "", options[["label"]])

    if(!is.null(check))
        options <- check(options)

    options
}

SweaveHooks <- function(options, run=FALSE, envir=.GlobalEnv)
{
    if(is.null(SweaveHooks <- getOption("SweaveHooks")))
        return(NULL)

    z <- character(0L)
    for(k in names(SweaveHooks)){
        if(k != "" && !is.null(options[[k]]) && options[[k]]){
            if(is.function(SweaveHooks[[k]])){
                z <- c(z, k)
                if(run)
                    eval(SweaveHooks[[k]](), envir=envir)
            }
        }
    }
    z
}





###**********************************************************


RweaveLatex <- function()
{
    list(setup = RweaveLatexSetup,
         runcode = RweaveLatexRuncode,
         writedoc = RweaveLatexWritedoc,
         finish = RweaveLatexFinish,
         checkopts = RweaveLatexOptions)
}

RweaveLatexSetup <-
    function(file, syntax, output=NULL, quiet=FALSE, debug=FALSE,
             stylepath, ...)
{
    dots <- list(...)
    if(is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep=".")
    } else prefix.string <- basename(sub("\\.tex$", "", output))

    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")

    if(missing(stylepath)) {
        p <- as.vector(Sys.getenv("SWEAVE_STYLEPATH_DEFAULT"))
        stylepath <- if(length(p) >= 1L && nzchar(p[1L])) identical(p, "TRUE") else FALSE
    }
    if(stylepath){
        styfile <- file.path(R.home("share"), "texmf", "Sweave")
        if(.Platform$OS.type == "windows")
            styfile <- gsub("\\\\", "/", styfile)
        if(length(grep(" ", styfile)))
            warning(gettextf("path to '%s' contains spaces,\n", styfile),
                    gettext("this may cause problems when running LaTeX"),
                    domain = NA)
    } else styfile <- "Sweave"

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=TRUE,
                    fig=FALSE, pdf=TRUE, eps=TRUE,
                    width=6, height=6, term=TRUE,
                    echo=TRUE, keep.source=FALSE, results="verbatim",
                    split=FALSE, strip.white="true", include=TRUE,
                    pdf.version=grDevices::pdf.options()$version,
                    pdf.encoding=grDevices::pdf.options()$encoding,
                    concordance=FALSE, expand=TRUE)
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveLatexOptions(options)

    list(output=output, styfile=styfile, havesty=FALSE, haveconcordance=FALSE,
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(), srclines=integer(0L),
         srcfile=srcfile(file))
}

makeRweaveLatexCodeRunner <- function(evalFunc=RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    RweaveLatexRuncode <- function(object, chunk, options)
      {
          if(!(options$engine %in% c("R", "S"))){
              return(object)
          }

          if(!object$quiet){
              cat(formatC(options$chunknr, width=2), ":")
              if(options$echo) cat(" echo")
              if(options$keep.source) cat(" keep.source")
              if(options$eval){
                  if(options$print) cat(" print")
                  if(options$term) cat(" term")
                  cat("", options$results)
                  if(options$fig){
                      if(options$eps) cat(" eps")
                      if(options$pdf) cat(" pdf")
                  }
              }
              if(!is.null(options$label))
                cat(" (label=", options$label, ")", sep="")
              cat("\n")
          }

          chunkprefix <- RweaveChunkPrefix(options)

          if(options$split){
              ## [x][[1L]] avoids partial matching of x
              chunkout <- object$chunkout[chunkprefix][[1L]]
              if(is.null(chunkout)){
                  chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
                  if(!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
              }
          }
          else
            chunkout <- object$output

	  saveopts <- options(keep.source=options$keep.source)
	  on.exit(options(saveopts))

          SweaveHooks(options, run=TRUE)

          chunkexps <- try(parse(text=chunk), silent=TRUE)
          RweaveTryStop(chunkexps, options)
          openSinput <- FALSE
          openSchunk <- FALSE

          if(length(chunkexps) == 0L)
            return(object)

          srclines <- attr(chunk, "srclines")
          linesout <- integer(0L)
          srcline <- srclines[1L]

	  srcrefs <- attr(chunkexps, "srcref")
	  if (options$expand)
	    lastshown <- 0L
	  else
	    lastshown <- srcline - 1L
	  thisline <- 0
          for(nce in seq_along(chunkexps))
            {
                ce <- chunkexps[[nce]]
                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
                    if (options$expand) {
                	srcfile <- attr(srcref, "srcfile")
                	showfrom <- srcref[1L]
                	showto <- srcref[3L]
                    } else {
                    	srcfile <- object$srcfile
                    	showfrom <- srclines[srcref[1L]]
                    	showto <- srclines[srcref[3L]]
                    }
                    dce <- getSrcLines(srcfile, lastshown+1, showto)
	    	    leading <- showfrom-lastshown
	    	    lastshown <- showto
                    srcline <- srclines[srcref[3L]]
                    while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
	    		dce <- dce[-1L]
	    		leading <- leading - 1L
	    	    }
	    	} else {
                    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
                    leading <- 1L
                }
                if(object$debug)
                  cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
                if(options$echo && length(dce)){
                    if(!openSinput){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1] <- srcline
                            thisline <- thisline + 1
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Sinput}",
                            file=chunkout, append=TRUE)
                        openSinput <- TRUE
                    }
		    cat("\n", paste(getOption("prompt"), dce[1L:leading], sep="", collapse="\n"),
		    	file=chunkout, append=TRUE, sep="")
                    if (length(dce) > leading)
                    	cat("\n", paste(getOption("continue"), dce[-(1L:leading)], sep="", collapse="\n"),
                    	    file=chunkout, append=TRUE, sep="")
		    linesout[thisline + seq_along(dce)] <- srcline
		    thisline <- thisline + length(dce)
                }

                                        # tmpcon <- textConnection("output", "w")
                                        # avoid the limitations (and overhead) of output text connections
                tmpcon <- file()
                sink(file=tmpcon)
                err <- NULL
                if(options$eval) err <- evalFunc(ce, options)
                cat("\n") # make sure final line is complete
                sink()
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if(length(output) == 1L & output[1L] == "") output <- NULL

                RweaveTryStop(err, options)

                if(object$debug)
                  cat(paste(output, collapse="\n"))

                if(length(output) & (options$results != "hide")){

                    if(openSinput){
                        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2L] <- srcline
                        thisline <- thisline + 2L
                        openSinput <- FALSE
                    }
                    if(options$results=="verbatim"){
                        if(!openSchunk){
                            cat("\\begin{Schunk}\n",
                                file=chunkout, append=TRUE)
                            linesout[thisline + 1L] <- srcline
                            thisline <- thisline + 1L
                            openSchunk <- TRUE
                        }
                        cat("\\begin{Soutput}\n",
                            file=chunkout, append=TRUE)
                        linesout[thisline + 1L] <- srcline
                        thisline <- thisline + 1L
                    }

                    output <- paste(output,collapse="\n")
                    if(options$strip.white %in% c("all", "true")){
                        output <- sub("^[[:space:]]*\n", "", output)
                        output <- sub("\n[[:space:]]*$", "", output)
                        if(options$strip.white=="all")
                          output <- sub("\n[[:space:]]*\n", "\n", output)
                    }
                    cat(output, file=chunkout, append=TRUE)
                    count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                    if (count > 0L) {
                    	linesout[thisline + 1L:count] <- srcline
                    	thisline <- thisline + count
                    }

                    remove(output)

                    if(options$results=="verbatim"){
                        cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
                        linesout[thisline + 1L:2L] <- srcline
                        thisline <- thisline + 2L
                    }
                }
            }

          if(openSinput){
              cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L:2L] <- srcline
              thisline <- thisline + 2L
          }

          if(openSchunk){
              cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(is.null(options$label) & options$split)
            close(chunkout)

          if(options$split & options$include){
              cat("\\input{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
              linesout[thisline + 1L] <- srcline
              thisline <- thisline + 1L
          }

          if(options$fig && options$eval){
              if(options$eps){
                  grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
                                        width=options$width, height=options$height,
                                        paper="special", horizontal=FALSE)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$pdf){
                  grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
                                 width=options$width, height=options$height,
                                 version=options$pdf.version,
                                 encoding=options$pdf.encoding)

                  err <- try({SweaveHooks(options, run=TRUE)
                              eval(chunkexps, envir=.GlobalEnv)})
                  grDevices::dev.off()
                  if(inherits(err, "try-error")) stop(err)
              }
              if(options$include) {
                  cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                      file=object$output, append=TRUE)
                  linesout[thisline + 1L] <- srcline
                  thisline <- thisline + 1L
              }
          }
          object$linesout <- c(object$linesout, linesout)
          return(object)
      }
    RweaveLatexRuncode
}

RweaveLatexRuncode <- makeRweaveLatexCodeRunner()

RweaveLatexWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

    if(length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE

    if(!object$havesty){
 	begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
 	which <- grep(begindoc, chunk)
 	if (length(which)) {
            chunk[which] <- sub(begindoc,
                                paste("\\\\usepackage{",
                                      object$styfile,
                                      "}\n\\\\begin{document}", sep=""),
                                chunk[which])
            linesout <- linesout[c(1L:which, which, seq(from=which+1L, length.out=length(linesout)-which))]
            object$havesty <- TRUE
        }
    }

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0L), because sub() will fail
            if(length(val) == 0L) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        if (isTRUE(object$options$concordance)
              && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep=".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                 paste("\\\\input{", prefix, "}", sep=""),
                                 chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)

    return(object)
}

RweaveLatexFinish <- function(object, error=FALSE)
{
    outputname <- summary(object$output)$description
    inputname <- object$srcfile$filename
    if(!object$quiet && !error)
        cat("\n",
            gettextf("You can now run LaTeX on '%s'", outputname),
            "\n", sep = "")
    close(object$output)
    if(length(object$chunkout))
        for(con in object$chunkout) close(con)
    if (object$haveconcordance) {
    	# This output format is subject to change.  Currently it contains
    	# three parts, separated by colons:
    	# 1.  The output .tex filename
    	# 2.  The input .Rnw filename
    	# 3.  The input line numbers corresponding to each output line.
    	#     This are compressed using the following simple scheme:
    	#     The first line number, followed by
    	#     a run-length encoded diff of the rest of the line numbers.
        linesout <- object$linesout
        vals <- rle(diff(linesout))
        vals <- c(linesout[1L], as.numeric(rbind(vals$lengths, vals$values)))
    	concordance <- paste(strwrap(paste(vals, collapse=" ")), collapse=" %\n")
    	special <- paste("\\Sconcordance{concordance:", outputname, ":", inputname, ":%\n",
    			 concordance,"}\n", sep="")
    	cat(special, file=object$concordfile)
    }
    invisible(outputname)
}

RweaveLatexOptions <- function(options)
{

    ## ATTENTION: Changes in this function have to be reflected in the
    ## defaults in the init function!

    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    NUMOPTS <- c("width", "height")
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string",
                   "engine", "label", "strip.white",
                   "pdf.version", "pdf.encoding")

    for(opt in names(options)){
        if(! (opt %in% NOLOGOPTS)){
            oldval <- options[[opt]]
            if(!is.logical(options[[opt]])){
                options[[opt]] <- c2l(options[[opt]])
            }
            if(is.na(options[[opt]]))
                stop(gettextf("invalid value for '%s' : %s", opt, oldval),
                     domain = NA)
        }
        else if(opt %in% NUMOPTS){
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }

    if(!is.null(options$results))
        options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results,
                                 c("verbatim", "tex", "hide"))

    if(!is.null(options$strip.white))
        options$strip.white <- tolower(as.character(options$strip.white))
    options$strip.white <- match.arg(options$strip.white,
                                     c("true", "false", "all"))

    options
}


RweaveChunkPrefix <- function(options)
{
    if(!is.null(options$label)){
        if(options$prefix)
            chunkprefix <- paste(options$prefix.string, "-",
                                 options$label, sep="")
        else
            chunkprefix <- options$label
    }
    else
        chunkprefix <- paste(options$prefix.string, "-",
                             formatC(options$chunknr, flag="0", width=3),
                             sep="")

    return(chunkprefix)
}

RweaveEvalWithOpt <- function (expr, options){
    if(options$eval){
        res <- try(withVisible(eval(expr, .GlobalEnv)),
                   silent=TRUE)
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}


RweaveTryStop <- function(err, options){

    if(inherits(err, "try-error")){
        cat("\n")
        msg <- paste(" chunk", options$chunknr)
        if(!is.null(options$label))
            msg <- paste(msg, " (label=", options$label, ")", sep="")
        msg <- paste(msg, "\n")
        stop(msg, err, call.=FALSE)
    }
}





###**********************************************************

Stangle <- function(file, driver=Rtangle(),
                    syntax=getOption("SweaveSyntax"), ...)
{
    Sweave(file=file, driver=driver, ...)
}

Rtangle <-  function()
{
    list(setup = RtangleSetup,
         runcode = RtangleRuncode,
         writedoc = RtangleWritedoc,
         finish = RtangleFinish,
         checkopts = RweaveLatexOptions)
}


RtangleSetup <- function(file, syntax,
                         output=NULL, annotate=TRUE, split=FALSE,
                         prefix=TRUE, quiet=FALSE)
{
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "R", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.[rsRS]$", "", output))
    }

    if(!split){
        if(!quiet)
            cat("Writing to file", output, "\n")
        output <- file(output, open="w")
    }
    else{
        if(!quiet)
            cat("Writing chunks to files ...\n")
        output <- NULL
    }

    options <- list(split=split, prefix=prefix,
                    prefix.string=prefix.string,
                    engine="R", eval=TRUE)

    list(output=output, annotate=annotate, options=options,
         chunkout=list(), quiet=quiet, syntax=syntax)
}


RtangleRuncode <-  function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    chunkprefix <- RweaveChunkPrefix(options)

    if(options$split){
        outfile <- paste(chunkprefix, options$engine, sep=".")
        if(!object$quiet)
            cat(options$chunknr, ":", outfile,"\n")
        ## [x][[1L]] avoids partial matching of x
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if(is.null(chunkout)){
            chunkout <- file(outfile, "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    if(object$annotate){
        cat("###################################################\n",
            "### chunk number ", options$chunknr,
            ": ", options$label,
            ifelse(options$eval, "", " eval=FALSE"), "\n",
            "###################################################\n",
            file=chunkout, append=TRUE, sep="")
    }

    hooks <- SweaveHooks(options, run=FALSE)
    for(k in hooks)
        cat("getOption(\"SweaveHooks\")[[\"", k, "\"]]()\n",
            file=chunkout, append=TRUE, sep="")

    if(!options$eval)
        chunk <- paste("##", chunk)

    cat(chunk,"\n", file=chunkout, append=TRUE, sep="\n")

    if(is.null(options$label) & options$split)
        close(chunkout)

    return(object)
}

RtangleWritedoc <- function(object, chunk)
{
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    return(object)
}


RtangleFinish <- function(object, error=FALSE)
{
    if(!is.null(object$output))
        close(object$output)

    if(length(object$chunkout)) {
        for(con in object$chunkout) close(con)
    }
}

