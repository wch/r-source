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
    chunknr <- 0
    chunk <- NULL

    namedchunks <- list()
    for(line in text){
        if(any(grep(syntax$doc, line))){
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
        else if(any(grep(syntax$code, line))){
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
            if(mode=="code" && any(grep(syntax$coderef, line))){
                chunkref <- sub(syntax$coderef, "\\1", line)
                if(!(chunkref %in% names(namedchunks)))
                    warning(gettextf("reference to unknown chunk '%s'",
                                     chunkref), domain = NA)
                line <- namedchunks[[chunkref]]
            }

            if(is.null(chunk))
                chunk <- line
            else
                chunk <- c(chunk, line)
        }
    }
    if(!is.null(chunk)){
        if(mode=="doc") driver$writedoc(drobj, chunk)
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
    f <- file[1]

    bf <- basename(f)
    df <- dirname(f)
    if(!file.exists(f)){
        f <- list.files(df, full.names=TRUE,
                        pattern=paste(bf, syntax$extension, sep=""))

        if(length(f)==0){
            stop(gettextf("no Sweave file with name '%s' found", file[1]),
                 domain = NA)
        }
        else if(length(f) > 1){
            stop(paste(gettextf("%d Sweave files for basename '%s' found:",
                                length(f), file),
                       paste("\n         ", f, collapse="")),
                 domain = NA)
        }
    }

    text <- readLines(f[1])
    ## <FIXME>
    ## This needs to be more refined eventually ...
    if(any(is.na(nchar(text, "c")))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        if(capabilities("iconv"))
            text <- iconv(text, "latin1", "")
        else
            stop("Found invalid multi-byte character data.", "\n",
                 "Cannot re-encode because 'iconv' is not available.", "\n",
                 "Try running R in a single-byte locale.")
    }
    ## </FIXME>

    pos <- grep(syntax$syntaxname, text)

    if(length(pos)>1){
        warning(gettextf("more than one syntax specification found, using the first one"), domain = NA)
    }
    if(length(pos)>0){
        sname <- sub(syntax$syntaxname, "\\1", text[pos[1]])
        syntax <- get(sname, mode = "list")
        if(class(syntax) != "SweaveSyntax")
            stop(gettextf("object '%s' does not have class \"SweaveSyntax\"",
                          sname), domain = NA)

        text <- text[-pos]
    }

    if(!is.null(syntax$input)){
        while(any(pos <- grep(syntax$input, text))){

            pos <- pos[1]
            ifile <- file.path(df, sub(syntax$input, "\\1", text[pos]))
            if(any(ifile==file)){
                stop(paste(gettextf("recursive Sweave input '%s' in stack",
                                    ifile),
                           paste("\n         ", 1:length(file), ": ",
                                 rev(file), collapse="")),
                 domain = NA)
            }
            itext <- SweaveReadFile(c(ifile, file), syntax)

            if(pos==1)
                text <- c(itext, text[-pos])
            else if(pos==length(text))
                text <- c(text[-pos], itext)
            else
                text <- c(text[1:(pos-1)], itext, text[(pos+1):length(text)])
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
        if(any(grep(s$extension, file))) return(s)
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
    text = readLines(file)
    if(is.null(output))
        output = sub(insynt$extension, syntax$trans$extension, basename(file))

    TN = names(syntax$trans)

    for(n in TN){
        if(n!="extension")
            text = gsub(insynt[[n]], syntax$trans[[n]], text)
    }

    cat(text, file=output, sep="\n")
    cat("Wrote file", output, "\n")
}




###**********************************************************

SweaveParseOptions <- function(text, defaults=list(), check=NULL)
{
    x <- sub("^[[:space:]]*\(.*\)", "\\1", text)
    x <- sub("\(.*[^[:space:]]\)[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")

    ## only the first option may have no name: the chunk label
    if(length(x)>0){
        if(length(x[[1]])==1){
            x[[1]] <- c("label", x[[1]])
        }
    }
    else
        return(defaults)

    if(any(sapply(x, length)!=2))
        stop(gettextf("parse error or empty option in\n%s", text), domain = NA)

    options <- defaults

    for(k in 1:length(x))
        options[[ x[[k]][1] ]] <- x[[k]][2]

    if(!is.null(options[["label"]]) && !is.null(options[["engine"]]))
        options[["label"]] <- sub(paste(".", options[["engine"]], "$",
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

    z <- character(0)
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
    function(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE)
{
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "tex", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.tex$", "", output))
    }
    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")

    if(stylepath){
        styfile <- file.path(R.home("share"), "texmf", "Sweave")
        if(.Platform$OS.type == "windows")
            styfile <- gsub("\\\\", "/", styfile)
        if(any(grep(" ", styfile)))
            warning(gettextf("path to '%s' contains spaces,\n", styfile),
                    gettext("this may cause problems when running LaTeX"),
                    domain = NA)
    }
    else
        styfile <- "Sweave"

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=eval,
                    fig=FALSE, pdf=pdf, eps=eps,
                    width=6, height=6, term=TRUE,
                    echo=echo, results="verbatim", split=split,
                    strip.white="true", include=TRUE)

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveLatexOptions(options)

    list(output=output, styfile=styfile, havesty=FALSE,
         debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list())
}

RweaveLatexRuncode <- function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    if(!object$quiet){
        cat(formatC(options$chunknr, width=2), ":")
        if(options$echo) cat(" echo")
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
        chunkout <- object$chunkout[[chunkprefix]]
        if(is.null(chunkout)){
            chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    SweaveHooks(options, run=TRUE)

    chunkexps <- try(parse(text=chunk), silent=TRUE)
    RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE

    if(length(chunkexps)==0)
        return(object)

    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
        if(object$debug)
            cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
        if(options$echo){
            if(!openSinput){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Sinput}",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            cat("\n", getOption("prompt"),
                paste(dce,
                      collapse=paste("\n", getOption("continue"), sep="")),
                file=chunkout, append=TRUE, sep="")
        }

        # tmpcon <- textConnection("output", "w")
        # avoid the limitations (and overhead) of output text connections
        tmpcon <- file()
        sink(file=tmpcon)
        err <- NULL
        if(options$eval) err <- RweaveEvalWithOpt(ce, options)
        cat("\n") # make sure final line is complete
        sink()
        output <- readLines(tmpcon)
        close(tmpcon)
        ## delete empty output
        if(length(output)==1 & output[1]=="") output <- NULL

        RweaveTryStop(err, options)

        if(object$debug)
            cat(paste(output, collapse="\n"))

        if(length(output)>0 & (options$results != "hide")){

            if(openSinput){
                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                openSinput <- FALSE
            }
            if(options$results=="verbatim"){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Soutput}\n",
                    file=chunkout, append=TRUE)
            }

            output <- paste(output,collapse="\n")
            if(options$strip.white %in% c("all", "true")){
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
                if(options$strip.white=="all")
                    output <- sub("\n[[:space:]]*\n", "\n", output)
            }
            cat(output, file=chunkout, append=TRUE)
            remove(output)

            if(options$results=="verbatim"){
                cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
            }
        }
    }

    if(openSinput){
        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
    }

    if(openSchunk){
        cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
    }

    if(is.null(options$label) & options$split)
        close(chunkout)

    if(options$split & options$include)
        cat("\\input{", chunkprefix, "}\n", sep="",
            file=object$output, append=TRUE)

    if(options$fig && options$eval){
        if(options$eps){
            postscript(file=paste(chunkprefix, "eps", sep="."),
                       width=options$width, height=options$height,
                       paper="special", horizontal=FALSE)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$pdf){
            pdf(file=paste(chunkprefix, "pdf", sep="."),
                width=options$width, height=options$height)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$include)
            cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
    }
    return(object)
}

RweaveLatexWritedoc <- function(object, chunk)
{
    if(any(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE

    if(!object$havesty){
        chunk <- gsub("\\\\begin\\{document\\}",
                      paste("\\\\usepackage{",
                            object$styfile,
                            "}\n\\\\begin{document}", sep=""),
                      chunk)
        object$havesty <- TRUE
    }

    while(any(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1]])
        cmd <- substr(chunk[pos[1]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0), because sub() will fail
            if(length(val)==0) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
    }
    while(any(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    return(object)
}

RweaveLatexFinish <- function(object, error=FALSE)
{
    if(!object$quiet && !error)
        cat("\n",
            gettextf("You can now run LaTeX on '%s'",
                     summary(object$output)$description),
            "\n", sep = "")
    close(object$output)
    if(length(object$chunkout) > 0)
        for(con in object$chunkout) close(con)
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
                   "engine", "label", "strip.white")

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

    options$results <- tolower(as.character(options$results))
    options$results <- match.arg(options$results,
                                 c("verbatim", "tex", "hide"))

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
        res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, NULL)),
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
        chunkout <- object$chunkout[[chunkprefix]]
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
    while(any(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }
    return(object)
}


RtangleFinish <- function(object, error=FALSE)
{
    if(!is.null(object$output))
        close(object$output)

    if(length(object$chunkout)>0){
        for(con in object$chunkout) close(con)
    }
}

