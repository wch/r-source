Sweave <- function(file, driver=RweaveLatex(), ...)
{
    if(is.character(driver))
        driver <- get(driver, mode="function")()
    else if(is.function(driver))
        driver <- driver()
    
    drobj <- driver$setup(file=file, ...)

    text <- readLines(file)
    
    mode <- "doc"
    chunknr <- 0
    chunk <- NULL
    
    for(line in text){
        if(any(grep("^@", line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "doc"
            }
            else{
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        }
        else if(any(grep("^<<.*>>=", line))){
            if(mode=="doc"){
                if(!is.null(chunk))
                    drobj <- driver$writedoc(drobj, chunk)
                mode <- "code"
            }
            else{
                if(!is.null(chunk))
                    drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "code"
            }
            chunkopts <- sub("^<<(.*)>>=.*", "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts,
                                            drobj$options,
                                            driver$checkopts)
            chunk <- NULL
            chunknr <- chunknr+1
            chunkopts$chunknr <- chunknr
        }
        else{
            if(is.null(chunk))
                chunk <- line
            else         
                chunk <- paste(chunk, line, sep="\n")
        }
    }
    if(mode=="doc") driver$writedoc(drobj, chunk)
    else drobj <- driver$runcode(drobj, chunk, chunkopts)
    
    driver$finish(drobj)
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
        stop(paste("Parse error or empty option in\n", text))

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

SweaveGetHooks <- function(options)
{
    if(!exists("SweaveHooks", mode="list"))
        return(NULL)
    
    z <- NULL
    for(k in names(SweaveHooks)){
        if(k != "" && !is.null(options[[k]]) && options[[k]]){
            if(is.function(SweaveHooks[[k]]))
                z <- c(z, parse(text=paste("SweaveHooks$",
                                k, "()", sep="")))
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
    function(file, output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE)
{
    if(is.null(output)){
        prefix.string <- basename(sub("\\.[rsRS]?nw$", "", file))
        output <- paste(prefix.string, "tex", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.tex$", "", output))
    }
    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")
    
    if(stylepath)
        styfile <- file.path(.path.package("tools"),
                             "Sweave", "Sweave")
    else
        styfile <- "Sweave"

    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=eval,
                    fig=FALSE, pdf=pdf, eps=eps, 
                    width=6, height=6, term=TRUE,
                    echo=echo, results="verbatim", split=split,
                    strip.white=TRUE, include=TRUE)
    
    list(output=output, styfile=styfile, havesty=FALSE,
         debug=debug, quiet=quiet,
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

    chunkexps <- SweaveGetHooks(options)
    nrhooks <- length(chunkexps)
    chunkexps <- c(chunkexps, parse(text=chunk))
    openSinput <- FALSE

    if(length(chunkexps)==0)
        return(object)
    
    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        dce <- deparse(ce)
        if(object$debug)
            cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
        if(options$echo && (nce > nrhooks)){
            if(!openSinput){
                cat("\\begin{Sinput}",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            cat("\nR> ", paste(dce, collapse="\n+  "),
                file=chunkout, append=TRUE, sep="")
        }

        tmpcon <- textConnection("output", "w")
        sink(file=tmpcon)
        err <- NULL
        if(options$eval) err <- RweaveEvalWithOpt(ce, options)
        sink()
        close(tmpcon)
        if(inherits(err, "try-error"))
            stop("Error while evaluating chunk\n")
        
        if(object$debug)
            cat(paste(output, collapse="\n"))
        
        if(length(output)>0 & (options$results!="hide")){
            if(openSinput){
                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                openSinput <- FALSE
            }
            if(options$results=="verbatim")
                cat("\\begin{Soutput}\n",
                    file=chunkout, append=TRUE)

            output <- paste(output,collapse="\n")
            if(options$strip.white){
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
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
            eval(chunkexps, envir=.GlobalEnv)
            dev.off()
        }
        if(options$pdf){
            pdf(file=paste(chunkprefix, "pdf", sep="."),
                width=options$width, height=options$height)
            eval(chunkexps, envir=.GlobalEnv)
            dev.off()
        }
        if(options$include)
            cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
    }
    return(object)
}

RweaveLatexWritedoc <- function(object, chunk)
{
    if(any(grep("\\usepackage[^\}]*Sweave.*\}", chunk)))
        object$havesty <- TRUE

    if(!object$havesty){
        chunk <- gsub("\\\\begin\\{document\\}",
                      paste("\\\\usepackage{",
                            object$styfile,
                            "}\n\\\\begin{document}", sep=""),
                      chunk)
        object$havesty <- TRUE
    }
    
    while((pos <-  regexpr("\\\\Sexpr{([^}]*)}", chunk)) >0)
    {
        cmd <- substr(chunk, pos+7, pos-2+attr(pos, "match.length"))
        if(object$options$eval)
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")
        
        chunk <- sub("\\\\Sexpr{[^}]*}", val, chunk)
    }
    while((pos <-  regexpr("\\\\SweaveOpts{([^}]*)}", chunk)) >0)
    {
        opts <- substr(chunk, pos+12, pos-2+attr(pos, "match.length"))
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        chunk <- sub("\\\\SweaveOpts{[^}]*}", "", chunk)
    }
    
    cat(chunk, "\n", file=object$output, append=TRUE)
    return(object)
}

RweaveLatexFinish <- function(object)
{
    if(!object$quiet)
        cat(paste("\nYou can now run LaTeX on",
                  summary(object$output)$description), "\n")
    close(object$output)
    if(length(object$chunkout)>0){
        for(con in object$chunkout) close(con)
    }
}

RweaveLatexOptions <- function(options)
{
    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    LOGOPTS <- c("fig", "pdf", "eps", "echo", "split",
                 "strip.white", "include", "prefix", "eval",
                 "print", "term")

    LOGOPTS <- LOGOPTS[LOGOPTS %in% names(options)]
    
    for(opt in LOGOPTS){
        oldval <- options[[opt]]
        if(!is.logical(options[[opt]])){
            options[[opt]] <- c2l(options[[opt]])
        }
        if(is.na(options[[opt]]))
            stop(paste("invalid value for", opt, ":", oldval))
    }

    options$results <- match.arg(options$results,
                                 c("verbatim", "tex", "hide"))

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
        res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, NULL)))
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}
    


###**********************************************************
            
Stangle <- function(file, driver=Rtangle(), ...){
    Sweave(file=file, driver=driver, ...)
}
            
Rtangle <-  function()
{
    list(setup = RtangleSetup,
         runcode = RtangleRuncode,
         writedoc = function(object, ...) return(object),
         finish = RtangleFinish,
         checkopts = RweaveLatexOptions)
}


RtangleSetup <- function(file, output=NULL, annotate=TRUE, split=FALSE,
                         prefix=TRUE, quiet=FALSE)
{
    if(is.null(output)){
        prefix.string <- basename(sub("\\.[rsRS]?nw$", "", file))
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
                    engine="R")

    list(output=output, annotate=annotate, options=options,
         chunkout=list(), quiet=quiet)
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

    if(object$annotate)
        cat("###################################################\n",
            "### chunk number ", options$chunknr,
            ": ", options$label, "\n",
            "###################################################\n",
            chunk,"\n\n", 
            file=chunkout, append=TRUE, sep="")
    else
        cat(chunk,"\n\n", 
            file=chunkout, append=TRUE, sep="")

    if(is.null(options$label) & options$split)
        close(chunkout)

    return(object)
}


RtangleFinish <- function(object)
{
    if(!is.null(object$output))
        close(object$output)

    if(length(object$chunkout)>0){
        for(con in object$chunkout) close(con)
    }
}










