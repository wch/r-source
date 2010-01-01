#  File src/library/tools/R/RdConv2.R
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


RdTags <- function(Rd) {
    res <- sapply(Rd, attr, "Rd_tag")
    if (!length(res)) res <- character(0)
    res
}

isBlankRd <- function(x)
    length(grep("^[[:blank:]]*\n?$", x, perl = TRUE)) == length(x) # newline optional

isBlankLineRd <- function(x) {
    attr(x, "srcref")[2L] == 1 &&
    length(grep("^[[:blank:]]*\n", x, perl = TRUE)) == length(x)   # newline required
}

stopRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    if (missing(Rdfile) && !is.null(srcref)) {
    	srcfile <- attr(srcref, "srcfile")
    	if (is.environment(srcfile))
    	    Rdfile <- srcfile$filename
    }
    if (missing(Rdfile) || is.null(Rdfile)) Rdfile <- ""
    else Rdfile <- paste(Rdfile, ":", sep="")

    msg <- if (is.null(srcref))
        paste(Rdfile, " ", ..., sep = "")
    else {
    	loc <- paste(Rdfile, srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
    	paste(loc, ": ", ..., sep="")
    }
    stop(msg, call. = FALSE, domain = NA)
}

warnRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    if (missing(Rdfile) && !is.null(srcref)) {
    	srcfile <- attr(srcref, "srcfile")
    	if (is.environment(srcfile))
    	    Rdfile <- srcfile$filename
    }
    if (missing(Rdfile) || is.null(Rdfile)) Rdfile <- ""
    else Rdfile <- paste(Rdfile, ":", sep="")

    msg <- if (is.null(srcref))
        paste(Rdfile, " ", ..., sep = "")
    else {
    	loc <- paste(Rdfile, srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
        paste(loc, ": ", ..., sep = "")
    }
    warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
}

RweaveRdDefaults <- list(
    width = 6,
    height = 6,
    eval = TRUE,
    fig = FALSE,
    echo = FALSE,
    keep.source = TRUE,
    results = "text",
    strip.white = "true",
    stage = "install")

RweaveRdOptions <- function(options)
{

    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }

    NUMOPTS <- c("width", "height")
    NOLOGOPTS <- c(NUMOPTS, "results", "stage", "strip.white")

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
                                 c("text", "verbatim", "rd", "hide"))
    if(!is.null(options$stage))
    	options$stage <- tolower(as.character(options$stage))
    options$stage <- match.arg(options$stage,
    				 c("build", "install", "render"))
    options
}

tagged <- function(x, tag) structure(x, Rd_tag=tag)

evalWithOpt <- function(expr, options, env)
{
    res <- structure("", Rd_tag="COMMENT")
    if(options$eval){
        result <- tryCatch(withVisible(eval(expr, env)), error=function(e) e)

        if(inherits(result, "error")) return(result)
        switch(options$results,
        "text" = if (result$visible)
		    res <- paste(as.character(result$value), collapse=" "),
        "verbatim" = if (result$visible) print(result$value),
        "rd" = res <- result$value)
    }
    return(res)
}

getDynamicFlags <- function(block) {
    flag <- attr(block, "dynamicFlag")
    if (is.null(flag)) c("#ifdef"=FALSE, "\\Sexpr"=FALSE)
    else c("#ifdef" = flag %% 2 > 0, "\\Sexpr" = flag %/% 2 > 0)
}

setDynamicFlags <- function(block, flags) {  # flags in format coming from getDynamicFlags
    flag <- sum(flags * c(1,2))
    if (flag == 0) flag <- NULL
    attr(block, "dynamicFlag") <- flag
    block
}

processRdChunk <- function(code, stage, options, env, Rdfile)
{
    if (is.null(opts <- attr(code, "Rd_option"))) opts <- ""
    codesrcref <- attr(code, "srcref")
    options <- utils:::SweaveParseOptions(opts, options, RweaveRdOptions)
    if (stage == options$stage) {
        #  The code below is very similar to RWeaveLatexRuncode, but simplified

        # Results as a character vector for now; convert to list later
        res <- character(0)
        code <- code[RdTags(code) != "COMMENT"]
	chunkexps <- tryCatch(parse(text = code), error = identity)
	if (inherits(chunkexps, "error")) stopRd(code, Rdfile, chunkexps)

	if(length(chunkexps) == 0L)
	    return(tagged(code, "LIST"))

	srcrefs <- attr(chunkexps, "srcref")
	lastshown <- 0L
	thisline <- 0
	err <- NULL
	for(nce in seq_along(chunkexps))
	{
	    ce <- chunkexps[[nce]]
	    if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
		srcfile <- attr(srcref, "srcfile")
		showfrom <- srcref[1L]
		showto <- srcref[3L]
		dce <- getSrcLines(srcfile, lastshown+1, showto)
		leading <- showfrom-lastshown
		lastshown <- showto
		while (length(dce) && grepl("^[[:blank:]]*$", dce[1L])) {
		    dce <- dce[-1L]
		    leading <- leading - 1L
		}
	    } else {
		dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
		leading <- 1L
	    }
	    if(options$echo && length(dce)) {
		res <- c(res,"\n",
                         paste(getOption("prompt"), dce[1L:leading],
                               sep="", collapse="\n"))
		if (length(dce) > leading)
		    res <- c(res, "\n",
                             paste(getOption("continue"), dce[-(1L:leading)],
                                   sep="", collapse="\n"))
		thisline <- thisline + length(dce)
	    }

	    tmpcon <- file()
	    sink(file = tmpcon)
	    if(options$eval) err <- evalWithOpt(ce, options, env)
	    res <- c(res, "\n") # make sure final line is complete
	    sink()
	    output <- readLines(tmpcon)
	    close(tmpcon)
	    ## delete empty output
	    if(length(output) == 1L & output[1L] == "") output <- NULL

	    if (inherits(err, "error")) {
	    	attr(code, "srcref") <- codesrcref
	    	stopRd(code, Rdfile, err$message)
	    }

	    if(length(output) & (options$results != "hide")){

		output <- paste(output, collapse="\n")
		if(options$strip.white %in% c("all", "true")) {
		    output <- sub("^[[:space:]]*\n", "", output)
		    output <- sub("\n[[:space:]]*$", "", output)
		    if(options$strip.white == "all")
		      output <- sub("\n[[:space:]]*\n", "\n", output)
		}
		res <- c(res, output)
		remove(output)
	    }
	}
	if (options$results == "rd") {
	    res <- err   # The last value of the chunk
	    tmpcon <- file()
	    writeLines(res, tmpcon, useBytes = TRUE)
	    res <- parse_Rd(tmpcon, fragment=TRUE)
	    flag <- getDynamicFlags(res)
	    res <- tagged(res, "LIST")
	    res <- setDynamicFlags(res, flag)
	    close(tmpcon)
	    res <- prepare_Rd(res, defines = .Platform$OS.type, options=options,
	                           stage2 = FALSE, stage3 = FALSE)
	} else if (options$results == "text")
	    res <- tagged(err, "TEXT")
	else if (length(res)) {
	    res <- lapply(as.list(res), function(x) tagged(x, "VERB"))
	    res <- tagged(res, "\\verb")
	} else res <- tagged("", "COMMENT")
    } else res <- code
    attr(res, "srcref") <- codesrcref
    res
}

processRdIfdefs <- function(blocks, defines)
{
    recurse <- function(block) {
    	if (!(getDynamicFlags(block)["#ifdef"])) return(block)

        if (!is.null(tag <- attr(block, "Rd_tag"))) {
	    if (tag %in% c("#ifdef", "#ifndef")) {
		target <- block[[1L]][[1L]]
		# The target will have picked up some whitespace and a newline
		target <- psub("[[:blank:][:cntrl:]]*", "", target)
		if ((target %in% defines) == (tag == "#ifdef")) {
		    flag <- getDynamicFlags(block[[2L]])
		    block <- tagged(block[[2L]], "#expanded")
		    block <- setDynamicFlags(block, flag)
		} else
		    block <- structure(tagged(paste(tag, target, "not active"),
                                              "COMMENT"),
		    		       srcref = attr(block, "srcref"))
	    }
	}
	if (is.list(block)) {
	    i <- 1L
	    flags <- getDynamicFlags(NULL)
	    while (i <= length(block)) {
	    	newval <- recurse(block[[i]])
	    	newtag <- attr(newval, "Rd_tag")
	    	if (!is.null(newtag) && newtag == "#expanded") { # ifdef has expanded.
	    	    all <- seq_along(block)
	    	    before <- all[all < i]
	    	    after <- all[all > i]
	    	    block <- tagged(c(block[before], newval, block[after]), tag)
	    	} else {
	    	    flags <- flags | getDynamicFlags(newval)
		    block[[i]] <- newval
		    i <- i+1L
		}
	    }
	    block <- setDynamicFlags(block, flags)
	}
	block
    }

    recurse(blocks)
}

processRdSexprs <-
    function(block, stage, options = RweaveRdDefaults,
             env = new.env(parent=globalenv()))
{
    recurse <- function(block) {
    	if (!getDynamicFlags(block)["\\Sexpr"]) return(block)

        if (is.list(block)) {
            if (!is.null(tag <- attr(block, "Rd_tag"))) {
        	if (tag == "\\Sexpr")
            	    block <- processRdChunk(block, stage, options, env)
            	else if (tag == "\\RdOpts")
    	    	    options <<-
                        utils:::SweaveParseOptions(block, options, RweaveRdOptions)
    	    }
	    for (i in seq_along(block))
		block[[i]] <- recurse(block[[i]])
	}
	block
    }
    recurse(block)
}

prepare_Rd <-
    function(Rd, encoding = "unknown", defines = NULL, stages = NULL,
             options = RweaveRdDefaults,
             stage2 = TRUE, stage3 = TRUE, ..., msglevel = 0)
{
    if (is.character(Rd)) {
        Rdfile <- Rd
        ## do it this way to get info in internal warnings
        Rd <- eval(substitute(parse_Rd(f, encoding = enc, ...),
                              list(f = Rd, enc = encoding)))
    } else if(inherits(Rd, "connection")) {
        Rdfile <- summary(Rd)
        Rd <- parse_Rd(Rd, encoding = encoding, ...)
    } else Rdfile <- attr(Rd, "Rdfile")
    if (is.null(Rdfile) && !is.null(srcref <- attr(Rd, "srcref")))
    	Rdfile <- attr(srcref, "srcfile")$filename
    pratt <- attr(Rd, "prepared")
    if (is.null(pratt)) pratt <- 0L
    if ("build" %in% stages)
    	Rd <- processRdSexprs(Rd, "build", options)
    if (!is.null(defines))
    	Rd <- processRdIfdefs(Rd, defines)
    for (stage in c("install", "render"))
    	if (stage %in% stages)
    	    Rd <- processRdSexprs(Rd, stage, options)
    if (pratt < 2L && stage2)
        Rd <- prepare2_Rd(Rd, Rdfile)
    meta <- attr(Rd, "meta")
    if (pratt < 3L && stage3)
        Rd <- prepare3_Rd(Rd, Rdfile, msglevel = msglevel)

    # Restore flags from any sections that are left
    Rd <- setDynamicFlags(Rd, apply(sapply(Rd, getDynamicFlags), 1, any))

    structure(Rd, Rdfile = Rdfile, class = "Rd", meta = meta)
}

prepare2_Rd <- function(Rd, Rdfile)
{
    sections <- RdTags(Rd)

    ## FIXME: we no longer make any use of \Rdversion
    version <- which(sections == "\\Rdversion")
    if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], Rdfile,
               "Only one \\Rdversion declaration is allowed")

    ## Give warning (pro tem) for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]",
                           unlist(Rd[sections == "TEXT"]),
                           perl = TRUE, useBytes = TRUE )))
        for(s in bad)
            warnRd(Rd[sections == "TEXT"][[s]], Rdfile,
                   "All text must be in a section")

    drop <- rep.int(FALSE, length(sections))

    where <- which(sections == "\\examples")
    if(length(where) > 1L) {
        warnRd(Rd[where[[2L]]], Rdfile,
               "Only one \\examples section is allowed: the first will be used")
        drop[where[-1L]] <- TRUE
    }

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1L) {
    	    warnRd(Rd[[enc[2L]]], Rdfile,
                   "Only one \\encoding declaration is allowed: the first will be used")
            drop[enc[-1L]] <- TRUE
            enc <- enc[[1L]]
        }
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, Rdfile, "'encoding' must be plain text")
    }

    dt <- which(sections == "\\docType")
    docTypes <- character(length(dt))
    if(length(dt)) {
        for(i in seq_along(dt)) {
            docType <- Rd[[dt[i]]]
            if(!identical(RdTags(docType), "TEXT"))
        	stopRd(docType, Rdfile, "'docType' must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    ## Drop all the parts that are not rendered
    extras <- c("COMMENT", "TEXT", "\\docType", "\\Rdversion", "\\RdOpts")
    drop <- drop | (sections %in% extras)
    bad <- ! sections %in% c(names(sectionOrder), extras)
    if (any(bad)) {
        for(s in which(bad))
            warnRd(Rd[[s]], Rdfile, "Section ",
                   sections[s], " is unrecognized and will be dropped")
        drop <- drop | bad
    }
    Rd <- Rd[!drop]
    sections <- sections[!drop]
    sortorder <- order(sectionOrder[sections])
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, Rdfile,
               "Sections \\title, and \\name must exist and be unique in Rd files")
    if (length(RdTags(Rd[[2L]])) > 1L)
        stopRd(RdTags(Rd[[2L]]), Rdfile,"\\name must only contain simple text")

    ## R-exts points out that ! | @ cause problems in \name:
    ## ggplot2 demonstrated it
    name_text <- as.character(Rd[[2L]])
    if(grepl("[!|@]", name_text))
        warnRd(RdTags(Rd[[2L]]), Rdfile,"\\name should not contain !, | or @")
    structure(Rd, meta = list(docType = docTypes))
}

prepare3_Rd <- function(Rd, Rdfile, msglevel = 0)
{
    ## Drop 'empty' sections: less rigorous than checkRd test
    keep <- rep(TRUE, length(Rd))
    checkEmpty <- function(x, this)
    {
        if(this) return(TRUE)
        if(is.list(x))
            for(xx in x) this <- checkEmpty(xx, this)
        else {
            tag <- attr(x, "Rd_tag")
            switch(tag,
                   COMMENT = {},
                   VERB =,
                   RCODE =,
                   TEXT = if(any(grepl("[^[:space:]]", s, perl = TRUE, useBytes=TRUE))) return(TRUE),
                   return(TRUE)
                   )
        }
        this
     }
    for (i in seq_along(Rd)) {
        this <- FALSE
        s0 <- section <- Rd[[i]]
        tag <- attr(section, "Rd_tag")
        if(tag == "\\section") {
            tagtitle <- sQuote(as.character(section[[1L]]))
            section <- section[[2L]]
        } else tagtitle <- tag
        for(s in section) this <- checkEmpty(s, this)
        keep[i] <- this
        if(!this && msglevel > 0)
            warnRd(s0, Rdfile, "Dropping empty section ", tagtitle)
    }
    Rd[keep]
}

sectionOrder <- c("\\title"=1, "\\name"=2, "\\alias"=2.1, "\\concept"=2.2,
                  "\\keyword"=2.3, "\\encoding"=2.4,
    "\\description"=3, "\\usage"=4, "\\synopsis"=4, "\\arguments"=5,
    "\\format"=6, "\\details"=7, "\\value"=8, "\\section"=9,
    "\\note"=10, "\\author" = 11, "\\source"=12, "\\references"=13,
    "\\seealso"=14, "\\examples"=15)

sectionTitles <-
    c("\\description"="Description", "\\usage"="Usage", "\\synopsis"="Usage",
      "\\arguments"="Arguments", "\\format"="Format", "\\details"="Details",
      "\\note"="Note", "\\section"="section", "\\author"="Author(s)",
      "\\references"="References", "\\source"="Source",
      "\\seealso"="See Also", "\\examples"="Examples", "\\value"="Value")

psub <- function(pattern, replacement, x)
##    gsub(pattern, replacement, x, perl = TRUE, useBytes = TRUE)
    .Internal(gsub(pattern, replacement, x, FALSE, TRUE, FALSE, TRUE))

psub1 <- function(pattern, replacement, x)
##    sub(pattern, replacement, x, perl = TRUE, useBytes = TRUE)
    .Internal(sub(pattern, replacement, x, FALSE, TRUE, FALSE, TRUE))

fsub <- function(pattern, replacement, x)
##    gsub(pattern, replacement, x, fixed = TRUE, useBytes = TRUE)
    .Internal(gsub(pattern, replacement, x, FALSE, FALSE, TRUE, TRUE))

fsub1 <- function(pattern, replacement, x)
##    sub(pattern, replacement, x, fixed = TRUE, useBytes = TRUE)
    .Internal(sub(pattern, replacement, x, FALSE, FALSE, TRUE, TRUE))


## for lists of messages, see ../man/checkRd.Rd
checkRd <- function(Rd, defines=.Platform$OS.type, stages="render",
                    unknownOK = TRUE, listOK = TRUE, ..., def_enc = FALSE)
{
    warnRd <- function(block, Rdfile, ..., level=0)
    {
        srcref <- attr(block, "srcref")
        msg <- if (is.null(srcref))
            paste("file '", Rdfile, "': ", ..., sep = "")
        else {
            loc <- paste(Rdfile, ":", srcref[1L], sep = "")
            if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
            paste(loc, ": ", ..., sep = "")
        }
        msg <- sprintf("checkRd: (%d) %s", level, msg)
        .messages <<- c(.messages, msg)
    }

    checkWrapped <- function(tag, block) checkContent(block, tag)

    checkLink <- function(tag, block) {
    	option <- attr(block, "Rd_option")
    	if(!is.null(option)) checkContent(option, tag)
    	checkContent(block, tag)
        get_link(block, tag, Rdfile) ## to do the same as Rd2HTML
    }

    ## blocktag is unused
    checkBlock <- function(block, tag, blocktag)
    {
	switch(tag,
               ## parser already warned here
               UNKNOWN = if (!unknownOK)
               stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
               VERB = ,
               RCODE = ,
               TEXT = {
                   if(!def_enc) {
                       ## check for encoding; this is UTF-8 if known
                       ## (but then def_enc = TRUE?)
                       msg2 <- if(inEnc2) "in second part of \\enc" else "without declared encoding"
                       if(Encoding(block) == "UTF-8")
                           warnRd(block, Rdfile, level = -1,
                                  "Non-ASCII contents ", msg2)
                       if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                           warnRd(block, Rdfile, level = -3,
                                  "Apparent non-ASCII contents ", msg2)
                   }
                   ## check if this renders as non-whitespace
                   if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
               },
               COMMENT = {},
               LIST = if (length(block)) {
                   deparse <- sQuote(paste(as.character.Rd(block), collapse=""))
                   if(!listOK)
                       stopRd(block, Rdfile, "Unnecessary braces at ", deparse)
                   else warnRd(block, Rdfile, level = -3,
                               "Unnecessary braces at ", deparse)
                   checkContent(block, tag)
               },
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize"=,
               "\\bold"=,
               "\\cite"=,
               "\\command"=,
               "\\dfn"=,
               "\\emph"=,
               "\\kbd"= checkContent(block, tag),
               "\\code"=,
               "\\preformatted"= checkCodeBlock(block, tag),
               "\\Sexpr"=,
               "\\special"=,
               "\\strong"=,
               "\\var" =,
               "\\verb"= checkContent(block, tag),
               "\\linkS4class" =,
               "\\link" = checkLink(tag, block),
               "\\email" =,
               "\\url" = has_text <<- TRUE,
               "\\cr" ={},
               "\\dots" =,
               "\\ldots" =,
               "\\R" = has_text <<- TRUE,
               "\\acronym" =,
               "\\env" =,
               "\\file" =,
               "\\option" =,
               "\\pkg" =,
               "\\samp" =,
               "\\sQuote" =,
               "\\dQuote" = checkContent(block, tag),
               "\\method" =,
               "\\S3method" =,
               "\\S4method" =
                   warnRd(block, Rdfile, level = 7, "Tag ", tag,
                          " not valid outside a code block"),
               "\\enc" = {
                   checkContent(block[[1L]], tag)
                   ## second arg should always be ASCII
                   save_enc <- def_enc
                   def_enc <<- FALSE
                   inEnc2 <<- TRUE
                   checkContent(block[[2L]], tag)
                   def_enc <<- save_enc
                   inEnc2 <<- FALSE
               },
               "\\eqn" =,
               "\\deqn" = {
                   checkContent(block[[1L]])
                   if (length(block) > 1L) checkContent(block[[2L]])
               },
               "\\tabular" = checkTabular(block),
               "\\subsection" = checkSection(block, tag),
               "\\if" =,
               "\\ifelse" = {
    		   condition <- block[[1L]]
    		   tags <- RdTags(condition)
    		   if (!all(tags %in% c("TEXT", "\\Sexpr")))
    		       stopRd(block, Rdfile, "Condition must be \\Sexpr or plain text")
    		   condition <- condition[tags == "TEXT"]
    		   allow <- .strip_whitespace(strsplit(paste(condition, collapse=""), ",")[[1L]])
    		   unknown <- allow[!(allow %in%
    		          c("", "latex", "example", "text", "html", "TRUE", "FALSE"))]
    		   if (length(unknown))
    		       warnRd(block, Rdfile, "Unrecognized format: ", unknown)
                   checkContent(block[[2L]])
                   if (tag == "\\ifelse")
                       checkContent(block[[3L]])
               },
               "\\out" = {
               	   tags <- RdTags(block)
               	   if (!all(tags == "VERB"))
               	       stopRd(block, Rdfile, "Must contain verbatim text")
               },
               warnRd(block, Rdfile, level = 7, "Tag ", tag, " not recognized"))
    }

    checkCodeBlock <- function(blocks, blocktag)
    {
	for (block in blocks) {
            tag <- attr(block, "Rd_tag")
            switch(tag,
                   ## parser already warned here
                   UNKNOWN = if (!unknownOK)
                   stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
                   VERB = ,
                   RCODE = ,
                   TEXT = {
                       if(!def_enc) {
                           ## check for encoding; this is UTF-8 if known
                           ## (but then def_enc = TRUE?)
                           msg2 <- if(inEnc2) "in second part of \\enc" else "without declared encoding"
                           if(Encoding(block) == "UTF-8")
                               warnRd(block, Rdfile, level = -1,
                                      "Non-ASCII contents ", msg2)
                           if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                               warnRd(block, Rdfile, level = -3,
                                      "Apparent non-ASCII contents ", msg2)
                       }
                       ## check if this renders as non-whitespace
                       if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
                   },
                   COMMENT = {},
                   "\\var" = checkCodeBlock(block, blocktag), # not preformatted, but the parser checks that
                   "\\special" = checkCodeBlock(block, blocktag),
                   "\\dots" = has_text <<- TRUE,
                   "\\ldots" = {
                       ## but it is rendered as ... in all converters
                       warnRd(block, Rdfile, level = -3,
                              "Tag ", tag, " is invalid in a code block")
                       has_text <<- TRUE
                   },
                   ## these are valid in \code, at least
                   "\\linkS4class" =,
                   "\\link" = checkLink(tag, block),
                   "\\method" =,
                   "\\S3method" =,
                   "\\S4method" = if(blocktag == "\\usage") {
                       checkContent(block[[1L]], tag) # generic
                       checkContent(block[[2L]], tag) # class
                   } else warnRd(block, Rdfile, level = 5,
                                 "Tag ", tag, " is only valid in \\usage"),
                   "\\dontrun" =,
                   "\\donttest" =,
                   "\\dontshow" =,
                   "\\testonly" = if(blocktag == "\\examples")
                   checkCodeBlock(block, blocktag)
                   else warnRd(block, Rdfile, level = 5,
                               "Tag ", tag, " is only valid in \\examples"),
                   {
                       warnRd(block, Rdfile, level = 5,
                              "Tag ", tag, " is invalid in a ",
                              blocktag, " block")
                       has_text <<- TRUE  # likely, e.g. \url
                   })
        }
    }

    checkTabular <- function(table) {
        has_text <<- TRUE
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    warnRd(table, Rdfile, level = 7,
                   "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed=TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    warnRd(table, Rdfile, level = 7,
                   "Unrecognized \\tabular format: ", table[[1L]][[1L]])
        tags <- RdTags(content)

        newrow <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format))
                    warnRd(table, Rdfile, level = 7,
                           "Only ", length(format),
                           " columns allowed in this table")
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	newcol <- TRUE
            },
            "\\cr" = {
            	newrow <- TRUE
            },
            checkBlock(content[[i]], tags[i], "\\tabular"))
        }
    }

    checkContent <- function(blocks, blocktag) {
        inlist <- FALSE

	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
            "\\item" = {
    	    	if (!inlist) inlist <- TRUE
    		switch(blocktag,
    		"\\arguments"= {
    		    checkContent(block[[1L]], tag)
    		    checkContent(block[[2L]], tag)
    		},
    		"\\value"=,
    		"\\describe"= {
    		    checkContent(block[[1L]], tag)
    		    checkContent(block[[2L]], tag)
    		},
    		"\\enumerate"=,
    		"\\itemize"= {})
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    		    inlist <- FALSE
    		}
    		checkBlock(block, tag, blocktag)
    	    })
	}
    }

    has_text <- FALSE
    checkSection <- function(section, tag) {
    	if (tag == "\\section" || tag == "\\subsection") {
    	    title <- section[[1L]]
            ## should be simple text
            if(length(title) < 1L || attr(title[[1L]], "Rd_tag") != "TEXT") {
                warnRd(section, Rdfile, level = 5,
                       "Title of \\section must be non-empty plain text")
            }
    	    checkContent(title, tag)
    	    section <- section[[2L]]
            ## replace 'tag' in message below
            tagtitle <- sQuote(as.character(title))
    	} else tagtitle <- tag
        has_text <<- FALSE
        if (tag %in% c("\\usage", "\\synopsis", "\\examples"))
            checkCodeBlock(section, tag)
    	else checkContent(section, tag)
        if(!has_text) warnRd(section, Rdfile, level = 3,
                             "Empty section ", tagtitle)
    }

    checkUnique <- function(tag) {
    	which <- which(sections == tag)
    	if (length(which) < 1L)
    	    warnRd(Rd, Rdfile, level = 7, "Must have a ", tag)
    	else {
            if (length(which) > 1L)
    	    warnRd(Rd[[which[2L]]], Rdfile, level = 7,
                   "Only one ", tag, " is allowed")
            empty <- TRUE
            for(block in Rd[[which]]) {
                switch(attr(block, "Rd_tag"),
                       TEXT = if(!grepl("^[[:space:]]*$", block))
                       empty <- FALSE,
                       empty <- FALSE)
            }
            if(empty)
                warnRd(Rd[[which]], Rdfile, level = 5,
                       "Tag ", tag, " must not be empty")
        }
    }

    dt <- which(RdTags(Rd) == "\\docType")
    docTypes <- character(length(dt))
    if (length(dt)) {
        for (i in dt) {
            docType <- Rd[[i]]
            if(!identical(RdTags(docType), "TEXT"))
        	warnRd(docType, Rdfile, level = 7,
                       "'docType' must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    .messages <- character()
    .whandler <-     function(e) {
        .messages <<- c(.messages, paste("prepare_Rd:", conditionMessage(e)))
        invokeRestart("muffleWarning")
    }

    Rd <- withCallingHandlers({
        prepare_Rd(Rd, defines=defines, stages=stages,
                   warningCalls = FALSE, ..., msglevel = 1)
    }, warning = .whandler)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    if (sections[1] == "\\title")  # if not, we've already been warned...
    	if (!all(RdTags(Rd[[1]]) == "TEXT"))
    	    warnRd(Rd[[1]], Rdfile, level = 5,
    	    	   "\\title content must be plain text")

    enc <- which(sections == "\\encoding")
    ## sanity was checked in prepare2_Rd
    if (length(enc)) def_enc <- TRUE

    inEnc2 <- FALSE
    if(!identical("package", docTypes))
        checkUnique("\\description")
    for (i in seq_along(sections))
        checkSection(Rd[[i]], sections[i])

    structure(.messages, class = "checkRd")
}

print.checkRd <- function(x, minlevel = -Inf, ...)
{
    fromParse <- grepl("^prepare_Rd", x)
    x1 <- x[fromParse]
    x2 <- x[!fromParse]
    levs <- as.numeric(sub("^checkRd: \\(([-0123456789]+)(.*)", "\\1", x2))
    xx <- if(minlevel > 0) x2[levs >= minlevel] else c(x1, x2[levs >= minlevel])
    writeLines(unique(xx))
    invisible(x)
}

testRdConditional <- function(format, conditional, Rdfile) {
    condition <- conditional[[1L]]
    tags <- RdTags(condition)
    if (!all(tags == "TEXT")) stopRd(conditional, Rdfile, "condition must be plain text")

    allow <- .strip_whitespace(strsplit(paste(condition, collapse=""), ",")[[1L]])
    any(c("TRUE", format) %in% allow)
}


