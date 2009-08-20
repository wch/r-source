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
    if (is.null(srcref)) stop(..., call. = FALSE, domain = NA)
    else {
    	loc <- paste(Rdfile, ":", srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
    	stop(call.=FALSE, loc, ": ", ..., domain = NA)
    }
}

warnRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    msg <- if (is.null(srcref))
        paste("file '", Rdfile, "': ", ..., sep = "")
    else {
    	loc <- paste(Rdfile, ":", srcref[1L], sep = "")
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
        result <- try(withVisible(eval(expr, env)),
                   silent=TRUE)
        if(inherits(result, "try-error")) return(result)
        switch(options$results,
        "text" = if (result$visible)
		    res <- paste(as.character(result$value), collapse=" "),
        "verbatim" = if (result$visible) print(result$value),
        "rd" = res <- result$value)
    }
    return(res)
}

processRdChunk <- function(code, stage, options, env, Rdfile)
{
    if (is.null(opts <- attr(code, "Rd_option"))) opts <- ""
    srcref <- attr(code, "srcref")
    options <- utils:::SweaveParseOptions(opts, options, RweaveRdOptions)
    if (stage == options$stage) {
        #  The code below is very similar to RWeaveLatexRuncode, but simplified

        # Results as a character vector for now; convert to list later
        res <- character(0)
        code <- code[RdTags(code) != "COMMENT"]
	chunkexps <- try(parse(text=code), silent=TRUE)
	if (inherits(chunkexps, "try-error")) stopRd(code, Rdfile, chunkexps)

	if(length(chunkexps) == 0L)
	    return(tagged(code, "LIST"))

	srclines <- attr(code, "srclines")
	srcline <- srclines[1L]

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
		srcline <- srclines[srcref[3L]]
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

	    if (inherits(err, "try-error")) stopRd(code, Rdfile, err)

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
	    writeLinesUTF8(res, tmpcon, "UTF-8")
	    res <- tagged(parse_Rd(tmpcon, fragment=TRUE), "LIST")
	    close(tmpcon)
	    res <- prepare_Rd(res, defines = .Platform$OS.type, options=options)
	} else if (options$results == "text")
	    res <- tagged(err, "TEXT")
	else if (length(res)) {
	    res <- lapply(as.list(res), function(x) tagged(x, "VERB"))
	    res <- tagged(res, "\\verb")
	} else res <- tagged("", "COMMENT")
    } else res <- code
    attr(res, "srcref") <- srcref
    res
}

processRdIfdefs <- function(blocks, defines)
{
    recurse <- function(block) {
        if (!is.null(tag <- attr(block, "Rd_tag"))) {
	    if (tag %in% c("#ifdef", "#ifndef")) {
		target <- block[[1L]][[1L]]
		# The target will have picked up some whitespace and a newline
		target <- gsubUTF8("[[:blank:][:cntrl:]]*", "", target)
		if ((target %in% defines) == (tag == "#ifdef"))
		    block <- tagged(block[[2L]], "#expanded")
		else
		    block <- structure(tagged(paste(tag, target, "not active"),
                                              "COMMENT"),
		    		       srcref = attr(block, "srcref"))
	    }
	}
	if (is.list(block)) {
	    i <- 1
	    while (i <= length(block)) {
	    	newval <- recurse(block[[i]])
	    	newtag <- attr(newval, "Rd_tag")
	    	if (!is.null(newtag) && newtag == "#expanded") { # ifdef has expanded.
	    	    all <- seq_along(block)
	    	    before <- all[all<i]
	    	    after <- all[all>i]
	    	    block <- tagged(c(block[before], newval, block[after]), tag)
	    	} else {
		    block[[i]] <- newval
		    i <- i+1
		}
	    }
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
             options = RweaveRdDefaults)
{
    Rdfile <- "not known"
    if (is.character(Rd)) {
        Rdfile <- Rd
        ## do it this way to get info in internal warnings
        Rd <- eval(substitute(parse_Rd(f, encoding = enc),
                              list(f = Rd, enc = encoding)))
    } else if(inherits(Rd, "connection")) {
        Rdfile <- summary(Rd)
        Rd <- parse_Rd(Rd, encoding = encoding)
    } else Rdfile <- attr(Rd, "Rdfile")
    if ("build" %in% stages)
    	Rd <- processRdSexprs(Rd, "build", options)
    if (!is.null(defines))
    	Rd <- processRdIfdefs(Rd, defines)
    for (stage in c("install", "render"))
    	if (stage %in% stages)
    	    Rd <- processRdSexprs(Rd, stage, options)
    structure(Rd, Rdfile=Rdfile, class = "Rd")
}

sectionOrder <- c("\\title"=1, "\\name"=2, "\\alias"=2.1, "\\keyword"=2.2,
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

## gsub in some locales (e.g. C) gets confused by UTF-8 characters.
## This wrapper strips and replaces the encoding marker on x.  It'll
## only work if the pattern and replacement are pure ascii, and x
## is UTF-8, as it is for text in Rd objects

## I think useBytes=TRUE suffices here (BDR)

gsubUTF8 <- function(pattern, replacement, x, ...) {
    Encoding(x) <- "unknown"
    x <- gsub(pattern, replacement, x, ...)
    Encoding(x) <- "UTF-8"
    x
}

## writeLines by default re-encodes strings to the local encoding.
## Avoid that by useBytes=TRUE

writeLinesUTF8 <- function(x, con, outputEncoding, ...) {
    if (outputEncoding != "UTF-8")
	x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
    writeLines(x, con, useBytes=TRUE, ...)
}


## This warns on
##  text outside sections
##  Unrecognized macro
##  Unnecessary braces
##  non-ASCII contents without declared encoding
##  \\ldots in code block
## and errors on
##  Bad \\link text
##  Bad \\link option -- must be text
##  unrecognized tags (can the parser do that?)
##  \\tabular format must be simple text
##  Unrecognized \\tabular format:
##  "Only ", length(format), " columns allowed in this table"
##  checkUnique \title, \name, \description (include non-empty)
##  Only one \\Rdversion declaration is allowed
##  Only one \\encoding declaration is allowed
##  Encoding/docType must be plain text
##  Unrecognized section (but I think the parser catches that)
##  \\name must only contain simple text.

## It currently misses
##  invalid markup in \[S3]method (txt, latex)
## but test on codeblocks suffices.

checkRd <- function(Rd, defines=.Platform$OS.type, stages="render",
                    unknownOK = TRUE, listOK = TRUE, ..., def_enc = FALSE)
{
    checkWrapped <- function(tag, block) checkContent(block, tag)

    checkLink <- function(tag, block) { # FIXME This doesn't handle aliases, and
                                        # doesn't cover all variations
    	option <- attr(block, "Rd_option")
    	if(!is.null(option)) checkContent(option, tag)
    	checkContent(block, tag)
        get_link(block, tag, Rdfile) ## to do the same as Rd2HTML
    }

    ## blocktag is unused
    checkBlock <- function(block, tag, blocktag)
    {
	switch(tag,
               UNKNOWN = if (!unknownOK)
               stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]])
               else warnRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
               VERB = ,
               RCODE = ,
               TEXT = {
                   if(!def_enc) {
                       ## check for encoding; this is UTF-8 if known
                       ## (but then def_enc = TRUE?)
                       if(Encoding(block) == "UTF-8")
                           warnRd(block, Rdfile,
                                  "Non-ASCII contents without declared encoding")
                       if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                           warnRd(block, Rdfile,
                                  "Apparent non-ASCII contents without declared encoding")
                   }
                   ## check if this renders as non-whitespace
                   if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
               },
               COMMENT = {},
               LIST = if (length(block)) {
                   deparse <- sQuote(paste(as.character.Rd(block), collapse=""))
                   if(!listOK)
                       stopRd(block, Rdfile, "Unnecessary braces at ", deparse)
                   else warnRd(block, Rdfile, "Unnecessary braces at ", deparse)
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
                   stopRd(block, Rdfile, "Tag ", tag,
                          " not valid outside a code block"),
               "\\enc" = {
                   checkContent(block[[1L]], tag)
                   ## second arg should always be ASCII
                   save_enc <- def_enc
                   def_enc <<- FALSE
                   checkContent(block[[2L]], tag)
                   def_enc <<- save_enc
               },
               "\\eqn" =,
               "\\deqn" = {
                   checkContent(block[[1L]])
                   if (length(block) > 1L) checkContent(block[[2L]])
               },
               "\\tabular" = checkTabular(block),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized"))
    }

    checkCodeBlock <- function(blocks, blocktag)
    {
	for (block in blocks) {
            tag <- attr(block, "Rd_tag")
            switch(tag,
                   UNKNOWN = if (!unknownOK)
                   stopRd(block, Rdfile, "Unrecognized macro ", block[[1L]])
                   else warnRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
                   VERB = ,
                   RCODE = ,
                   TEXT = {
                       if(!def_enc) {
                           ## check for encoding; this is UTF-8 if known
                           ## (but then def_enc = TRUE?)
                           if(Encoding(block) == "UTF-8")
                               warnRd(block, Rdfile,
                                      "Non-ASCII contents without declared encoding")
                           if(grepl("<[0123456789abcdef][0123456789abcdef]>", block))
                               warnRd(block, Rdfile,
                                      "Apparent non-ASCII contents without declared encoding")
                       }
                       ## check if this renders as non-whitespace
                       if(!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
                   },
                   COMMENT = {},
                   "\\var" = checkCodeBlock(block, blocktag), # not preformatted, but the parser checks that
                   "\\special" = checkCodeBlock(block, blocktag),
                   "\\dots" = has_text <<- TRUE,
                   "\\ldots" = {
                       warnRd(block, Rdfile, "Tag ", tag,
                              " is invalid in a code block")
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
                   } else stopRd(block, Rdfile, "Tag ", tag,
                                 " is only valid in \\usage"),
                   "\\dontrun" =,
                   "\\donttest" =,
                   "\\dontshow" =,
                   "\\testonly" = if(blocktag == "\\examples")
                   checkCodeBlock(block, blocktag)
                   else stopRd(block, Rdfile, "Tag ", tag,
                               " is only valid in \\examples"),
                   stopRd(block, Rdfile, "Tag ", tag,
                          " is invalid in a ", blocktag, " block"))
        }
    }

    checkTabular <- function(table) {
        has_text <<- TRUE
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed=TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, Rdfile,
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
                    stopRd(table, Rdfile,
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
    	if (tag == "\\section") {
    	    title <- section[[1L]]
            ## should be simple text
            if(length(title) < 1L || attr(title[[1L]], "Rd_tag") != "TEXT")
                warnRd(Rd, Rdfile, "Section title must be plain text")
    	    checkContent(title, tag)
    	    section <- section[[2L]]
            ## replace 'tag' in message below
            tagtitle <- sQuote(as.character(title))
    	} else tagtitle <- tag
        has_text <<- FALSE
        if (tag %in% c("\\usage", "\\synopsis", "\\examples"))
            checkCodeBlock(section, tag)
    	else checkContent(section, tag)
        if(!has_text) warnRd(section, Rdfile, "Empty section ", tagtitle)
    }

    checkUnique <- function(tag) {
    	which <- which(sections == tag)
    	if (length(which) < 1L)
    	    stopRd(Rd, Rdfile, "Must have a ", tag)
    	else if (length(which) > 1L)
    	    stopRd(Rd[[which[2L]]], Rdfile, "Only one ", tag, " is allowed")
        empty <- TRUE
        for(block in Rd[[which]]) {
            switch(attr(block, "Rd_tag"),
                   TEXT = if(!grepl("^[[:space:]]*$", block)) empty <- FALSE,
                   empty <- FALSE)
        }
        if(empty)
            warnRd(Rd[[which]], Rdfile, "Tag ", tag, " must not be empty")
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1L && as.numeric(Rd[[version]][[1L]]) < 2) {
        ## <FIXME>
        ## Should we unconditionally warn (or notify using message())?
        ## CRAN currently (2009-07-28) has more than 250 \Rdversion{1.1}
        ## packages ...
        if(identical(getOption("verbose"), TRUE))
            warning("checkRd is designed for Rd version 2 or higher")
        ## </FIXME>
    }
    else if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], Rdfile,
               "Only one \\Rdversion declaration is allowed")

    ## Give warning (pro tem) for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]",
                           unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	warnRd(Rd[sections == "TEXT"][[bad[1L]]], Rdfile,
               "All text must be in a section")

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1L)
    	    stopRd(Rd[[enc[2L]]], Rdfile,
                   "Only one \\encoding declaration is allowed")
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, Rdfile, "Encoding must be plain text")
        def_enc <- TRUE
    }

    dt <- which(sections == "\\docType")
    docTypes <- character(length(dt))
    if (length(dt)) {
        for (i in dt) {
            docType <- Rd[[i]]
            if(!identical(RdTags(docType), "TEXT"))
        	stopRd(docType, Rdfile, "docType must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    checkUnique("\\title")
    checkUnique("\\name")
    if(!identical("package", docTypes))
        checkUnique("\\description")

    name <- Rd[[which(sections == "\\name")]]
    tags <- RdTags(name)
    if (length(tags) > 1L)
        stopRd(name, Rdfile, "\\name must only contain simple text")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType",
                            "\\encoding", "\\keyword", "\\Rdversion", "\\RdOpts")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1L]]], Rdfile,
               "Section ", sections[which(bad)[1L]],
               " unrecognized")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]

    for (i in seq_along(sections))
    	checkSection(Rd[[i]], sections[i])

    TRUE
}
