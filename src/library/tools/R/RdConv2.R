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

get_link <- function(arg, tag) {
    ## 'topic' is the name to display, 'dest' is the topic to link to
    ## optionaly in package 'pkg'.  If 'target' is set it is the file
    ## to link to in HTML help

    ## \link[=bar]{foo} means shows foo but treat this as a link to bar.
    ## \link[pkg]{foo} means show foo and link to *file* foo in package pkg
    ## \link{pkg:bar]{foo} means show foo and link to file bar in package pkg.

    if (!all(RdTags(arg) == "TEXT"))
    	stopRd(arg, "Bad \\link text")

    option <- attr(arg, "Rd_option")

    dest <- paste(unlist(arg), collapse = "")
    topic <- dest
    targetfile <- NULL
    pkg <- NULL
    if (!is.null(option)) {
        if (!identical(attr(option, "Rd_tag"), "TEXT"))
    	    stopRd(option, "Bad \\link option")
    	if (length(grep("^=", option, perl = TRUE)))
    	    dest <- sub("^=", "", option, perl = TRUE)
    	else if (length(grep(":", option, perl = TRUE))) {
    	    targetfile <- sub("^[^:]*:", "", option, perl = TRUE)
    	    pkg <- sub(":.*", "", option, perl = TRUE)
    	} else {
            targetfile <- dest
    	    pkg <- option
    	}
    }
    if (tag == "\\linkS4class") dest <- paste(dest, "-class", sep="")
    list(topic = topic, dest = dest, pkg = pkg, targetfile = targetfile)
}

transform_S3_method  <- function(x)
{
    ## should take a call such as \method{foo}{bar}
    ## and convert it to a parse_Rd version of the required line(s)
    x
}

transform_S4_method  <- function(x)
{
    ## should take a call such as \S4method{foo}{bar}
    ## and convert it to a parse_Rd version of the required line(s)
    x
}


# translation of Utils.pm function of the same name, plus "unknown"
mime_canonical_encoding <- function(encoding)
{
    encoding[encoding %in% c("", "unknown")] <- localeToCharset()[1]
    encoding <- tolower(encoding)
    encoding <- sub("iso_8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding <- sub("iso8859-([0-9]+)", "iso-8859-\\1", encoding)
    encoding[encoding == "latin1"] <-  "iso-8859-1"
    encoding[encoding == "latin2"] <-  "iso-8859-2"
    encoding[encoding == "latin3"] <-  "iso-8859-3"
    encoding[encoding == "latin4"] <-  "iso-8859-4"
    encoding[encoding == "cyrillic"] <-"iso-8859-5"
    encoding[encoding == "arabic"] <-  "iso-8859-6"
    encoding[encoding == "greek"] <-   "iso-8859-7"
    encoding[encoding == "hebrew"] <-  "iso-8859-8"
    encoding[encoding == "latin5"] <-  "iso-8859-9"
    encoding[encoding == "latin6"] <-  "iso-8859-10"
    encoding[encoding == "latin8"] <-  "iso-8859-14"
    encoding[encoding == "latin-9"] <- "iso-8859-15"
    encoding[encoding == "latin10"] <- "iso-8859-16"
    encoding[encoding == "utf8"] <-    "utf-8"
    encoding
}

RdTags <- function(Rd) {
    res <- sapply(Rd, function(element) attr(element, "Rd_tag"))
    if (!length(res)) res <- character(0)
    res
}

isBlankRd <- function(x)
    length(grep("^[[:blank:]]*\n?$", x, perl = TRUE)) == length(x) # newline optional

isBlankLineRd <- function(x) {
    attr(x, "srcref")[2L] == 1 &&
    length(grep("^[[:blank:]]*\n", x, perl = TRUE)) == length(x)   # newline required
}

stopRd <- function(block, ...)
{
    srcref <- attr(block, "srcref")
    if (is.null(srcref)) stop(..., call. = FALSE, domain = NA)
    else {
    	loc <- paste(attr(srcref, "srcfile")$filename,
                     ":", srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
    	stop(call.=FALSE, loc, ": ", ..., domain = NA)
    }
}

warnRd <- function(block, Rdfile, ...)
{
    srcref <- attr(block, "srcref")
    if (is.null(srcref))
        warning("file '", Rdfile, "': ", ...,
                call. = FALSE, domain = NA, immediate. = TRUE)
    else {
    	loc <- paste(attr(srcref, "srcfile")$filename,
                     ":", srcref[1L], sep = "")
    	if (srcref[1L] != srcref[3L]) loc <- paste(loc, "-", srcref[3L], sep="")
    	warning(loc, ": ", ..., call. = FALSE, domain = NA, immediate. = TRUE)
    }
}

RweaveRdDefaults <- list(
    width=6,
    height=6,
    eval=TRUE,
    fig=FALSE,
    echo=FALSE,
    keep.source=TRUE,
    results="text",
    strip.white="true",
    stage="install")
    
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

processRdChunk <- function(code, stage, options, env) {
    if (is.null(opts <- attr(code, "Rd_option"))) opts <- ""
    srcref <- attr(code, "srcref")
    options <- utils:::SweaveParseOptions(opts, options, RweaveRdOptions)
    if (stage == options$stage) {
        #  The code below is very similar to RWeaveLatexRuncode, but simplified
        
        # Results as a character vector for now; convert to list later
        res <- character(0)  
        code <- code[RdTags(code) != "COMMENT"]
	chunkexps <- try(parse(text=code), silent=TRUE)
	if (inherits(chunkexps, "try-error")) stopRd(code, chunkexps)

	if(length(chunkexps) == 0L)
	    return(tagged(code, "LIST"))

	srclines <- attr(code, "srclines")
	srcline <- srclines[1L]

	srcrefs <- attr(chunkexps, "srcref")
	lastshown <- 0L
	thisline <- 0
	err <- NULL
	for(nce in 1L:length(chunkexps))
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
		while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
		    dce <- dce[-1L]
		    leading <- leading - 1L
		}
	    } else {
		dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
		leading <- 1L
	    }
	    if(options$echo && length(dce)) {
		res <- c(res,"\n", paste(getOption("prompt"), dce[1L:leading], sep="", collapse="\n"))
		if (length(dce) > leading)
		    res <- c(res, "\n", paste(getOption("continue"), dce[-(1L:leading)], sep="", collapse="\n"))
		thisline <- thisline + length(dce)
	    }

	    tmpcon <- file()
	    sink(file=tmpcon)
	    if(options$eval) err <- evalWithOpt(ce, options, env)
	    res <- c(res, "\n") # make sure final line is complete
	    sink()
	    output <- readLines(tmpcon)
	    close(tmpcon)
	    ## delete empty output
	    if(length(output) == 1L & output[1L] == "") output <- NULL

	    if (inherits(err, "try-error")) stopRd(code, err)

	    if(length(output) & (options$results != "hide")){

		output <- paste(output,collapse="\n")
		if(options$strip.white %in% c("all", "true")){
		    output <- sub("^[[:space:]]*\n", "", output)
		    output <- sub("\n[[:space:]]*$", "", output)
		    if(options$strip.white=="all")
		      output <- sub("\n[[:space:]]*\n", "\n", output)
		}
		res <- c(res, output)
		remove(output)
	    }
	}
	if (options$results == "rd") {
	    res <- err   # The last value of the chunk
	    tmpcon <- file()
	    writeLines(res, tmpcon)
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
		target <- gsub("[[:blank:][:cntrl:]]*", "", target)
		if ((target %in% defines) == (tag == "#ifdef")) 
		    block <- tagged(block[[2L]], "#expanded")
		else 
		    block <- structure(tagged(paste(tag, target, "not active"), "COMMENT"),
		    		       srcref=attr(block, "srcref"))   		       
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

processRdSexprs <- function(block, stage, options=RweaveRdDefaults, env=new.env(parent=globalenv()))
{
    recurse <- function(block) {
        if (is.list(block)) {
            if (!is.null(tag <- attr(block, "Rd_tag"))) {
        	if (tag == "\\Sexpr")   
            	    block <- processRdChunk(block, stage, options, env)
            	else if (tag == "\\RdOpts") 
    	    	    options <<- utils:::SweaveParseOptions(block, options, RweaveRdOptions)
    	    }
	    for (i in seq_along(block))
		block[[i]] <- recurse(block[[i]])
	}    
	block
    }
    recurse(block)
}

prepare_Rd <- function(Rd, encoding="unknown", defines=NULL, stages=NULL, options=RweaveRdDefaults) {
    Rdfile <- "not known"
    if (is.character(Rd)) {
        Rdfile <- Rd
        ## do it this way to get info in internal warnings
        Rd <- eval(substitute(parse_Rd(f, encoding = enc),
                              list(f = Rd, enc = encoding)))
    } else if(inherits(Rd, "connection")) {
        Rdfile <- summary(Rd)
        Rd <- parse_Rd(Rd, encoding = encoding)
    }
    if ("build" %in% stages)
    	Rd <- processRdSexprs(Rd, "build", options)
    if (!is.null(defines))
    	Rd <- processRdIfdefs(Rd, defines)
    for (stage in c("install", "render"))
    	if (stage %in% stages)
    	    Rd <- processRdSexprs(Rd, stage, options)
    structure(Rd, Rdfile=Rdfile)
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

## FIXME: better to really use XHTML
Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, CHM = FALSE, 
             stages = "render", outputEncoding = "", ...)
{
    of <- function(...) writeLines(paste(...), con, sep = '')
    of0 <- function(...) writeLines(paste(..., sep=""), con, sep ="")
    of1 <- function(text) writeLines(text, con, sep = "")

    pendingClose <- pendingOpen <- character(0)  # Used for infix methods

    nlinks <- 0L
### These correspond to HTML wrappers
    HTMLTags <- c("\\bold"="B",
    	          "\\cite"="CITE",
                  "\\code"="code",
                  "\\command"="CODE",
                  "\\dfn"="DFN",
                  "\\emph"="EM",
                  "\\kbd"="KBD",
                  "\\preformatted"="pre",
#                  "\\special"="PRE",
                  "\\strong"="STRONG",
                  "\\var"="VAR",
                  "\\verb"="PRE")
    # These have simple substitutions
    HTMLEscapes <- c("\\R"='<font face="Courier New,Courier" color="#666666"><b>R</b></font>',
    		     "\\cr"="<br>",
    		     "\\dots"="...",
    		     "\\ldots"="...")
    ## These correspond to idiosyncratic wrappers
    HTMLLeft <- c("\\acronym"='<acronym><span class="acronym">',
    		  "\\donttest"="",
    		  "\\env"='<span class="env">',
                  "\\file"='&lsquo;<span class="file">',
                  "\\option"='<span class="option">',
                  "\\pkg"='<span class="pkg">',
                  "\\samp"='<span class="samp">',
                  "\\sQuote"="&lsquo;",
                  "\\dQuote"="&ldquo;")
    HTMLRight <- c("\\acronym"='</span></acronym>',
    		   "\\donttest"="",
    		   "\\env"="</span>",
                   "\\file"='</span>&rsquo;',
                   "\\option"="</span>",
                   "\\pkg"="</span>",
                   "\\samp"="</span>",
                   "\\sQuote"="&rsquo;",
                   "\\dQuote"="&rdquo;")

    trim <- function(x) {
        x <- sub("^\\s*", "", x, perl = TRUE)
        sub("\\s*$", "", x, perl = TRUE)
    }

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2L] # FIXME: what if no srcref?, start col
	if (isBlankLineRd(x)) "</p>\n<p>\n"
	else if(start == 1) gsub("^\\s+", "", x, perl = TRUE)
        else x
    }

    ## FIXME: what other substitutions do we need?
    ## possibly quotes if the parser had left alone -- NA.Rd
    htmlify <- function(x) {
	x <- gsub("&", "&amp;", x, fixed = TRUE)
	x <- gsub("---", "&mdash;", x, fixed = TRUE)
	x <- gsub("--", "&ndash;", x, fixed = TRUE)
	x <- gsub("``", "&ldquo;", x, fixed = TRUE)
	x <- gsub("''", "&rdquo;", x, fixed = TRUE)
        x <- gsub("`([^']+)'", "&lsquo;\\1&rsquo;", x, perl=TRUE)
	x <- gsub("`", "'", x, fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	gsub(">", "&gt;", x, fixed = TRUE)
    }
    vhtmlify <- function(x) { # code version
	x <- gsub("&", "&amp;", x, fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	gsub(">", "&gt;", x, fixed = TRUE)
    }

    HTMLeqn <- function(x)
    {
        x <- htmlify(x)
        ## historical escapes for math
        x <- gsub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon)", "&\\1;", x)
        x <- gsub("\\\\left\\(", "(", x)
        x <- gsub("\\\\right", "\\)", x)
        x <- gsub("\\le", "&lt;=", x)
        x <- gsub("\\ge", "&gt;=", x)
        x
    }

    writeWrapped <- function(tag, block) {
    	of0("<", HTMLTags[tag], ">")
    	writeContent(block, tag)
    	of0("</",  HTMLTags[tag], ">")
    }

    topic2Path <- function(alias, package, type, lib.loc=NULL)
    {
        paths <- sapply(.find.package(package, lib.loc, verbose = FALSE),
                        function(p) index.search(alias, p, "AnIndex", type))
        paths[paths != ""]
    }

    checkInfixMethod <- function(blocks) {
    	# Is this a method which needs special formatting?
    	if ( length(blocks) == 1 && RdTags(blocks) == "TEXT" &&
    	     blocks[[1]] %in% c("[","[[","$") ) {
    	    pendingOpen <<- blocks[[1]]
    	    return(TRUE)
    	} else return(FALSE)
    }
    	
    ## FIXME: depends on CHM or not
    ## Cross-packages CHM links are of the form
    ## <a onclick="findlink('stats', 'weighted.mean.html')" style="text-decoration: underline; color: blue; cursor: hand">weighted.mean</a>
    writeLink <- function(tag, block) {
	parts <- get_link(block, tag)

    	if (is.null(parts$targetfile)) {
            topic <- parts$dest
            htmlfile <- paste(topic, ".html", sep = "") ## pro tem
            ## should be <a href=\"..\/..\/..\/doc\/html\/search\/SearchObject.html?$argkey\">$arg<\/a>/s; where $argkey is the topic and $arg the HTMLiied version.
            ## htmltfile <- paste("../../../doc/html/search\/SearchObject.html?", parts%dest, sep= "")
            if (!is.null(Links)) {
                tmp <- Links[topic]
                if (is.na(tmp)) {
                    warnRd(block, Rdfile, "missing link ", sQuote(topic))
                } else htmlfile <- tmp
            }
    	} else if (is.null(parts$pkg) || parts$pkg == package)
    	    htmlfile <- paste(parts$targetfile, ".html", sep="")
    	else
    	    htmlfile <- paste("../../", parts$pkg, "/html/",
                              parts$targetfile, ".html", sep="")

    	of0('<a href="', htmlfile, '">')
    	writeContent(block, tag)
    	of1('</a>')
        nlinks <<- nlinks + 1L
    }

    writeComment <- function(txt) {
       	txt <- sub("^%", "", txt, fixed = TRUE)
       	txt <- sub("\n", "", txt)
       	txt <- gsub("--", "- - ", txt, fixed = TRUE)
       	txt <- gsub(">", "&gt;", txt, fixed = TRUE)
	of("<!-- ", txt, " -->\n")
    }

    writeLR <- function(block, tag) {
        of1(HTMLLeft[tag])
        writeContent(block, tag)
        of1(HTMLRight[tag])
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1('## Not run: ') # had trailing space before: FIXME remove
            writeContent(block, tag)
            ## FIXME only needs a \n here if not at left margin
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = of1(vhtmlify(block)),
               TEXT = of1(addParaBreaks(htmlify(block), blocktag)),
               COMMENT = {},
               LIST =,
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize"=writeContent(block, tag),
               "\\bold"=,
               "\\cite"=,
               "\\code"=,
               "\\command"=,
               "\\dfn"=,
               "\\emph"=,
               "\\kbd"=,
               "\\preformatted"=,
               "\\strong"=,
               "\\var" =,
               "\\verb"= writeWrapped(tag, block),
               "\\special"= writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               "\\email" = of0('<a href="mailto:', block[[1L]], '">', htmlify(block[[1L]]), '</a>'),
               ## FIXME: encode, not htmlify
               "\\url" = of0('<a href="', block[[1L]], '">', block[[1L]], '</a>'),
               "\\Sexpr"= { of1("\\Sexpr")  # This is only here if processing didn't get it...
	                    option <- attr(block, "Rd_option")
	                    if (!is.null(option)) of0("[", option, "]")
	                    of1("{")
	                    writeContent(block, tag)
	                    of1("}")
	                  },
               "\\cr" =,
               "\\dots" =,
               "\\ldots" =,
               "\\R" = of1(HTMLEscapes[tag]),
               "\\acronym" =,
               "\\donttest" =,
               "\\env" =,
               "\\file" =,
               "\\option" =,
               "\\pkg" =,
               "\\samp" =,
               "\\sQuote" =,
               "\\dQuote" =  writeLR(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = writeContent(block[[1L]], tag),
               "\\eqn" = {
                   of1("<i>")
                   block <- block[[length(block)]];
                   ## FIXME: space stripping needed: see Special.html
                   writeContent(block, tag)
                   of1("</i>")
               },
               "\\deqn" = {
                   of1('</p><p align="center"><i>')
                   block <- block[[length(block)]];
                   writeContent(block, tag)
                   of1('</i></p><p>')
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" = {
                   ## FIXME: special methods for operators
                   class <- as.character(block[[2L]])
                   if (class == "default")
                       of1('## Default S3 method:\n')
                   else {
                       of1("## S3 method for class '")
                       writeContent(block[[2L]], tag)
                       of1("':\n")
                   }
                   if (!checkInfixMethod(block[[1L]]))
                       writeContent(block[[1L]], tag)
               },
               "\\S4method" = {
                   of1("## S4 method for signature '")
                   writeContent(block[[2L]], tag)
                   of1("':\n")
                   if (!checkInfixMethod(block[[1L]]))
                       writeContent(block[[1L]], tag)
               },
               "\\tabular" = writeTabular(block),
               stopRd(block, "Tag ", tag, " not recognized.")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "")[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, "Unrecognized \\tabular format: ", table[[1L]][[1L]])
        format <- c(l="left", c="center", r="right")[format]

        tags <- RdTags(content)

        of1('\n</p>\n<table summary="Rd table">\n')
        newrow <- TRUE
        newcol <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	of1("<tr>\n ")
            	newrow <- FALSE
            	col <- 0
            	newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format))
                    stopRd(table, "Only ", length(format), " columns allowed in this table.")
            	of0('<td align="', format[col], '">')
            	newcol <- FALSE
            }
            switch(tags[i],
            "\\tab" = {
            	of1('</td>')
            	newcol <- TRUE
            },
            "\\cr" = {
            	if (!newcol) of1('</td>')
            	of1('\n</tr>\n')
            	newrow <- TRUE
            },
            writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        if (!newcol) of1('</td>')
        if (!newrow) of1('\n</tr>\n')
        of1('\n</table><p>\n')
    }

    writeContent <- function(blocks, blocktag) {
        inlist <- FALSE
        itemskip <- FALSE

	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            if (length(pendingOpen)) { # Handle $, [ or [[ methods
            	if (tag == "RCODE" &&
            	    length(grep("^\\(", block))) {
            	    block <- sub("^\\(", "", block)
            	    arg1 <- sub("[,)[:space:]].*", "", block)
            	    block <- sub(paste(arg1, "[[:space:]]*,[[:space:]]*", sep=""), "", block)
            	    of0(arg1, pendingOpen)
            	    if (pendingOpen == "$")
            	    	pendingClose <<- ""
            	    else
            	    	pendingClose <<- chartr("[", "]", pendingOpen)
            	} else of0("`", pendingOpen, "`")
            	pendingOpen <<- character(0)
            }
            if (length(pendingClose) && tag == "RCODE" 
                && length(grep("\\)", block))) { # Finish it off...
            	of0(sub("\\).*", "", block), pendingClose)
            	block <- sub("[^)]*\\)", "", block)
            	pendingClose <<- character(0)
            }
            switch(tag,
            "\\item" = {
    	    	if (!inlist) {
    	    	    switch(blocktag,
                           "\\value" =  of1('<table summary="R valueblock">\n'),
                           "\\arguments" = of1('<table summary="R argblock">\n'),
                           "\\itemize" = of1("<ul>\n"),
                           "\\enumerate" = of1("<ol>\n"),
                           "\\describe" = of1("<dl>\n"))
    	    	    inlist <- TRUE
    		} else {
    		    if (blocktag %in% c("\\itemize", "\\enumerate")) {
    		    	of1("</li>\n")
                        ## We have \item ..., so need to skip the space.
                        itemskip <- TRUE
                    }
    		}
    		switch(blocktag,
   		"\\value"=,
     		"\\arguments"={
    		    of1('<tr valign="top"><td><code>')
    		    writeContent(block[[1L]], tag)
    		    of1('</code></td>\n<td>\n')
    		    writeContent(block[[2L]], tag)
    		    of1('</td></tr>')
    		},
    		"\\describe"= {
    		    of1("<dt>")
    		    writeContent(block[[1L]], tag)
    		    of1("</dt><dd>")
    		    writeContent(block[[2L]], tag)
    		    of1("</dd>")
    		},
    		"\\enumerate" =,
    		"\\itemize"= of1("<li>"))
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\arguments", "\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    	    	    switch(blocktag,
     	    	    "\\value" = of1("</table>\n"),
    	    	    "\\describe" = of1("</dl>\n"))
    		    inlist <- FALSE
    		}
                if (itemskip) {
                    ## The next item must be TEXT, and start with a space.
                    itemskip <- FALSE
                    if (tag == "TEXT") {
                        txt <- gsub("^ ", "", as.character(block), perl = TRUE)
                        of1(txt)
                    } else writeBlock(block, tag, blocktag) # should not happen
                } else writeBlock(block, tag, blocktag) 
    	    })
	}
	if (inlist)
	    switch(blocktag,
		"\\value"=,
		"\\arguments"=of1("</table>\n"),
		"\\itemize"=of1("</ul>\n"),
		"\\enumerate"=of1("</ol>\n"),
		# "\\value"=,
		"\\describe"=of1("</dl>\n"))
    }

    writeSection <- function(section, tag) {
        if (tag == "\\alias") return() ## \alias only used on CHM header
    	of1("\n\n<h3>")
    	if (tag == "\\section") {
    	    title <- section[[1L]]
    	    section <- section[[2L]]
            ## FIXME: this needs trimming of whitespace
    	    writeContent(title, tag)
    	} else
    	    of1(sectionTitles[tag])
    	if (tag %in% c("\\examples", "\\synopsis", "\\usage"))
    	    para <- "pre" else para <- "p"
        of1("</h3>\n")
        ## \arguments is a single table, not a para
        if (tag == "\\arguments") para <- ""
    	if(nzchar(para)) of0("\n<", para, ">")
    	if (length(section)) {
	    ## There may be an initial \n, so remove that
	    s1 <- section[[1L]][1L]
	    if (RdTags(s1) == "TEXT" && s1 == "\n") section <- section[-1]
	    writeContent(section, tag)
	}
    	if(nzchar(para)) of0("</", para, ">\n")
    }

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
            if (outputEncoding != "") {
            	warning('outputEncoding changed to "" on stdout')
            	outputEncoding <- ""
            }
        } else {
	    con <- file(out, "w", encoding=outputEncoding)
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }
    
    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...) 
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1L && as.numeric(version[[1L]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], "Only one \\Rdversion declaration is allowed")

    ## Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	stopRd(Rd[sections == "TEXT"][[bad[1L]]], "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding",
                            "\\keyword", "\\Rdversion", "\\RdOpts")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1L]]], "Section ", sections[which(bad)[1L]], " unrecognized.")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, "Sections \\title, and \\name must exist and be unique in Rd files.")

    title <- Rd[[1L]]
    name <- Rd[[2L]]
    tags <- RdTags(name)
    if (length(tags) > 1L) stopRd(name, "\\name must only contain simple text.")

    name <- htmlify(name[[1L]])

    if(CHM)
        of0('<html><head><title>')
    else
        of0('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
            '<html><head><title>R: ')
    ## special for now, as we need to remove leading and trailing spaces
    title <- trim(as.character(title))
    title <- htmlify(paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                           collapse=" "))
    of1(title)
    of0('</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset=',
        mime_canonical_encoding(outputEncoding),
        '">\n')
    if(CHM) {
        of0('<link rel="stylesheet" type="text/css" href="Rchm.css">\n',
            '</head><body>\n\n')
        of0('<table width="100%"><tr><td>', name, '(', package, ')',
            '</td><td align="right">R Documentation</td></tr></table>\n')
        of1('<object type="application/x-oleobject" classid="clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e">\n')
        aliases <- sapply(Rd[RdTags(Rd) == "\\alias"], as.character)
        ## FIXME: (un)escape as needed
        of0('<param name="keyword" value="R:   ', aliases, '">\n')
        ## space is deliberate, used in sorting indices
        of0('<param name="keyword" value=" ', title, '">\n')
        of1('</object>\n\n\n')
    } else
        of0('<link rel="stylesheet" type="text/css" href="../../R.css">\n',
            '</head><body>\n\n',
            '<table width="100%" summary="page for ', name, ' {', package,
            '}"><tr><td>',name,' {', package,
            '}</td><td align="right">R Documentation</td></tr></table>\n\n')

    of0("<h2>", title,'</h2>\n')

    for (i in seq_along(sections)[-(1:2)])
    	writeSection(Rd[[i]], sections[i])

    if (CHM) {
        if (nlinks > 0)
            writeLines(paste('',
                             '<script Language="JScript">',
                             'function findlink(pkg, fn) {',
                             'var Y, link;',
                             'Y = location.href.lastIndexOf("\\\\") + 1;',
                             'link = location.href.substring(0, Y);',
                             'link = link + "../../" + pkg + "/chtml/" + pkg + ".chm::/" + fn;',
                             'location.href = link;', '}', '</script>',
                             sep = '\n'), con)
    }
    if (package != "") {
    	version <- paste('Package <em>', package, 
    	                 '</em> version ', packageDescription(package, fields="Version"), ' ', sep='')
    } else version <- ''
    of0('\n',
        '<hr><div align="center">[', version, '<a href="00Index.html">Index</a>]</div>\n',
        '</body></html>\n')
    return(out)
}

checkRd <- function(Rd, defines=.Platform$OS.type, stages="render", 
                    unknownOK = TRUE, listOK = TRUE, ...)
{
    checkWrapped <- function(tag, block) {
    	checkContent(block, tag)
    }

    checkLink <- function(tag, block) { # FIXME This doesn't handle aliases, and
                                        # doesn't cover all variations
    	option <- attr(block, "Rd_option")
    	if(!is.null(option)) checkContent(option, tag)
    	checkContent(block, tag)
    }

    checkBlock <- function(block, tag, blocktag) {
	switch(tag,
	UNKNOWN = if (!unknownOK) stopRd(block, "Unrecognized macro ", block[[1L]]) else warnRd(block, Rdfile, "Unrecognized macro ", block[[1L]]),
	VERB = ,
	RCODE = ,
	TEXT = ,
	COMMENT = {},
	LIST = if (length(block)) {
		deparse <- sQuote(paste(as.character.Rd(block), collapse=""))
		if(!listOK)
               	    stopRd(block, "Unnecessary braces at ", deparse)
               else warnRd(block, Rdfile, "Unnecessary braces at ", deparse)
              },
	"\\describe"=,
	"\\enumerate"=,
	"\\itemize"=,
	"\\bold"=,
	"\\cite"=,
	"\\code"=,
	"\\command"=,
	"\\dfn"=,
	"\\emph"=,
	"\\kbd"=,
	"\\preformatted"=,
	"\\Sexpr"=,
	"\\special"=,
	"\\strong"=,
	"\\var" =,
	"\\verb"= checkContent(tag, block),
	"\\linkS4class" =,
	"\\link" = checkLink(tag, block),
	"\\email" =,
	"\\url" =,
	"\\cr" =,
	"\\dots" =,
	"\\ldots" =,
	"\\R" = {},
	"\\acronym" =,
	"\\dontrun" =,
	"\\donttest" =,
	"\\env" =,
	"\\file" =,
	"\\option" =,
	"\\pkg" =,
	"\\samp" =,
	"\\sQuote" =,
	"\\dQuote" = checkContent(block, tag),
	"\\method" =,
	"\\S3method" =,
	"\\S4method" =,
	"\\enc" = {
	    checkContent(block[[1L]], tag)
	    checkContent(block[[2L]], tag)
	},
	"\\eqn" =,
	"\\deqn" = {
	    checkContent(block[[1L]])
	    if (length(block) > 1L) checkContent(block[[2L]])
	},
	"\\dontshow" =,
	"\\testonly" = checkContent(block, tag),
	"\\tabular" = checkTabular(block),
        stopRd(block, "Tag ", tag, " not recognized."))
    }

    checkTabular <- function(table) {
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "")[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, "Unrecognized \\tabular format: ", table[[1L]][[1L]])
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
                    stopRd(table, "Only ", length(format), " columns allowed in this table.")
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
    		"\\arguments"={
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

    checkSection <- function(section, tag) {
    	if (tag == "\\section") {
    	    title <- section[[1L]]
    	    section <- section[[2L]]
    	    checkContent(title, tag)
    	}
    	checkContent(section, tag)
    }

    checkUnique <- function(tag) {
    	which <- which(sections == tag)
    	if (length(which) < 1L)
    	    stopRd(Rd, "Must have a ", tag)
    	else if (length(which) > 1L)
    	    stopRd(Rd[[which[2L]]], "Only one ", tag, " is allowed")
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1L && as.numeric(version[[1L]]) < 2)
    	warning("checkRd is designed for Rd version 2 or higher.")
    else if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], "Only one \\Rdversion declaration is allowed")

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1L)
    	    stopRd(Rd[[enc[2L]]], "Only one \\encoding declaration is allowed")
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, "Encoding must be plain text")
    }

    dt <- which(sections == "\\docType")
    docTypes <- character(length(dt))   
    if (length(dt)) {
        for (i in dt) {
            docType <- Rd[[i]]
            if(!identical(RdTags(docType), "TEXT"))
        	stopRd(docType, "docType must be plain text")
            docTypes[i] <- docType[[1L]]
         }
    }

    checkUnique("\\title")
    checkUnique("\\name")
    if(!identical("package", docTypes))
        checkUnique("\\description")

    name <- Rd[[which(sections == "\\name")]]
    tags <- RdTags(name)
    if (length(tags) > 1L) stopRd(name, "\\name must only contain simple text.")

    for (i in seq_along(sections))
    	checkSection(Rd[[i]], sections[i])

    TRUE
}

Rd2ex <-
    function(Rd, out="", defines=.Platform$OS.type, stages="render", outputEncoding="", ...)
{
    of0 <- function(...) writeLines(paste(..., sep=""), con, sep ="")
    of1 <- function(text) writeLines(text, con, sep = "")

    wr <- function(x) {
        x <- remap(x)
        paste("###", strwrap(x, 73, indent=1, exdent=3), sep="", collapse="\n")
    }
    remap <- function(x) {
        ## \link, \var are untouched in comments: e.g. is.R
        x <- gsub("\\\\(link|var)\\{([^}]+)\\}", "\\2", x, perl = TRUE)
        ## FIXME not valid in perl: use lookbehind instead.
        x <- gsub("(^|[^\\])\\\\([%{])", "\\1\\2", x)
        x <- gsub("\\\\(l|)dots", "...", x, perl = TRUE)
        ## Want to leave file bytes unchanged
        Encoding(x) <- "unknown"
        x
    }

    render <- function(x, prefix = "")
    {
        tag <- attr(x, "Rd_tag")
        if(tag %in% c("\\dontshow", "\\testonly")) {
            ## There are fancy rules here if not followed by \n
            of1("## Don't show: ")
            if (!length(grep("^\n", x[[1L]], perl = TRUE)))
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            if (!length(grep("\n$", x[[length(x)]], perl = TRUE)))
                writeLines("", con)
            of1("## End Don't show")
        } else if (tag  == "\\dontrun") {
            ## Special case for one line.
            if (length(x) == 1L) {
                of1("## Not run: ")
                render(x[[1L]], prefix)
            } else {
                of1("## Not run: ")
                if (!length(grep("^\n", x[[1L]], perl = TRUE))) {
                    writeLines("", con)
                    render(x[[1L]], paste("##D", prefix))
                } else render(x[[1L]], prefix)
                for(i in 2:length(x)) render(x[[i]], paste("##D", prefix))
                if (!length(grep("\n$", x[[length(x)]], perl = TRUE)))
                    writeLines("", con)
                of1("## End(Not run)")
            }
        } else if (tag  == "\\donttest") {
            of1("## No test: ")
            if (!length(grep("^\n", x[[1L]], perl = TRUE)))
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            if (!length(grep("\n$", x[[length(x)]], perl = TRUE)))
                writeLines("", con)
            of1("## End(No test)")
        } else if (tag == "COMMENT") {
            ## % can escape a whole line (e.g. beavers.Rd) or
            ## be trailing when we want a NL
            ## This is not right (leaading spaces?) but it may do
            if(attr(x, "srcref")[2L] > 1) writeLines("", con)
        } else if (tag %in% c("\\dots", "\\ldots")) {
            of1("...")
        } else {
            txt <- unlist(x)
            of0(prefix, remap(txt))
        }
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    where <- which(sections == "\\examples")
    if(length(where)) {
	if (is.character(out)) {
	    if(out == "") {
		con <- stdout()
		if (outputEncoding != "") {
		    warning('outputEncoding changed to "" on stdout')
		    outputEncoding <- ""
		}
	    } else {
		con <- file(out, "w", encoding = outputEncoding)
		on.exit(close(con))
	    }
        } else {
            con <- out
            out <- summary(con)$description
        }

        if(length(which) > 1L)
            warning("more than one \\examples section, using the first")
        if(any(f <- sections == "\\encoding")) {
            encoding <- unlist(Rd[[which(f)]])[1L]
            of0("### Encoding: ", encoding, "\n\n")
        }
        nameblk <- sections == "\\name"
        if (any(nameblk)) {
            ## perl wrapped here, but it seems unnecessary
            name <- as.character(Rd[[ which(nameblk)[1L] ]])
            of0("### Name: ", name, "\n")
        }
        titleblk <- sections == "\\title"
        if (any(titleblk)) {
            title <- as.character(Rd[[ which(titleblk)[1L] ]])
            ## remove empty lines, leading whitespace
            title <- paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                           collapse=" ")
            ## FIXME: more?
            title <- gsub("(---|--)", "-", title, perl =  TRUE)
        } else title <- "No title found"
        of0(wr(paste("Title: ", title, sep='')), "\n")
        aliasblks <- sections == "\\alias"
        if (any(aliasblks)) {
            aliases <- unlist(Rd[aliasblks])
            sp <- grep(" ", aliases, fixed = TRUE)
            aliases[sp] <- paste("'", aliases[sp], "'", sep = "")
            of0(wr(paste("Aliases: ", paste(aliases, collapse=" "), sep="")), "\n")
        }
        keyblks <- sections == "\\keyword"
        if (any(keyblks)) {
            keys <- unlist(Rd[keyblks])
            keys <- gsub("^\\s+", "", keys, perl = TRUE)
            of0(wr(paste("Keywords: ", paste(keys, collapse=" "), sep="")), "\n")
        }
        writeLines(c("", "### ** Examples"), con)
        ex <- Rd[[ where[1L] ]]
        for (i in seq_along(ex)) render(ex[[i]])
        of1("\n\n\n")
    }
    out
}

findHTMLlinks <- function(pkgDir = "", lib.loc = NULL)
{
    ## The priority order is
    ## This package
    ## The standard packages
    ## along lib.loc.

    get_links <- function(p) {
        if(file.exists(f <- file.path(p, "Meta", "Rd.rds"))) {
            info <- .readRDS(f)
            topics <- info$Aliases
            lens <- sapply(topics, length)
            structure(file.path("../..", basename(p), "html",
                                rep.int(info$File, lens)),
                      names = unlist(topics))
        } else character()
    }
    
    if(is.null(lib.loc)) lib.loc <- .libPaths()

    Links <- list()
    for(lib in rev(lib.loc))
        Links <- c(Links,
                   lapply(rev(dir(lib, full.names = TRUE)), get_links))
    Links <- c(Links,
               lapply(file.path(.Library,
                                c("base", "utils", "graphics",
                                  "grDevices", "stats", "datasets",
                                  "methods")),
                      get_links))
    if(nzchar(pkgDir))
        Links <- c(Links, list(get_links(pkgDir)))
    Links <- unlist(Links)
    
    ## now latest names are newest, so
    Links <- rev(Links)
    Links <- Links[!duplicated(names(Links))]
    gsub("[Rr]d$", "html", Links)
}

