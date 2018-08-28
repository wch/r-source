#  File src/library/tools/R/Rd2txt.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

## This stops on
##  unrecognized tag
##  \\tabular format must be simple text
##  too many columns for format
##  invalid markup in \[S3]method
##  "Tag ", tag, " not expected in code block"

tabExpand <- function(x) {
    srcref <- attr(x, "srcref")
    if (is.null(srcref)) start <- 0L
    else start <- srcref[5L] - 1L
    .Call(C_doTabExpand, x, start)
}

Rd2txt_options <- local({
    opts <- list(width = 80L,
                 minIndent = 10L,
    	         extraIndent = 4L,
    	         sectionIndent = 5L,
    	         sectionExtra = 2L,
    	         itemBullet = "* ",
    	         enumFormat = function(n) sprintf("%d. ", n),
    	         showURLs = FALSE,
                 code_quote = TRUE,
                 underline_titles = TRUE)
    function(...) {
        args <- list(...)
        if (!length(args))
            return(opts)
        else {
            if (is.list(args[[1L]])) args <- args[[1L]]
            result <- opts[names(args)]
            opts[names(args)] <<- args
            invisible(result)
        }
    }
})

transformMethod <- function(i, blocks, Rdfile) {
    editblock <- function(block, newtext)
    	list(structure(newtext, Rd_tag = attr(block, "Rd_tag"),
    	                   srcref = attr(block, "srcref")))

    # Most of the internal functions below are more like macros
    # than functions; they mess around with these variables:

    chars <- NULL
    char <- NULL
    j <- NULL

    findOpen <- function(i) {
    	j <- i
    	char <- NULL
    	while (j < length(blocks)) {
    	    j <- j + 1L
    	    tag <- attr(blocks[[j]], "Rd_tag")
    	    if (tag == "RCODE") {

    	        # FIXME:  This search and the ones below will be fooled
    	        # by "#" comments

    	    	chars <- strsplit(blocks[[j]], "")[[1]]
    		parens <- cumsum( (chars == "(") - (chars == ")") )
    		if (any(parens > 0)) {
		    char <- which.max(parens > 0)
    	   	    break
    	   	}
    	    }
    	}
    	if (is.null(char))
    	    stopRd(block, Rdfile, sprintf("no parenthesis following %s", blocktag))
    	chars <<- chars
    	char <<- char
    	j <<- j
    }

    findComma <- function(i) {
	j <- i
	level <- 1L
	char <- NULL
	while (j < length(blocks)) {
	    j <- j + 1L
	    tag <- attr(blocks[[j]], "Rd_tag")
	    if (tag == "RCODE") {
		chars <- strsplit(blocks[[j]], "")[[1]]
		parens <- level + cumsum( (chars == "(") - (chars == ")") )
		if (any(parens == 1 & chars == ",")) {
		    char <- which.max(parens == 1 & chars == ",")
		    break
		}
		if (any(parens == 0))
		    break
		level <- parens[length(parens)]
	    }
	}
	if (is.null(char))
	    stopRd(block, Rdfile, sprintf("no comma in argument list following %s", blocktag))
        chars <<- chars
        char <<- char
        j <<- j
    }


    findClose <- function(i) {
        j <- i
    	level <- 1L
    	char <- NULL
    	while (j < length(blocks)) {
    	    j <- j + 1L
    	    tag <- attr(blocks[[j]], "Rd_tag")
    	    if (tag == "RCODE") {
    	    	chars <- strsplit(blocks[[j]], "")[[1]]
    	    	parens <- level + cumsum( (chars == "(") - (chars == ")") )
    	    	if (any(parens == 0)) {
    	    	    char <- which(parens == 0)[1]
    	    	    break
    	    	}
    	    	level <- parens[length(parens)]
    	    }
    	}
    	if (is.null(char))
    	    stopRd(block, Rdfile, sprintf("no closing parenthesis following %s", blocktag))
	chars <<- chars
        char <<- char
        j <<- j
    }

    rewriteBlocks <- function()
    	c(blocks[seq_len(j-1L)],
    	            editblock(blocks[[j]],
    	                      paste(chars[seq_len(char)], collapse="")),
    	            if (char < length(chars))
    	                editblock(blocks[[j]],
    	                          paste(chars[-seq_len(char)], collapse="")),
	            if (j < length(blocks)) blocks[-seq_len(j)])

    deleteBlanks <- function() {
	while (char < length(chars)) {
	    if (chars[char + 1] == " ") {
	    	char <- char + 1
	    	chars[char] <- ""
	    } else
	    	break
	}
	char <<- char
	chars <<- chars
    }

    block <- blocks[[i]]
    blocktag <- attr(block, "Rd_tag")
    srcref <- attr(block, "srcref")
    class <- block[[2L]] # or signature
    generic <- as.character(block[[1L]])
    default <- as.character(class) == "default"

    if(generic %in% c("[", "[[", "$")) {
	## need to assemble the call by matching parens in RCODE
	findOpen(i) # Sets chars, char and j
	chars[char] <- ""
	blocks <- c(blocks[seq_len(j-1L)],
	            editblock(blocks[[j]],
	                      paste(chars[seq_len(char)], collapse="")),
	            if (char < length(chars))
	                editblock(blocks[[j]],
	                          paste(chars[-seq_len(char)], collapse="")),
	            if (j < length(blocks)) blocks[-seq_len(j)])

	findComma(j) # Sets chars, char and j
	chars[char] <- generic
	# Delete blanks after the comma
	deleteBlanks()
	blocks <- rewriteBlocks()

	findClose(j)
	# Edit the closing paren
	chars[char] <- switch(generic,
		"[" = "]",
		"[[" = "]]",
		"$" = "")
	blocks[j] <- editblock(blocks[[j]],
	                         paste(chars, collapse=""))

	methodtype <- if (grepl("<-", blocks[[j]])) "replacement " else ""
    } else if(grepl(sprintf("^%s$",
			   paste(c("\\+", "\\-", "\\*",
				   "\\/", "\\^", "<=?",
				   ">=?", "!=?", "==",
				   "\\&", "\\|", "!",
				   "\\%[[:alnum:][:punct:]]*\\%"),
				 collapse = "|")),
		   generic)) {
        ## Binary operators and unary '!'.
	findOpen(i)

	if (generic != "!") {
            chars[char] <- ""
            blocks <- rewriteBlocks()
	    findComma(j)
	    chars[char] <- paste0(" ", generic, " ")
	    # Delete blanks after the comma
	    deleteBlanks()
	    blocks <- rewriteBlocks()
	} else {
            chars[char] <- "!"
	    blocks <- rewriteBlocks()
        }

	findClose(j)
	chars[char] <- ""
	blocks[j] <- editblock(blocks[[j]],
	                         paste(chars, collapse=""))

	methodtype <- ""
    } else {
        findOpen(i)
	chars[char] <- paste0(generic, "(")
	blocks <- rewriteBlocks()
	findClose(j)
	methodtype <- if (grepl("<-", blocks[[j]])) "replacement " else ""
    }

    if (blocktag == "\\S4method") {
        ## some signatures are very long.
        blocks <- if(nchar(class) > 50L) {
            cl <- paste0("'", as.character(class), "'")
            if(nchar(cl) > 70L) {
                cl <- strsplit(cl, ",")[[1L]]
                ncl <- length(cl)
                cl[-ncl] <- paste0(cl[-ncl], ",")
                cl[-1L] <- paste0("  ", cl[-1L])
            }
            cl <- paste("##", cl, collapse="\n")
            c( blocks[seq_len(i-1L)],
              list(structure(paste0("## S4 ", methodtype, "method for signature \n"),
                             Rd_tag="RCODE", srcref=srcref)),
              list(structure(cl, Rd_tag="TEXT", srcref=srcref)),
              list(structure("\n", Rd_tag="RCODE", srcref=srcref)),
              blocks[-seq_len(i)] )
        } else
            c( blocks[seq_len(i-1L)],
              list(structure(paste0("## S4 ", methodtype, "method for signature '"),
                             Rd_tag="RCODE", srcref=srcref)),
              class,
              list(structure("'\n", Rd_tag="RCODE", srcref=srcref)),
              blocks[-seq_len(i)] )
    } else if (default)
    	blocks <- c( blocks[seq_len(i-1)],
    		     list(structure(paste0("## Default S3 ", methodtype, "method:\n"),
    		     	       Rd_tag="RCODE", srcref=srcref)),
    		     blocks[-seq_len(i)] )
    else
    	blocks <- c( blocks[seq_len(i-1)],
		     list(structure(paste0("## S3 ", methodtype, "method for class '"),
			   Rd_tag="RCODE", srcref=srcref)),
		     class,
		     list(structure("'\n", Rd_tag="RCODE", srcref=srcref)),
		     blocks[-seq_len(i)] )
    blocks
}

Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type,
             stages = "render", outputEncoding = "",
             fragment = FALSE, options, ...)
{

    ## we need to keep track of where we are.
    buffer <- character()	# Buffer not yet written to con
    				# Newlines have been processed, each line in buffer is
    				# treated as a separate input line (but may be wrapped before output)
    linestart <- TRUE		# At start of line?
    indent <- 0L		# Default indent
    wrapping <- TRUE		# Do word wrap?
    keepFirstIndent <- FALSE	# Keep first line indent?
    dropBlank <- FALSE		# Drop initial blank lines?
    haveBlanks <- 0L		# How many blank lines have just been written?
    enumItem <- 0L		# Last enumeration item number
    inEqn <- FALSE		# Should we do edits needed in an eqn?
    sectionLevel <- 0		# How deeply nested within sections/subsections

    saveOpts <- Rd2txt_options()
    on.exit(Rd2txt_options(saveOpts))# Rd files may change these, so restore them
    				     # whether or not the caller set them.
    if (!missing(options)) Rd2txt_options(options)

## these attempt to mimic pre-2.10.0 layout
    WIDTH <- 0.9 * Rd2txt_options()$width
    HDR_WIDTH <- WIDTH - 2L

    startCapture <- function() {
    	save <- list(buffer=buffer, linestart=linestart, indent=indent,
                     wrapping=wrapping, keepFirstIndent=keepFirstIndent,
                     dropBlank=dropBlank, haveBlanks=haveBlanks,
                     enumItem=enumItem, inEqn=inEqn)
    	buffer <<- character()
    	linestart <<- TRUE
    	indent <<- 0L
    	wrapping <<- TRUE
    	keepFirstIndent <<- FALSE
    	dropBlank <<- FALSE
    	haveBlanks <<- 0L
    	enumItem <<- 0L
    	inEqn <<- FALSE
    	save
    }

    endCapture <- function(saved) {
    	result <- buffer
    	buffer <<- saved$buffer
    	linestart <<- saved$linestart
    	indent <<- saved$indent
    	wrapping <<- saved$wrapping
    	keepFirstIndent <<- saved$keepFirstIndent
    	dropBlank <<- saved$dropBlank
    	haveBlanks <<- saved$haveBlanks
    	enumItem <<- saved$enumItem
    	inEqn <<- saved$inEqn
    	result
    }

    ## for efficiency
    WriteLines <-
        if(outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...)
            writeLines(x, con, useBytes = TRUE, ...)
    } else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }

    ## Use display widths as used by cat not print.
    frmt <- function(x, justify="left", width = 0L) {
        justify <- match.arg(justify, c("left", "right", "centre", "none"))
        w <- sum(nchar(x, "width")) # copes with 0-length x
        if(w < width && justify != "none") {
            excess <- width - w
            left <- right <- 0L
            if(justify == "left") right <- excess
            else if(justify == "right")  left <- excess
            else if(justify == "centre") {
                left <- excess %/% 2
                right <- excess-left
            }
            paste(c(rep_len(" ", left), x, rep_len(" ", right)), collapse = "")
        } else x
    }

    wrap <- function(doWrap = TRUE)
	if (doWrap != wrapping) { flushBuffer(); wrapping <<- doWrap }

    putw <- function(...)  { wrap(TRUE); put(...) }

    putf <- function(...)  { wrap(FALSE); put(...) }

    put <- function(...) {
        txt <- paste0(..., collapse="")
        trail <- endsWith(txt, "\n")
        # Convert newlines
        txt <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
        if (dropBlank) {
            while(length(txt) && grepl("^[[:space:]]*$", txt[1L]))
            	txt <- txt[-1L]
            if (length(txt)) dropBlank <<- FALSE
        }
        if(!length(txt)) return()
        haveBlanks <<- 0

        if (linestart) buffer <<- c(buffer, txt)
        else if (length(buffer)) {
            buffer[length(buffer)] <<-
                paste0(buffer[length(buffer)], txt[1L])
            buffer <<- c(buffer, txt[-1L])
        }
        else buffer <<- txt
        linestart <<- trail
    }

    flushBuffer <- function() {
    	if (!length(buffer)) return()

    	if (wrapping) {
	    if (keepFirstIndent) {
		first <- nchar(psub1("[^ ].*", "", buffer[1L]))
		keepFirstIndent <<- FALSE
	    } else
		first <- indent

	    buffer <<- c(buffer, "")  # Add an extra blank sentinel
	    blankLines <- grep("^[[:space:]]*$", buffer)
	    result <- character()
	    start <- 1L
	    for (i in seq_along(blankLines)) {
		if (blankLines[i] > start) {
		    result <- c(result,
                                strwrap(paste(buffer[start:(blankLines[i]-1L)],
                                              collapse = " "),
                                        WIDTH, indent = first, exdent = indent))
		    first <- indent
                }
                result <- c(result, "")
		start <- blankLines[i]+1L
	    }
            ## we want to collapse multiple blank lines when wrapping
            ## and to remove the sentinel (which we need to do first or
            ## we will drop a single blank line)
            buffer <<- result[-length(result)]
            empty <- !nzchar(buffer)
            drop <- empty & c(FALSE, empty[-length(empty)])
            buffer <<- buffer[!drop]
	} else {  # Not wrapping
	    if (keepFirstIndent) {
		if (length(buffer) > 1L)
		    buffer[-1L] <<- paste0(strrep(" ", indent), buffer[-1L])
		keepFirstIndent <- FALSE
	    } else
		buffer <<- paste0(strrep(" ", indent), buffer)
	}

    	if (length(buffer)) WriteLines(buffer, con, outputEncoding)
    	buffer <<- character()
    	linestart <<- TRUE
    }

    encoding <- "unknown"

    li <- l10n_info()
    ## See the comment in ?Rd2txt as to why we do not attempt fancy quotes
    ## in Windows CJK locales -- and in any case they would need more work
    ## This covers the common single-byte locales and Thai (874)
    use_fancy_quotes <-
        (.Platform$OS.type == "windows" &&
         ((li$codepage >= 1250 && li$codepage <= 1258) || li$codepage == 874)) ||
        li[["UTF-8"]]

    if(!isFALSE(getOption("useFancyQuotes")) &&
       use_fancy_quotes) {
        ## On Windows, Unicode literals are translated to local code page
    	LSQM <- intToUtf8("0x2018") # Left single quote
    	RSQM <- intToUtf8("0x2019") # Right single quote
    	LDQM <- intToUtf8("0x201c") # Left double quote
    	RDQM <- intToUtf8("0x201d") # Right double quote
    } else {
        LSQM <- RSQM <- "'"
        LDQM <- RDQM <- '"'
    }

    trim <- function(x) {
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }

    ## underline via backspacing
    txt_header <- function(header) {
        opts <- Rd2txt_options()
        header <- paste(strwrap(header, WIDTH), collapse="\n")
        if (opts$underline_titles) {
            letters <- strsplit(header, "", fixed = TRUE)[[1L]]
            isaln <- grep("[[:alnum:]]", letters)
            letters[isaln] <- paste0("_\b", letters[isaln])
            paste(letters, collapse = "")
        } else header
    }

    unescape <- function(x) {
        x <- psub("(---|--)", "-", x)
        x
    }

    writeCode <- function(x) {
        txt <- as.character(x)
        if(inEqn) txt <- txt_eqn(txt)
        txt <- fsub('"\\{"', '"{"', txt)
        ## \dots gets left in noquote.Rd
        txt <- fsub("\\dots",  "...", txt)
        put(txt)
    }

    # This function strips pending blank lines, then adds n new ones.
    blankLine <- function(n = 1L) {
    	while (length(buffer) &&
               grepl("^[[:blank:]]*$", buffer[length(buffer)]))
    	    buffer <<- buffer[-length(buffer)]
	flushBuffer()
	if (n > haveBlanks) {
	    buffer <<- rep_len("", n - haveBlanks)
	    flushBuffer()
	    haveBlanks <<- n
	}
	dropBlank <<- TRUE
    }

    txt_eqn <- function(x) {
        x <- psub("\\\\(Alpha|Beta|Gamma|Delta|Epsilon|Zeta|Eta|Theta|Iota|Kappa|Lambda|Mu|Nu|Xi|Omicron|Pi|Rho|Sigma|Tau|Upsilon|Phi|Chi|Psi|Omega|alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega|sum|prod|sqrt)", "\\1", x)
        x <- psub("\\\\(dots|ldots)", "...", x)
        x <- fsub("\\le", "<=", x)
        x <- fsub("\\ge", ">=", x)
        x <- fsub("\\infty", "Inf", x)
        ## FIXME: are these needed?
        x <- psub("\\\\(bold|strong|emph|var)\\{([^}]*)\\}", "\\2", x)
        x <- psub("\\\\(code|samp)\\{([^}]*)\\}", "'\\2'", x)
        x
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            putf('## Not run:\n')
            writeCodeBlock(block, tag)
            blankLine(0L)
            putf('## End(Not run)\n')
        } else {
            putf('## Not run: ')
            writeCodeBlock(block, tag)
            blankLine(0L)
        }
    }

    writeQ <- function(block, tag, quote=tag)
    {
        if (use_fancy_quotes) {
            if (quote == "\\sQuote") {
                put(LSQM); writeContent(block, tag); put(RSQM)
            } else {
                put(LDQM); writeContent(block, tag); put(RDQM)
            }
        } else {
            if (quote == "\\sQuote") {
                put("'"); writeContent(block, tag); put("'")
            } else {
                put("\""); writeContent(block,tag); put("\"")
            }
        }
    }

    writeBlock <- function(block, tag, blocktag) {
        switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = writeCode(tabExpand(block)),
               TEXT = if(blocktag == "\\command") putw(block) else putw(unescape(tabExpand(block))),
               USERMACRO =,
               "\\newcommand" =,
               "\\renewcommand" =,
               COMMENT = {},
               LIST = writeContent(block, tag),
               "\\describe" = {
               	   blankLine(0L)
                   writeContent(block, tag)
                   blankLine()
               },
               "\\itemize"=,
               "\\enumerate"= {
               	   blankLine(0L)
                   enumItem0 <- enumItem
                   enumItem <<- 0L
                   indent0 <- indent
                   opts <- Rd2txt_options()
                   indent <<- max(opts$minIndent,
                              indent + opts$extraIndent)
                   dropBlank <<- TRUE
                   writeContent(block, tag)
                   blankLine()
                   indent <<- indent0
                   enumItem <<- enumItem0
               },
               "\\code"=,
               "\\command"=,
               "\\env"=,
               "\\file"=,
               "\\kbd"=,
               "\\option"=,
               "\\pkg"=,
               "\\samp" = {
                   opts <- Rd2txt_options()
                   if(opts$code_quote)
                       writeQ(block, tag, quote="\\sQuote")
                   else writeContent(block,tag)
               },
               "\\email" = {
                   put("<email: ",
                       trimws(gsub("\n", "",
                                   paste(as.character(block),
                                         collapse=""))),
                       ">")
               },
               "\\url" = {
                   put("<URL: ",
                       trimws(gsub("\n", "",
                                   paste(as.character(block),
                                         collapse=""))),
                       ">")
               },
               "\\href" = {
                   opts <- Rd2txt_options()
                   writeContent(block[[2L]], tag)
                   if (opts$showURLs)
  			put(" (URL: ",
  			    trimws(gsub("\n", "",
                                        paste(as.character(block[[1L]]),
                                              collapse=""))),
  			    ")")
               },
               "\\Sexpr"= put(as.character.Rd(block, deparse=TRUE)),
               "\\acronym" =,
               "\\cite"=,
               "\\dfn"= ,
               "\\special" = ,
               "\\var" = writeContent(block, tag),

               "\\bold"=,
               "\\strong"= {
                   put("*")
                   writeContent(block, tag)
                   put("*")
               },
               "\\emph"= {
                   put("_")
                   writeContent(block, tag)
                   put("_")
               },
               "\\sQuote" =,
               "\\dQuote"= writeQ(block, tag) ,
               "\\preformatted"= {
                   putf("\n")
                   writeCodeBlock(block, tag)
               },
               "\\verb"= put(block),
               "\\linkS4class" =,
               "\\link" = writeContent(block, tag),
               "\\cr" = {
                   ## we want to print out what we have, and if
                   ## followed immediately by \n (as it usually is)
                   ## discard that.  This is not entirely correct,
                   ## but it is better than before ....
                   flushBuffer()
                   dropBlank <<- TRUE
                   },
               "\\dots" =,
               "\\ldots" = put("..."),
               "\\R" = put("R"),
               "\\enc" = {
                   ## Test to see if we can convert the encoded version
                   txt <- as.character(block[[1L]])
                   test <- iconv(txt, "UTF-8", outputEncoding, mark = FALSE)
                   txt <- if(!is.na(test)) txt else as.character(block[[2L]])
                   put(txt)
               } ,
               "\\eqn" = {
                   block <- block[[length(block)]]
                   ## FIXME: treat 2 of 2 differently?
                   inEqn0 <- inEqn
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   inEqn <<- inEqn0
               },
               "\\deqn" = {
                   blankLine()
                   block <- block[[length(block)]]
                   save <- startCapture()
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   eqn <- endCapture(save)
                   eqn <- frmt(eqn, justify="centre", width=WIDTH-indent)
                   putf(paste(eqn, collapse="\n"))
    		   blankLine()
               },
               "\\figure" = {
                   blankLine()
                   save <- startCapture()
                   writeContent(block[[length(block)]], tag)
                   alt <- endCapture(save)
                   if (length(alt)) {
                   	alt <- frmt(alt, justify = "centre",
                                    width = WIDTH - indent)
                   	putf(paste(alt, collapse = "\n"))
                   	blankLine()
                   }
               },
               "\\tabular" = writeTabular(block),
               "\\subsection" = writeSection(block, tag),
               "\\if"=,
               "\\ifelse" =
                   if (testRdConditional("text", block, Rdfile))
               		writeContent(block[[2L]], tag)
               	   else if (tag == "\\ifelse")
               	   	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block))
		   put(block[[i]]),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
    	formats <- table[[1L]]
    	content <- table[[2L]]
    	if (length(formats) != 1L || RdTags(formats) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	formats <- strsplit(formats[[1L]], "", fixed = TRUE)[[1L]]
        tags <- RdTags(content)
        entries <- list()
        row <- 1L
        col <- 1L
        save <- startCapture()
        dropBlank <<- TRUE
        newEntry <- function() {
            entries <<- c(entries, list(list(text=trim(endCapture(save)),
	                   	             row=row, col=col)))
            save <<- startCapture()
            dropBlank <<- TRUE
        }
        for (i in seq_along(tags)) {
            switch(tags[i],
                  "\\tab" = {
                  	newEntry()
                   	col <- col + 1
                   	if (col > length(formats))
                   	    stopRd(content[[i]], Rdfile,
                                   sprintf("too many columns for format '%s'",
                                           table[[1L]]))
                   },
                   "\\cr" = {
                   	newEntry()
                   	row <- row + 1L
			col <- 1L
                    },
                   writeBlock(content[[i]], tags[i], "\\tabular")
                   )
        }
        newEntry()
        endCapture(save)
        entries <- with(entries[[length(entries)]],
        	    {
                        if (!length(text) && col == 1L)
                            entries[-length(entries)]
                        else
                            entries
                    })
        rows <- entries[[length(entries)]]$row
        cols <- max(sapply(entries, function(e) e$col))
        widths <- rep_len(0L, cols)
        lines <- rep_len(1L, rows)
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            while(length(e$text) && !nzchar(e$text[length(e$text)])) {
            	e$text <- e$text[-length(e$text)]
            	entries[[i]] <- e
            }
            if (any(nzchar(e$text)))
            	widths[e$col] <- max(widths[e$col], max(nchar(e$text, "w")))
            lines[e$row] <- max(lines[e$row], length(e$text))
        }
        result <- matrix("", sum(lines), cols)
        for (i in seq_len(cols))
            result[, i] <- strrep(" ", widths[i])
        firstline <- c(1L, 1L+cumsum(lines))
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            if(!length(e$text)) next
            ## FIXME: this is not right: it justifies strings as if
            ## they are escaped, so in particular \ takes two columns.
            text <- frmt(e$text, justify=formats[e$col], width=widths[e$col])
            for (j in seq_along(text))
            	result[firstline[e$row] + j - 1L, e$col] <- text[j]
        }
        blankLine()
        indent0 <- indent
        indent <<- indent + 1L
        for (i in seq_len(nrow(result))) {
            putf(paste0(" ", result[i,], " ", collapse=""))
# This version stripped leading blanks on the first line
#            for (j in seq_len(cols))
#            	putf(" ", result[i,j], " ")
            putf("\n")
        }
        blankLine()
        indent <<- indent0
    }

    writeCodeBlock <- function(blocks, blocktag)
    {
    	tags <- RdTags(blocks)
	i <- 0
	while (i < length(tags)) {
	    i <- i + 1
            block <- blocks[[i]]
            tag <- tags[i]
            switch(tag,
                   "\\method" =,
                   "\\S3method" =,
                   "\\S4method" = {
                   	blocks <- transformMethod(i, blocks, Rdfile)
                   	tags <- RdTags(blocks)
                   	i <- i - 1
                   },
                   UNKNOWN =,
                   VERB =,
                   RCODE =,
                   TEXT = writeCode(tabExpand(block)),
                   "\\donttest" =,
                   "\\special" =,
                   "\\var" = writeCodeBlock(block, tag),
                   "\\dots" =, # \ldots is not really allowed
                   "\\ldots" = put("..."),
                   "\\dontrun"= writeDR(block, tag),
		   USERMACRO =,
		   "\\newcommand" =,
		   "\\renewcommand" =,
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   ## All the markup such as \emph
                   stopRd(block, Rdfile, "Tag ", tag,
                          " not expected in code block")
                   )
        }
    }

    writeContent <- function(blocks, blocktag) {
        itemskip <- FALSE
	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
                   "\\item" = {
                       switch(blocktag,
                              "\\describe"= {
                                  blankLine()
                                  save <- startCapture()
                                  dropBlank <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  DLlab <- endCapture(save)
                                  indent0 <- indent
                                  opts <- Rd2txt_options()
                                  indent <<- max(opts$minIndent,
                                                 indent + opts$extraIndent)
                                  keepFirstIndent <<- TRUE
                                  putw(strrep(" ", indent0),
                                       frmt(paste0(DLlab),
                                            justify="left", width=indent),
                                       " ")
                                  writeContent(block[[2L]], tag)
			  	  blankLine(0L)
                                  indent <<- indent0
                              },
                              "\\value"=,
                              "\\arguments"= {
                                  blankLine()
                                  save <- startCapture()
                                  dropBlank <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  DLlab <- endCapture(save)
                                  indent0 <- indent
                                  opts <- Rd2txt_options()
                                  indent <<- max(opts$minIndent, indent + opts$extraIndent)
                                  keepFirstIndent <<- TRUE
                                  putw(frmt(paste0(DLlab, ": "),
                                              justify="right", width=indent))
                                  writeContent(block[[2L]], tag)
			  	  blankLine(0L)
                                  indent <<- indent0
                              },
                              "\\itemize" =,
                              "\\enumerate" = {
                              	  blankLine()
                              	  keepFirstIndent <<- TRUE
                              	  opts <- Rd2txt_options()
                              	  if (blocktag == "\\itemize")
                              	      label <- opts$itemBullet
                              	  else {
                              	      enumItem <<- enumItem + 1L
                              	      label <- opts$enumFormat(enumItem)
                              	  }
                              	  putw(frmt(label, justify="right",
                                            width=indent))
                              })
                       itemskip <- TRUE
                   },
               { # default
                   if (itemskip) {
                       ## The next item must be TEXT, and start with a space.
                       itemskip <- FALSE
                       if (tag == "TEXT") {
                           txt <- psub("^ ", "", as.character(tabExpand(block)))
                           put(txt)
                       } else writeBlock(block, tag, blocktag) # should not happen
                   } else writeBlock(block, tag, blocktag)
               })
	}
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return()
    	save <- c(indent, sectionLevel, keepFirstIndent, dropBlank, wrapping)
    	blankLine(min(sectionLevel, 1L))
    	titlePrefix <- strrep("  ", sectionLevel)
    	opts <- Rd2txt_options()
        indent <<- opts$sectionIndent + opts$sectionExtra*sectionLevel
        sectionLevel <<- sectionLevel + 1
        keepFirstIndent <<- TRUE
        if (tag == "\\section" || tag == "\\subsection") {
            ## section header could have markup
            title <- .Rd_format_title(.Rd_get_text(section[[1L]]))
            putf(titlePrefix, txt_header(title), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
    	    writeContent(section[[2L]], tag)
    	} else if (tag %in% c("\\usage", "\\examples")) {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- FALSE
            keepFirstIndent <<- FALSE
            writeCodeBlock(section, tag)
    	} else {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
            writeContent(section, tag)
        }
        blankLine()

        indent <<- save[1L]
        sectionLevel <<- save[2L]
        keepFirstIndent <<- save[3L]
        dropBlank <<- save[4L]
        wrapping <<- save[5L]
    }

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con), add=TRUE)
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, fragment=fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    if (fragment) {
    	if (sections[1L] %in% names(sectionOrder))
    	    for (i in seq_along(sections))
    	    	writeSection(Rd[[i]], sections[i])
    	else
    	    for (i in seq_along(sections))
    	    	writeBlock(Rd[[i]], sections[i], "")
    } else {
	title <- .Rd_format_title(.Rd_get_title(Rd))

	name <- trim(Rd[[2L]][[1L]])

	if(nzchar(package)) {
	    left <- name
	    mid <- if(nzchar(package)) paste0("package:", package) else ""
	    right <- "R Documentation"
	    if(encoding != "unknown")
		right <- paste0(right, "(", encoding, ")")
	    pad <- max(HDR_WIDTH - nchar(left, "w") - nchar(mid, "w") - nchar(right, "w"), 0)
	    pad0 <- pad %/% 2L
	    pad1 <- strrep(" ", pad0)
	    pad2 <- strrep(" ", pad - pad0)
	    putf(paste0(left, pad1, mid, pad2, right, "\n\n"))
	}

	putf(txt_header(title))
	blankLine()

	for (i in seq_along(sections)[-(1:2)])
	    writeSection(Rd[[i]], sections[i])
    }
    blankLine(0L)
    invisible(out)
}
