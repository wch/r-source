#  File src/library/tools/R/Rd2txt.R
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
    .Call(doTabExpand, x, start)
}

Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type,
             stages = "render", outputEncoding = "",
             width = getOption("help_text_width", 80L), ...)
{
    ## these attempt to mimic pre-2.10.0 layout
    WIDTH <- 0.9 * width
    HDR_WIDTH <- WIDTH - 2L

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

    wrap <- function(doWrap = TRUE)
	if (doWrap != wrapping) { flushBuffer(); wrapping <<- doWrap }

    putw <- function(...)  { wrap(TRUE); put(...) }

    putf <- function(...)  { wrap(FALSE); put(...) }

    put <- function(...) {
        txt <- paste(..., collapse="", sep="")
        trail <- grepl("\n$", txt)
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
                paste(buffer[length(buffer)], txt[1L], sep="")
            buffer <<- c(buffer, txt[-1L])
        }
        else buffer <<- txt
        linestart <<- trail
    }

    blanks <- function(n)
	if (n) paste(rep(" ", n), collapse="") else ""

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
		    buffer[-1L] <<- paste(blanks(indent), buffer[-1L], sep="")
		keepFirstIndent <- FALSE
	    } else
		buffer <<- paste(blanks(indent), buffer, sep="")
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

    if(getOption("useFancyQuotes") && use_fancy_quotes) {
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

    striptitle <- function(text) {
        text <- fsub("\\", "", text)
        text <- fsub("---", "_", text)
        text <- fsub("--", "-", text)
        text
    }

    txt_striptitle <- function(text) {
        text <- striptitle(text)
        if(use_fancy_quotes) {
            text <- fsub("``", LDQM, text)
            text <- fsub("''", RDQM, text)
            text <- psub("`([^']+)'", paste(LSQM, "\\1", RSQM, sep=""), text)
            text <- fsub("`", "'", text)
        } else {
            text <- psub("(``|'')", '"', text)
            text <- fsub("`", "'", text)
        }
        text
    }

    ## underline via backspacing
    txt_header <- function(header) {
        header <- paste(strwrap(header, WIDTH), collapse="\n")
        letters <- strsplit(header, "", fixed = TRUE)[[1L]]
        isaln <- grep("[[:alnum:]]", letters)
        letters[isaln] <- paste("_\b", letters[isaln], sep="")
        paste(letters, collapse = "")
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
        txt <-fsub("\\dots",  "...", txt)
        put(txt)
    }

    # This function strips pending blank lines, then adds n new ones.
    blankLine <- function(n = 1L) {
    	while (length(buffer) &&
               grepl("^[[:blank:]]*$", buffer[length(buffer)]))
    	    buffer <<- buffer[-length(buffer)]
	flushBuffer()
	if (n > haveBlanks) {
	    buffer <<- rep("", n - haveBlanks)
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
               LIST =,
               COMMENT = {},
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
                   indent <<- max(10L, indent+4L)
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
               "\\samp" = writeQ(block, tag, quote="\\sQuote"),
               "\\email"=  put("<email: ",
                               gsub("\n", "", paste(as.character(block), collapse="")),
                               ">"),
                "\\url"= put("<URL: ",
                              gsub("\n", "", paste(as.character(block), collapse="")),
                              ">") ,
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
                   eqn <- format(eqn, justify="centre", width=WIDTH-indent)
                   putf(eqn)
    		   blankLine()
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
        widths <- rep(0L, cols)
        lines <- rep(1L, rows)
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            while(length(e$text) && !nzchar(e$text[length(e$text)])) {
            	e$text <- e$text[-length(e$text)]
            	entries[[i]] <- e
            }
            if (any(nzchar(e$text)))
            	widths[e$col] <- max(widths[e$col], max(nchar(e$text)))
            lines[e$row] <- max(lines[e$row], length(e$text))
        }
        result <- matrix("", sum(lines), cols)
        for (i in seq_len(cols))
            result[, i] <- blanks(widths[i])
        firstline <- c(1L, 1L+cumsum(lines))
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            text <- format(e$text, justify=formats[e$col], width=widths[e$col])
            for (j in seq_along(text))
            	result[firstline[e$row] + j - 1L, e$col] <- text[j]
        }
        blankLine()
        indent0 <- indent
        indent <<- indent + 1L
        for (i in seq_len(nrow(result))) {
            for (j in seq_len(cols))
            	putf(" ", result[i,j], " ")
            putf("\n")
        }
        blankLine()
        indent <<- indent0
    }

    writeCodeBlock <- function(blocks, blocktag)
    {
    	tags <- RdTags(blocks)

	for (i in seq_along(tags)) {
            block <- blocks[[i]]
            tag <- attr(block, "Rd_tag")
            switch(tag,
                   "\\method" =,
                   "\\S3method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       if(generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\ldots" =, # not really right
                                              "\\dots" = "...",
                                              RCODE = as.character(blocks[[j]]),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           txt <- psub1("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- fsub1("@generic@", generic, txt)
                           if (generic == "[")
                               txt <- psub1("\\)([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- psub1("\\)([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- psub1("\\)([^)]*)$", "\\1", txt)
                           if (grepl("<-\\s*value", txt))
                               putf("## S3 replacement method for class '")
                           else
                               putf("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else if(grepl(sprintf("^%s$",
                                               paste(c("\\+", "\\-", "\\*",
                                                       "\\/", "\\^", "<=?",
                                                       ">=?", "!=?", "==",
                                                       "\\&", "\\|",
                                                       "\\%[[:alnum:][:punct:]]*\\%"),
                                                     collapse = "|")),
                                       generic)) {
                           ## Binary operators and unary '!'.
                           ## Need to assemble the call.
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              RCODE = as.character(tabExpand(blocks[[j]])),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           nms <- as.character(res[[1L]])[-1L]
                           len <- length(nms)
                           ## (There should be no default values).
                           if((len == 1L) && (generic == "!")) {
                               ## Unary: !.
                               txt <- sprintf("! %s\n", nms[1L])
                           } else if((len == 2L) && (generic != "!")) {
                               ## Binary: everything but !.
                               txt <- sprintf("%s %s %s\n",
                                              nms[1L], generic, nms[2L])
                           } else {
                               warnRd(block, Rdfile,
                                      sprintf("arity problem for \\method{%s}{%s}",
                                              generic, class))
                               txt <- paste(generic, txt)
                           }
                           putf("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               putf('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1L]][[1L]])) {
                               putf("## S3 replacement method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               putf("':\n")
                           } else {
                               putf("## S3 method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               putf("':\n")
                           }
                           writeCodeBlock(block[[1L]], tag)
                       }
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
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   "\\S4method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       if(generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\ldots" =, # not really right
                                              "\\dots" = "...",
                                              RCODE = as.character(tabExpand(blocks[[j]])),
                                              stopRd(block, Rdfile, sprintf("invalid markup '%s' in %s", tg, tag)))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1L
                           }
                           txt <- psub1("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- fsub1("@generic@", generic, txt)
                           if (generic == "[")
                               txt <- psub1("\\)([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- psub1("\\)([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- psub1("\\)([^)]*)$", "\\1", txt)
                           if (grepl("<-\\s*value", txt))
                               putf("## S4 replacement method for signature '")
                           else
                               putf("## S4 method for signature '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i + 1L]] <-
                               structure(txt, Rd_tag = "RCODE")
                       } else {
                           putf("## S4 method for signature '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           writeCodeBlock(block[[1L]], tag)
                       }
                   },
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
                                  indent <<- max(10L, indent + 4L)
                                  keepFirstIndent <<- TRUE
                                  putw(paste(rep(" ", indent0), collapse=""),
                                       format(paste(DLlab,  sep=""),
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
                                  indent <<- max(10L, indent + 4L)
                                  keepFirstIndent <<- TRUE
                                  putw(format(paste(DLlab, ": ", sep=""),
                                              justify="right", width=indent))
                                  writeContent(block[[2L]], tag)
			  	  blankLine(0L)
                                  indent <<- indent0
                              },
                              "\\itemize" =,
                              "\\enumerate" = {
                              	  blankLine()
                              	  keepFirstIndent <<- TRUE
                              	  if (blocktag == "\\itemize")
                              	      label <- "* "
                              	  else {
                              	      enumItem <<- enumItem + 1L
                              	      label <- paste(enumItem, ". ", sep="")
                              	  }
                              	  putw(format(label, justify="right",
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

    toChar <- function(x)
    {
        out <- character()
        for(i in seq_along(x)) {
            this <- x[[i]]
            out <- c(out,
                     switch(attr(this, "Rd_tag"),
                            "\\ldots" =,
                            "\\dots" = "...",
                            "\\R" = "R",
                            "\\bold" =,
                            "\\strong" =,
                            "\\emph" = toChar(this),
                            "\\code" = sQuote(toChar(this)),
                            as.character(this))
                     )
        }
        paste(out, collapse = "")
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return()
    	save <- c(indent, sectionLevel, keepFirstIndent, dropBlank, wrapping)
    	blankLine(min(sectionLevel, 1L))
    	titlePrefix <- paste(rep("  ", sectionLevel), collapse="")
        indent <<- 5L + 2L*sectionLevel
        sectionLevel <<- sectionLevel + 1
        keepFirstIndent <<- TRUE
        if (tag == "\\section" || tag == "\\subsection") {
            ## section header could have markup
            putf(titlePrefix, txt_header(toChar(section[[1L]])), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
    	    writeContent(section[[2L]], tag)
    	} else if (tag %in% c("\\usage", "\\synopsis", "\\examples")) {
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

        indent <<- save[1]
        sectionLevel <<- save[2]
        keepFirstIndent <<- save[3]
        dropBlank <<- save[4]
        wrapping <<- save[5]
    }

    if (is.character(out)) {
        if(out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading and trailing whitespace, \n
    title <- trim(paste(psub1("^\\s+", "", title[nzchar(title)]), collapse=" "))
    title <- fsub("\n", "", title)

    name <- trim(Rd[[2L]][[1L]])

    if(nzchar(package)) {
        left <- name
        mid <- if(nzchar(package)) paste("package:", package, sep = "") else ""
        right <- "R Documentation"
        if(encoding != "unknown")
            right <- paste(right, "(", encoding, ")", sep="")
        pad <- max(HDR_WIDTH - nchar(left, "w") - nchar(mid, "w") - nchar(right, "w"), 0)
        pad0 <- pad %/% 2L
        pad1 <- paste(rep.int(" ", pad0), collapse = "")
        pad2 <- paste(rep.int(" ", pad - pad0), collapse = "")
        putf(paste(left, pad1, mid, pad2, right, "\n\n", sep=""))
    }

    putf(txt_header(txt_striptitle(title)))
    blankLine()

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    blankLine(0L)
    invisible(out)
}
