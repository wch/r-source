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

## This warns on text outside sections
## and errors on
##  unrecognized tags (can the parser do that?)
##  \\tabular format must be simple text
##  too many columns for format
##  invalid markup in \[S3]method
##  "Tag ", tag, " not expected in code block"
##  Only one \\Rdversion declaration is allowed
##  Unrecognized section (but I think the parser catches that)
##  Sections \\title, and \\name must exist and be unique in Rd files
##  \\name must only contain simple text

Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type, stages = "render",
             outputEncoding = "", ...)
{
    WIDTH <- 72L
    HDR_WIDTH <- 70L

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

    wrap <- function(doWrap = TRUE)
	if (doWrap != wrapping) { flushBuffer(); wrapping <<- doWrap }

    putw <- function(...)  { wrap(TRUE); put(...) }

    putf <- function(...)  { wrap(FALSE); put(...) }

    put <- function(...) {
        txt <- paste(..., collapse="", sep="")
        trail <- grepl("\n$", txt)
        # Convert newlines
        txt <- strsplit(txt, "\n", fixed=TRUE)[[1]]
        if (dropBlank) {
            while(length(txt) && grepl("^[[:space:]]*$",txt[1]))
            	txt <- txt[-1]
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
		first <- nchar(sub("[^ ].*", "", buffer[1]))
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
                                        indent=first, exdent=indent))
		    first <- indent
		}
		result <- c(result, buffer[blankLines[i]])
		start <- blankLines[i]+1L
	    }
	    buffer <<- result[-length(result)] # remove the sentinel
	} else {  # Not wrapping
	    if (keepFirstIndent) {
		if (length(buffer) > 1L)
		    buffer[-1L] <<- paste(blanks(indent), buffer[-1L], sep="")
		keepFirstIndent <- FALSE
	    } else
		buffer <<- paste(blanks(indent), buffer, sep="")
	}

    	if (length(buffer)) writeLinesUTF8(buffer, con, outputEncoding)
    	buffer <<- character()
    	linestart <<- TRUE
    }

    encoding <- "unknown"

    trim <- function(x) {
        x <- sub("^\\s*", "", x, perl = TRUE)
        sub("\\s*$", "", x, perl = TRUE)
    }

    striptitle <- function(text) {
        Encoding(text) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        text <- gsub("\\", "", text, fixed = TRUE)
        text <- gsub("---", "_", text, fixed = TRUE)
        text <- gsub("--", "-", text, fixed = TRUE)
        Encoding(text) <- "UTF-8"
        text
    }

    txt_striptitle <- function(text) {
        text <- striptitle(text)
        Encoding(text) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        if (.Platform$OS.type == "windows" &&
            Sys.getlocale("LC_CTYPE") != "C") {
            text <- gsub("``", LDQM, text, fixed = TRUE)
            text <- gsub("''", RDQM, text, fixed = TRUE)
            text <- gsub("`([^']+)'", paste(LSQM, "\\1", RSQM, sep=""), text)
            text <- gsub("`", "'", text, fixed = TRUE)
        } else {
            text <- gsub("(``|'')", '"', text)
            text <- gsub("`", "'", text, fixed = TRUE)
        }
        Encoding(text) <- "UTF-8"
        text
    }

    ## underline via backspacing
    txt_header <- function(header) {
        header <- paste(strwrap(header, WIDTH), collapse="\n")
        letters <- strsplit(header, "", fixed=TRUE)[[1L]]
        isaln <- grep("[[:alnum:]]", letters)
        letters[isaln] <- paste("_\b", letters[isaln], sep="")
        paste(letters, collapse = "")
    }

    unescape <- function(x) {
        x <- gsubUTF8("(---|--)", "-", x)
        x
    }

    writeCode <- function(x) {
        txt <- as.character(x)
        if(inEqn) txt <- txt_eqn(txt)
        Encoding(txt) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        txt <- gsub('"\\{"', '"{"', txt, fixed = TRUE)
        ## \dots gets left in noquote.Rd
        txt <- gsub("\\dots",  "....", txt, fixed = TRUE)
        Encoding(txt) <- "UTF-8"
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
	}
	haveBlanks <<- n
	dropBlank <<- TRUE
    }

    txt_eqn <- function(x) {
        Encoding(x) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        x <- gsub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon|psi)", "\\1", x)
        x <- gsub("\\\\(bold|strong|emph|var)\\{([^}]*)\\}", "\\2", x)
        x <- gsub("\\\\(ode|samp)\\{([^}]*)\\}", "'\\2'", x)
        Encoding(x) <- "UTF-8"
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

    if (.Platform$OS.type == "windows") {
        ## On Windows, Unicode literals are translated to local code page
    	LSQM <- intToUtf8("0x2018") # Left single quote
    	RSQM <- intToUtf8("0x2019") # Right single quote
    	LDQM <- intToUtf8("0x201c") # Left double quote
    	RDQM <- intToUtf8("0x201d") # Right double quote
    }

    writeQ <- function(block, tag, quote=tag)
    {
        if (.Platform$OS.type == "windows"
            && Sys.getlocale("LC_CTYPE") != "C") {
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
               RCODE = writeCode(block),
               TEXT = putw(unescape(block)),
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
               "\\email"=  put("<email: ", as.character(block), ">"),
                "\\url"= put("<URL: ", as.character(block), ">"),
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
               "\\cr" = put("\n"),
               "\\dots" =,
               "\\ldots" = put("..."),
               "\\R" = put("R"),
               "\\enc" = {
                   txt <- as.character(block[[2L]])
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
                       if (generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1L
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
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
                           txt <- sub("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- sub("@generic@", generic, txt, fixed = TRUE)
                           if (generic == "[")
                               txt <- sub(")([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- sub(")([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- sub(")([^)]*)$", "\\1", txt)
                           if (grepl("<-\\s*value", txt))
                               putf("## S3 replacement method for class '")
                           else
                               putf("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           putf("':\n")
                           blocks[[i+1L]] <- structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               putf('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1]][[1L]])) {
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
                   "\\special" =,
                   UNKNOWN =,
                   VERB =,
                   RCODE =,
                   TEXT = writeCode(block),
                   "\\var" = writeCodeBlock(block, tag),
                   "\\dots" =, # \ldots is not really allowed
                   "\\ldots" = put("..."),
                   "\\donttest" = writeCodeBlock(block, tag),
                   "\\dontrun"= writeDR(block, tag),
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   "\\S4method" = {
                       putf("## S4 method for signature '")
                       writeCodeBlock(block[[2L]], tag)
                       putf("':\n")
                       writeCodeBlock(block[[1L]], tag)
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
                              "\\describe"=,
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
                           txt <- gsubUTF8("^ ", "", as.character(block),
                                           perl = TRUE)
                           put(txt)
                       } else writeBlock(block, tag, blocktag) # should not happen
                   } else writeBlock(block, tag, blocktag)
               })
	}
    }

    writeSection <- function(section, tag) {
    	blankLine(0L)
        indent <<- 5L
        keepFirstIndent <<- TRUE
        if (tag == "\\section") {
            putf(txt_header(as.character(section[[1L]])), ":")
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
    	stopRd(Rd[[version[2L]]], Rdfile, "Only one \\Rdversion declaration is allowed")

    ## Give warning (pro tem) for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]",
                           unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	warnRd(Rd[sections == "TEXT"][[bad[1L]]], Rdfile,
               "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding",
                            "\\keyword", "\\alias", "\\Rdversion", "\\RdOpts")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1L]]], Rdfile, "Section ", sections[which(bad)[1L]], " unrecognized")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, Rdfile, "Sections \\title, and \\name must exist and be unique in Rd files")

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading and trailing whitespace, \n
    title <- trim(paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                        collapse=" "))
    title <- gsubUTF8("\n", "", title, fixed = TRUE)

    name <- Rd[[2L]]
    tags <- RdTags(name)
    if (length(tags) > 1L) stopRd(name, Rdfile, "\\name must only contain simple text")

    name <- trim(name[[1L]])

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
