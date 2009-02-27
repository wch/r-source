#  File src/library/tools/R/Rd2tex.R
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


## surplus whitespace
## Listing environments are minimal at best, \\describe \\tabular
## e.g. DateTimeClasses
## \cr in arguments, cbind
## getDLLRegisteedRoutines
## c\car{x} in regex
## spacing in zMethods


Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type)
{
    WIDTH <- 72L
    HDR_WIDTH <- 70L

    ## we need to keep track of where we are.
    nch <- 0L
    text <- ""
    indent <- 0L

    inEqn <- FALSE
    DLlab <- character()
    DLval <- character()

    Of <- function(text) writeLines(text, con)
    of0 <- function(...)  of1(paste(..., sep=""))
    of1 <- function(txt) {
        txt <- as.character(txt)
        if(!length(txt) || !nzchar(txt)) return()
        ## are there any line breaks?
        if (0 &&!length(grep("\n", txt))) {
            pad <- if (nch) "" else paste(rep(" ", indent), collapse = "")
            text <<- paste(text, pad, txt, sep = "")
            nch <<- nch + nchar(txt)
        } else {
            ## was there a trailing \n?
            trail <- length(grep("\n$", txt)) > 0L
            lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
            nl <- length(lines)
             if(!nl) return()
            ind <- paste(rep(" ", indent), collapse = "")
            pad <- rep(ind, nl)
            if(nch) pad[1L] <- ""
            eol <- rep("\n", nl)
             if (!trail) eol[nl] <- ""
            text <<- paste(text, paste(pad, lines, eol, sep=""), sep="")
            nch <<- if(trail) 0L else nch + nchar(lines[nl])
         }
    }

    encoding <- "unknown"

    trim <- function(x) {
        x <- sub("^\\s*", "", x, perl = TRUE)
        sub("\\s*$", "", x, perl = TRUE)
    }

    striptitle <- function(text) {
        text <- gsub("\\", "", text, fixed = TRUE)
        text <- gsub("---", "_", text, fixed = TRUE)
        text <- gsub("--", "-", text, fixed = TRUE)
        text
    }

    txt_striptitle <- function(text) {
        text <- striptitle(text)
        if (.Platform$OS.type == "windows" &&
            Sys.getlocale("LC_CTYPE") != "C") {
            text <- gsub("``", "\x93", text, fixed = TRUE)
            text <- gsub("''", "\x94", text, fixed = TRUE)
            text <- gsub("`([^']+)'", "\x93\\1\x94", text)
            text <- gsub("`", "'", text, fixed = TRUE)
        } else {
            text <- gsub("(``|'')", '"', text)
            text <- gsub("`", "'", text, fixed = TRUE)
        }
        text
    }

    ## underline via backspacing
    txt_header <- function(header) {
        header <- paste(strwrap(header, WIDTH), collapse="\n")
        letters <- strsplit(header, "")[[1L]]
        isaln <- grep("[[:alnum:]]", letters)
        letters[isaln] <- paste("_\b", letters[isaln], sep="")
        paste(letters, collapse = "")
    }

    txt_sect <- function(header) paste(txt_header(header), ":", sep = "")

    inCodeBlock <- FALSE ## used to indicate to texify where we are

    addParaBreaks <- function(x, tag) {
        if (inEqn) {
            of1(txt_eqn(x))
            return()
        }
        start <- attr(x, "srcref")[2L]
        if (isBlankLineRd(x)) {
            writePara()
            Of("")
	} else {
            if (start == 1) x <- gsub("^\\s+", "", x, perl = TRUE)
            of1(x)
        }
    }

    unescape <- function(x) {
        x <- gsub("(---|--)", "-", x)
        x
    }

    codeB <- function(x) {
        txt <- as.character(x)
        if(inEqn) txt <- txt_eqn(txt)
        txt <- gsub('"\\{"', '"{"', txt, fixed = TRUE)
        ## \dots gets left in noquote.Rd
        txt <- gsub("\\dots",  "....", txt, fixed = TRUE)
        of1(txt)
    }

    txt_eqn <- function(x) {
        x <- gsub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon|psi)", "\\1", x)
        x <- gsub("\\\\(bold|strong|emph|var)\\{([^}]*)\\}", "\\2", x)
        x <- gsub("\\\\(ode|samp)\\{([^}]*)\\}", "'\\2'", x)
        x
    }

    writeComment <- function(txt) {
	of1("\nn") ## FIXME, should include the \n?
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1('## Not run: ') # had trailing space before: FIXME remove
            writeCodeBlock(block, tag)
            ## FIXME only needs a \n here if not at left margin
            of1('## End(Not run)')
        } else {
            of1('## Not run: ')
            writeCodeBlock(block, tag)
        }
    }


    writeQ <- function(block, tag)
    {
        if (.Platform$OS.type == "windows" && Sys.getlocale("LC_CTYPE") != "C") {
            if (tag == "\\sQuote") {
                of1("\x91"); writeContent(block, tag); of1("\x92")
            } else {
                of1("\x93"); writeContent(block, tag); of1("\x94")
            }
        } else {
            if (tag == "\\sQuote") {
                of1("'"); writeContent(block, tag); of1("'")
            } else {
                of1("\""); writeContent(block,tag); of1("\"")
            }
        }
    }

    writeArgItem <- function(block, tag)
    {
        writePara()
        text0 <- text
        indent0 <- indent
        text <<- ""; indent <<- 0
        writeContent(block[[1L]], tag)
        label <- format(paste(text, ":", sep = ""),
                        justify = "right", width = 9)
        ## Do this by paras
        block <- block[[2L]]
        parabreaks <- sapply(block,
                             function(x) attr(x, "Rd_tag") == "TEXT"
                             && as.character(x) == "\n"
                             && attr(x, "srcref")[2L] == 1)
        pb <- which(c(parabreaks, TRUE))
        first <-1
        for (i in pb) {
            if(first > 1) Of("")
            text <<- ""
            writeContent(block[first:(i-1)], tag)
            text1 <-
                if(first == 1)
                    paste(strwrap(text, indent = 0, exdent = 10,
                                  width = WIDTH,
                                  initial = paste(label,"")),
                          collapse = "\n")
                else
                    paste(strwrap(text, indent = 10, exdent = 10,
                                  width = WIDTH),
                          collapse = "\n")
            Of(text1)
            first <- i+1
        }
        text <<- ""
        indent <<- indent0
        nch <- 0L
    }


    writePara <- function()
    {
        ## This assumes that we have collected all the contents of a
        ## running para for a section.
        Of(strwrap(text, width = WIDTH, indent = indent, exdent = indent))
        text <<- ""
        nch <- 0L
    }

    writeDeqn <- function()
    {
        ## FIXME: multi-line inputs
        text <- trim(text)
        text <- txt_eqn(text)
        nc <- nchar(text)
        pad <- paste(rep.int(" ", indent + (WIDTH - nc) %/% 2L), collapse = "")
        Of(paste("\n", pad, text, "\n", sep = ""))
        text <<- ""
        nch <- 0L
    }

    indentprev <- 0L
    startEnumItem <- function(block, tag)
    {
        ## write out any previous one
        writeEnumItem()
        ## start collecting for one item
        writePara()
        of1("   *  ")
        indentprev <<- indent
        indent <<- indent + 6L
    }

    writeEnumItem <- function()
    {
        if(!indentprev) return()
        writePara()
        indent <<- indentprev
        indentprev <<- 0L
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = codeB(block),
               TEXT = addParaBreaks(unescape(block), blocktag),
               LIST =,
               COMMENT = {},
               "\\describe"= {
                   writePara()
                   #print("in \\describe")
                   indent0 <- indent; indent <<- 0L
                   writeContent(block, tag)
                   tab <- formatDL(DLlab, DLval, style = "list")
                   indent <<- indent0
                   text <- ""
                   Of("")
                   pad <- paste(rep.int(" ", indent), collapse = "")
                   Of(paste(pad, tab, sep= "", collapse="\n\n"))
                   DLlab <<-DLval <<- character()
               },
               "\\enumerate"=,
               "\\itemize"= {
                   writePara()
                   writeContent(block, tag)
                   writeEnumItem()
               },
               ## FIXME: why not use smart quotes?
               "\\command"=,
               "\\env"=,
               "\\file"=,
               "\\kbd"=,
               "\\option"=,
               "\\pkg"=,
               "\\samp" = { of1("'"); writeContent(block, tag); of1("'")},
               "\\email"=  of0("<email: ", as.character(block), ">"),
                "\\url"= of0("<URL: ", as.character(block), ">"),
               "\\acronym" =,
               "\\cite"=,
               "\\dfn"= ,
               "\\special" = ,
               "\\var" = writeContent(block, tag),

               "\\code" = { of1("'"); writeContent(block, tag); of1("'")},

               "\\bold"=,
               "\\strong"= {
                   of1("*")
                   writeContent(block, tab)
                   of1("*")
               },
               "\\emph"= {
                   of1("_")
                   writeContent(block, tag)
                   of1("_")
               },
               "\\sQuote" =,
               "\\dQuote"= writeQ(block, tag) ,
               "\\preformatted"= {
                   writePara()
                   writeCodeBlock(block, tag)
                   Of(text)
                   text <<- ""
               },
               "\\verb"= of1(block),
               "\\linkS4class" =,
               "\\link" = writeContent(block, tag),
               "\\cr" = of1("\n"),
               "\\dots" =,
               "\\ldots" = of1("..."),
               "\\R" = of1("R"),
               "\\enc" = {
                   txt <- as.character(block[[2L]])
                   of1(txt)
               } ,
               "\\eqn" = {
                   block <- block[[length(block)]]
                   ## FIXME: treat 2 of 2 differently?
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   inEqn <<- FALSE
               },
               "\\deqn" = {
                   writePara()
                   block <- block[[length(block)]]
                   inEqn <<- TRUE
                   writeContent(block, tag)
                   inEqn <<- FALSE
                   writeDeqn()
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
        content <- preprocessRd(content, defines)
        tags <- attr(content, "RdTags")
        of0('\n\\Tabular{', format, '}{')
        for (i in seq_along(tags)) {
            switch(tags[i],
                   "\\tab" = of1("&"),
                   "\\cr" = of1("\\\\"),
                   writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        of1('}')
    }

    writeCodeBlock <- function(blocks, blocktag) {
        blocks <- preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag<- attr(block, "Rd_tag"),
                   "\\method" =,
                   "\\S3method" = {
                       class <- as.character(block[[2L]])
                       generic <- as.character(block[[1L]])
                       if (generic %in% c("[", "[[", "$")) {
                           ## need to assemble the call
                           j <- i + 1
                           txt <- ""
                           repeat {
                               this <- switch(tg <- attr(blocks[[j]], "Rd_tag"),
                                              "\\dots" = "...",
                                              RCODE = as.character(blocks[[j]]),
                                              stop(tg, " should not get here"))
                               txt <- paste(txt, this, sep = "")
                               blocks[[j]] <- structure("", Rd_tag = "COMMENT")
                               if(grepl("\n$", txt)) {
                                   res <- try(parse(text = paste("a", txt)))
                                   if(!inherits(res, "try-error")) break
                               }
                               j <- j + 1
                           }
                           #print(txt)
                           txt <- sub("\\(([^,]*),\\s*", "\\1@generic@", txt)
                           txt <- sub("@generic@", generic, txt, fixed = TRUE)
                           if (generic == "[")
                               txt <- sub(")([^)]*)$", "]\\1", txt)
                           else if (generic == "[[")
                               txt <- sub(")([^)]*)$", "]]\\1", txt)
                           else if (generic == "$")
                               txt <- sub(")([^)]*)$", "\\1", txt)
                           #print(txt)
                           if (grepl("<-\\s*value", txt))
                               of1("## S3 replacement method for class '")
                           else
                               of1("## S3 method for class '")
                           writeCodeBlock(block[[2L]], tag)
                           of1("':\n")
                           blocks[[i+1]] <- structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               of1('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1]][[1L]])) {
                               of1("## S3 replacement method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               of1("':\n")
                           }else {
                               of1("## S3 method for class '")
                               writeCodeBlock(block[[2L]], tag)
                               of1("':\n")
                           }
                           writeCodeBlock(block[[1L]], tag)
                       }
                   },
                   "\\special" =,
                   UNKNOWN =,
                   VERB =,
                   RCODE =,
                   TEXT = codeB(block),
                   "\\var" = writeCodeBlock(block, tag),
                   "\\dots" =,
                   "\\ldots" = of1("..."),
                   "\\donttest" = writeCodeBlock(block, tag),
                   "\\dontrun"= writeDR(block, tag),
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   "\\S4method" = {
                       of1("## S4 method for signature '")
                       writeCodeBlock(block[[2L]], tag)
                       of1("':\n")
                       writeCodeBlock(block[[1L]], tag)
                   },
                   stopRd(block, "Tag ", tag, " not expected in code block.")
                   )
        }
    }
    writeContent <- function(blocks, blocktag) {
        itemskip <- FALSE

	blocks <- preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
                   "\\item" = {
                       switch(blocktag,
                              "\\describe" = {
                                  text <<- ""
                                  writeContent(block[[1L]], tag)
                                  DLlab <<- c(DLlab, text)
                                  ## FICME: handle paras here
                                  text <<- ""
                                  writeContent(block[[2L]], tag)
                                  DLval <<- c(DLval, text)
                                  text <<- ""
                              },
                              "\\value"=,
                              "\\arguments"= writeArgItem(block, tag),
                              "\\enumerate" =,
                              "\\itemize"=  startEnumItem(block, tag)
                              )
                       itemskup <- TRUE
                   },
               { # default
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
    }

    writeSection <- function(section, tag) {
        text <<- ""; indent <<- 5L
        if (tag == "\\section") {
            Of(txt_sect(as.character(section[[1L]])))
            Of("")
    	    writeContent(section[[2L]], tag)
            writePara()
            Of("")
    	} else if (tag %in% c("\\usage", "\\synopsis", "\\examples")) {
            Of(txt_sect(sectionTitles[tag]))
            writeCodeBlock(section, tag)
            Of(text)
        } else {
            Of(c(txt_sect(sectionTitles[tag]), ""))
            writeContent(section, tag)
            writePara()
            Of("")
        }
    }

    Rdfile <- "not known"

    if (is.character(Rd)) {
        Rdfile <- Rd
        ## do it this way to get info in internal warnings
        Rd <- eval(substitute(parse_Rd(f, encoding = enc),
                              list(f = Rd, enc = encoding)))
    } else if(inherits(Rd, "connection")) {
        Rdfile <- summary(Rd)
        Rd <- tools::parse_Rd(Rd, encoding = encoding)
    }

    if (is.character(out)) {
        if(out == "") con <- stdout()
        else {
	    con <- file(out, "w")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    ## Process top level ifdef's.
    Rd <- preprocessRd(Rd, defines)
    sections <- attr(Rd, "RdTags")

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1L && as.numeric(version[[1L]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1L)
    	stopRd(Rd[[version[2L]]], "Only one \\Rdversion declaration is allowed")

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1L)
    	    stopRd(Rd[[enc[2L]]], "Only one \\encoding declaration is allowed")
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, "Encoding must be plain text")
    	encoding <- trim(encoding[[1L]])
        if (encoding != "unknown")
            encoding <- latex_canonical_encoding(encoding)
    }

    ## Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	stopRd(Rd[sections == "TEXT"][[bad[1L]]], "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding",
                            "\\keyword", "\\alias", "\\Rdversion")
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

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading and trailing whitespace, \n
    title <- trim(paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                        collapse=" "))
    title <- gsub("\n", "", title, fixed = TRUE)

    name <- Rd[[2L]]
    tags <- RdTags(name)
    if (length(tags) > 1L) stopRd(name, "\\name must only contain simple text.")

    name <- trim(name[[1L]])

    if(nzchar(package)) {
        left <- name
        mid <- if(nzchar(package)) paste("package:", package, sep = "") else ""
        right <- "R Documentation"
        if(encoding != "unknown")
            right <- paste(right, "(", encoding, ")", sep="")
        pad <- HDR_WIDTH - nchar(left, "w") - nchar(mid, "w") - nchar(right, "w")
        pad0 <- pad %/% 2L
        pad1 <- paste(rep.int(" ", pad0), collapse = "")
        pad2 <- paste(rep.int(" ", pad - pad0), collapse = "")
        Of(paste(left, pad1, mid, pad2, right, "\n", sep=""))
    }

    Of(c(txt_header(txt_striptitle(title)), ""))

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    out
}
