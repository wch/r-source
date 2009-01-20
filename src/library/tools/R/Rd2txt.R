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
## Listing environments are minimal at best, \\tabular
## e.g. DateTimeClasses
## conversion of -- --- (Extract.factor), `` (NA)
## odd list() in links e.g. Logic
## \\dontrun appears twice

Rd2txt <-
    function(Rd, out="", package = "", defines=.Platform$OS.type)
{
    WIDTH <- 75L

    ## we need to keep track of where we are.
    nch <- 0L
    text <- ""
    indent <- 0L

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
            lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
            nl <- length(lines)
             if(!nl) return()
            ind <- paste(rep(" ", indent), collapse = "")
            pad <- rep(ind, nl)
            if(nch) pad[1] <- ""
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
            text <- gsub("(``|'')", '"', text, fixed = TRUE)
            text <- gsub("`", "'", text, fixed = TRUE)
        }
        text
    }

    ## underline via backspacing
    txt_header <- function(header) {
        letters <- strsplit(header, "")[[1]]
        isaln <- grep("[[:alnum:]]", letters)
        letters[isaln] <- paste("_\b", letters[isaln], sep="")
        paste(letters, collapse = "")
    }

    txt_sect <- function(header) paste(txt_header(header), ":", sep = "")

    inCodeBlock <- FALSE ## used to indicate to texify where we are

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2]
        if (isBlankLineRd(x)) {
            writePara()
            Of("")
	} else {
            if (start == 1) x <- gsub("^\\s+", "", x, perl = TRUE)
            of1(x)
        }
    }

    ## Needed?
    txt_eqn <- function(x) {
        x
    }

    writeComment <- function(txt) {
	of1("\nn") ## FIXME, should include the \n?
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1) {
            of1('## Not run: ') # had trailing space before: FIXME remove
            writeCodeBlock(block, tag)
            ## FIXME only needs a \n here if not at left margin
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeCodeBlock(block, tag)
       }
    }


    writeQ <- function(block, tag)
    {
        if (.Platform$OS.type == "windows" && Sys.getlocale("LC_CTYPE") != "C") {
            if (tag == "\\sQuote") of0("\x91", as.character(block), "\x92")
            else of0("\x93", as.character(block), "\x94")
        } else {
            if (tag == "\\sQuote") of0("'", as.character(block), "'")
            else of0("\"", as.character(block), "\"")
        }
    }

    writeArgItem <- function(block, tag)
    {
        writePara()
        text0 <- text
        indent0 <- indent
        text <<- ""; indent <<- 0
        writeContent(block[[1]], tag)
        label <- format(paste(text, ":", sep = ""),
                        justify = "right", width = 9)
        text <<- ""
        writeContent(block[[2]], tag)
        text1 <- paste(strwrap(text, indent = 0, exdent = 10, width = WIDTH,
                               initial = paste(label,"")),
                       collapse = "\n")
        Of(text1)
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
               RCODE = of1(as.character(block)),
               TEXT = addParaBreaks(block, blocktag),
               COMMENT = {},
               LIST =,
               "\\describe"= {
                   writePara()
                   of1("\\describe{")
                   writeContent(block, tag)
                   of1("}")
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
                "\\strong"= of0("*", as.character(block), "*"),
               "\\emph"= of0("_", as.character(block), "_"),
               "\\sQuote" =,
               "\\dQuote"= writeQ(block, tag) ,
               "\\preformatted"= {
                   inCodeBlock <<- TRUE
                   of1("\\begin{alltt}\n")
                   writeContent(block, tag) # FIXME, translations?
                   of1("\\end{alltt}\n")
                   inCodeBlock <<- FALSE
               },
               "\\verb"= of1(block),
               "\\linkS4class" =,
               "\\link" = writeContent(block, tag),
               "\\cr" = of1("\n"),
               "\\dots" =,
               "\\ldots" = of1("..."),
               "\\R" = of1("R"),
               "\\enc" = {
                   txt <- as.character(iblock[[2]])
                   of1(txt)
               } ,
               "\\eqn" = {
                   if (length(block) == 2 && is.list(block[[1]]))
                       block <- block[[2]]
                   writeContent(block, tag)
               },
               "\\deqn" = {
                   writePara()
                   if (length(block) == 2 && is.list(block[[1]]))
                       block <- block[[2]]
                   writeContent(block, tag)
                   writeDeqn()
               },
               "\\tabular" = writeTabular(block),
               stopRd(block, "Tag ", tag, " not recognized.")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1]]
    	content <- table[[2]]
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
            switch(tag,
                   "\\special" =,
                   UNKNOWN =,
                   VERB =,
                   RCODE =,
                   TEXT = of1(as.character(block)),
                   "\\var" = writeCodeBlock(block, tag),
                   "\\dots" =,
                   "\\ldots" = of1("..."),
                   "\\donttest" = writeCodeBlock(block, tag),
                   "\\dontrun"= writeDR(block, tag),
                   COMMENT =,
                   "\\dontshow" =,
                   "\\testonly" = {}, # do nothing
                   "\\method" =,
                   "\\S3method" = {
                       ## FIXME: special methods for [ [<- and operators
                       class <- as.character(block[[2]])
                       if (class == "default")
                           of1('## Default S3 method:\n')
                       else {
                           of1("## S3 method for class '")
                           writeCodeBlock(block[[2]], tag)
                           of1("':\n")
                       }
                       writeCodeBlock(block[[1]], tag)
                   },
                   "\\S4method" = {
                       of1("## S4 method for signature '")
                       writeCodeBlock(block[[2]], tag)
                       of1("':\n")
                       writeCodeBlock(block[[1]], tag)
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
                                  of1('\\item[')
                                  writeContent(block[[1]], tag)
                                  of1('] ')
                                  writeContent(block[[2]], tag)
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
            Of(txt_sect(as.character(section[[1]])))
            Of("")
    	    writeContent(section[[2]], tag)
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
    if (length(version) == 1 && as.numeric(version[[1]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1)
    	stopRd(Rd[[version[2]]], "Only one \\Rdversion declaration is allowed")

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1)
    	    stopRd(Rd[[enc[2]]], "Only one \\encoding declaration is allowed")
    	encoding <- Rd[[enc]]
    	if (!identical(RdTags(encoding), "TEXT"))
    	    stopRd(encoding, "Encoding must be plain text")
    	encoding <- trim(encoding[[1]])
        if (encoding != "unknown")
            encoding <- latex_canonical_encoding(encoding)
    }

    ## Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	stopRd(Rd[sections == "TEXT"][[bad[1]]], "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding",
                            "\\keyword", "\\alias", "\\Rdversion")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1]]], "Section ", sections[which(bad)[1]], " unrecognized.")
    sortorder <- order(sortorder)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, "Sections \\title, and \\name must exist and be unique in Rd files.")

    title <- as.character(Rd[[1]])
    ## remove empty lines, leading whitespace
    title <- trim(paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                        collapse=" "))

    name <- Rd[[2]]
    tags <- RdTags(name)
    if (length(tags) > 1) stopRd(name, "\\name must only contain simple text.")

    name <- trim(name[[1]])

    if(nzchar(package)) {
        pad <- WIDTH - nchar(name) - nchar(package) - 20L
        if(encoding != "unknowwn") pad <- pad - 2L - nchar(encoding)
        pad <- pad %/% 2L
        pad <- paste(rep.int(" ", pad), collapse = "")
        Of(paste(name, pad, "package:", package, pad, "R Documentation",
                 if(encoding != "unknown") paste("(", encoding, ")", sep=""),
                 "\n", sep = ""))
    }

    Of(c(txt_header(txt_striptitle(title)), ""))

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    out
}
