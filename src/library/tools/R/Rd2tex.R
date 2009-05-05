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

## Remaining problems
## escapes in comments in codeblocks (kappa, qr, qraux, source)
## [cosmetic) white-spacing in \[d]eqn (Random, Special, polyroot)


latex_canonical_encoding  <- function(encoding)
{
    encoding <- tolower(encoding)
    encoding <- sub("iso_8859-([0-9]+)", "iso-8859-\\1", encoding)

    encoding[encoding == "iso-8859-1"] <-  "latin1"
    encoding[encoding == "iso-8859-2"] <-  "latin2"
    encoding[encoding == "iso-8859-3"] <-  "latin3"
    encoding[encoding == "iso-8859-4"] <-  "latin4"
    encoding[encoding == "iso-8859-5"] <-  "cyrillic"
    encoding[encoding == "iso-8859-6"] <-  "arabic"
    encoding[encoding == "iso-8859-7"] <-  "greek"
    encoding[encoding == "iso-8859-1"] <-  "hebrew"
    encoding[encoding == "iso-8859-9"] <-  "latin5"
    encoding[encoding == "iso-8859-10"] <-  "latin6"
    encoding[encoding == "iso-8859-14"] <-  "latin8"
    encoding[encoding %in% c("latin-9", "iso-8859-15")] <-  "latin9"
    encoding[encoding == "iso-8859-16"] <-  "latin10"
    encoding[encoding == "utf-8"] <-  "utf8"
    encoding
}


Rd2latex <- function(Rd, out="", defines=.Platform$OS.type, encoding="unknown")
{
    last_char <- ""
    of0 <- function(...) of1(paste(..., sep=""))
    of1 <- function(text) {
        Encoding(text) <- "unknown"
        writeLines(text, con, sep = "")
        ## FIXME: this depends on a single-byte locale
        nc <- nchar(text)
        last_char <<- substr(text, nc, nc)
    }

    trim <- function(x) {
        x <- sub("^\\s*", "", x, perl = TRUE)
        sub("\\s*$", "", x, perl = TRUE)
    }

    envTitles <- c("\\description"="Description", "\\usage"="Usage",
        "\\synopsis"="Usage", "\\arguments"="Arguments",
        "\\format"="Format", "\\details"="Details", "\\note"="Note",
        "\\section"="", "\\author"="Author",
        "\\references"="References", "\\source"="Source",
        "\\seealso"="SeeAlso", "\\examples"="Examples",
        "\\value"="Value")

    sectionExtras <-
    c("\\usage"="verbatim",
      "\\synopsis"="verbatim",
      "\\arguments"="ldescription",
      "\\examples"="ExampleCode")

    inCodeBlock <- FALSE ## used to indicate to vtexify where we are
    inCode <- FALSE
    inEqn <- FALSE
    inPre <- FALSE

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2L]
        if (isBlankLineRd(x)) "\n"
	else if (start == 1) gsub("^\\s+", "", x, perl = TRUE)
        else x
    }

    ltxeqn <- function(x) {
#        gsub("([{}%])", "\\\\\\1", x)
        x
    }

    ## FIXME: what other substitutions do we need?
    texify <- function(x) {
        if(inEqn) return(ltxeqn(x))
        x <- gsub("([&$%_#])", "\\\\\\1", x)
        ## pretty has braces in text.
        x <- gsub("{", "\\{", x, fixed = TRUE)
        x <- gsub("}", "\\}", x, fixed = TRUE)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
        x
    }

    ## Use something like gsub("(?<!\\\\)x", "a", x, perl= TRUE)

    ## version for RCODE and VERB
    ## inCodeBlock is in alltt, where only \ { } have their usual meaning
    vtexify <- function(x) {
        if(inEqn) return(ltxeqn(x))
        x <- gsub("\\\\[l]+dots", "...", as.character(x))
        ## unescape (should not be escaped: but see kappa.Rd)
        x <- gsub("\\\\([$^&~_#])", "\\1", x)
        if (inCodeBlock) {
            ## We do want to escape { }, but unmatched braces had
            ## to be escaped in earlier versions (e.g. Paren.Rd, body.tex).
            ## So fix up for now
            x <- sub('"\\{"', '"{"', x, fixed = TRUE)
            return(x)
        }
        if (inPre) {
            x <- gsub("(?<!\\\\)\\{", "\\\\{", x, perl= TRUE)
            x <- gsub("(?<!\\\\)}", "\\\\}", x, perl= TRUE)
            return(x)
        }
        BSL = '@BSL@';
        x <- gsub("\\", BSL, x, fixed = TRUE)
        x <- gsub("(?<!\\\\)\\{", "\\\\{", x, perl= TRUE)
        x <- gsub("(?<!\\\\)}", "\\\\}", x, perl= TRUE)
        x <- gsub("(?<!\\\\)([&$%_#])", "\\\\\\1", x, perl= TRUE)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
        x <- gsub(BSL, "\\bsl{}", x, fixed = TRUE)
        ## avoid conversion to guillemets
        x <- gsub("<<", "<{}<", x, fixed = TRUE)
        x <- gsub(">>", ">{}>", x, fixed = TRUE)
        x <- gsub(",,", ",{},", x, fixed = TRUE) # ,, is a ligature in the ae font.
        x <- gsub("\\\\bsl{}var\\\\{([^}]+)\\\\}", "\\\\var{\\1}", x, perl = TRUE)
        x
    }

    ## FIXME only used for \verb, not right yet
    writeWrapped <- function(tag, block) {
    	of0("\\verb|")
    	writeContent(block, tag)
    	of1("|")
    }

    writePass <- function(block, tag) {
    	of0(tag, "{")
    	writeContent(block, tag)
    	of1("}")
    }

    writeRlike <- function(block, tag) {
    	of0(tag, "{")
    	writeContent(block, tag)
    	of1("}")
    }

    writeVerb <- function(block, tag) {
        ## no interpretation needed
    	of0(tag, "{")
    	writeContent(block, tag)
    	of1("}")
    }

    ## could be more complicated, e.g. force.Rd
    writeLink <- function(tag, block) {
        parts <- get_link(block, tag)
        of0("\\LinkA{", latex_escape_name(parts$topic), "}{",
            latex_link_trans0(parts$dest), "}")
    }

    writeComment <- function(txt) {
	of0(txt, '\n') ## FIXME, should include the \n
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

    ltxstriptitle <- function(x)
    {
        x <- gsub("\\R", "\\R{}", x, fixed = TRUE)
        x <- gsub("(?<!\\\\)([&$%_#])", "\\\\\\1", x, perl= TRUE)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
        x
    }

    latex_escape_name <- function(x)
    {
        x <- gsub("([$#~_&])", "\\\\\\1", x) #- escape them
        x <- gsub("{", "\\textbraceleft{}", x, fixed = TRUE)
        x <- gsub("|", "\\textbraceright{}", x, fixed = TRUE)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
        x <- gsub("%", "\\Rpercent{}", x, fixed = TRUE)
        x <- gsub("\\\\", "\\textbackslash{}", x, fixed = TRUE)
        ## avoid conversion to guillemets
        x <- gsub("<<", "<{}<", x, fixed = TRUE)
        x <- gsub(">>", ">{}>", x, fixed = TRUE)
        x
    }

    latex_link_trans0 <- function(x)
    {
        x <- gsub("\\\\Rdash", ".Rdash.", x, perl=TRUE)
        x <- gsub("-", ".Rdash.", x, perl=TRUE)
        x <- gsub("\\\\_", ".Rul.", x, perl=TRUE)
        x <- gsub("\\\\\\$", ".Rdol.", x, perl=TRUE)
        x <- gsub("\\\\\\^", ".Rcaret.", x, perl=TRUE)
        x <- gsub("\\^", ".Rcaret.", x, perl=TRUE)
        x <- gsub("_", ".Rul.", x, perl=TRUE)
        x <- gsub("\\$", ".Rdol.", x, perl=TRUE)
        x <- gsub("\\\\#", ".Rhash.", x, perl=TRUE) #
        x <- gsub("#", ".Rhash.", x, perl=TRUE)   #
        x <- gsub("\\\\&", ".Ramp.", x, perl=TRUE)
        x <- gsub("&", ".Ramp.", x, perl=TRUE)
        x <- gsub("\\\\~", ".Rtilde.", x, perl=TRUE)
        x <- gsub("~", ".Rtilde.", x, perl=TRUE)
        x <- gsub("\\\\%", ".Rpcent.", x, perl=TRUE)
        x <- gsub("%", ".Rpcent.", x, perl=TRUE)
        x <- gsub("\\\\\\\\", ".Rbl.", x, perl=TRUE)
        x <- gsub("\\{", ".Rlbrace.", x, perl=TRUE)
        x <- gsub("\\}", ".Rrbrace.", x, perl=TRUE)
        x
    }

    latex_code_trans  <- function(x)
    {
        BSL = '@BSL@';
        LATEX_SPECIAL = '$^&~_#'

        if(grepl(LATEX_SPECIAL, x)) {
            x <- gsub("\\\\\\\\", BSL, x)
            ## unescape (should not be escaped)
            x <- gsub("\\\\([$^&~_#])", "\\1", x)
            x <- gsub("[$^&~_#]", "\\1&", x) #- escape them
            x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE) # ^ is SPECIAL
            x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
            x <- gsub(BSL, "\\bsl{}", x, fixed = TRUE)
            x <- gsub("\\", "\\bsl{}", x, fixed = TRUE)
        }
        ## avoid conversion to guillemets
        x <- gsub("<<", "<{}<", x, fixed = TRUE)
        x <- gsub(">>", ">{}>", x, fixed = TRUE)
        x <- gsub(",,", ",{},", x, fixed = TRUE) # ,, is a ligature in the ae font.
        x <- gsub("\\\\bsl{}var\\\\{([^}]+)\\\\}", "\\var{\\1}", x)
        x
}

    latex_link_trans <- function(x)
    {
        x <- gsub("<-\\.", "<\\Rdash.", x)
        x <- gsub("<-$", "<\\Rdash", x)
        x
    }

    latex_code_alias <- function(x)
    {
        ## FIXME do better
        x <- gsub("{", "\\{", x, fixed = TRUE)
        x <- gsub("}", "\\}", x, fixed = TRUE)
        x <- gsub("(^|[^\\])([&$%_])", "\\1\\\\\\2", x)
        x <- gsub("(^|[^\\])([&$%_])", "\\1\\\\\\2", x)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
        x <- gsub("<-", "<\\Rdash", x, fixed = TRUE)
        gsub("([!|])", '"\\1', x, perl = TRUE)
    }

    latex_code_aliasAA <- function(x)
    {
        x <- latex_code_trans(x)
        x <- latex_link_trans(x)
        gsub("\\\\([!|])", '"\\1', x, perl = TRUE)
    }

    currentAlias <- NA_character_

    writeAlias <- function(block, tag) {
        alias <- as.character(block)
        aa <- "\\aliasA{"
        if(is.na(currentAlias)) currentAlias <<- name
        if (pmatch(paste(currentAlias, ".", sep=""), alias, 0L)) {
            aa <- "\\methaliasA{"
        } else currentAlias <<- alias
        ## 'name' is linked from the header
        if (alias == name) return()
        alias2 <- latex_link_trans0(alias)
        of0(aa, latex_code_alias(alias), "}{",
            latex_escape_name(name), "}{", alias2, "}\n")
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = of1(vtexify(block)),
               TEXT = of1(addParaBreaks(texify(block), blocktag)),
               COMMENT = {},
               LIST = writeContent(block, tag),
               "\\describe"= {
                   of1("\\describe{")
                   writeContent(block, tag)
                   of1("}")
               },
               "\\enumerate"={
                   of1("\\Enumerate{")
                   writeContent(block, tag)
                   of1("}")
               },
               "\\itemize"= {
                   of1("\\Itemize{")
                   writeContent(block, tag)
                   of1("}")
               },
               ## Verbatim-like
               "\\command"=,
               "\\env" =,
               "\\kbd"=,
               "\\option" =,
               "\\samp" = writeVerb(block, tag),
               ## really verbatim
                "\\url"= of0("\\url{", as.character(block), "}"),
               ## R-like
               "\\code"= {
                   inCode <<- TRUE
                   writeRlike(block, tag)
                   inCode <<- FALSE
               },
               ## latex-like
               "\\acronym" =,
               "\\bold"=,
               "\\cite"=,
               "\\dfn"=,
               "\\dQuote"=,
               "\\email"=,
               "\\emph"=,
               "\\file" =,
               "\\pkg" =,
               "\\sQuote" =,
                "\\strong"=,
                "\\var" = writePass(block, tag),
               "\\preformatted"= {
                   inPre <<- TRUE
                   of1("\\begin{alltt}")
                   writeContent(block, tag)
                   of1("\\end{alltt}\n")
                   inPre <<- FALSE
               },
               "\\verb"= writeWrapped(tag, block),
               "\\special"= writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               "\\cr" = of1("\\\\{}"), ## might be followed by [
               "\\dots" =,
               "\\ldots" = of1(if(inCode || inCodeBlock) "..."  else tag),
               "\\R" = of0(tag, "{}"),
               "\\donttest" = writeContent(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = {
                   txt <- as.character(if(encoding !=" unknown") block[[1L]] else block[[2L]])
                   Encoding(txt) <- "unkownn"
                   of1(txt)
               } ,
               "\\eqn" =,
               "\\deqn" = {
                   of0(tag, "{")
                   inEqn <<- TRUE
                   writeContent(block[[1L]], tag)
                   inEqn <<- FALSE
                   of0('}{}')
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" = {
                   ## should not get here
##                    ## FIXME: special methods for [ [<- and operators
##                    class <- as.character(block[[2L]])
##                    if (class == "default")
##                        of1('## Default S3 method:\n')
##                    else {
##                        of1("## S3 method for class '")
##                        writeContent(block[[2L]], tag)
##                        of1("':\n")
##                    }
##                    writeContent(block[[1L]], tag)
               },
               "\\S4method" = {
                   of1("## S4 method for signature '")
                   writeContent(block[[2L]], tag)
                   of1("':\n")
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
        content <- preprocessRd(content, defines)
        tags <- attr(content, "RdTags")
        of0('\n\\Tabular{', format, '}{')
        for (i in seq_along(tags)) {
            switch(tags[i],
                   "\\tab" = of1("&"),
                   "\\cr" = of1("\\\\{}"),
                   writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        of1('}')
    }

    writeContent <- function(blocks, blocktag) {
        inList <- FALSE
        itemskip <- FALSE

	blocks <- preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")

	for (i in seq_along(tags)) {
            block <- blocks[[i]]
            switch(tag <- attr(block, "Rd_tag"),
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
                           writeContent(block[[2L]], tag)
                           of1("':\n")
                           blocks[[i+1]] <- structure(txt, Rd_tag = "RCODE")
                       } else {
                           if (class == "default")
                               of1('## Default S3 method:\n')
                           else if (grepl("<-\\s*value", blocks[[i+1]][[1L]])) {
                               of1("## S3 replacement method for class '")
                               writeContent(block[[2L]], tag)
                               of1("':\n")
                           }else {
                               of1("## S3 method for class '")
                               writeContent(block[[2L]], tag)
                               of1("':\n")
                           }
                           writeContent(block[[1L]], tag)
                       }
                   },
                   "\\item" = {
                       if (blocktag == "\\value" && !inList) {
                           of1("\\begin{ldescription}\n")
                           inList <- TRUE
                       }
                       switch(blocktag,
                              "\\describe" = {
                                  of1('\\item[')
                                  writeContent(block[[1L]], tag)
                                  of1('] ')
                                  writeContent(block[[2L]], tag)
                              },
                              "\\value"=,
                              "\\arguments"={
                                  of1('\\item[\\code{')
                                  inCode <<- TRUE
                                  writeContent(block[[1L]], tag)
                                  inCode <<- FALSE
                                  of1('}] ')
                                  writeContent(block[[2L]], tag)
                              },
                              "\\enumerate" =,
                              "\\itemize"= {
                                  of1("\\item ")
                                  itemskip <- TRUE
                              })
                       itemskup <- TRUE
                   },
               { # default
                   if (inList && !(tag == "TEXT" && isBlankRd(block))) {
                       of1("\\end{ldescription}\n")
                       inList <- FALSE
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
        if (inList) of1("\\end{ldescription}\n")
    }

    writeSectionInner <- function(section, tag)
    {
        if (length(section)) {
	    ## need \n unless one follows, so
	    nxt <- section[[1L]]
	    if (!attr(nxt, "Rd_tag") %in% c("TEXT", "RCODE") ||
		substr(as.character(nxt), 1L, 1L) != "\n") of1("\n")
	    writeContent(section, tag)
	    inCodeBlock <<- FALSE
	    if (last_char != "\n") of1("\n")
	}
    }

    writeSection <- function(section, tag) {
        if (tag == "\\alias")
            writeAlias(section, tag)
        else if (tag == "\\keyword") {
            key <- trim(section)
            of0("\\keyword{", latex_escape_name(key), "}{", ltxname, "}\n")
        } else if (tag == "\\section") {
            of0("%\n\\begin{Section}{")
            writeContent(section[[1L]], tag)
            of1("}")
    	    writeSectionInner(section[[2L]], tag)
            of1("\\end{Section}\n")
    	} else {
            title <- envTitles[tag]
            of0("%\n\\begin{", title, "}")
            if(tag %in% c("\\author", "\\description", "\\details", "\\note",
                          "\\references", "\\seealso", "\\source"))
                of1("\\relax")
            extra <- sectionExtras[tag]
            if(!is.na(extra)) of0("\n\\begin{", extra, "}")
            if(tag %in% c("\\usage", "\\examples")) inCodeBlock <<- TRUE
            writeSectionInner(section, tag)
            if(!is.na(extra)) of0("\\end{", extra, "}\n")
            of0("\\end{", title, "}\n")
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

    ## Print initial comments
    ## for (i in seq_along(sections)) {
    ## 	if (sections[i] != "COMMENT") break
    ##	writeComment(Rd[[i]])
    ##}

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
        if (encoding != "unknown") {
            of0("\\inputencoding{", latex_canonical_encoding(encoding), "}\n")
        }
    }

    ## Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	stopRd(Rd[sections == "TEXT"][[bad[1L]]], "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding", "\\Rdversion")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	stopRd(Rd[[which(bad)[1L]]], "Section ", sections[which(bad)[1L]], " unrecognized.")
    ## Need to sort the aliases.
    nm <- character(length(Rd))
    isAlias <- RdTags(Rd) == "\\alias"
    nm[isAlias] <- sapply(Rd[isAlias], as.character)
    sortorder <- order(sortorder, toupper(nm), nm)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, "Sections \\title, and \\name must exist and be unique in Rd files.")

    title <- as.character(Rd[[1L]])
    ## remove empty lines, leading whitespace
    title <- trim(paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                        collapse=" "))
    ## substitutions?

    name <- Rd[[2L]]
    tags <- RdTags(name)
    if (length(tags) > 1L) stopRd(name, "\\name must only contain simple text.")

    name <- trim(as.character(name[[1L]]))
    ltxname <- latex_escape_name(name)

    of0('\\HeaderA{', ltxname, '}{',
        ltxstriptitle(title), '}{',
        latex_link_trans0(name), '}\n')

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    out
}
