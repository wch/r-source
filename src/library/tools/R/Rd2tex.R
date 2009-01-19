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


## Things to do
## munging of links and aliases
## encoding
## yet more escapes, I expect

Rd2latex <-
    function(Rd, out="", defines=.Platform$OS.type, encoding="unknown")
{
    of <- function(...) writeLines(paste(...), con, sep = '')
    of0 <- function(...) writeLines(paste(..., sep=""), con, sep ="")
    of1 <- function(text) writeLines(text, con, sep = "")

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

    inCodeBlock <- FALSE ## used to indicate to texify where we are
    inCode <- FALSE

    ltxtags <- c("\\verb"="PRE")

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2]
        if (tools:::isBlankLineRd(x)) "\n"
	else if (start == 1) gsub("^\\s+", "", x, perl = TRUE)
        else x
    }

    ## Needed?
    ltxeqn <- function(x) {
        x
    }

    ## FIXME: what other substitutions do we need?
    texify <- function(x) {
        x <- gsub("([&$%_])", "\\\\\\1", x)
#        x <- gsub("{", "\\textbraceleft}", x, fixed = TRUE)
#        x <- gsub("}", "\\textbraceright}", x, fixed = TRUE)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde}", x, fixed = TRUE)
        x
    }

    ## version for RCODE and VERB
    vtexify <- function(x) {
        x <- gsub("\\\\[l]+dots", "...", x)
        if (inCodeBlock) return(x)
        x <- gsub("([&$%_])", "\\\\\\1", x)
        x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
        x <- gsub("~", "\\textasciitilde}", x, fixed = TRUE)
        ## avoid conversion to guillemets
        x <- gsub("<<", "<{}<", x, fixed = TRUE)
        x <- gsub(">>", ">{}>", x, fixed = TRUE)
        x
    }

    ## only used for \verb, not right
    writeWrapped <- function(tag, block) {
    	of0("\\", ltxtags[tag], "{")
    	writeContent(block, tag)
    	of1("}")
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

    writeLink <- function(tag, block) {
        ## FIXME
        link <- as.character(block)
        of0("\\LinkA{", link, "}{", link, "}")
    }

    writeComment <- function(txt) {
	of0(txt, '\n') ## FIXME, should include the \n
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1) {
            of1('## Not run: ') # had trailing space before: FIXME remove
            writeContent(block, tag)
            ## FIXME only needs a \n here if not at left margin
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
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

    latex_code_aliasAA <- function(x)
    {
        x <- latex_code_trans(x)
        x <- latex_link_trans(x)
        gsub("\\\\([!|])", '"\\1', x, perl = TRUE)
    }

    latex_code_alias <- function(x)
    {
        x <- gsub("([&$%_])", "\\\\\\1", x)
        x <- gsub("<-", "<\\Rdash", x)
        gsub("\\\\([!|])", '"\\1', x, perl = TRUE)
        x
    }

    writeAlias <- function(block, tag) {
        ## FIXME much more here, including methalias
        alias <- latex_code_alias(block)
        ## These break PDF-indexing, and name is linked from the header
        if (alias %in% c(name, "(", "{", "{-class")) return()
        alias2 <- latex_link_trans0(alias)
        of0("\\aliasA{", alias, "}{", name, "}{", alias2, "}\n")
    }

    writeBlock <- function(block, tag, blocktag) {
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = of1(vtexify(block)),
               TEXT = of1(addParaBreaks(texify(block), blocktag)),
               COMMENT = {},
               LIST =,
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
                   inCodeBlock <<- TRUE
                   of1("\\begin{alltt}\n")
                   writeContent(block, tag) # FIXME, translations?
                   of1("\\end{alltt}\n")
                   inCodeBlock <<- FALSE
               },
               "\\verb"= writeWrapped(tag, block),
               "\\special"= writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               "\\cr" = of1("\\\\"),
               ## FIXME: code is similar
               "\\dots" =,
               "\\ldots" = of1(if(inCode || inCodeBlock) "..."  else tag),
               "\\R" = of0(tag, "{}"),
               "\\donttest" = writeContent(block, tag),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" =  # FIXME:  this could sometimes use the first arg -- does not at present, though
               writeContent(block[[2]], tag),
               "\\eqn" =,
               "\\deqn" = {
                   of0(tag, "{", ltxeqn(block[[1]]),'}{}')
               },
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
                       writeContent(block[[2]], tag)
                       of1("':\n")
                   }
                   writeContent(block[[1]], tag)
               },
               "\\S4method" = {
                   of1("## S4 method for signature '")
                   writeContent(block[[2]], tag)
                   of1("':\n")
                   writeContent(block[[1]], tag)
               },
               "\\tabular" = writeTabular(block),
               tools:::stopRd(block, "Tag ", tag, " not recognized.")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1]]
    	content <- table[[2]]
    	if (length(format) != 1 || tools:::RdTags(format) != "TEXT")
    	    tools:::stopRd(table, "\\tabular format must be simple text")
        content <- tools:::preprocessRd(content, defines)
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

    writeContent <- function(blocks, blocktag) {
        inList <- FALSE
        itemskip <- FALSE

	blocks <- tools:::preprocessRd(blocks, defines)
	tags <- attr(blocks, "RdTags")

	for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag,
                   "\\item" = {
                       if (blocktag == "\\value" && !inList) {
                           of1("\\begin{ldescription}\n")
                           inList <- TRUE
                       }
                       switch(blocktag,
                              "\\describe" = {
                                  of1('\\item[')
                                  writeContent(block[[1]], tag)
                                  of1('] ')
                                  writeContent(block[[2]], tag)
                              },
                              "\\value"=,
                              "\\arguments"={
                                  of1('\\item[\\code{')
                                  writeContent(block[[1]], tag)
                                  of1('}] ')
                                  writeContent(block[[2]], tag)
                              },
                              "\\enumerate" =,
                              "\\itemize"= of1("\\item "))
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

    writeSection <- function(section, tag) {
        if (tag == "\\alias")
            writeAlias(section, tag)
        else if (tag == "\\keyword") {
            section <- sub("^\\s*", "", section, perl = TRUE)
            section <- sub("\\s*$", "", section, perl = TRUE)
            of0("\\keyword{", as.character(section), "}{", name, "}\n")
        } else if (tag == "\\section") {
            of0("%\n\\begin{Section}{", section[[1]], "}\n")
    	    writeContent(section[[2]], tag)
            of1("\\end{Section}\n")
    	} else {
            title <- envTitles[tag]
            of0("%\n\\begin{", title, "}")
            if(tag %in% c("\\author", "\\description", "\\details", "\\note",
                          "\\references", "\\seealso", "\\source"))
                of1("\\relax ")
            ## space needed for
            ## \note{This function may not be implemented on all systems.}
            extra <- sectionExtras[tag]
            if(!is.na(extra)) of0("\n\\begin{", extra, "}")

            if(tag %in% c("\\usage", "\\examples")) inCodeBlock <<- TRUE
            writeContent(section, tag)
            inCodeBlock <<- FALSE
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
    Rd <- tools:::preprocessRd(Rd, defines)
    sections <- attr(Rd, "RdTags")

    ## Print initial comments
    ## for (i in seq_along(sections)) {
    ## 	if (sections[i] != "COMMENT") break
    ##	writeComment(Rd[[i]])
    ##}

    version <- which(sections == "\\Rdversion")
    if (length(version) == 1 && as.numeric(version[[1]]) < 2)
    	warning("Rd2HTML is designed for Rd version 2 or higher.")
    else if (length(version) > 1)
    	tools:::stopRd(Rd[[version[2]]], "Only one \\Rdversion declaration is allowed")

    enc <- which(sections == "\\encoding")
    if (length(enc)) {
    	if (length(enc) > 1)
    	    tools:::stopRd(Rd[[enc[2]]], "Only one \\encoding declaration is allowed")
    	encoding <- Rd[[enc]]
    	if (!identical(tools:::RdTags(encoding), "TEXT"))
    	    tools:::stopRd(encoding, "Encoding must be plain text")
    	encoding <- encoding[[1]]
    }

    ## Give error for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]", unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	tools:::stopRd(Rd[sections == "TEXT"][[bad[1]]], "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding", "\\Rdversion")
    Rd <- Rd[!drop]
    sections <- sections[!drop]

    sortorder <- tools:::sectionOrder[sections]
    if (any(bad <- is.na(sortorder)))
    	tools:::stopRd(Rd[[which(bad)[1]]], "Section ", sections[which(bad)[1]], " unrecognized.")
    ## Need to sort the aliases.
    nm <- character(length(Rd))
    isAlias <- tools:::RdTags(Rd) == "\\alias"
    nm[isAlias] <- sapply(Rd[isAlias], as.character)
    sortorder <- order(sortorder, toupper(nm), nm)
    Rd <- Rd[sortorder]
    sections <- sections[sortorder]
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	tools:::stopRd(Rd, "Sections \\title, and \\name must exist and be unique in Rd files.")

    title <- as.character(Rd[[1]])
    ## remove empty lines, leading whitespace
    title <- paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                   collapse=" ")
    title <- sub("^\\s*", "", title, perl = TRUE)
    title <- sub("\\s*$", "", title, perl = TRUE)
    ## substitutions?

    name <- Rd[[2]]
    tags <- tools:::RdTags(name)
    if (length(tags) > 1) tools:::stopRd(name, "\\name must only contain simple text.")

    name <- name[[1]]
    ## FIXME, yes, escapes needed in name

    of0('\\HeaderA{', name, '}{', title, '}{', name, '}\n')

    for (i in seq_along(sections)[-(1:2)])
        writeSection(Rd[[i]], sections[i])

    out
}
