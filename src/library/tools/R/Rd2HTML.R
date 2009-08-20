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

get_link <- function(arg, tag, Rdfile) {
    ## 'topic' is the name to display, 'dest' is the topic to link to
    ## optionaly in package 'pkg'.  If 'target' is set it is the file
    ## to link to in HTML help

    ## \link[=bar]{foo} means shows foo but treat this as a link to bar.
    ## \link[pkg]{foo} means show foo and link to *file* foo in package pkg
    ## \link{pkg:bar]{foo} means show foo and link to file bar in package pkg.

    if (!all(RdTags(arg) == "TEXT"))
    	stopRd(arg, Rdfile, "Bad \\link text")

    option <- attr(arg, "Rd_option")

    dest <- paste(unlist(arg), collapse = "")
    topic <- dest
    targetfile <- NULL
    pkg <- NULL
    if (!is.null(option)) {
        if (!identical(attr(option, "Rd_tag"), "TEXT"))
    	    stopRd(option, Rdfile, "Bad \\link option -- must be text")
    	if (grepl("^=", option, perl = TRUE))
    	    dest <- sub("^=", "", option, perl = TRUE)
    	else if (grepl(":", option, perl = TRUE)) {
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

if(FALSE) {
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
## This warns on
##  text outside sections
##  missing links
## and errors on
##  Bad \\link text
##  Bad \\link option -- must be text
##  unrecognized tags (can the parser do that?)
##  \\tabular format must be simple text
##  Unrecognized \\tabular format:
##  "Only ", length(format), " columns allowed in this table"
##  Only one \\Rdversion declaration is allowed
##  Unrecognized section (but I think the parser catches that)
##  Sections \\title, and \\name must exist and be unique in Rd files
##  \\name must only contain simple text


## FIXME: better to really use XHTML
Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, CHM = FALSE,
             stages = "render", outputEncoding = "UTF-8", ...)
{
    of <- function(...)
        writeLinesUTF8(paste(...), con, outputEncoding, sep = "")
    of0 <- function(...)
        writeLinesUTF8(paste(..., sep=""), con, outputEncoding, sep = "")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

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
	else if(start == 1) gsubUTF8("^\\s+", "", x, perl = TRUE)
        else x
    }

    ## FIXME: what other substitutions do we need?
    ## possibly quotes if the parser had left alone -- NA.Rd


    htmlify <- function(x) {
        ## FIXME: just use useBytes = TRUE?
    	Encoding(x) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
	x <- gsub("&", "&amp;", x, fixed = TRUE)
	x <- gsub("---", "&mdash;", x, fixed = TRUE)
	x <- gsub("--", "&ndash;", x, fixed = TRUE)
	x <- gsub("``", "&ldquo;", x, fixed = TRUE)
	x <- gsub("''", "&rdquo;", x, fixed = TRUE)
        x <- gsub("`([^']+)'", "&lsquo;\\1&rsquo;", x, perl=TRUE)
	x <- gsub("`", "'", x, fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	x <- gsub(">", "&gt;", x, fixed = TRUE)
	Encoding(x) <- "UTF-8"
	x
    }

    vhtmlify <- function(x) { # code version
    	Encoding(x) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
	x <- gsub("&", "&amp;", x, fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	x <- gsub(">", "&gt;", x, fixed = TRUE)
	Encoding(x) <- "UTF-8"
	x
    }

    HTMLeqn <- function(x)
    {
        x <- htmlify(x)
        ## historical escapes for math
    	Encoding(x) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        x <- gsub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon)", "&\\1;", x)
        x <- gsub("\\\\left\\(", "(", x)
        x <- gsub("\\\\right", "\\)", x)
        x <- gsub("\\le", "&lt;=", x)
        x <- gsub("\\ge", "&gt;=", x)
        Encoding(x) <- "UTF-8"
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

    writeLink <- function(tag, block) {
	parts <- get_link(block, tag, Rdfile)

        writeHref <- function() {
            of0('<a href="', htmlfile, '">')
            writeContent(block, tag)
            of1('</a>')
        }

    	if (is.null(parts$targetfile)) {
            topic <- parts$dest
            htmlfile  <- NA_character_
            if (!is.null(Links)) {
                tmp <- Links[topic]
                if (!is.na(tmp)) htmlfile <- tmp
            }
            if (is.na(htmlfile)) {
                ## FIXME for CHM?
                ## 'should be <a href="../../../doc/html/search/SearchObject.html?$argkey">$arg<\/a>/s; where $argkey is the topic and $arg the HTMLified version.'
                warnRd(block, Rdfile, "missing link ", sQuote(topic))
                htmlfile <- paste("../../../doc/html/search/SearchObject.html?",
                                   parts$dest, sep= "")
                writeHref()
            } else {
                ## treat links in the same package specially -- needed for CHM
                pkg_regexp <- paste("^../../", package, "/html/", sep = "")
                if (grepl(pkg_regexp, htmlfile)) {
                    htmlfile <- sub(pkg_regexp, "", htmlfile)
                } else if (CHM) {
                    otherpkg <- sub("^../../([^/]*).*", "\\1", htmlfile)
                    htmlfile <- paste("findlink('", otherpkg, "', '",
                                      basename(htmlfile), "')",
                                      sep="")
                    of0('<a onclick="', htmlfile,
                        '" style="text-decoration: underline; color: blue; cursor: hand">')
                    writeContent(block, tag)
                    of1('</a>')
                } else writeHref()
            }
    	} else if (is.null(parts$pkg) || parts$pkg == package) {
    	    htmlfile <- paste(parts$targetfile, ".html", sep="")
            writeHref()
    	} else {
            if(CHM) {
                ## Cross-packages CHM links are of the form
                ## <a onclick="findlink('stats', 'weighted.mean.html')" style="text-decoration: underline; color: blue; cursor: hand">weighted.mean</a>
                htmlfile <- paste("findlink('", parts$pkg, "', '",
                                  parts$targetfile, ".html", "')",
                                  sep="")
                of0('<a onclick="', htmlfile,
                    '" style="text-decoration: underline; color: blue; cursor: hand">')
                writeContent(block, tag)
                of1('</a>')
            } else {
                htmlfile <- paste("../../", parts$pkg, "/html/",
                                  parts$targetfile, ".html", sep="")
                writeHref()
            }
        }

        nlinks <<- nlinks + 1L
    }

    writeComment <- function(txt) {
       	Encoding(txt) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
       	txt <- sub("^%", "", txt, fixed = TRUE)
       	txt <- sub("\n", "", txt)
       	txt <- gsub("--", "- - ", txt, fixed = TRUE)
       	txt <- gsub(">", "&gt;", txt, fixed = TRUE)
       	Encoding(txt) <- "UTF-8"
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
               ## cwhmisc has an empty \\email
               "\\email" = if(length(block)) of0('<a href="mailto:', block[[1L]], '">', htmlify(block[[1L]]), '</a>'),
               ## FIXME: encode, not htmlify
               ## watch out for empty URLs (TeachingDemos has one)
               "\\url" = if(length(block)) of0('<a href="', block[[1L]], '">', block[[1L]], '</a>'),
               "\\Sexpr"= of0(as.character.Rd(block, deparse=TRUE)),
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
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed=TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, Rdfile,
                   "Unrecognized \\tabular format: ", table[[1L]][[1L]])
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
                    stopRd(table, Rdfile,
                           "Only ", length(format),
                           " columns allowed in this table")
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
            	if (tag == "RCODE" && grepl("^\\(", block)) {
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
                && grepl("\\)", block)) { # Finish it off...
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
                        txt <- gsubUTF8("^ ", "", as.character(block), perl = TRUE)
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
    	stopRd(Rd[[version[2L]]], Rdfile,
               "Only one \\Rdversion declaration is allowed")

    ## Give warning (pro tem) for nonblank text outside a section
    if (length(bad <- grep("[^[:blank:][:cntrl:]]",
                           unlist(Rd[sections == "TEXT"]), perl = TRUE )))
    	warnRd(Rd[sections == "TEXT"][[bad[1L]]], Rdfile,
               "All text must be in a section")

    ## Drop all the parts that are not rendered
    drop <- sections %in% c("COMMENT", "TEXT", "\\concept", "\\docType", "\\encoding",
                            "\\keyword", "\\Rdversion", "\\RdOpts")
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
    if (!identical(sections[1:2], c("\\title", "\\name")))
    	stopRd(Rd, Rdfile,
               "Sections \\title, and \\name must exist and be unique in Rd files")

    title <- Rd[[1L]]
    name <- Rd[[2L]]
    tags <- RdTags(name)
    if (length(tags) > 1L)
        stopRd(name, Rdfile,"\\name must only contain simple text")

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
            writeLinesUTF8(paste('',
                             '<script Language="JScript">',
                             'function findlink(pkg, fn) {',
                             'var Y, link;',
                             'Y = location.href.lastIndexOf("\\\\") + 1;',
                             'link = location.href.substring(0, Y);',
                             'link = link + "../../" + pkg + "/chtml/" + pkg + ".chm::/" + fn;',
                             'location.href = link;', '}', '</script>',
                             sep = '\n'), con, outputEncoding)
    }
    version <- if (package != "")
    	paste('Package <em>', package,
              '</em> version ',
              packageDescription(package, fields="Version"), ' ', sep='')
    else ""
    of0('\n',
        '<hr><div align="center">[', version, '<a href="00Index.html">Index</a>]</div>\n',
        '</body></html>\n')
    invisible(out)
}

findHTMLlinks <- function(pkgDir = "", lib.loc = NULL)
{
    ## The priority order is
    ## This package
    ## The standard packages
    ## along lib.loc.

    if(is.null(lib.loc)) lib.loc <- .libPaths()

    Links <- lapply(rev(lib.loc), .find_HTML_links_in_library)
    Links <- c(Links,
               lapply(file.path(.Library,
                                c("base", "utils", "graphics",
                                  "grDevices", "stats", "datasets",
                                  "methods")),
                      .find_HTML_links_in_package))
    if(nzchar(pkgDir))
        Links <- c(Links, list(.find_HTML_links_in_package(pkgDir)))
    Links <- unlist(Links)

    ## now latest names are newest, so
    Links <- rev(Links)
    Links <- Links[!duplicated(names(Links))]
    gsub("[Rr]d$", "html", Links)
}

.find_HTML_links_in_package <-
function(dir)
{
    if(file_test("-f", f <- file.path(dir, "Meta", "links.rds")))
        .readRDS(f)
    else if(file_test("-f", f <- file.path(dir, "Meta", "Rd.rds")))
        .build_links_index(.readRDS(f), basename(dir))
    else character()
}

.find_HTML_links_in_library <-
function(dir)
{
    if(file_test("-f", f <- file.path(dir, ".Meta", "links.rds")))
        .readRDS(f)
    else
        .build_library_links_index(dir)
}

.build_library_links_index <-
function(dir)
{
    unlist(lapply(rev(dir(dir, full.names = TRUE)),
                  .find_HTML_links_in_package))
}
