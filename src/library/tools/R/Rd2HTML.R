
#  File src/library/tools/R/Rd2HTML.R
#
#  Copyright (C) 1995-2013 The R Core Team
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

## also used by Rd2latex, but only 'topic' and 'dest'
get_link <- function(arg, tag, Rdfile) {
    ## 'topic' is the name to display, 'dest' is the topic to link to
    ## optionaly in package 'pkg'.  If 'target' is set it is the file
    ## to link to in HTML help

    ## \link[=bar]{foo} means shows foo but treat this as a link to bar.
    ## \link[pkg]{bar} means show bar and link to *file* bar in package pkg
    ## \link{pkg:bar]{foo} means show foo and link to file bar in package pkg.
    ## As from 2.10.0, look for topic 'bar' if file not found.

    if (!all(RdTags(arg) == "TEXT"))
    	stopRd(arg, Rdfile, "Bad \\link text")

    option <- attr(arg, "Rd_option")

    topic <- dest <- paste(unlist(arg), collapse = "")
    targetfile <- NULL
    pkg <- NULL
    if (!is.null(option)) {
        if (!identical(attr(option, "Rd_tag"), "TEXT"))
    	    stopRd(option, Rdfile, "Bad \\link option -- must be text")
    	if (grepl("^=", option, perl = TRUE, useBytes = TRUE))
    	    dest <- psub1("^=", "", option)
    	else if (grepl(":", option, perl = TRUE, useBytes = TRUE)) {
    	    targetfile <- psub1("^[^:]*:", "", option)
    	    pkg <- psub1(":.*", "", option)
    	} else {
            targetfile <- dest
    	    pkg <- as.character(option)
    	}
    }
    if (tag == "\\linkS4class") dest <- paste0(dest, "-class")
    list(topic = topic, dest = dest, pkg = pkg, targetfile = targetfile)
}


# translation of Utils.pm function of the same name, plus "unknown"
mime_canonical_encoding <- function(encoding)
{
    encoding[encoding %in% c("", "unknown")] <-
        utils::localeToCharset()[1L]
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
    encoding[encoding == "ascii"] <-   "us-ascii" # from W3C validator
    encoding
}

htmlify <- function(x) {
    x <- fsub("&", "&amp;", x)
    x <- fsub("---", "&mdash;", x)
    x <- fsub("--", "&ndash;", x)
    x <- fsub("``", "&ldquo;", x)
    x <- fsub("''", "&rdquo;", x)
    x <- psub("`([^']+)'", "&lsquo;\\1&rsquo;", x)
    x <- fsub("`", "'", x)
    x <- fsub("<", "&lt;", x)
    x <- fsub(">", "&gt;", x)
    x <- fsub('"\\{"', '"{"', x)
    x <- fsub('"', '&quot;', x)
    x
}

vhtmlify <- function(x, inEqn = FALSE) { # code version
    x <- fsub("&", "&amp;", x)
    x <- fsub("<", "&lt;", x)
    x <- fsub(">", "&gt;", x)
    x <- fsub('"\\{"', '"{"', x)
    ## http://htmlhelp.com/reference/html40/entities/symbols.html
    if(inEqn) {
        x <- psub("\\\\(Alpha|Beta|Gamma|Delta|Epsilon|Zeta|Eta|Theta|Iota|Kappa|Lambda|Mu|Nu|Xi|Omicron|Pi|Rho|Sigma|Tau|Upsilon|Phi|Chi|Psi|Omega|alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega|le|ge|sum|prod)", "&\\1;", x)
        x <- psub("\\\\(dots|ldots)", "&\\hellip;", x)
        x <- fsub("\\infty", "&infin;", x)
        x <- fsub("\\sqrt", "&radic;", x)
    }
    x
}

# URL encode anything other than alphanumeric, . and _

urlify <- function(x) { # make a string legal in a URL
    chars <- unlist(strsplit(x, ""))
    hex <- paste0("%", as.character(charToRaw(x)))
    mixed <- ifelse(grepl("[0-9a-zA-Z._]", chars), chars, hex)
    paste(mixed, collapse="")
}

# Ampersands should be escaped in proper HTML URIs

escapeAmpersand <- function(x) gsub("&", "&amp;", x, fixed=TRUE)

## This gets used two ways:

## 1) With dynamic = TRUE from tools:::httpd()
##    Here generated links are of the forms
##    ../../pkg/help/topic
##    file.html
##    ../../pkg/html/file.html
##    and links are never missing: topics are always linked as
##    ../../pkg/help/topic for the current packages, and this means
##    'search this package then all the others, and show all matches
##    if we need to go outside this packages'

## 2) With dynamic = FALSE from .convertRdfiles (with Links[2], used for
##    prebuilt HTML pages) and .Rdconv (no link lookup)
##    Here generated links are of the forms
##    file.html
##    ../../pkg/html/file.html
##    and missing links (those without an explicit package, and
##    those topics not in Links[2]) don't get linked anywhere.

## FIXME: better to use XHTML
Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, Links2 = NULL,
             stages = "render", outputEncoding = "UTF-8",
             dynamic = FALSE, no_links = FALSE, fragment=FALSE,
             stylesheet = "R.css", ...)
{
    if (missing(no_links) && is.null(Links) && !dynamic) no_links <- TRUE
    version <- ""
    if(!identical(package, "")) {
        if(length(package) > 1L) {
            version <- package[2L]
            package <- package[1L]
        } else {
            dir <- dirname(package)
            if((dir != "") &&
               file_test("-f", dfile <- file.path(package,
                                                  "DESCRIPTION"))) {
                version <- .read_description(dfile)["Version"]
                package <- basename(package)
            } else {
                ## Should we really do this?
                ## Used when Rdconv is given a package argument.
                version <- utils::packageDescription(package,
                                                     fields = "Version")
            }
        }
        if(is.na(version)) version <- ""
    }

    ## writeLines by default re-encodes strings to the local encoding.
    ## Avoid that by useBytes=TRUE
    writeLinesUTF8 <-
        if (outputEncoding == "UTF-8" ||
           (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...)
            writeLines(x, con, useBytes = TRUE, ...)
    } else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }

    of <- function(...)
        writeLinesUTF8(paste(...), con, outputEncoding, sep = "")
    of0 <- function(...)
        writeLinesUTF8(paste0(...), con, outputEncoding, sep = "")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

    pendingClose <- pendingOpen <- character()  # Used for infix methods

    inEqn <- FALSE		# Should we do edits needed in an eqn?
    sectionLevel <- 0L		# How deeply nested within section/subsection
    inPara <- FALSE		# Are we in a <p> paragraph? If NA, we're not, but we're not allowed to be


### These correspond to HTML wrappers
    HTMLTags <- c("\\bold"="b",
    	          "\\cite"="cite",
                  "\\code"="code",
                  "\\command"="code",
                  "\\dfn"="dfn",
                  "\\emph"="em",
                  "\\kbd"="kbd",
                  "\\preformatted"="pre",
#                  "\\special"="pre",
                  "\\strong"="strong",
                  "\\var"="var",
                  "\\verb"="pre")
    # These have simple substitutions
    HTMLEscapes <- c("\\R"='<span style="font-family: Courier New, Courier; color: #666666;"><b>R</b></span>',
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
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }

    addParaBreaks <- function(x) {
	if (isBlankLineRd(x) && isTRUE(inPara)) {
	    inPara <<- FALSE
	    return("</p>\n")
	}
        start <- attr(x, "srcref")[2L] # FIXME: what if no srcref?, start col
	if (start == 1) x <- psub("^\\s+", "", x)
	if (isTRUE(!inPara) && !all(grepl("^[[:blank:]\n]*$", x, perl = TRUE))) {
	    x <- c("<p>", x)
	    inPara <<- TRUE
	}
        x
    }

    enterPara <- function(enter = TRUE) {
	if (enter && isTRUE(!inPara)) {
            of0("<p>")
            inPara <<- TRUE
        }
    }

    leavePara <- function(newval) {
    	if (isTRUE(inPara)) of0("</p>\n")
    	inPara <<- newval
    }

    writeWrapped <- function(tag, block, doParas) {
    	if (!doParas || HTMLTags[tag] == "pre")
            leavePara(NA)
        else
            enterPara()
        if (!isBlankRd(block)) {
    	    of0("<", HTMLTags[tag], ">")
    	    writeContent(block, tag)
    	    of0("</",  HTMLTags[tag], ">")
    	}
    }

    checkInfixMethod <- function(blocks)
    	# Is this a method which needs special formatting?
    	if ( length(blocks) == 1 && RdTags(blocks) == "TEXT" &&
    	     blocks[[1L]] %in% c("[", "[[", "$") ) {
    	    pendingOpen <<- blocks[[1L]]
    	    TRUE
    	} else FALSE

    writeLink <- function(tag, block, doParas) {
	parts <- get_link(block, tag, Rdfile)

        writeHref <- function() {
            enterPara(doParas)
            savePara <- inPara
            inPara <<- NA
            if (!no_links) of0('<a href="', htmlfile, '">')
            writeContent(block, tag)
            if (!no_links) of1('</a>')
            inPara <<- savePara
        }

    	if (is.null(parts$targetfile)) {
            ## ---------------- \link{topic} and \link[=topic]{foo}
            topic <- parts$dest
    	    if (dynamic) { # never called with package=""
                htmlfile <- paste0("../../", urlify(package), "/help/", urlify(topic))
                writeHref()
                return()
            } else {
            	htmlfile  <- NA_character_
            	if (!is.null(Links)) {
            	    tmp <- Links[topic]
            	    if (!is.na(tmp)) htmlfile <- tmp
                    else {
                        tmp <- Links2[topic]
                        if (!is.na(tmp)) htmlfile <- tmp
                    }
            	}
            }
            if (is.na(htmlfile)) {
                ## Used to use the search engine, but we no longer have one,
                ## and we don't get here for dynamic help.
                if (!no_links)
                    warnRd(block, Rdfile, "missing link ", sQuote(topic))
                writeContent(block, tag)
            } else {
                ## treat links in the same package specially -- was needed for CHM
                pkg_regexp <- paste0("^../../", urlify(package), "/html/")
                if (grepl(pkg_regexp, htmlfile)) {
                    htmlfile <- sub(pkg_regexp, "", htmlfile)
                }
                writeHref()
            }
    	} else {
            ## ----------------- \link[pkg]{file} and \link[pkg:file]{bar}
            htmlfile <- paste0(urlify(parts$targetfile), ".html")
            if (!dynamic && !no_links &&
               nzchar(pkgpath <- system.file(package = parts$pkg))) {
                ## check the link, only if the package is found
                OK <- FALSE
                if (!file.exists(file.path(pkgpath, "html", htmlfile))) {
                    ## does not exist as static HTML, so look harder
                    f <- file.path(pkgpath, "help", "paths.rds")
                    if (file.exists(f)) {
                        paths <- sub("\\.[Rr]d$", "", basename(readRDS(f)))
                        OK <- parts$targetfile %in% paths
                    }
                } else OK <- TRUE
                if (!OK) {
                    ## so how about as a topic?
                    file <- utils:::index.search(parts$targetfile, pkgpath)
                    if (!length(file)) {
                        warnRd(block, Rdfile,
                               "file link ", sQuote(parts$targetfile),
                               " in package ", sQuote(parts$pkg),
                               " does not exist and so has been treated as a topic")
                        parts$targetfile <- basename(file)
                    } else {
                        warnRd(block, Rdfile, "missing file link ",
                               sQuote(parts$targetfile))
                    }
                }
            }
            if (parts$pkg == package) {
                ## use href = "file.html"
                writeHref()
            } else {
                ## href = "../../pkg/html/file.html"
                htmlfile <- paste0("../../", urlify(parts$pkg), "/html/", htmlfile)
                writeHref()
            }
        }
    }

    writeComment <- function(txt) {
       	txt <- psub1("^%", "", txt)
       	txt <- fsub1("\n", "", txt)
       	txt <- fsub("--", "- - ", txt)
       	txt <- fsub(">", "&gt;", txt)
	of("<!-- ", txt, " -->\n")
    }

    writeLR <- function(block, tag, doParas) {
    	enterPara(doParas)
        of1(HTMLLeft[tag])
        writeContent(block, tag)
        of1(HTMLRight[tag])
    }

    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1('## Not run: ')
            writeContent(block, tag)
            of1('\n## End(Not run)')
        } else {
            of1('## Not run: ')
            writeContent(block, tag)
       }
    }

    writeBlock <- function(block, tag, blocktag) {
        doParas <- !(blocktag %in% c("\\tabular"))
	switch(tag,
               UNKNOWN =,
               VERB = of1(vhtmlify(block, inEqn)),
               RCODE = of1(vhtmlify(block)),
               TEXT = of1(if(doParas) addParaBreaks(htmlify(block))else vhtmlify(block)),
               USERMACRO =,
               "\\newcommand" =,
               "\\renewcommand" =,
               COMMENT = {},
               LIST = writeContent(block, tag),
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize" = {
               	   leavePara(FALSE)
                   writeContent(block, tag)
               },
               "\\bold" =,
               "\\cite" =,
               "\\code" =,
               "\\command" =,
               "\\dfn" =,
               "\\emph" =,
               "\\kbd" =,
               "\\preformatted" =,
               "\\strong" =,
               "\\var" =,
               "\\verb" = writeWrapped(tag, block, doParas),
               "\\special" = writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block, doParas),
               ## cwhmisc has an empty \\email
               "\\email" = if (length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   enterPara(doParas)
                   of0('<a href="mailto:', url, '">', htmlify(url), '</a>')},
               ## FIXME: encode, not htmlify
               ## watch out for empty URLs (TeachingDemos has one)
               "\\url" = if(length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   enterPara(doParas)
                   of0('<a href="', escapeAmpersand(url), '">', htmlify(url), '</a>')
               },
               "\\href" = {
               	   if(length(block[[1L]])) {
               	   	url <- paste(as.character(block[[1L]]), collapse="")
               	   	url <- gsub("\n", "", url)
		        enterPara(doParas)
               	   	of0('<a href="', escapeAmpersand(url), '">')
               	   	closing <- "</a>"
               	   } else closing <- ""
               	   savePara <- inPara
               	   inPara <<- NA
               	   writeContent(block[[2L]], tag)
               	   of0(closing)
               	   inPara <<- savePara
               },
               "\\Sexpr"= of0(as.character.Rd(block, deparse=TRUE)),
               "\\cr" = of1(HTMLEscapes[tag]),
               "\\dots" =,
               "\\ldots" =,
               "\\R" = {
                   enterPara(doParas)
               	   of1(HTMLEscapes[tag])
               },
               "\\acronym" =,
               "\\donttest" =,
               "\\env" =,
               "\\file" =,
               "\\option" =,
               "\\pkg" =,
               "\\samp" =,
               "\\sQuote" =,
               "\\dQuote" =  writeLR(block, tag, doParas),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = writeContent(block[[1L]], tag),
               "\\eqn" = {
                   inEqn <<- TRUE
                   of1("<i>")
                   block <- block[[length(block)]];
                   ## FIXME: space stripping needed: see Special.html
                   writeContent(block, tag)
                   of1("</i>")
                   inEqn <<- FALSE
               },
               "\\deqn" = {
                   inEqn <<- TRUE
                   leavePara(TRUE)
                   of1('<p style="text-align: center;"><i>')
                   block <- block[[length(block)]];
                   writeContent(block, tag)
                   of0('</i>')
                   leavePara(FALSE)
                   inEqn <<- FALSE
               },
               "\\figure" = {
                   ## This is what is needed for static html pages
                   if(dynamic) of1('<img src="figures/')
                   else of1('<img src="../help/figures/')
                   writeContent(block[[1]], tag)
                   of1('" ')
               	   if (length(block) > 1L
               	       && length(imgoptions <- .Rd_get_latex(block[[2]]))
		       && grepl("^options: ", imgoptions)) {
		       # There may be escaped percent signs within
		       imgoptions <- gsub("\\%", "%", imgoptions, fixed=TRUE)
                       of1(sub("^options: ", "", imgoptions))
	           } else {
		       of1('alt="')
		       writeContent(block[[length(block)]], tag)
		       of1('"')
		   }
                   ## <FIXME>
                   ## We currently generate HTML 4.01 transitional.
                   ## When using
                   ##   <img ...... />
                   ## W3C Markup Validator warns
                   ##   NET-enabling start-tag requires SHORTTAG YES
                   ## Hence use
                   ##   <img ......  >
                   ## for now, and change if/when moving to XHTML.
                   of1(' >')
                   ##   of1(' />')
                   ## </FIXME>
               },
               "\\dontshow" =,
               "\\testonly" = {}, # do nothing
               "\\method" =,
               "\\S3method" =,
               "\\S4method" = {
                   # Should not get here
               },
               "\\tabular" = writeTabular(block),
               "\\subsection" = writeSection(block, tag),
               "\\if" =,
               "\\ifelse" =
               	    if (testRdConditional("html", block, Rdfile))
			writeContent(block[[2L]], tag)
		    else if (tag == "\\ifelse")
		    	writeContent(block[[3L]], tag),
               "\\out" = for (i in seq_along(block))
		   of1(block[[i]]),
               stopRd(block, Rdfile, "Tag ", tag, " not recognized")
               )
    }

    writeTabular <- function(table) {
    	format <- table[[1L]]
    	content <- table[[2L]]
    	if (length(format) != 1 || RdTags(format) != "TEXT")
    	    stopRd(table, Rdfile, "\\tabular format must be simple text")
    	format <- strsplit(format[[1L]], "", fixed = TRUE)[[1L]]
    	if (!all(format %in% c("l", "c", "r")))
    	    stopRd(table, Rdfile,
                   "Unrecognized \\tabular format: ", table[[1L]][[1L]])
        format <- c(l="left", c="center", r="right")[format]

        tags <- RdTags(content)

	leavePara(FALSE)
	of1('\n<table summary="Rd table">\n')
        newrow <- TRUE
        newcol <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
            	of1("<tr>\n ")
            	newrow <- FALSE
            	col <- 0
            }
            if (newcol) {
                col <- col + 1L
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
            	newcol <- TRUE
            },
            writeBlock(content[[i]], tags[i], "\\tabular"))
            leavePara(FALSE)
        }
        if (!newcol) of1('</td>')
        if (!newrow) of1('\n</tr>\n')
        of1('\n</table>\n')
    }

    writeContent <- function(blocks, blocktag) {
        inlist <- FALSE
        itemskip <- FALSE

	tags <- RdTags(blocks)

	i <- 0
	while (i < length(tags)) {
	    i <- i + 1
            tag <- tags[i]
            block <- blocks[[i]]
            if (length(pendingOpen)) { # Handle $, [ or [[ methods
            	if (tag == "RCODE" && grepl("^\\(", block)) {
            	    block <- sub("^\\(", "", block)
            	    arg1 <- sub("[,)[:space:]].*", "", block)
		    block <- sub(paste0(arg1, "[[:space:]]*,[[:space:]]*"),
				 "", block)
            	    of0(arg1, pendingOpen)
            	    if (pendingOpen == "$")
            	    	pendingClose <<- ""
            	    else
            	    	pendingClose <<- chartr("[", "]", pendingOpen)
            	} else of0("`", pendingOpen, "`")
            	pendingOpen <<- character()
            }
            if (length(pendingClose) && tag == "RCODE"
                && grepl("\\)", block)) { # Finish it off...
            	of0(sub("\\).*", "", block), pendingClose)
            	block <- sub("[^)]*\\)", "", block)
            	pendingClose <<- character()
            }
            switch(tag,
            "\\method" =,
            "\\S3method" =,
            "\\S4method" = {
               	blocks <- transformMethod(i, blocks, Rdfile)
               	tags <- RdTags(blocks)
               	i <- i - 1
            },
            "\\item" = {
    	    	leavePara(FALSE)
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
    		    inPara <<- NA
    		    writeContent(block[[1L]], tag)
    		    of1('</code></td>\n<td>\n')
    		    inPara <<- FALSE
    		    writeContent(block[[2L]], tag)
    		    leavePara(FALSE)
    		    of1('</td></tr>')
    		},
    		"\\describe"= {
    		    of1("<dt>")
    		    inPara <<- NA
    		    writeContent(block[[1L]], tag)
    		    of1("</dt><dd>")
    		    inPara <<- FALSE
    		    writeContent(block[[2L]], tag)
    		    leavePara(FALSE)
    		    of1("</dd>")
    		},
    		"\\enumerate" =,
    		"\\itemize"= {
    		    inPara <<- FALSE
    		    of1("<li>")
    		})
    	    },
    	    { # default
    	    	if (inlist && !(blocktag %in% c("\\itemize", "\\enumerate"))
    	    	           && !(tag == "TEXT" && isBlankRd(block))) {
    	    	    switch(blocktag,
    	    	    "\\arguments" =,
     	    	    "\\value" = of1("</table>\n"),
    	    	    "\\describe" = of1("</dl>\n"))
    		    inlist <- FALSE
    		    inPara <<- FALSE
    		}
                if (itemskip) {
                    ## The next item must be TEXT, and start with a space.
                    itemskip <- FALSE
                    if (tag == "TEXT") {
                        txt <- addParaBreaks(htmlify(block))
                        of1(txt)
                    } else writeBlock(block, tag, blocktag) # should not happen
                } else writeBlock(block, tag, blocktag)
    	    })
	}
	if (inlist) {
	    leavePara(FALSE)
	    switch(blocktag,
		"\\value"=,
		"\\arguments" = of1("</table>\n"),
		"\\itemize" = of1("</li></ul>\n"),
		"\\enumerate" = of1("</li></ol>\n"),
		# "\\value"=,
		"\\describe" = of1("</dl>\n"))
	}
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return() ## \alias only used on CHM header

        leavePara(NA)
        save <- sectionLevel
        sectionLevel <<- sectionLevel + 1L
    	of1(paste0("\n\n<h", sectionLevel+2L, ">"))

    	if (tag == "\\section" || tag == "\\subsection") {
    	    title <- section[[1L]]
    	    section <- section[[2L]]
            ## FIXME: this needs trimming of whitespace
    	    writeContent(title, tag)
    	} else
    	    of1(sectionTitles[tag])
        of1(paste0("</h", sectionLevel+2L, ">\n\n"))
        if (tag %in% c("\\examples", "\\usage")) {
            of1("<pre>")
            inPara <<- NA
            pre <- TRUE
        } else {
            inPara <<- FALSE
            pre <- FALSE
        }
    	if (length(section)) {
	    ## There may be an initial \n, so remove that
	    s1 <- section[[1L]][1L]
	    if (RdTags(section)[1] == "TEXT" && s1 == "\n") section <- section[-1L]
	    writeContent(section, tag)
	}
	leavePara(FALSE)
	if (pre) of0("</pre>\n")
    	sectionLevel <<- save
    }

    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines = defines, stages = stages,
                     fragment = fragment, ...)
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
	name <- htmlify(Rd[[2L]][[1L]])

	of0('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
            '<html><head><title>')
        ## of0('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
        ##     '<html xmlns="http://www.w3.org/1999/xhtml">',
	##     '<head><title>')
	headtitle <- strwrap(.Rd_format_title(.Rd_get_title(Rd)),
	                     width=65, initial="R: ")
	if (length(headtitle) > 1) headtitle <- paste0(headtitle[1], "...")
	of1(htmlify(headtitle))
	of0('</title>\n',
	    '<meta http-equiv="Content-Type" content="text/html; charset=',
	    mime_canonical_encoding(outputEncoding),
	    '">\n')

	of0('<link rel="stylesheet" type="text/css" href="',
	    stylesheet,
	    '">\n',
	    '</head><body>\n\n',
	    '<table width="100%" summary="page for ', htmlify(name))
	if (nchar(package))
	    of0(' {', package, '}"><tr><td>',name,' {', package,'}')
	else
	    of0('"><tr><td>',name)
	of0('</td><td style="text-align: right;">R Documentation</td></tr></table>\n\n')

	of1("<h2>")
	inPara <- NA
	title <- Rd[[1L]]
	writeContent(title,sections[1])
	of1("</h2>")
	inPara <- FALSE

	for (i in seq_along(sections)[-(1:2)])
	    writeSection(Rd[[i]], sections[i])

	if(version != "")
	    version <- paste0('Package <em>',package,'</em> version ',version,' ')
	of0('\n')
	if (version != "")
	    of0('<hr><div style="text-align: center;">[', version,
		if (!no_links) '<a href="00Index.html">Index</a>',
		']</div>')
	of0('\n',
	    '</body></html>\n')
    }
    invisible(out)
}

findHTMLlinks <- function(pkgDir = "", lib.loc = NULL, level = 0:2)
{
    ## The priority order is
    ## This package (level 0)
    ## The standard packages (level 1)
    ## along lib.loc (level 2)

    if (is.null(lib.loc)) lib.loc <- .libPaths()

    Links <- list()
    if (2 %in% level)
        Links <- c(Links, lapply(rev(lib.loc), .find_HTML_links_in_library))
    if (1 %in% level) {
        base <- unlist(.get_standard_package_names()[c("base", "recommended")],
                       use.names = FALSE)
        Links <- c(Links,
                   lapply(file.path(.Library, base),
                          .find_HTML_links_in_package))
    }
    if (0 %in% level && nzchar(pkgDir))
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
    if (file_test("-f", f <- file.path(dir, "Meta", "links.rds")))
        readRDS(f)
    else if (file_test("-f", f <- file.path(dir, "Meta", "Rd.rds")))
        .build_links_index(readRDS(f), basename(dir))
    else character()
}

.find_HTML_links_in_library <-
function(dir)
{
    if (file_test("-f", f <- file.path(dir, ".Meta", "links.rds")))
        readRDS(f)
    else
        .build_library_links_index(dir)
}

.build_library_links_index <-
function(dir)
{
    unlist(lapply(rev(dir(dir, full.names = TRUE)),
                  .find_HTML_links_in_package))
}
