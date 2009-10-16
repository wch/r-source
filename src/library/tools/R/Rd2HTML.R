#  File src/library/tools/R/Rd2HTML.R
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
    if (tag == "\\linkS4class") dest <- paste(dest, "-class", sep="")
    list(topic = topic, dest = dest, pkg = pkg, targetfile = targetfile)
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
    encoding[encoding == "ascii"] <-   "us-ascii" # from W3C validator
    encoding
}

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
             dynamic = FALSE, no_links = FALSE, ...)
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
        writeLinesUTF8(paste(..., sep=""), con, outputEncoding, sep = "")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

    pendingClose <- pendingOpen <- character(0)  # Used for infix methods

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
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }

    addParaBreaks <- function(x, tag) {
        start <- attr(x, "srcref")[2L] # FIXME: what if no srcref?, start col
	if (isBlankLineRd(x)) "</p>\n<p>\n"
	else if (start == 1) psub("^\\s+", "", x)
        else x
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
	x
    }

    vhtmlify <- function(x) { # code version
	x <- fsub("&", "&amp;", x)
	x <- fsub("<", "&lt;", x)
	x <- fsub(">", "&gt;", x)
	x
    }

    HTMLeqn <- function(x)
    {
        x <- htmlify(x)
        ## historical escapes for math
        x <- psub("\\\\(Gamma|alpha|Alpha|pi|mu|sigma|Sigma|lambda|beta|epsilaon)", "&\\1;", x)
        x <- fsub("\\left(", "(", x)
        x <- fsub("\\right", ")", x)
        x <- fsub("\\le", "&lt;=", x)
        x <- fsub("\\ge", "&gt;=", x)
        x
    }

    writeWrapped <- function(tag, block) {
    	of0("<", HTMLTags[tag], ">")
    	writeContent(block, tag)
    	of0("</",  HTMLTags[tag], ">")
    }

    checkInfixMethod <- function(blocks)
    	# Is this a method which needs special formatting?
    	if ( length(blocks) == 1 && RdTags(blocks) == "TEXT" &&
    	     blocks[[1]] %in% c("[", "[[", "$") ) {
    	    pendingOpen <<- blocks[[1]]
    	    TRUE
    	} else FALSE

    writeLink <- function(tag, block) {
	parts <- get_link(block, tag, Rdfile)

        writeHref <- function() {
            if (!no_links) of0('<a href="', htmlfile, '">')
            writeContent(block, tag)
            if (!no_links) of1('</a>')
        }

    	if (is.null(parts$targetfile)) {
            ## ---------------- \link{topic} and \link[=topic]{foo}
            topic <- parts$dest
    	    if (dynamic) { # never called with package=""
                htmlfile <- paste("../../", package, "/help/", topic, sep="")
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
                pkg_regexp <- paste("^../../", package, "/html/", sep = "")
                if (grepl(pkg_regexp, htmlfile)) {
                    htmlfile <- sub(pkg_regexp, "", htmlfile)
                }
                writeHref()
            }
    	} else {
            ## ----------------- \link[pkg]{file} and \link[pkg:file]{bar}
            htmlfile <- paste(parts$targetfile, ".html", sep="")
            if (!dynamic && !no_links &&
               nzchar(pkgpath <- system.file(package = parts$pkg))) {
                ## check the link, only if the package is found
                OK <- FALSE
                if (!file.exists(file.path(pkgpath, "html", htmlfile))) {
                    ## does not exist as static HTML, so look harder
                    f <- file.path(pkgpath, "help", "paths.rds")
                    if (file.exists(f)) {
                        paths <- sub("\\.[Rr]d$", "", basename(.readRDS(f)))
                        OK <- parts$targetfile %in% paths
                    }
                } else OK <- TRUE
                if (!OK) {
                    ## so how about as a topic?
                    file <- index.search(parts$targetfile, pkgpath,
                                         type = "html")
                    if (nzchar(file)) {
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
                htmlfile <- paste("../../", parts$pkg, "/html/", htmlfile,
                                  sep="")
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

    writeLR <- function(block, tag) {
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
	switch(tag,
               UNKNOWN =,
               VERB =,
               RCODE = of1(vhtmlify(block)),
               TEXT = of1(addParaBreaks(htmlify(block), blocktag)),
               COMMENT = {},
               LIST =,
               "\\describe"=,
               "\\enumerate"=,
               "\\itemize" = writeContent(block, tag),
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
               "\\verb" = writeWrapped(tag, block),
               "\\special" = writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block),
               ## cwhmisc has an empty \\email
               "\\email" = if (length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   of0('<a href="mailto:', url, '">', htmlify(url), '</a>')},
               ## FIXME: encode, not htmlify
               ## watch out for empty URLs (TeachingDemos has one)
               "\\url" = if(length(block)) {
                   url <- paste(as.character(block), collapse="")
                   url <- gsub("\n", "", url)
                   of0('<a href="', url, '">', url, '</a>')
               },
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
            	    block <- sub(paste(arg1, "[[:space:]]*,[[:space:]]*",
                                       sep = ""), "", block)
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
                        txt <- psub("^ ", "", as.character(block))
                        of1(txt)
                    } else writeBlock(block, tag, blocktag) # should not happen
                } else writeBlock(block, tag, blocktag)
    	    })
	}
	if (inlist)
	    switch(blocktag,
		"\\value"=,
		"\\arguments" = of1("</table>\n"),
		"\\itemize" = of1("</ul>\n"),
		"\\enumerate" = of1("</ol>\n"),
		# "\\value"=,
		"\\describe" = of1("</dl>\n"))
    }

    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword"))
            return() ## \alias only used on CHM header
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
    	if (nzchar(para)) of0("\n<", para, ">")
    	if (length(section)) {
	    ## There may be an initial \n, so remove that
	    s1 <- section[[1L]][1L]
	    if (RdTags(s1) == "TEXT" && s1 == "\n") section <- section[-1]
	    writeContent(section, tag)
	}
    	if (nzchar(para)) of0("</", para, ">\n")
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

    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    title <- Rd[[1L]]
    name <- htmlify(Rd[[2L]][[1L]])

    of0('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>R: ')
    ## special for now, as we need to remove leading and trailing spaces
    title <- trim(as.character(title))
    title <- htmlify(paste(psub1("^\\s+", "", title[nzchar(title)]),
                           collapse = " "))
    of1(title)
    of0('</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset=',
        mime_canonical_encoding(outputEncoding),
        '">\n')

    of0('<link rel="stylesheet" type="text/css"',
        if (no_links) 'href="R.css">' else 'href="../../R.css">',
        '\n</head><body>\n\n',
        '<table width="100%" summary="page for ', name, ' {', package,
        '}"><tr><td>',name,' {', package,
        '}</td><td align="right">R Documentation</td></tr></table>\n\n')

    of0("<h2>", title,'</h2>\n')

    for (i in seq_along(sections)[-(1:2)])
    	writeSection(Rd[[i]], sections[i])

    if(version != "")
        version <- paste('Package <em>', package,
                         '</em> version ', version,
                         ' ', sep='')
    of0('\n',
        '<hr><div align="center">[', version,
        if (!no_links) '<a href="00Index.html">Index</a>',
        ']</div>\n', '</body></html>\n')
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
        .readRDS(f)
    else if (file_test("-f", f <- file.path(dir, "Meta", "Rd.rds")))
        .build_links_index(.readRDS(f), basename(dir))
    else character()
}

.find_HTML_links_in_library <-
function(dir)
{
    if (file_test("-f", f <- file.path(dir, ".Meta", "links.rds")))
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
