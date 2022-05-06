#  File src/library/tools/R/Rd2HTML.R
#
#  Copyright (C) 1995-2021 The R Core Team
#  Part of the R package, https://www.R-project.org
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

## also used by Rd2latex, but only 'topic' and 'dest'
get_link <- function(arg, tag, Rdfile) {
    ## 'topic' is the name to display, 'dest' is the topic to link to
    ## optionaly in package 'pkg'.  If 'target' is set it is the file
    ## to link to in HTML help

    ## \link[=bar]{foo} means shows foo but treat this as a link to bar.
    ## \link[pkg]{bar} means show bar and link to *file* bar in package pkg
    ## \link{pkg:bar]{foo} means show foo and link to file bar in package pkg.
    ## As from 2.10.0, look for topic 'bar' if file not found.
    ## As from 4.1.0, prefer topic 'bar' over file 'bar' (in which case 'targetfile' is a misnomer)

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

## translation of Utils.pm function of the same name, plus "unknown"
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
        x <- psub("\\\\(dots|ldots)", "&hellip;", x)
        x <- fsub("\\infty", "&infin;", x)
        x <- fsub("\\sqrt", "&radic;", x)
    }
    x
}

shtmlify <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;" , s, fixed = TRUE)
    s <- gsub(">", "&gt;" , s, fixed = TRUE)
    s
}

## URL encode for use in href attributes.
urlify <- function(x, reserved = FALSE, repeated = FALSE) {
    ## When reserved is a logical, like
    ##   utils::URLencode(x, reserved)
    ## with '&' replaced by '&amp;' and hence directly usable for href
    ## attributes.  Equivalently, one could use
    ##   escapeAmpersand(utils::URLencode(x, reserved))
    ## Alternatively, reserved can be a string giving the reserved chars
    ## not to percent encode if it starts with a '^', and to percent
    ## encode otherwise (perhaps utils::URLencode() should be enhanced
    ## accordingly?).
    ##
    ## According to RFC 3986 <https://tools.ietf.org/html/rfc3986>, the
    ## reserved characters are
    ##   c(gendelims, subdelims)
    ## with
    ##   gendelims <- c(":", "/", "?", "#", "[", "]", "@")
    ##   subdelims <- c("!", "$", "&", "'", "(", ")", "*", "+", ",", ";", "=")
    ## The following is
    ##   paste(c(gendelims, subdelims), collapse = "")
    ## re-arranged for convenient use in regexp (negated) character
    ## classes:
    alldelims <- "][!$&'()*+,;=:/?@#"
    ## See also <https://url.spec.whatwg.org/#valid-url-string>.
    
    if(!repeated && grepl("%[[:xdigit:]]{2}", x, useBytes = TRUE)) {
        gsub("&", "&amp;", x, fixed = TRUE)
    } else {
        chars <- unlist(strsplit(x, ""))
        hex <- vapply(chars,
                      function(x)
                      paste0("%", toupper(as.character(charToRaw(x))),
                             collapse = ""),
                      "")
        if(is.character(reserved)) {
            reserved <- paste(reserved, collapse = "")
            reserved <- if(startsWith(reserved, "^"))
                            substring(reserved, 2L)
                        else      
                            rawToChar(setdiff(charToRaw(alldelims),
                                              charToRaw(reserved)))
            escape <- any(charToRaw(reserved) == charToRaw("&"))
        } else if(!reserved) {
            reserved <- alldelims
            escape <- TRUE
        } else {
            reserved <- ""
            escape <- FALSE
        }
        todo <- paste0("[^",
                       reserved,
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                       "abcdefghijklmnopqrstuvwxyz0123456789._~-",
                       "]")
        x <- paste(ifelse(grepl(todo, chars), hex, chars), collapse = "")
        if(escape)
            x <- gsub("&", "&amp;", x, fixed = TRUE)
        x
    }
}

urlify_email_address <- function(x) {
    ## As per RFC 6068
    ## <https://datatracker.ietf.org/doc/html/rfc6068#section-2> we must
    ## percent encode
    ##   "%"
    ##   from gendelims:   c("/", "?", "#", "[", "]")  
    ##   from subdelims:   c("&", ";", "=")
    urlify(x, reserved = "][#?/&;=%", repeated = TRUE)
}

urlify_doi <- function(x) {
    ## According to
    ##   <https://www.doi.org/doi_handbook/2_Numbering.html#2.2>
    ## a DOI name can "incorporate any printable characters from the
    ## legal graphic characters of Unicode".  The subsequent
    ##   <https://www.doi.org/doi_handbook/2_Numbering.html#htmlencoding>
    ## discussed encoding issues but is a bit vague.
    ## For now, percent encode all reserved characters but the slash.
    urlify(x, reserved = "^/", repeated = TRUE)
}

## Ampersands should be escaped in proper HTML URIs
escapeAmpersand <- function(x) gsub("&", "&amp;", x, fixed = TRUE)

invalid_HTML_chars_re <-
    "[\u0001-\u0008\u000b\u000c\u000e-\u001f\u007f-\u009f]"

## topics can contain weird characters like % / & !, so need to be
## encoded. We allow for different encodings for URLs and filenames
## (although mostly the same for now).

## & -> %26F is OK, & -> &amp; is NOT OK with dynamic help
topic2url <- function(x)
{
    if(config_val_to_logical(Sys.getenv("_R_HELP_USE_URLENCODE_",
                                        "FALSE")))
        utils::URLencode(x, reserved = TRUE)
    else
        vapply(x, urlify, "", reserved = TRUE) # to vectorize (used in toHTML.R)
}
topic2filename <- function(x)
    gsub("%", "+", utils::URLencode(x, reserved = TRUE))

## Create HTTP redirect files for aliases; called only during package
## installation if static help files are enabled. Files are named
## after aliases, which may contain 'undesirable' characters. These
## are escaped using topic2filename(). Analogous escaping needs to be
## done when creating links in HTML output as well, but ONLY for
## static HTML (dynamic help is already capable of handling such
## links)
createRedirects <- function(file, Rdobj)
{
    linksToTopics <-
        config_val_to_logical(Sys.getenv("_R_HELP_LINKS_TO_TOPICS_", "TRUE"))
    if (!linksToTopics) return(invisible()) # do nothing
    ## create a HTTP redirect for each 'alias' in .../pkg/help/
    redirHTML <-
        sprintf("<!DOCTYPE html>\n<html><head><meta http-equiv='refresh' content='0; url=../html/%s'><title>HTTP redirect</title></head><body></body></html>\n",
                urlify(basename(file), reserved = TRUE))
    toProcess <- which(RdTags(Rdobj) == "\\alias")
    helpdir <- paste0(dirname(dirname(file)), "/help") # .../pkg/help/
    aliasName <- function(i) trimws(Rdobj[[i]][[1]])
    aliasFile <- function(i) file.path(helpdir, sprintf("%s.html", topic2filename(aliasName(i))))
    redirMsg <- function(type, src, dest, status) {
        ## change sprintf to gettextf to make translatable, but seems unnecessary
        msg <- sprintf("\nREDIRECT:%s\t %s -> %s [ %s ]", type, src, dest, status)
        message(msg, appendLF = FALSE)
    }
    ## remove duplicate aliases, if any
    aliasesToProcess <- sapply(toProcess, aliasName)
    toProcess <- toProcess[!duplicated(aliasesToProcess)]
    for (i in toProcess) {
        aname <- aliasName(i)
        afile <- aliasFile(i)
        if (file.exists(afile)) {
            ## warning("Previous alias or file overwritten by alias: ", aname)
            msg <- sprintf("\nREDIRECT:topic\t Previous alias or file overwritten by alias: %s",
                           afile)
            message(msg, appendLF = FALSE)
        }
        try(suppressWarnings(cat(redirHTML, file = afile)), silent = TRUE) # Fails for \alias{%/%}
        ## redirMsg("topic", aname, basename(file), if (file.exists(afile)) "SUCCESS" else "FAILURE")
        if (!file.exists(afile)) redirMsg("topic", aname, basename(file), "FAILURE")
    }
    ## Also add .../pkg/help/file.html -> ../pkg/html/file.html as fallback
    ## when topic is not found (but do not overwrite)
    file.fallback <- file.path(helpdir, basename(file))
    if (!file.exists(file.fallback)) {
        try(cat(redirHTML, file = file.fallback), silent = TRUE)
        ## redirMsg("file", basename(file), basename(file), if (file.exists(file.fallback)) "SUCCESS" else "FAILURE")
        if (!file.exists(file.fallback)) redirMsg("file", basename(file), basename(file),  "FAILURE")
    }
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

Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, Links2 = NULL,
             stages = "render", outputEncoding = "UTF-8",
             dynamic = FALSE, no_links = FALSE, fragment=FALSE,
             stylesheet = if (dynamic) "/doc/html/R.css" else "R.css",
             texmath = getOption("help.htmlmath"),
             ...)
{
    ## Is this package help, as opposed to from Rdconv or similar?
    ## Used to decide whether redirect files should be created when
    ## generating static HTML
    package_help <- inherits(Rd, "Rd") && (length(package) == 2L)
    if (missing(no_links) && is.null(Links) && !dynamic) no_links <- TRUE
    linksToTopics <-
        config_val_to_logical(Sys.getenv("_R_HELP_LINKS_TO_TOPICS_", "TRUE"))
    enhancedHTML <-
        config_val_to_logical(Sys.getenv("_R_HELP_ENABLE_ENHANCED_HTML_", "TRUE"))
    version <- ""
    if(!identical(package, "")) {
        if(length(package) > 1L) {
            version <- package[2L]
            package <- package[1L]
        } else {
            dir <- dirname(package)
            if(nzchar(dir) &&
               file_test("-f", dfile <- file.path(package, "DESCRIPTION"))) {
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

    of0 <- function(...)
        writeLinesUTF8(paste0(...), con, outputEncoding, sep = "")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

    pendingClose <- pendingOpen <- character()  # Used for infix methods

    inEqn <- FALSE		# Should we do edits needed in an eqn?
    sectionLevel <- 0L		# How deeply nested within section/subsection
    inPara <- FALSE		# Are we in a <p> paragraph? If NA, we're not, but we're not allowed to be
    inAsIs <- FALSE             # Should we show characters "as is"?

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
                  "\\var"="var")
    # These have simple substitutions
    HTMLEscapes <- c("\\R"='<span style="font-family: Courier New, Courier; color: #666666;"><b>R</b></span>',
    		     "\\cr"="<br />",
    		     "\\dots"="...",
    		     "\\ldots"="...")
    ## These correspond to idiosyncratic wrappers
    HTMLLeft <- c("\\acronym"='<abbr><span class="acronym">',
    		  "\\donttest"="",
    		  "\\env"='<span class="env">',
                  "\\file"='&lsquo;<span class="file">',
                  "\\option"='<span class="option">',
                  "\\pkg"='<span class="pkg">',
                  "\\samp"='&lsquo;<span class="samp">&#8288;',
                  "\\sQuote"="&lsquo;",
                  "\\dQuote"="&ldquo;",
                  "\\verb"='<code style="white-space: pre;">&#8288;')
    HTMLRight <- c("\\acronym"='</span></abbr>',
    		   "\\donttest"="",
    		   "\\env"="</span>",
                   "\\file"='</span>&rsquo;',
                   "\\option"="</span>",
                   "\\pkg"="</span>",
                   "\\samp"="&#8288;</span>&rsquo;",
                   "\\sQuote"="&rsquo;",
                   "\\dQuote"="&rdquo;",
                   "\\verb"="&#8288;</code>")

    addParaBreaks <- function(x) {
	if (isBlankLineRd(x) && isTRUE(inPara)) {
	    inPara <<- FALSE
	    return("</p>\n")
	}
	## TODO: can we get 'start col' if no srcref ?
	if (utils:::getSrcByte(x) == 1L) x <- psub("^\\s+", "", x)
	if (isFALSE(inPara) && !all(grepl("^[[:blank:]\n]*$", x, perl = TRUE))) {
	    x <- c("<p>", x)
	    inPara <<- TRUE
	}
        x
    }

    enterPara <- function(enter = TRUE) {
	if (enter && isFALSE(inPara)) {
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
        saveAsIs <- inAsIs
        asis <- !is.na(match(tag, "\\command"))
        if(asis) inAsIs <<- TRUE
        if (!isBlankRd(block)) {
    	    of0("<", HTMLTags[tag], ">")
    	    writeContent(block, tag)
    	    of0("</",  HTMLTags[tag], ">")
    	}
        if(HTMLTags[tag] == "pre")
            inPara <<- FALSE
        if(asis) inAsIs <<- saveAsIs
    }

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
                htmlfile <- paste0("../../", urlify(package), "/help/", topic2url(topic))
                writeHref()
                return()
            }
            else if (linksToTopics && !is.null(Links) && !is.na(Links[topic]) &&
                     startsWith(Links[topic], paste0("../../", urlify(package)))) {
                ## only if the topic exists in the package (else look
                ## harder below). 'Links' contains all topics in the
                ## package, but also those in base+recommended
                ## packages. We do this branch only if this is a
                ## within-package link
                htmlfile <- paste0("../../", urlify(package), "/help/", topic2filename(topic), ".html")
                writeHref()
                return()

            } else { # identify actual file containing topic
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
            if (!dynamic && !linksToTopics && !no_links &&
                nzchar(pkgpath <- system.file(package = parts$pkg))) {
                ## old-style static HTML: prefer filename over topic,
                ## so treat as filename and topic2url() instead of
                ## topic2filename()
                htmlfile <- paste0(topic2url(parts$targetfile), ".html")
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
                    if (length(file)) {
                        ## warnRd(block, Rdfile,
                        ##        "file link ", sQuote(parts$targetfile),
                        ##        " in package ", sQuote(parts$pkg),
                        ##        " does not exist and so has been treated as a topic")
                        parts$targetfile <- basename(file)
                    } else {
                        warnRd(block, Rdfile, "missing file link ",
                               sQuote(parts$targetfile))
                    }
                }
            }
            if (parts$pkg == package) { # within same package
                if (linksToTopics)
                    htmlfile <-
                        if (dynamic) paste0("../help/", topic2url(parts$targetfile))
                        else paste0("../help/", topic2filename(parts$targetfile), ".html")
                else # use href = "file.html"
                    htmlfile <- paste0(topic2url(parts$targetfile), ".html")
                writeHref()
            } else {  # link to different package
                ## href = "../../pkg/html/file.html"
                if (linksToTopics)
                    htmlfile <-
                        if (dynamic) paste0("../../", urlify(parts$pkg), "/help/",
                                            topic2url(parts$targetfile))
                        else paste0("../../", urlify(parts$pkg), "/help/",
                                    topic2filename(parts$targetfile), ".html")
                else
                    htmlfile <- paste0("../../", urlify(parts$pkg), "/html/",
                                       topic2url(parts$targetfile), ".html") # FIXME Is this always OK ??
                writeHref()
            }
        }
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
        doParas <- (blocktag %notin% c("\\tabular"))
	switch(tag,
               UNKNOWN =,
               VERB = of1(vhtmlify(block, inEqn)),
               RCODE = of1(vhtmlify(block)),
               TEXT = of1(if(doParas && !inAsIs) addParaBreaks(htmlify(block)) else vhtmlify(block)),
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
               "\\var" = writeWrapped(tag, block, doParas),
               "\\special" = writeContent(block, tag), ## FIXME, verbatim?
               "\\linkS4class" =,
               "\\link" = writeLink(tag, block, doParas),
               ## cwhmisc has an empty \\email
               "\\email" = if (length(block)) {
                   url <- lines2str(as.character(block))
                   enterPara(doParas)
                   ## FIXME: urlify
                   of0('<a href="mailto:', urlify_email_address(url), '">',
                       htmlify(url), '</a>')},
               ## watch out for empty URLs (TeachingDemos had one)
               "\\url" = if(length(block)) {
                   url <- lines2str(as.character(block))
                   enterPara(doParas)
                   of0('<a href="', urlify(url), '">', htmlify(url), '</a>')
               },
               "\\href" = {
                   closing <-
                       if(length(block[[1L]])) {
                           url <- lines2str(as.character(block[[1L]]))
                           enterPara(doParas)
                           of0('<a href="', urlify(url), '">')
                           "</a>"
                       }
                       else ""
               	   savePara <- inPara
               	   inPara <<- NA
               	   writeContent(block[[2L]], tag)
               	   of0(closing)
               	   inPara <<- savePara
               },
               "\\Sexpr"= of0(as.character.Rd(block, deparse=TRUE)),
               "\\cr" =,
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
               "\\dQuote" =,
               "\\verb" = writeLR(block, tag, doParas),
               "\\dontrun"= writeDR(block, tag),
               "\\enc" = writeContent(block[[1L]], tag),
               "\\eqn" = {
                   block <-
                       if (doTexMath) block[[1L]]
                       else block[[length(block)]]
                   if(length(block)) {
                       enterPara(doParas)
                       inEqn <<- !doTexMath
                       if (doTexMath) of1('<code class="reqn">') # safer than of1('\\(') etc.
                       else of1("<i>")
                       ## FIXME: space stripping needed: see Special.html
                       writeContent(block, tag)
                       if (doTexMath) of1('</code>') # of1('\\)')
                       else of1("</i>")
                       inEqn <<- FALSE
                   }
               },
               "\\deqn" = {
                   block <-
                       if (doTexMath) block[[1L]]
                       else block[[length(block)]]
                   if(length(block)) {
                       inEqn <<- !doTexMath
                       leavePara(TRUE)
                       if (doTexMath) of1('<p style="text-align: center;"><code class="reqn">')
                       else of1('<p style="text-align: center;"><i>')
                       writeContent(block, tag)
                       if (doTexMath) of1('</code>\n')
                       else of0('</i>')
                       leavePara(FALSE)
                       inEqn <<- FALSE
                   }
               },
               "\\figure" = {
                   enterPara(doParas)
                   ## This is what is needed for static html pages
                   if(dynamic) of1('<img src="figures/')
                   else of1('<img src="../help/figures/')
                   writeContent(block[[1]], tag)
                   of1('" ')
               	   if (length(block) > 1L
               	       && length(imgoptions <- .Rd_get_latex(block[[2]]))
		       && startsWith(imgoptions[1L], "options: ")) {
		       ## There may be escaped percent signs within
		       imgoptions <- gsub("\\%", "%",
                                          paste(imgoptions,
                                                collapse = " "),
                                          fixed=TRUE)
                       of1(sub("^options: ", "", imgoptions))
                       ## Expert use may have forgotten alt ...
                       if(!grepl("\\balt *=", imgoptions)) {
                           of1(' alt="')
                           writeContent(block[[1L]], tag)
                           of1('"')
                       }
	           } else {
		       of1('alt="')
		       writeContent(block[[length(block)]], tag)
		       of1('"')
                   }
                   of1(' />')
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

	leavePara(NA)
	of1('\n<table>\n')
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
            	of0('<td style="text-align: ', format[col], ';">')
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
        }
        if (!newcol) of1('</td>')
        if (!newrow) of1('\n</tr>\n')
        of1('\n</table>\n')
        inPara <<- FALSE
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
            	if (tag == "RCODE" && startsWith(block, "(")) {
            	    block <- sub("^\\(", "", block)
            	    arg1 <- sub("[,)[:space:]].*", "", block)
		    block <- sub(paste0(arg1, "[[:space:]]*,[[:space:]]*"),
				 "", block)
            	    of0(arg1, pendingOpen)
                    pendingClose <<-
                        if(pendingOpen == "$") ""
                        else chartr("[", "]", pendingOpen)
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
                           "\\value" =  of1('<table>\n'),
                           "\\arguments" = of1('<table>\n'),
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
     		"\\arguments"= {
    		    of1('<tr style="vertical-align: top;"><td>')
    		    inPara <<- NA
                    ## Argh.  Quite a few packages put the items in
                    ## their value section inside \code.
                    if(identical(RdTags(block[[1L]])[1L], "\\code")) {
                        writeContent(block[[1L]], tag)
                    } else {
                        of1('<code>')
                        writeContent(block[[1L]], tag)
                        of1('</code>')
                    }
    		    of1('</td>\n<td>\n')
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
    	    	if (inlist && (blocktag %notin% c("\\itemize", "\\enumerate"))
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
                }
                else writeBlock(block, tag, blocktag) # "typical default"
    	    })
	}
	if (inlist) {
	    leavePara(FALSE)
	    switch(blocktag,
		"\\value"=,
		"\\arguments" = of1("</table>\n"),
		"\\itemize"   = of1("</li></ul>\n"),
		"\\enumerate" = of1("</li></ol>\n"),
		# "\\value"=,
		"\\describe"  = of1("</dl>\n"))
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
            if (dynamic && enhancedHTML && tag == "\\examples" && !is.null(firstAlias))
                of1(sprintf("<p><a href='../Example/%s'>Run examples</a></p>",
                            topic2url(firstAlias)))
            of1("<pre><code class='language-R'>")
            inPara <<- NA
            pre <- TRUE
        } else {
            inPara <<- FALSE
            pre <- FALSE
        }
    	if (length(section)) {
	    ## There may be an initial \n, so remove that
	    s1 <- section[[1L]][1L]
	    if (RdTags(section)[1] %in% c("TEXT", "RCODE") && s1 == "\n") section <- section[-1L]
	    writeContent(section, tag)
	}
	leavePara(FALSE)
	if (pre) of0("</code></pre>\n")
    	sectionLevel <<- save
    }

    ## ----------------------- Continue in main function -----------------------
    create_redirects <- FALSE
    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        } else {
	    con <- file(out, "wt")
            create_redirects <- !dynamic && package_help
	    on.exit(close(con))
	}
    } else {
    	con <- out
    	out <- summary(con)$description
    }

    Rd <- prepare_Rd(Rd, defines = defines, stages = stages,
                     fragment = fragment, ...)
    ## Check if man page already uses mathjaxr package
    ## (then skip mathjax processing)
    uses_mathjaxr <- function(rd)
    {
        done <- TRUE
        ## go through one by one until we hit \description
        for (frag in rd) {
            if (attr(frag, "Rd_tag") == "\\description") {
                done <- FALSE
                break
            }
        }
        if (done) return(FALSE)
        ## go through one by one until we hit \loadmathjax
        for (subfrag in frag) {
            if (identical(attr(subfrag, "Rd_tag"), "USERMACRO") &&
                identical(attr(subfrag, "macro"), "\\loadmathjax"))
                return(TRUE)
        }
        return(FALSE)
    }
    ## Both katex and mathjax need custom config scripts. For dynamic
    ## HTML these are in /doc/html/*-config.js (as well as the main
    ## katex script and CSS), but for static HTML, the appropriate
    ## relative path is not computable in general. So, for static HTML
    ## we only support katex, using a CDN for the main files and
    ## embedding the config in the output file itself
    if (is.null(texmath)) texmath <- "katex"
    if (texmath == "mathjax" && !dynamic) texmath <- "katex"
    doTexMath <- enhancedHTML && !uses_mathjaxr(Rd) &&
        texmath %in% c("katex", "mathjax")

    ## KaTeX / Mathjax resources (if they are used)
    if (doTexMath && texmath == "katex") {
        KATEX_JS <-
            if (dynamic) "/doc/html/katex/katex.js"
            else "https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.js"
        KATEX_CSS <- if (dynamic) "/doc/html/katex/katex.css"
                     else "https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.css"
        KATEX_CONFIG <-
            if (dynamic) "/doc/html/katex-config.js"
            else c("const macros = { \"\\\\R\": \"\\\\textsf{R}\", \"\\\\code\": \"\\\\texttt\"};", 
                   "function processMathHTML() {",
                   "    var l = document.getElementsByClassName('reqn');", 
                   "    for (let e of l) { katex.render(e.textContent, e, { throwOnError: false, macros }); }", 
                   "    return;",
                   "}")
    }
    if (doTexMath && texmath == "mathjax") {
        MATHJAX_JS <-
            if (dynamic && requireNamespace("mathjaxr", quietly = TRUE))
                "/library/mathjaxr/doc/mathjax/es5/tex-chtml-full.js"
            else
                "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"
        MATHJAX_CONFIG <-
            if (dynamic) "/doc/html/mathjax-config.js"
            else "../../../doc/html/mathjax-config.js"
    }
    if (enhancedHTML) {
        PRISM_JS <- 
            if (dynamic) "/doc/html/prism.js"
            else NULL # "../../../doc/html/prism.js"
        PRISM_CSS <- 
            if (dynamic) "/doc/html/prism.css"
            else NULL # "../../../doc/html/prism.css"
    }
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
        if (create_redirects) createRedirects(out, Rd)
	name <- htmlify(Rd[[2L]][[1L]])
        firstAlias <-
            trimws(Rd[[ which(sections == "\\alias")[1] ]][[1]])
        of0('<!DOCTYPE html>',
            "<html>",
	    '<head><title>')
	headtitle <- strwrap(.Rd_format_title(.Rd_get_title(Rd)),
	                     width=65, initial="R: ")
	if (length(headtitle) > 1) headtitle <- paste0(headtitle[1], "...")
	of1(htmlify(headtitle))
	of0('</title>\n',
	    '<meta http-equiv="Content-Type" content="text/html; charset=',
	    mime_canonical_encoding(outputEncoding),
	    '" />\n')
        of1('<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />\n')
        ## include CSS from prismjs.com for code highlighting
        if (enhancedHTML && length(PRISM_CSS) == 1L) of0('<link href="', urlify(PRISM_CSS), '" rel="stylesheet" />\n')
        if (doTexMath) {
            if (texmath == "katex") {
                of0('<link rel="stylesheet" href="', urlify(KATEX_CSS), '">\n',
                    if (dynamic) paste0('<script type="text/javascript" src="', urlify(KATEX_CONFIG), '"></script>\n')
                    else paste0('<script type="text/javascript">\n', paste(KATEX_CONFIG, collapse = "\n"), '</script>\n'),
                    '<script defer src="', urlify(KATEX_JS), '"\n    onload="processMathHTML();"></script>\n')
            }
            else if (texmath == "mathjax") {
                of0('<script type="text/javascript" src="', urlify(MATHJAX_CONFIG), '"></script>\n',
                    '<script type="text/javascript" async src="', urlify(MATHJAX_JS), '"></script>\n')
            }
        }
	of0('<link rel="stylesheet" type="text/css" href="',
	    urlify(stylesheet),
	    '" />\n',
	    '</head><body><div class="container">\n\n',
	    '<table style="width: 100%;">',
            '<tr><td>',
            name)
	if (nchar(package))
	    of0(' {', package, '}')
	of0('</td><td style="text-align: right;">R Documentation</td></tr></table>\n\n')

	of1("<h2>")
	inPara <- NA
	title <- Rd[[1L]]
	writeContent(title,sections[1])
	of1("</h2>")
	inPara <- FALSE

	for (i in seq_along(sections)[-(1:2)])
	    writeSection(Rd[[i]], sections[i])

	if(nzchar(version))
	    version <- paste0('Package <em>',package,'</em> version ',version,' ')
	of0('\n')
	if(nzchar(version))
	    of0('<hr /><div style="text-align: center;">[', version,
		if (!no_links) '<a href="00Index.html">Index</a>',
		']</div>')
	of0('\n</div>\n')
        ## include JS from prismjs.com for code highlighting
        if (enhancedHTML && length(PRISM_JS) == 1L) of0('<script src="', urlify(PRISM_JS), '"></script>\n')
        of0('</body></html>\n')
    }
    invisible(out)
} ## Rd2HTML()


## The following functions return 'relative' links assuming that all
## packages are installed in the same virtual library tree.

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

## These helper functions can optionally return the absolute path as
## well (in the local file system)

.find_HTML_links_in_package <-
function(dir, absolute = FALSE)
{
    ans <- 
        if (file_test("-f", f <- file.path(dir, "Meta", "links.rds")))
            readRDS(f)
        else if (file_test("-f", f <- file.path(dir, "Meta", "Rd.rds")))
            .build_links_index(readRDS(f), basename(dir))
        else character()
    if (absolute)
        structure(file.path(dir, "html", basename(ans), fsep = "/"),
                  names = names(ans))
    else
        ans
}

.find_HTML_links_in_library <-
function(dir, absolute = FALSE)
{
    ans <- 
        if (file_test("-f", f <- file.path(dir, ".Meta", "links.rds")))
            readRDS(f)
        else
            .build_library_links_index(dir)
    if (absolute)
        structure(file.path(dir, substring(ans, first = 7), fsep = "/"), # drop initial "../../"
                  names = names(ans))
    else
        ans
}

.build_library_links_index <-
function(dir)
{
    unlist(lapply(rev(dir(dir, full.names = TRUE)),
                  .find_HTML_links_in_package))
}

