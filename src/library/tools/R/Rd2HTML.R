#  File src/library/tools/R/Rd2HTML.R
#
#  Copyright (C) 1995-2024 The R Core Team
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
    ## 'topic' is the text to display (used by Rd2latex, also as \index entry),
    ## 'dest' is the topic to link to (unless for option [pkg:bar]).
    ## Package-anchored links have non-NULL 'pkg' and 'targetfile',
    ## where the latter is the topic/file to link to in HTML help.

    ## \link{foo}: show and link to topic foo.
    ## \link[=bar]{foo} means shows foo but treat this as a link to *topic* bar.
    ## \link[pkg]{bar} means show bar and link to topic/file bar in package pkg.
    ## \link[pkg:bar]{foo} means show foo and link to topic/file bar in package pkg.
    ## As from 2.10.0, look for topic 'bar' if file not found.
    ## As from 4.1.0, prefer topic 'bar' over file 'bar' (in which case 'targetfile' is a misnomer)
    ## As from 4.5.0, allow markup in link text for variants 2 and 4.

    isTEXT <- all(RdTags(arg) == "TEXT")
    option <- attr(arg, "Rd_option")

    topic <- dest <- paste(unlist(arg), collapse = "")
    targetfile <- NULL
    pkg <- NULL
    if (!is.null(option)) {
        if (!identical(attr(option, "Rd_tag"), "TEXT"))
    	    stopRd(option, Rdfile, "Bad \\link option -- must be text")
        option <- as.character(option)
        if (startsWith(option, "="))
    	    dest <- psub1("^=", "", option)
        else if (grepl(":", option, fixed = TRUE)) {
    	    targetfile <- psub1("^[^:]*:", "", option)
    	    pkg <- psub1(":.*", "", option)
    	} else {
            if (!isTEXT)
                stopRd(arg, Rdfile, "Bad \\link[pkg]{topic} -- argument must be text")
            targetfile <- dest
            pkg <- option
    	}
    } else if (!isTEXT)
        stopRd(arg, Rdfile, "Bad \\link topic -- must be text")

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
    s <- fsub("&", "&amp;", s)
    s <- fsub("<", "&lt;" , s)
    s <- fsub(">", "&gt;" , s)
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
    ## According to RFC 3986 <https://www.rfc-editor.org/rfc/rfc3986>, the
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
    ## <https://www.rfc-editor.org/rfc/rfc6068#section-2> we must
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
## The next few are for generating URL fragment ids
string2id <- function(x)
    gsub("%", "+", utils::URLencode(x, reserved = TRUE))
name2id <- function(x) string2id(x)
topic2id <- function(x) sprintf("topic+%s", string2id(x))
topic2href <- function(x, destpkg = NULL, hooks = list())
{
    if (is.null(destpkg)) sprintf("#%s", topic2id(x))
    else {
        FUN <- hooks$pkg_href
        if (is.null(FUN)) FUN <- function(pkg) sprintf("%s.html", pkg)
        ## Need a way to turn links to unavailable packages into
        ## "nothing", e.g. when building package HTML refmans.
        ## We do so if FUN() gave "nothing" or the special '#'.
        if(!length(s <- FUN(destpkg)) || (s == "#"))
            "#"
        else
            sprintf("%s#%s", s, topic2id(x))
    }
}

## We want to give an id to each top-level section tags, arguments,
## and _maybe_ other \item{} objects (eventually). The following
## function tries to implement a rule to construct such an id given
## relevant information.

## Ideally, the id-s should be predictable so we could reference them
## from other help pages using some standard markup, but this may or
## may not be possible. The standard sections should be unique, so can
## be standardized, but arbitrary \section{}-s and \item{}-s inside
## sections other than \argument{}-s are potentially problematic. In
## addition, the HTML may be used in standalone help pages or in a
## combined per-package refman; in the second case, id-s must be
## distinguished for different pages. For this, we will use the
## \name{} of a page, which must uniquely define every help page
## within a package.

## The current rule is as follows:
## - If name != NULL, it will be used as a prefix
## - Standard sections will get standardized ids
## - remaining sections will require a string to be supplied

## Note that tagid can be a vector (for comma-separated items)

tag2id <- function(tag, name = NULL, tagid = section2id[tag])
{
    section2id <- 
        c("\\description" = "_sec_description", "\\usage"    = "_sec_usage",
          "\\arguments"   = "_sec_arguments",   "\\format"   = "_sec_format",
          "\\details"     = "_sec_details",     "\\note"     = "_sec_note",
          "\\section"     = "_sec_section",     "\\author"   = "_sec_author",
          "\\references"  = "_sec_references",  "\\source"   = "_sec_source",
          "\\seealso"     = "_sec_seealso",     "\\examples" = "_sec_examples",
          "\\value"       = "_sec_value")
    if (anyNA(tagid)) return(NULL) # or "" ?
    id <- if (is.null(name)) tagid
          else paste(name2id(name), tagid, sep = "_:_")
    string2id(gsub("[[:space:]]+", "-", id))
}

rdfragment2text <- function(rd, html = TRUE)
{
    if (html) {
        ## utils::capture.output(Rd2HTML(rd, fragment = TRUE)) has
        ## unclosed <p>. Handle this as tools:::.extract_news_from_Rd
        ## does
        s <- utils::capture.output(Rd2HTML(rd, fragment = TRUE)) |> trimws()
        i <- which(startsWith(s, "<p>") & !endsWith(s, "</p>"))
        if (length(i)) {
            z <- s[i]
            j <- which((lengths(gregexpr("</?p>", z)) %% 2L) > 0L)
            if (length(j)) 
                s[i[j]] <- paste0(z[j], "</p>")
        }
        paste(s, collapse = "\n")
    }
    else
        (utils::capture.output(Rd2txt(rd, fragment = TRUE))
            |> paste(collapse = "\n")
            |> trimws())
}


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
    aliasesToProcess <- vapply(toProcess, aliasName, "")
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

## There is a third use (from R 4.4.0), which is to generate
## single-page HTML refmans for an entire package via pkg2HTML(),
## which calles Rd2HTML(standalone = FALSE) for each help page. 


Rd2HTML <-
    function(Rd, out = "", package = "", defines = .Platform$OS.type,
             Links = NULL, Links2 = NULL,
             stages = "render", outputEncoding = "UTF-8",
             dynamic = FALSE, no_links = FALSE, fragment = FALSE,
             stylesheet = if (dynamic) "/doc/html/R.css" else "R.css",
             texmath = getOption("help.htmlmath"),
             concordance = FALSE,
             standalone = TRUE,
             hooks = list(),
             toc = isTRUE(getOption("help.htmltoc")),
             Rhtml = FALSE, # TODO: guess from 'out' if non-missing
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
    if (!no_links && !linksToTopics && !standalone) {
        warning("links not supported for 'standalone = FALSE' when _R_HELP_LINKS_TO_TOPICS_=false")
        no_links <- TRUE
    }
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
    
    if (concordance)
    	conc <- activeConcordance()
    else
    	conc <- NULL
    if (toc) { # not meaningful unless standalone = TRUE
        if (!standalone) toc <- FALSE
        else toc_entries <- list()
    }

    skipNewline <- FALSE
    of0 <- function(...)
        of1(paste0(...))
    of1 <- function(text) {
        if (skipNewline) {
            skipNewline <<- FALSE
            if (text == "\n") return()
        }
    	if (concordance)
    	    conc$addToConcordance(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")
    }

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
    HTMLEscapes <- c("\\R"='<span class="rlang"><b>R</b></span>',
    		     "\\cr"="<br>",
    		     "\\dots"="...",
    		     "\\ldots"="...")
    ## These correspond to idiosyncratic wrappers
    HTMLLeft <- c("\\abbr"='<abbr>',
                  "\\acronym"='<abbr><span class="acronym">',
    		  "\\donttest"="", "\\dontdiff"="",
    		  "\\env"='<span class="env">',
                  "\\file"='&lsquo;<span class="file">',
                  "\\option"='<span class="option">',
                  "\\pkg"='<span class="pkg">',
                  "\\samp"='&lsquo;<span class="samp">&#8288;',
                  "\\sQuote"="&lsquo;",
                  "\\dQuote"="&ldquo;",
                  "\\verb"='<code style="white-space: pre;">&#8288;')
    HTMLRight <- c("\\abbr"="</abbr>",
                   "\\acronym"='</span></abbr>',
    		   "\\donttest"="", "\\dontdiff"="",
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
	    x <- paste0("<p>", x)
	    inPara <<- TRUE
	}
        x
    }

    enterPara <- function(enter = TRUE) {
	if (enter && isFALSE(inPara)) {
            of1("<p>")
            inPara <<- TRUE
        }
    }

    leavePara <- function(newval) {
    	if (isTRUE(inPara)) of1("</p>\n")
    	inPara <<- newval
    }

    writeItemAsCode <- function(blocktag, block, addID = blocktag == "\\arguments") {
        ## Argh.  Quite a few packages put the items in their value
        ## section inside \code.
        for(i in which(RdTags(block) == "\\code"))
            attr(block[[i]], "Rd_tag") <- "Rd"

        ## Usually RdTags(block)[1L] == "TEXT", except when it is
        ## \\dots, \\ldots, etc. Potentially more complicated in cases
        ## like \item{foo, \dots, bar}, where block will have length >
        ## 1. We want to (a) split on comma and treat each part as a
        ## separate argument / value, and (b) for blocktag==arguments
        ## only, add an id tag so that we can have internal links.

        ## We do this by 'deparsing' block[[1L]], using as.character.Rd()
        s <- as.character.Rd(block)
        toEsc <- s %in% names(HTMLEscapes)
        if (any(toEsc)) s[toEsc] <- HTMLEscapes[s[toEsc]]

        ## Now just join, split on comma, wrap individually inside
        ## </code>, and unsplit. This will be problematic if any
        ## TeX-like macros remain, but that should not happen in
        ## practice for \item-s inside \arguments or \value.

        s <- trimws(strsplit(paste(s, collapse = ""), ",", fixed = TRUE)[[1]])
        s <- s[nzchar(s)] # unlikely to matter, but just to be safe
        item_value <- vhtmlify(s)
        s <- if (addID) {
                 item_id <- tag2id(name = if (standalone) NULL else name, tagid = s)
                 if (toc)
                     toc_entries <<-
                         c(toc_entries,
                           list(argitem =
                                    list(id = item_id,
                                         value = sprintf("<code>%s</code>",
                                                         item_value))))
                 sprintf('<code id="%s">%s</code>', item_id, item_value)
             }
             else sprintf('<code>%s</code>', item_value)
        s <- paste0(s, collapse = ", ")
        of1(s)
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
                htmlfile <-
                    if (standalone)
                        paste0("../../", urlify(package), "/help/", topic2filename(topic), ".html")
                    else
                        topic2href(topic) # htmlfile is actually a link target within current file
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
                if (!standalone) {
                    htmlfile <- topic2href(topic,
                                           destpkg = strsplit(htmlfile, "/", fixed = TRUE)[[1]][[3]],
                                           hooks = hooks)
                }
                else {
                    ## treat links in the same package specially -- was needed for CHM
                    pkg_regexp <- paste0("^../../", urlify(package), "/html/")
                    if (grepl(pkg_regexp, htmlfile)) {
                        htmlfile <- sub(pkg_regexp, "", htmlfile)
                    }
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
                        else if (standalone) paste0("../help/", topic2filename(parts$targetfile), ".html")
                        else topic2href(parts$targetfile)
                else # use href = "file.html"
                    htmlfile <- paste0(topic2url(parts$targetfile), ".html")
                writeHref()
            } else {  # link to different package
                ## href = "../../pkg/html/file.html"
                if (linksToTopics)
                    htmlfile <-
                        if (dynamic) paste0("../../", urlify(parts$pkg), "/help/",
                                            topic2url(parts$targetfile))
                        else if (standalone) paste0("../../", urlify(parts$pkg), "/help/",
                                                    topic2filename(parts$targetfile), ".html")
                        else topic2href(parts$targetfile,
                                        destpkg = urlify(parts$pkg),
                                        hooks = hooks)
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
        if (Rhtml && length(block) > 1L)
            of1("\nend.rcode-->\n\n<!--begin.rcode eval=FALSE\n")
        of1('## Not run: ')
        writeContent(block, tag)
        if (length(block) > 1L) {
            of1('\n## End(Not run)')
            if (Rhtml) of1("\nend.rcode-->\n\n<!--begin.rcode\n")
        }
    }

    writeBlock <- function(block, tag, blocktag) {
    	if (concordance)
    	    conc$saveSrcref(block)
        doParas <- (blocktag %notin% c("\\tabular"))
	switch(tag,
               UNKNOWN =,
               VERB = if (Rhtml && blocktag == "\\dontrun") of1(block)
                      else of1(vhtmlify(block, inEqn)),
               RCODE = if (Rhtml) of1(block) else of1(vhtmlify(block)),
               TEXT = of1(if(doParas && !inAsIs) addParaBreaks(htmlify(block)) else vhtmlify(block)),
               USERMACRO =,
               "\\newcommand" =,
               "\\renewcommand" = {},
               COMMENT = if (utils:::getSrcByte(block) == 1L)
                             skipNewline <<- TRUE,
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
               "\\abbr" =,
               "\\acronym" =,
               "\\donttest" =, "\\dontdiff" =,
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
                       else of0('<p style="',
                                if (length(block) <= 3) 'text-align: center'
                                else 'white-space: pre', # as in Rd2txt()
                                ';"><i>')
                       writeContent(block, tag)
                       if (doTexMath) of1('</code>\n')
                       else of1('</i>')
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
                   of1('>')
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
	if (concordance)
	    conc$saveSrcref(table)
        newrow <- TRUE
        newcol <- TRUE
        ## Argh.  As of 2025-08, about 3000 CRAN packages have \\tabular
        ## with a trailing \cr ending the last row, which is invalid as
        ## per R-exts (the \cr starts another row which has a different
        ## number of fields than the other rows), and when processed
        ## results in bad HTML (spotted by v.NU but not HTML Tidy).  We
        ## could have checkRd() complain, but given the number of
        ## offenders let's drop such trainling \cr before processing, at
        ## least for the time being. 
        if(any(ind <- (tags == "\\cr"))) {
            i <- max(which(ind))
            j <- seq.int(i + 1L, length.out = length(content) - i)
            if(all(grepl("^[[:space:]]*$",
                         vapply(content[j], .Rd_deparse, "")))) {
                content <- content[-i]
                tags <- tags[-i]
            }
        }
        for (i in seq_along(tags)) {
            if (concordance)
                conc$saveSrcref(content[[i]])
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
            if (concordance)
            	conc$saveSrcref(block)
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
                           "\\value" =  of1('<table role = "presentation">\n'),
                           "\\arguments" = of1('<table role = "presentation">\n'),
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
    		    of1('<tr><td>')
    		    inPara <<- NA
                    writeItemAsCode(blocktag, block[[1L]])
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
    	    	           && tag != "COMMENT"
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

        ## compute id and toc entries if required
        if (toc) {
            if (tag %in% c("\\section", "\\subsection")) {
                sec_value <- rdfragment2text(section[[1L]])
                sec_id <-
                    tag2id(name = if (standalone) NULL else name,
                           tagid = rdfragment2text(section[[1L]], html = FALSE))
            }
            else {
                sec_value <- paste0("<p>", sectionTitles[tag], "</p>")
                sec_id <- tag2id(tag = tag, name = if (standalone) NULL else name)
            }
            toc_entry <- list(id = trimws(sec_id), value = trimws(sec_value))
            toc_entries <<-
                c(toc_entries,
                  if (tag == "\\subsection") list(subsection = toc_entry)
                  else list(section = toc_entry))
            of1(paste0("\n\n<h", sectionLevel+2L, " id='", sec_id, "'>"))
        }
        else of1(paste0("\n\n<h", sectionLevel+2L, ">"))
        if (concordance)
            conc$saveSrcref(section)
    	if (tag == "\\section" || tag == "\\subsection") {
    	    title <- section[[1L]]
    	    section <- section[[2L]]
            ## FIXME: this needs trimming of whitespace
    	    writeContent(title, tag)
    	} else
    	    of1(sectionTitles[tag])
        of1(paste0("</h", sectionLevel+2L, ">\n\n"))
        if (tag == "\\usage") {
            of1("<pre><code class='language-R'>")
            inPara <<- NA
            pre <- TRUE
        } else if (tag == "\\examples") {
            if (dynamic && enhancedHTML && !Rhtml && !is.null(firstAlias))
                of1(sprintf("<p><a href='../Example/%s'>Run examples</a></p>",
                            topic2url(firstAlias)))
            if (Rhtml) of1("\n\n<!--begin.rcode\n") else of1("<pre><code class='language-R'>")
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
        if (pre) # must be \usage or \examples
            if (Rhtml && tag == "\\examples") of1("\nend.rcode-->\n\n")
            else of1("</code></pre>\n")
    	sectionLevel <<- save
    }

    ## Write a navigation menu (if toc == TRUE) based on toc_entries
    writeNav <- function() {

        of0('<nav class="topic" aria-label="Section Navigation">\n',
            '<div class="dropdown-menu">\n',
            '<h1>Contents</h1>\n',
            '<ul class="menu">\n')

        currentLevel <- 1L # entry_types = argitem, subsection are level 2
        ## toc_entries <- list( section|subsection|argitem = list(id, value) )
        entry_types <- names(toc_entries)
        for (i in seq_along(toc_entries)) {
            newLevel <-
                if (entry_types[[i]] %in% c("argitem", "subsection")) 2L
                else 1L
            if (newLevel > currentLevel) of1("  <ul>")
            else if (newLevel < currentLevel) of1("  </ul>")
            currentLevel <- newLevel
            e <- toc_entries[[i]] # id, value can be vectors
            of0(sprintf("<li><a href='#%s'>%s</a></li>\n", e$id, e$value))
        }

        of0('</ul>\n',
            '</div>\n',
            '</nav>')
    }


    
    ## ----------------------- Continue in main function -----------------------
    info <- list() # attribute to be returned if standalone = FALSE
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
	if (concordance)
            conc$saveSrcref(.Rd_get_section(Rd, "title"))
	headtitle <- strwrap(.Rd_format_title(.Rd_get_title(Rd)),
	                     width=65, initial="R: ")
	if (length(headtitle) > 1) headtitle <- paste0(headtitle[1], "...")

        ## Create HTML header and footer
        if (standalone) {
            hfcomps <- # should we be able to specify static URLs here?
                HTMLcomponents(title = headtitle, logo = FALSE,
                               up = NULL,
                               top = NULL,
                               css = stylesheet,
                               outputEncoding = outputEncoding,
                               dynamic = dynamic, prism = enhancedHTML,
                               doTexMath = doTexMath, texmath = texmath,
                               PRISM_CSS_STATIC = NULL, PRISM_JS_STATIC = NULL)
            of1(paste(hfcomps$header, collapse = "")) # write out header
            of1('<main>')
            of0('\n\n<table style="width: 100%;">',
                '<tr><td>',
                name)
            if (nchar(package))
                of0(' {', package, '}')
            of0('</td><td style="text-align: right;">R Documentation</td></tr></table>\n\n')
        }

        ## id can identify help page when combined with others, and
        ## also needed to form argument id-s programmatically
        if (!standalone) {
            of0("<h2 id='", name2id(name), "'>")
        }
        else
            of0("<h2>")
	inPara <- NA
	title <- Rd[[1L]]
        info$name <- name
        info$title <- rdfragment2text(title)
        info$htmltitle <- info$title # Rd2HTML(fragment = TRUE) gives unbalanced <p>
        info$mathjaxr <- uses_mathjaxr(Rd)
        info$pkgsummary <- FALSE # possibly updated below if alias ends with '-package'
	if (concordance)
	    conc$saveSrcref(title)
	writeContent(title, sections[1])
	of1("</h2>")
	inPara <- FALSE
        if (!standalone) {
            ## create empty spans with aliases as id, so that we can link
            for (a in unique(trimws(unlist(Rd[ which(sections == "\\alias") ])))) {
                if (endsWith(a, "-package")) info$pkgsummary <- TRUE
                of0("<span id='", topic2id(a), "'></span>")
            }
        }
	for (i in seq_along(sections)[-(1:2)])
	    writeSection(Rd[[i]], sections[i])

	if(nzchar(version))
	    version <- paste0('Package <em>', package, '</em> version ', version, ' ')
	of1('\n')
        if (standalone) {
            if(nzchar(version))
                of0('<hr><div style="text-align: center;">[', version,
                    if (!no_links) '<a href="00Index.html">Index</a>',
                    ']</div>')
            of1('</main>\n')
            if (toc) writeNav()
            of1(paste(hfcomps$footer, collapse = "")) # write out footer
        }
        else attr(out, "info") <- info
    }
    if (concordance) {
    	conc$srcFile <- Rdfile
    	concdata <- followConcordance(conc$finish(), attr(Rd, "concordance"))
    	# NB:  Prior to R 4.3.0, the srcFile could be
    	#      an absolute path, possibly containing a 
    	#      colon.  This strips the leading part up to
    	#      "/man/".
    	concdata$srcFile <- stripPathTo(concdata$srcFile, "man")
    	attr(out, "concordance") <- concdata
    	of0('<!-- ',
    	    as.character(concdata),
    	    ' -->\n')
    }
    invisible(out)
} ## Rd2HTML()


## The following functions return 'relative' links assuming that all
## packages are installed in the same virtual library tree.

findHTMLlinks <-
function(pkgDir, lib.loc = NULL, level = 0 : 3)
{
    ## A variant of the above which splits levels for base and
    ## recommended packages, such that
    ##   Level 0: this package (installed in pkgDir)
    ##   Level 1: base packages
    ##   Level 2: recommended packages
    ##   Level 3: all packages installed in lib.loc
    if (is.null(lib.loc)) lib.loc <- .libPaths()

    Links <- list()
    if(3 %in% level)
        Links <- c(Links, lapply(lib.loc, .find_HTML_links_in_library))
    if(2 %in% level)
        Links <- c(lapply(file.path(.Library,
                                    .get_standard_package_names()$recommended),
                          .find_HTML_links_in_package),
                   Links)
    if(1 %in% level)
        Links <- c(lapply(file.path(.Library,
                                    .get_standard_package_names()$base),
                          .find_HTML_links_in_package),
                   Links)
    if (0 %in% level && nzchar(pkgDir))
        Links <- c(list(.find_HTML_links_in_package(pkgDir)), Links)
    Links <- unlist(Links)
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

.DESCRIPTION_to_HTML <- function(descfile, dynamic = FALSE) {

    ## Similar to .DESCRIPTION_to_latex().

    trfm <- .gsub_with_transformed_matches

    ## A variant of htmlify() which optionally adds hyperlinks and does
    ## not HTMLify dashes inside these.
    htmlify_text <- function(x, a = FALSE, d = FALSE) {
        ## Use 'd' to indicate HTMLifying Description texts,
        ## transforming DOI and arXiv pseudo-URIs.
        x <- fsub("&", "&amp;", x)
        x <- fsub("``", "&ldquo;", x)
        x <- fsub("''", "&rdquo;", x)
        x <- psub("`([^']+)'", "&lsquo;\\1&rsquo;", x)
        x <- fsub("`", "'", x)
        x <- fsub("<", "&lt;", x)
        x <- fsub(">", "&gt;", x)
        if(a) {
            ## URL regexp as in .DESCRIPTION_to_latex().  CRAN uses
            ##   &lt;(URL: *)?((https?|ftp)://[^[:space:]]+)[[:space:]]*&gt;
            ##   ([[:space:]])((https?|ftp)://[[:alnum:]/.:@+\\_~%#?=&;,-]+[[:alnum:]/])
            ## (also used in toRd.citation().
            x <- trfm("&lt;(http://|ftp://|https://)([^[:space:],>]+)&gt;",
                      "<a href=\"\\1%s\">\\1\\2</a>",
                      x,
                      urlify,
                      2L)
        }
        if(d) {
            x <- trfm("&lt;(DOI|doi):[[:space:]]*([^<[:space:]]+[[:alnum:]])&gt;",
                      "&lt;<a href=\"https://doi.org/%s\">doi:\\2</a>&gt;",
                      x,
                      ## <FIXME>
                      ## Why not urlify?
                      function(u) utils::URLencode(u, TRUE),
                      ## </FIXME>
                      2L)
            x <- trfm("&lt;(arXiv|arxiv):(([[:alpha:].-]+/)?[[:digit:].]+)(v[[:digit:]]+)?([[:space:]]*\\[[^]]+\\])?&gt;",
                      "&lt;<a href=\"https://doi.org/10.48550/arXiv.%s\">doi:10.48550/arXiv.\\2</a>&gt;",
                      x,
                      urlify,
                      2L)
        }
        if(a || d) {
            ## Avoid mdash/ndash htmlification in the anchored parts.
            m <- gregexpr("<a href=\"[^>]*\">[^<>]*</a>", x)
            regmatches(x, m, invert = TRUE) <-
                lapply(regmatches(x, m, invert = TRUE),
                       function(x) {
                           x <- fsub("---", "&mdash;", x)
                           x <- fsub("--", "&ndash;", x)
                           x
                       })
        } else {
            x <- fsub("---", "&mdash;", x)
            x <- fsub("--", "&ndash;", x)
        }
        x
    }

    htmlify_compare_ops <- function(x) {
        x <- fsub("<=", "&le;", x)
        x <- fsub(">=", "&ge;", x)
        x <- fsub("!=", "&ne;", x)
        x
    }
    
    htmlify_license_spec <- function(x, p) {
        do_one <- function(x) {
            x <- gsub("[[:space:]]*([+|])[[:space:]]*", " \\1 ", x)
            a <- analyze_license(x)
            if(!a$is_standardizable) return(htmlify(x))

            htmlify_component_texts <- function(x) {
                x <- fsub("&", "&amp;", x)
                x <- fsub("<=", "&le;", x)
                x <- fsub(">=", "&ge;", x)
                x <- fsub("!=", "&ne;", x)
                x <- fsub("<", "&lt;", x)
                x <- fsub(">", "&gt;", x)
                x
            }

            components <- a$components
            expansions <- unlist(a$expansions)
            expanded <- length(expansions) > length(components)
            y <- character(length(expansions))
        
            ## Unlimited.
            y[expansions == "Unlimited"] <- "Unlimited"
            
            ## License file pointers.
            ## <FIXME>
            ## For now only hyperlink for dynamic help.
            re <- "(.*[^[:space:]])?(([[:space:]]*\\+[[:space:]]*)?file )(LICEN[CS]E)"
            ind <- grepl(re, expansions)
            if(any(ind)) {
                y[ind] <-
                    sub(re,
                        if(dynamic) {
                            sprintf("\\2<a href=\"/library/%s/\\4\">\\4</a>",
                                    p)
                        } else "\\2\\4",
                        expansions[ind])
                expansions[ind] <- sub(re, "\\1", expansions[ind])
            }
            ## </FIXME>

            ## Components with labels in the R license db.
            ## For dynamic help, use the common licenses shipped with R
            ## instead of the R-project.org license URLs.
            ldb <- R_license_db()
            pos <- match(expansions, ldb$Labels)
            ind <- !is.na(pos)
            if(any(ind)) {
                pos <- pos[ind]
                urls <- if(dynamic) {
                            paths <- ldb[pos, "File"]
                            ifelse(nzchar(paths),
                                   sprintf("/licenses/%s",
                                           basename(paths)),
                                   ldb[pos, "URL"])
                        } else
                            urls <- ldb[pos, "URL"]
                texts <- if(expanded) {
                             expansions[ind]
                         } else {
                             sub("[[:space:]]*\\+.*", "", components[ind])
                         }
                y[ind] <-
                    sprintf("<a href=\"%s\">%s</a>%s",
                            vapply(urls, urlify, ""),
                            htmlify_component_texts(texts),
                            y[ind])
            }
            
            y <- paste(y, collapse = " | ")
            if(expanded) {
                y <- sprintf("%s [expanded from: %s]",
                             y,
                             paste(htmlify_component_texts(components),
                                   collapse = " | "))
            }
        
            y
        }

        v <- unique(x)
        s <- vapply(v, do_one, "")
        s[match(x, v)]
    }

    htmlify_depends_spec <- function(x) {
        chunks <- strsplit(x, ",")
        ## Canonicalize.
        entries <- sub("^[[:space:]]*(.*)[[:space:]]*$", "\\1",
                       unlist(chunks, use.names = FALSE))
        entries <- sub("[[:space:]]*\\(", " (", entries)
        ## Try splitting at the first white space.
        pos <- regexpr("[[:space:]]", entries)
        names <- ifelse(pos == -1L, entries,
                        substring(entries, 1L, pos - 1L))
        rests <- ifelse(pos == -1L, "", substring(entries, pos))
        found <- logical(length(names))
        for(lib.loc in .libPaths()) {
            ## Very basic test for installed package ...
            found <- found | file.exists(file.path(lib.loc, names,
                                                   "DESCRIPTION"))
        }
        names[found] <- sprintf("<a href=\"/library/%s\">%s</a>",
                                names[found],
                                names[found])
        vapply(split(paste(names, rests, sep = ""),
                     rep.int(seq_along(chunks), lengths(chunks))),
               paste, "", collapse = ", ")
    }

    ## See <https://orcid.org/trademark-and-id-display-guidelines> for
    ## ORCID identifier display guidelines.
    ## We want the ORCID id transformed into a hyperlinked ORCID icon
    ## right after the family name (but before the roles).  We can
    ## achieve this by adding the canonicalized ORCID id (URL) to the
    ## 'family' element and simultaneously dropping the ORCID id from
    ## the 'comment' element, and then re-format.
    ## See <https://ror.readme.io/docs/display> for ROR display
    ## guidelines.
    .format_authors_at_R_field_with_expanded_identifiers <- function(a) {
        x <- utils:::.read_authors_at_R_field(a)
        format_person1 <- function(e) {
            cmt <- e$comment
            pos <- which((names(cmt) == "ORCID") &
                         grepl(.ORCID_iD_variants_regexp, cmt))
            if(length(pos) == 1L) {
                e$family <-
                    c(e$family,
                      sprintf("<https://replace.me.by.orcid.org/%s>",
                              .ORCID_iD_canonicalize(cmt[pos])))
                cmt <- cmt[-pos]
            }
            ## Of course, a person should not have both ORCID and ROR
            ## identifiers: could check for that.
            pos <- which((names(cmt) == "ROR") &
                         grepl(.ROR_ID_variants_regexp, cmt))
            if(length(pos) == 1L) {
                e$family <-
                    c(e$family,
                      sprintf("<https://replace.me.by.ror.org/%s>",
                              .ROR_ID_canonicalize(cmt[pos])))
                cmt <- cmt[-pos]
            }
            e$comment <- if(length(cmt)) cmt else NULL
            e
        }
        x <- lapply(unclass(x), format_person1)
        class(x) <- "person"
        utils:::.format_authors_at_R_field_for_author(x)
    }
    
    desc <- enc2utf8(.read_description(descfile))
    ## Drop empty fields: these are usually taken as missing.    
    desc <- desc[nzchar(desc)]
    pack <- desc["Package"]
    aatr <- desc["Authors@R"]
    ## <FIXME>
    ## .DESCRIPTION_to_latex() drops the
    ##    Package Packaged Built
    ## fields: why?  Should we do the same?
    ## Note that the package name will be used for the title in the HTML
    ## refman, so perhaps really drop.
    desc <- desc[names(desc) %w/o%
                 c("Package", "Authors@R")]
    ## </FIXME>

    ## <FIXME>
    ## What should we do with email addresses in the
    ##   Author Maintainer Contact
    ## fields?
    ## CRAN obfuscates, .DESCRIPTION_to_latex() uses \email which only
    ## adds markup but does not create mailto: URLs.
    ## </FIXME>

    if(!is.na(aatr))
        desc["Author"] <-
            .format_authors_at_R_field_with_expanded_identifiers(aatr)

    ## Take only Title and Description as *text* fields.
    desc["Title"] <- htmlify_text(desc["Title"])
    desc["Description"] <-
        htmlify_text(desc["Description"], a = TRUE, d = TRUE)
    ## Now the other fields.
    fields <- setdiff(names(desc),
                      c("Title", "Description", "License"))
    theops <- intersect(fields,
                        c("Depends", "Imports", "LinkingTo",
                          "Suggests", "Enhances"))
    desc[fields] <- fsub("&", "&amp;", desc[fields])
    ## Do this before turning '<' and '>' to HTML entities.
    desc[theops] <- htmlify_compare_ops(desc[theops])
    ## Do this before adding HTML markup ...
    desc[fields] <- fsub("<", "&lt;", desc[fields])
    desc[fields] <- fsub(">", "&gt;", desc[fields])
    ## HTMLify URLs and friends.
    for(f in intersect(fields,
                       c("URL", "BugReports",
                         "Additional_repositories",
                         ## BioC ...
                         "git_url"
                         ))) {
        ## The above already changed & to &amp; which urlify will
        ## do once more ...
        trafo <- function(s) urlify(gsub("&amp;", "&", s))
        desc[f] <- trfm("(^|[^>\"?])((https?|ftp)://[^[:space:],]*)",
                        "\\1<a href=\"%s\">\\2</a>",
                        desc[f],
                        trafo,
                        2L)
    }

    if(!is.na(aatr)) {
        desc["Author"] <-
            gsub(sprintf("&lt;https://replace.me.by.orcid.org/(%s)&gt;",
                         .ORCID_iD_regexp),
                 paste0("<a href=\"https://orcid.org/\\1\">",
                        "<img alt=\"ORCID iD\" ",
                        if(dynamic)
                            " src=\"/doc/html/orcid.svg\" "
                        else
                            " src=\"https://cloud.R-project.org/web/orcid.svg\" ",
                        "style=\"width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle\"",
                        "></a>"),
                 desc["Author"])
        desc["Author"] <-
            gsub(sprintf("&lt;https://replace.me.by.ror.org/(%s)&gt;",
                         .ROR_ID_regexp),
                 paste0("<a href=\"https://ror.org/\\1\">",
                        "<img alt=\"ROR ID\" ",
                        if(dynamic)
                            " src=\"/doc/html/ror.svg\" "
                        else
                            " src=\"https://cloud.R-project.org/web/ror.svg\" ",
                        "style=\"width:20px; height:20px; margin-left:4px; margin-right:4px; vertical-align:middle\"",
                        "></a>"),
                 desc["Author"])
    }

    desc["License"] <- htmlify_license_spec(desc["License"], pack)

    if(dynamic)
        desc[theops] <- htmlify_depends_spec(desc[theops])

    ## <TODO>
    ## For dynamic help we should be able to further enhance by
    ## hyperlinking file pointers to
    ##   AUTHORS COPYRIGHTS
    ## </TODO>

    c("<table role='presentation'>",
      sprintf("<tr>\n<td>%s:</td>\n<td>%s</td>\n</tr>",
              names(desc), desc),
      "</table>")
}
