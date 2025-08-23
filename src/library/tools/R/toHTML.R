#  File src/library/tools/R/toHTML.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team

toHTML <- function(x, ...) UseMethod("toHTML")

HTMLheader <-
function(title="R", logo=TRUE,
         up=NULL,
         top=file.path(Rhome, "doc/html/index.html"),
         Rhome="",
         css = file.path(Rhome, "doc/html/R.css"),
         headerTitle = paste("R:", title),
         outputEncoding = "UTF-8")
{
    result <-
        c('<!DOCTYPE html>',
          "<html>",
          paste0('<head><title>', headerTitle, '</title>'),
          paste0('<meta http-equiv="Content-Type" content="text/html; charset=',
                 mime_canonical_encoding(outputEncoding), '">'),
          '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">',
          paste0('<link rel="stylesheet" type="text/css" href="', css, '">'),
          '</head><body><div class="container">',
          paste('<h1>', title))
    if (logo)
    	result <- c(result,
                    paste0('<img class="toplogo" src="',
                           file.path(Rhome, 'doc/html/Rlogo.svg'),
                           '" alt="[R logo]">'))
    result <- c(result, '</h1>', '<hr>')
    if (!is.null(up) || !is.null(top)) {
    	result <- c(result, '<div style="text-align: center;">')
    	if (!is.null(up))
    	    result <- c(result,
    	        paste0('<a href="', up, '"><img class="arrow" src="',
                       file.path(Rhome, 'doc/html/left.jpg'),
                       '" alt="[Up]"></a>'))
    	if (!is.null(top))
    	    result <- c(result,
    	    	paste0('<a href="', top, '"><img class="arrow" src="',
    	    	      file.path(Rhome, 'doc/html/up.jpg'),
    	    	      '" alt="[Top]"></a>'))
    	result <- c(result, '</div>')
    }
    result
}

toHTML.packageIQR <-
function(x, ...)
{
    db <- x$results

    # Re-encode as utf-8
    x$title <- iconv(x$title, to="UTF-8")
    x$footer <- iconv(x$footer, to="UTF-8")
    db <- iconv(db, to="UTF-8")

    ## Split according to Package.
    out <- if(nrow(db) == 0L)
         NULL
    else
        lapply(split(1:nrow(db), db[, "Package"]),
               function(ind) db[ind, c("Item", "Title"), drop = FALSE])

    result <- HTMLheader(...)

    for(pkg in names(out)) {
        result <- c(result,
		    paste0('<h2>', htmlify(x$title), ' in package &lsquo;',
			   htmlify(pkg), '&rsquo;</h2>'),
		    '<table cols="2" style="width: 100%;">',
		    paste0('<tr>\n',
			   ' <td style="text-align: left; vertical-align: top; width: 10%;">\n',
			   htmlify(out[[pkg]][, "Item"]),
			   '\n </td>\n',
                           ' <td style="text-align: left; vertical-align: top; width: 90%;">\n',
			   htmlify(out[[pkg]][, "Title"]),
			   '\n </td>\n</tr>\n'),
		    '</table>')
    }
    if(!is.null(x$footer))
    	result <- c(result, '<p>',
    	                    htmlify(x$footer),
    	                    '</p>')
    result <- c(result, '</div></body></html>')
    result
}

toHTML.news_db <-
function(x, ...)
{
    ## local version
    htmlify2 <- function(x) {
        x <- psub("<([[:alnum:]._]+)>", "@VAR@\\1@EVAR@", x)
        x <- fsub("&", "&amp;", x)
        x <- fsub("---", "&mdash;", x)
        ## usually a flag like --timing
        ## x <- fsub("--", "&ndash;", x)
        x <- fsub("``", "&ldquo;", x)
        x <- fsub("''", "&rdquo;", x)
        x <- psub("`([^']+)'", "&lsquo;\\1&rsquo;", x)
        x <- fsub("`", "'", x)
        x <- fsub("<", "&lt;", x)
        x <- fsub(">", "&gt;", x)
        x <- fsub("@VAR@", "<var>", x)
        x <- fsub("@EVAR@", "</var>", x)
        x
    }

    ## For now, only do something if the NEWS file could be read without
    ## problems, see utils:::print.news_db():
    if(!.news_db_has_no_bad_entries(x))
        return(character())

    print_items <- function(x)
        c("<ul>", sprintf("<li>%s</li>", x), "</ul>")

    if(is.null(x$HTML))
        x$HTML <- htmlify2(iconv(x$Text, to = "UTF-8"))

    vchunks <- split(x, x$Version)
    vchunks <-
        vchunks[order(numeric_version(sub(" *patched", ".1", names(vchunks)),
                                      strict = FALSE), # "R-devel" -> NA
                      na.last = FALSE, decreasing = TRUE)]
    dates <- vapply(vchunks, function(v) v$Date[1L], "")
    vheaders <- sprintf("<h2>Changes in version %s%s</h2>",
                        names(vchunks),
                        ifelse(is.na(dates), "",
                               sprintf(" (%s)", dates)))
    c(HTMLheader(...),
      unlist(lapply(seq_along(vchunks),
                    function(i) {
                        vchunk <- vchunks[[i]]
                        if(all(!is.na(category <- vchunk$Category)
                               & nzchar(category))) {
                            ## need to preserve order of headings.
                            cchunks <- split(vchunk,
                                             factor(category, levels=unique(category)))
                            c(vheaders[i],
                              Map(function(h, t)
                                  c(h, print_items(t$HTML)),
                                  sprintf("<h3>%s</h3>",
                                          htmlify2(names(cchunks))),
                                  cchunks))
                        } else {
                            c(vheaders[i],
                              print_items(vchunk$HTML))
                        }
                    })
             ),
      "</div></body></html>")
}

toHTML.news_db_from_md <-
function(x, ...)
{
    do_vchunk <- function(vchunk) {
        cheaders <- vchunk$Category
        ind <- nzchar(cheaders)
        cheaders[ind] <- paste0("<h3>", cheaders[ind], "</h3>")
        z <- unlist(Map(c, cheaders, vchunk$HTML),
                    use.names = FALSE)
        z[nzchar(z)]
    }

    vchunks <- split(x, x$Version)
    ## Re-order according to decreasing version.
    vchunks <- vchunks[order(numeric_version(names(vchunks),
                                             strict = FALSE),
                             decreasing = TRUE)]

    dates <- vapply(vchunks, function(v) v$Date[1L], "")
    vheaders <- sprintf("<h2>Changes in version %s%s</h2>",
                        names(vchunks),
                        ifelse(is.na(dates), "",
                               sprintf(" (%s)", dates)))

    c(HTMLheader(...),
      unlist(Map(c, vheaders, lapply(vchunks, do_vchunk))),
      "</div></body></html>")
}

# To support static linking, URLs should be relative.
# Argument "depth" below says how far down in the hierarchy
# we are starting from, e.g. /library/stats/html/mean.html
# is depth 3
# .writeVignetteHtmlIndex() uses depth=NULL to omit the directory prefix.

makeVignetteTable <- function(vignettes, depth=2) {
    out <- c('<table style="width: 100%;">',
             '<col style="width: 22%;">',
             '<col style="width:  2%;">',
             '<col style="width: 50%;">',
             '<col style="width:  8%;">',
             '<col style="width:  8%;">',
             '<col style="width:  8%;">')
    for (i in seq_len(nrow(vignettes))) {
	Outfile <- vignettes[i, "PDF"]
	topic <- file_path_sans_ext(Outfile)
	Title <- vignettes[i, "Title"]
	File  <- vignettes[i, "File"]
	R     <- vignettes[i, "R"]
	pkg   <- vignettes[i, "Package"]
        root <- if (!is.null(depth))
                    c(rep.int("../", depth), "library/", pkg, "/doc/")
	link  <- c('<a href="', root,
		  if (nchar(Outfile)) Outfile else File, '">',
		  pkg, "::", topic, '</a>')
	line <- c('<tr><td style="text-align: right; vertical-align: top;">', link,
		    '</td>\n<td></td><td style="vertical-align: top;">', Title,
		    '</td>\n<td style="vertical-align: top;">',
		    if (nchar(Outfile))
			c('<a href="', root, Outfile,'">', vignette_type(Outfile), '</a>'),
		    '</td>\n<td style="vertical-align: top;">',
		    '<a href="', root, File,'">source</a>',
		    '</td>\n<td style="vertical-align: top; white-space: nowrap">',
		    if (nchar(R))
		    	c('<a href="', root, R,'">R code</a>'),
		    '</td></tr>')
	out <- c(out, paste(line, collapse=''))
     }
     c(out, '</table>')
}

makeDemoTable <- function(demos, depth=2) {
    out <- c('<table style="width: 100%;">',
             '<col style="width: 22%;">',
             '<col style="width:  2%;">',
             '<col style="width: 54%;">',
             '<col style="width: 20%;">')
    for (i in seq_len(nrow(demos))) {
	topic <- demos[i, "Topic"]
	pkg <- demos[i, "Package"]
        root <- c(rep.int("../", depth), "library/", pkg, "/")
	Title <- demos[i, "Title"]
	path <- file.path(demos[i, "LibPath"], "demo")
	files <- basename(list_files_with_type(path, "demo", full.names=FALSE))
	file <- files[topic == file_path_sans_ext(files)]
	if (length(file) == 1) {
	    link <- c('<a href="', root, 'demo/', file, '">',
			  pkg, "::", topic, '</a>')
	    runlink <- c(' <a href="', root, 'Demo/', topic,
	                 '">(Run demo)</a>')
	} else {
	    link <- c(pkg, "::", topic)
	    runlink <- ""
	}
	line <- c('<tr><td style="text-align: right; vertical-align: top;">', link,
		    '</td>\n<td></td><td style="vertical-align: top;">', Title,
		    '</td>\n<td style="vertical-align: top; white-space: nowrap">', runlink,
		    '</td></tr>')
	out <- c(out, paste(line, collapse=''))
     }
     c(out, '</table>')
}

makeHelpTable <- function(help, depth=2) {
    out <- c('<table style="width: 100%;">',
             '<col style="width: 22%;">',
             '<col style="width:  2%;">',
             '<col style="width: 74%;">')
    pkg <- help[, "Package"]
    ## Target could be ../library/pkg/help/topic or ../library/pkg/html/filename.html
    ## We only have topic, so can only do the former. Topics may contain
    ## special characters, so need to be encoded.
    root <- paste0(strrep("../", depth), "library/", pkg, "/help/")
    topic <- help[, "Topic"]
    Title <- help[, "Title"]
    links <- paste0('<a href="', root, topic2url(topic), '">',
		    ifelse(nchar(pkg), paste0(pkg, "::"), ""),
		    topic, '</a>')
    lines <- paste0('<tr><td style="text-align: right; vertical-align: top;">', links,
		    '</td>\n<td></td><td style="vertical-align: top;">', Title,
		    '</td></tr>')
    c(out, lines, '</table>')
}

toHTML.citation <-
function(x, header = TRUE, ...)
{
    len <- length(x)
    if(!len) return(character())

    is_non_blank_string <- function(s) {
        (length(s) == 1L) && length(grep("[^[:blank:]]", s))
    }

    format_entry_as_text <- function(x) {
        c(if(is_non_blank_string(header <- x$header))
          c("<p>", htmlify(header), "</p>"),
          "<blockquote>",
          ## Proceed as in .format_bibentry_as_citation used by
          ## utils:::print.bibentry: use textVersion if given.
          ## <FIXME>
          ## Stop using textVersion eventually ...
          if(!is.null(tv <- x$textVersion)) {
              c("<p>", htmlify(tv), "</p>")
          } else {
              format(x, "html")
          },
          ## </FIXME>
          "</blockquote>",
          if(is_non_blank_string(footer <- x$footer))
          c("<p>", htmlify(footer), "</p>")
          )
    }

    format_entry_as_BibTeX <- function(x) {
        bib <- unclass(utils::toBibtex(x))
        len <- length(bib)
        out <- c(paste0("  ", bib[1L]),
                 strwrap(bib[-c(1L, len)], indent = 4L, exdent = 6L),
                 "  }")
        c("<pre>",
          htmlify(out, FALSE),
          "</pre>")
    }

    htmlify <- function(s, a = TRUE) {
        ## See <https://en.wikipedia.org/wiki/Character_encodings_in_HTML>
        ## which in turn refers to
        ## <http://www.w3.org/TR/REC-html40/sgml/sgmldecl.html>: HTML
        ## forbids characters with Unicode code points
        ##   0 to 31 except 9, 10 and 13 (\t, \n and \r)
        ## and
        ##   127 to 159
        ## (octal \000 to \037 and \177 to \237).
        ## Replace these by hex bytes.
        s <- .replace_chars_by_hex_subs(s, invalid_HTML_chars_re)
        s <- fsub("&", "&amp;", s)
        s <- fsub("<", "&lt;",  s)
        s <- fsub(">", "&gt;",  s)
        if(a) {
            ## Some people have <http://something> as recommended for
            ## in-text URLs.
            s <- .gsub_with_transformed_matches("&lt;(URL: *)?((https?|ftp)://[^[:space:]]+)[[:space:]]*&gt;",
                                                "&lt;<a href=\"%s\">\\2</a>&gt;",
                                                s,
                                                urlify,
                                                2L)
            ## Need to ignore results of the above translation ...
            ## Regexp based on Perl HTML::TextToHTML, note that the dash
            ## must be last ...
            s <- .gsub_with_transformed_matches("([[:space:]])((https?|ftp)://[[:alnum:]/.:@+\\_~%#?=&;,-]+[[:alnum:]/])",
                                                "\\1<a href=\"%s\">\\2</a>",
                                                s,
                                                urlify,
                                                2L)
            s <- .gsub_with_transformed_matches("&lt;(DOI|doi):[[:space:]]*([^<[:space:]]+[[:alnum:]])&gt;",
                                                "&lt;<a href=\"https://doi.org/%s\">doi:\\2</a>&gt;",
                                                s,
                                                urlify,
                                                2L)
            s <- .gsub_with_transformed_matches("[^>\"](DOI|doi):[[:space:]]*([^<[:space:]&]+[[:alnum:]])",
                                                "&lt;<a href=\"https://doi.org/%s\">doi:\\2</a>&gt;",
                                                s,
                                                urlify,
                                                2L)
        }
        s
    }

    package <- attr(x, "package")

    if (!(is.character(header) || is.logical(header))) {
        warning("unknown header specification")
	header <- TRUE
    }
    if (identical(header, "R")) {
        header <- HTMLheader(...)
	footer <- c("</div></body>", "</html>")
    } else if (isFALSE(header)) {
        header <- character(0L)
	footer <- character(0L)
    } else {
        if(isTRUE(header))
            header <-
                c("<head>",
                  if(is.null(package))
                      "<title>Citation information</title>"
                  else
                      sprintf("<title>%s citation information</title>",
                              package),
                  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">",
                  '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">',
                  "</head>")
        header <- c("<!DOCTYPE html>",
                  "<html>",
                  header,
		  '<body><div class="container">')
	footer <- c("</div></body>", "</html>")
    }

    c(header,
      if(is_non_blank_string(mheader <- attr(x, "mheader")))
      c("<p>", htmlify(mheader), "</p>"),
      do.call(c, lapply(x, format_entry_as_text)),
      if(is_non_blank_string(mfooter <- attr(x, "mfooter")))
      c("<p>", htmlify(mfooter), "</p>"),
      c("<p>",
        ngettext(len,
                 "Corresponding BibTeX entry:",
                 "Corresponding BibTeX entries:"),
        "</p>",
        do.call(c, lapply(x, format_entry_as_BibTeX))),
      footer)
}


## Similar to HTMLheader, but for internal use (for now at
## least). Refactors creation of HTML header and footer as previously
## done by Rd2HTML(), to allow re-use.

HTMLcomponents <- function(title = "R", logo = FALSE,
                           up = NULL,
                           top = NULL, # file.path(Rhome, "doc/html/index.html"),
                           Rhome = "",
                           css = file.path(Rhome, "doc/html/R.css"),
                           headerTitle = title,
                           outputEncoding = "UTF-8",

                           dynamic = FALSE, prism = TRUE,
                           doTexMath = TRUE, texmath = "katex",

                           ## URLs to be used for static HTML (only)
                           ## Ignored if dynamic = TRUE

                           KATEX_JS_STATIC = "https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.js",
                           KATEX_CSS_STATIC = "https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.css",
                           MATHJAX_JS_STATIC = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js",
                           MATHJAX_CONFIG_STATIC = file.path(Rhome, "doc/html/mathjax-config.js"),
                           PRISM_JS_STATIC = c("https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js",
                                               "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-r.min.js"),
                           PRISM_CSS_STATIC = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism.min.css",
                           language = NA_character_
                           )
{
    header <- character(0)
    footer <- character(0)
    addh <- function(...) { header <<- c(header, ...) }
    addf <- function(...) { footer <<- c(footer, ...) }

    ## KaTeX / Mathjax resources (if they are used)
    if (doTexMath && texmath == "katex") {
        KATEX_JS <-
            if (dynamic) "/doc/html/katex/katex.js"
            else KATEX_JS_STATIC
        KATEX_CSS <- if (dynamic) "/doc/html/katex/katex.css"
                     else KATEX_CSS_STATIC
        KATEX_CONFIG <-
            if (dynamic) "/doc/html/katex-config.js"
            else c(r"(const macros = { "\\R": "\\textsf{R}", "\\mbox": "\\text", "\\code": "\\texttt"};)",
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
                MATHJAX_JS_STATIC
        MATHJAX_CONFIG <-
            if (dynamic) "/doc/html/mathjax-config.js"
            else MATHJAX_CONFIG_STATIC
    }
    if (prism) {
        PRISM_JS <- 
            if (dynamic) "/doc/html/prism.js"
            else PRISM_JS_STATIC
        PRISM_CSS <- 
            if (dynamic) "/doc/html/prism.css"
            else PRISM_CSS_STATIC
    }

    addh('<!DOCTYPE html>',
         if(!is.na(language))
             sprintf('<html lang="%s">', language)
         else
             "<html>",
         '<head><title>')

    ## headtitle <- strwrap(.Rd_format_title(.Rd_get_title(Rd)),
    ##                      width=65, initial="R: ")
    ## if (length(headtitle) > 1) headtitle <- paste0(headtitle[1], "...")

    addh(htmlify(headerTitle))
    addh('</title>\n',
         '<meta http-equiv="Content-Type" content="text/html; charset=',
         mime_canonical_encoding(outputEncoding),
         '">\n')
    addh('<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">\n')
    ## include CSS from prismjs.com for code highlighting
    if (prism && length(PRISM_CSS) == 1L)
        addh('<link href="', urlify(PRISM_CSS), '" rel="stylesheet">\n')
    if (doTexMath) {
        if (texmath == "katex") {
            addh('<link rel="stylesheet" href="', urlify(KATEX_CSS), '">\n',
                if (dynamic) paste0('<script src="', urlify(KATEX_CONFIG), '"></script>\n')
                else paste0('<script>\n', paste(KATEX_CONFIG, collapse = "\n"), '</script>\n'),
                '<script defer src="', urlify(KATEX_JS), '"\n    onload="processMathHTML();"></script>\n')
        }
        else if (texmath == "mathjax") {
            addh('<script src="', urlify(MATHJAX_CONFIG), '"></script>\n',
                 '<script async src="', urlify(MATHJAX_JS), '"></script>\n')
        }
    }
    addh(paste0('<link rel="stylesheet" type="text/css" href="', css, '">\n'),
         '</head><body>',
         '<div class="container">')


    ## Footer:
    addf('\n</div>\n') # closes div.container
    ## include JS from prismjs.com for code highlighting
    if (prism && length(PRISM_JS) > 0L)
        for (u in PRISM_JS)
            addf('<script src="', urlify(u), '"></script>\n')
    addf('</body></html>\n')

    ## Optional part of header (title + logo, up, top)

    if (!nzchar(title)) {
        addh('<h1>', title)
        if (logo)
            addh(paste0('<img class="toplogo" src="',
                        file.path(Rhome, 'doc/html/Rlogo.svg'),
                        '" alt="[R logo]">'))
        addh('</h1>', '<hr>')
    }
    if (!is.null(up) || !is.null(top)) {
    	addh('<div style="text-align: center;">')
    	if (!is.null(up))
    	    addh(paste0('<a href="', up, '"><img class="arrow" src="',
                        file.path(Rhome, 'doc/html/left.jpg'),
                        '" alt="[Up]"></a>'))
    	if (!is.null(top))
    	    addh(paste0('<a href="', top, '"><img class="arrow" src="',
                        file.path(Rhome, 'doc/html/up.jpg'),
                        '" alt="[Top]"></a>'))
    	addh('</div>')
    }

    return(list(header = header, footer = footer))
}
