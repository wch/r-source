toHTML <- function(x, ...) UseMethod("toHTML")

#
#  Copyright (C) 1995-2018 The R Core Team

HTMLheader <-
function(title="R", logo=TRUE,
         up=NULL,
         top=file.path(Rhome, "doc/html/index.html"),
         Rhome="",
         css = file.path(Rhome, "doc/html/R.css"),
         headerTitle = paste("R:", title),
         outputEncoding = "UTF-8")
{
    result <- c('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
        '<html xmlns="http://www.w3.org/1999/xhtml">',
	paste0('<head><title>', headerTitle, '</title>'),
	paste0('<meta http-equiv="Content-Type" content="text/html; charset=',
	       mime_canonical_encoding(outputEncoding), '" />'),
	paste0('<link rel="stylesheet" type="text/css" href="', css, '" />'),
	'</head><body>',
	paste('<h1>', title))
    if (logo)
    	result <- c(result,
                    paste0('<img class="toplogo" src="',
                           file.path(Rhome, 'doc/html/Rlogo.svg'),
                           '" alt="[R logo]" />'))
    result <- c(result, '</h1>', '<hr/>')
    if (!is.null(up) || !is.null(top)) {
    	result <- c(result, '<div style="text-align: center;">')
    	if (!is.null(up))
    	    result <- c(result,
    	        paste0('<a href="', up, '"><img class="arrow" src="',
                       file.path(Rhome, 'doc/html/left.jpg'),
                       '" alt="[Up]" /></a>'))
    	if (!is.null(top))
    	    result <- c(result,
    	    	paste0('<a href="', top, '"><img class="arrow" src="',
    	    	      file.path(Rhome, 'doc/html/up.jpg'),
    	    	      '" alt="[Top]" /></a>'))
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
		    '<table cols="2" width="100%">',
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
    result <- c(result, '</body></html>')
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
        vchunks[order(as.numeric_version(sub(" *patched", ".1",
                                             names(vchunks))),
                      decreasing = TRUE)]
    vheaders <- sprintf("<h2>Changes in version %s</h2>",
                        names(vchunks))
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
                              print_items(vchunk$Text))
                        }
                    })
             ),
      "</body></html>")
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

    dates <- sapply(vchunks, function(v) v$Date[1L])    
    vheaders <- sprintf("<h2>Changes in version %s%s</h2>",
                        names(vchunks),
                        ifelse(is.na(dates), "",
                               sprintf(" (%s)", dates)))

    c(HTMLheader(...),
      unlist(Map(c, vheaders, lapply(vchunks, do_vchunk))),
      "</body></html>")
}

# To support static linking, URLs should be relative.
# Argument "depth" below says how far down in the hierarchy
# we are starting from, e.g. /library/stats/html/mean.html
# is depth 3

makeVignetteTable <- function(vignettes, depth=2) {
    out <- c('<table width="100%">',
             '<col style="width: 22%;" />',
             '<col style="width:  2%;" />',
             '<col style="width: 50%;" />',
             '<col style="width:  8%;" />',
             '<col style="width:  8%;" />',
             '<col style="width:  8%;" />')
    for (i in seq_len(nrow(vignettes))) {
	Outfile <- vignettes[i, "PDF"]
	topic <- file_path_sans_ext(Outfile)
	Title <- vignettes[i, "Title"]
	File  <- vignettes[i, "File"]
	R     <- vignettes[i, "R"]
	pkg   <- vignettes[i, "Package"]
        root <- c(rep.int("../", depth), "library/", pkg, "/doc/")
	link  <- c('<a href="', root,
		  if (nchar(Outfile)) Outfile else File, '">',
		  pkg, "::", topic, '</a>')
	line <- c('<tr><td style="text-align: right; vertical-align: top;">', link,
		    '</td>\n<td></td><td valign="top">', Title,
		    '</td>\n<td valign="top">',
		    if (nchar(Outfile))
			c('<a href="', root, Outfile,'">', vignette_type(Outfile), '</a>'),
		    '</td>\n<td valign="top">',
		    '<a href="', root, File,'">source</a>',
		    '</td>\n<td valign="top" style="white-space: nowrap">',
		    if (nchar(R))
		    	c('<a href="', root, R,'">R code</a>'),
		    '</td></tr>')
	out <- c(out, paste(line, collapse=''))
     }
     c(out, '</table>')
}

makeDemoTable <- function(demos, depth=2) {
    out <- c('<table width="100%">',
             '<col style="width: 22%;" />',
             '<col style="width:  2%;" />',
             '<col style="width: 54%;" />',
             '<col style="width: 20%;" />')
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
	                 '">(Run demo in console)</a>')
	} else {
	    link <- c(pkg, "::", topic)
	    runlink <- ""
	}
	line <- c('<tr><td style="text-align: right; vertical-align: top;">', link,
		    '</td>\n<td></td><td valign="top">', Title,
		    '</td>\n<td valign="top" style="white-space: nowrap">', runlink,
		    '</td></tr>')
	out <- c(out, paste(line, collapse=''))
     }
     c(out, '</table>')
}

makeHelpTable <- function(help, depth=2) {
    out <- c('<table width="100%">',
             '<col style="width: 22%;" />',
             '<col style="width:  2%;" />',
             '<col style="width: 74%;" />')
    pkg <- help[, "Package"]
    root <- paste0(strrep("../", depth), "library/", pkg, "/html/")
    topic <- help[, "Topic"]
    Title <- help[, "Title"]
    links <- paste0('<a href="', root, topic, '.html">',
		    ifelse(nchar(pkg), paste0(pkg, "::"), ""),
		    topic, '</a>')
    lines <- paste0('<tr><td style="text-align: right; vertical-align: top;">', links,
		    '</td>\n<td></td><td valign="top">', Title,
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
        ## See <http://en.wikipedia.org/wiki/Character_encodings_in_HTML>
        ## which in turn refers to
        ## <http://www.w3.org/TR/REC-html40/sgml/sgmldecl.html>: HTML
        ## forbids characters with Unicode code points
        ##   0 to 31 except 9, 10 and 13 (\t, \n and \r)
        ## and
        ##   127 to 159
        ## (octal \000 to \037 and \177 to \237).
        ## Replace these by hex bytes.
        s <- .replace_chars_by_hex_subs(s, invalid_HTML_chars_re)
        s <- gsub("&", "&amp;", s, fixed = TRUE)
        s <- gsub("<", "&lt;", s, fixed = TRUE)
        s <- gsub(">", "&gt;", s, fixed = TRUE)
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
            s <- .gsub_with_transformed_matches("([^>\"])((https?|ftp)://[[:alnum:]/.:@+\\_~%#?=&;,-]+[[:alnum:]/])",
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
	footer <- c("</body>", "</html>")
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
                  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />",
                  "</head>")
        header <- c("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
                  "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
                  header,
		  "<body>")
	footer <- c("</body>", "</html>")
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
