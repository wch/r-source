#  File src/library/tools/R/news.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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


## .build_news_db_from_R_NEWS <-
## function()
## {
##     db <- readNEWS(chop = "keepAll")
##     ## This currently is a list of x.y lists of x.y.z lists of
##     ## categories list of entries.
##     flatten <- function(e)
##         cbind(rep.int(names(e), lengths(e)),
##               unlist(lapply(e,
##                             function(s) {
##                                 ## Also remove leading white space and
##                                 ## trailing blank lines.
##                                 lapply(s,
##                                        function(e)
##                                            sub("[[:space:]]*$", "",
##                                                paste(sub("^ ", "", e),
##                                                      collapse = "\n")))
##                             }),
##                             use.names = FALSE))
##     db <- lapply(Reduce(c, db), flatten)
##     db <- do.call(rbind, Map(cbind, names(db), db))
##     ## Squeeze in an empty date column.
##     .make_news_db(cbind(db[, 1L], NA_character_, db[, -1L]),
##                   logical(nrow(db)))
## }

.build_news_db <-
function(package, lib.loc = NULL, format = NULL, reader = NULL)
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    ## Or maybe use find.package()?

    ## <FIXME>
    ## We had planned to eventually add support for DESCRIPTION
    ##   News/File
    ##   News/Format
    ##   News/Reader
    ##   News/Reader@R
    ## entries.  But now that we're moving to NEWS.Rd, there seems
    ## little point in providing format/reader support ...
    ## </FIXME>

    ## Look for new-style inst/NEWS.Rd installed as NEWS.Rd
    ## If not found, look for NEWS.md.
    ## If not found, look at old-style
    ##   NEWS inst/NEWS
    ## installed as NEWS (and ignore ChangeLog files).
    nfile <- file.path(dir, "NEWS.Rd")
    if(file_test("-f", nfile))
        return(.build_news_db_from_package_NEWS_Rd(nfile))

    nfile <- file.path(dir, "NEWS.md")
    if(file_test("-f", nfile))
        return(.build_news_db_from_package_NEWS_md(nfile))

    nfile <- file.path(dir, "NEWS")
    if(!file_test("-f", nfile))
        return(invisible())
    ## Return NULL for now, no message that there is no NEWS or
    ## ChangeLog file.

    if(!is.null(format))
        .NotYetUsed("format", FALSE)
    if(!is.null(reader))
        .NotYetUsed("reader", FALSE)

    reader <- .news_reader_default

    reader(nfile)
}

.news_reader_default <-
function(file)
{
    verbose <- getOption("verbose")

    .collapse <- function(s) paste(s, collapse = "\n")

    lines <- readLines(file, warn = FALSE)

    ## Re-encode if necessary.
    if(any(ind <- is.na(nchar(lines, allowNA = TRUE)))) {
        dir <- dirname(file)
        if(basename(dir) == "inst")
            dir <- dirname(file)
        ## This should now contain the DESCRIPTION file.
        encoding <-
            if(file.exists(dfile <- file.path(dir, "DESCRIPTION")))
                .read_description(dfile)["Encoding"]
            else
                NA
        if(!is.na(encoding))
            lines[ind] <- iconv(lines[ind], encoding, "")
        ## Last resort.
        if(any(is.na(nchar(lines[ind], allowNA = TRUE))))
            lines[ind] <- iconv(lines[ind], "", "", sub = "byte")
    }

    ## Save what we read in case we cannot figure out the news, in which
    ## case we simply return one entry with the whole text.
    olines <- lines
    ## Get rid of underlines and friends.
    lines <-
        lines[!grepl("^[[:space:]]*[[:punct:]]*[[:space:]]*$", lines)]

    ## Determine lines containing version numbers, without being too
    ## liberal.
    re_valid_package_name <- .standard_regexps()$valid_package_name
    re_v <- sprintf("^([[:space:]]*(%s)|(%s))(%s).*$",
                    paste0("CHANGES? *(IN|FOR).*VERSION *",
                           "|",
                           "CHANGES? *(IN|FOR|TO) *"),
                    sprintf(paste(## TeachingDemos pomp ouch
                                  "NEW IN .*",
                                  ## HyperbolicDist nls2 proto
                                  "VERSION:? *",
                                  "%s +",
                                  ## E.g., lattice:
                                  ##   Changes in lattice 0.17
                                  "CHANGES IN %s +",
                                  ## sv*
                                  "== Changes in %s +",
                                  ## tcltk2
                                  "== Version +",
                                  ## R2WinBUGS
                                  "update *",
                                  "v *",
                                  "",
                                  sep = "|"),
                            re_valid_package_name,
                            re_valid_package_name,
                            re_valid_package_name),
                    .standard_regexps()$valid_package_version
                    )
    ## Some people use
    ##   $PACKAGE version $VERSION
    ## Let us try handling this later, or ask people to write their own
    ## readers.
    ind <- grepl(re_v, lines, ignore.case = TRUE)

    if(!any(ind))
        return(.make_news_db(cbind(NA_character_,
                                   NA_character_,
                                   NA_character_,
                                   .collapse(olines))))
    ## Could add an empty list of bad chunks (as none were found).

    ## Everything before the first version line is a header which will
    ## be dropped.
    if(!ind[1L]) {
	pos <- seq_len(which.max(ind) - 1L)
        lines <- lines[-pos]
        ind <- ind[-pos]
    }

    ## Try catching date entries at the end of version lines as well.
    re_d <- sprintf("^.*(%s)[[:punct:][:space:]]*$",
                    "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")
    ## Could try to allow for non ISO date specs ...

    ## Version lines determine the chunks, which after the version line
    ## should either start with a line tag (category) or an itemize
    ## "bullet".
    chunks <- split(lines, cumsum(ind))

    do_chunk <- function(chunk, header = NA_character_) {
        ## Process a single chunk.

        ## If there is no category header, the first line is the version
        ## line, after which the next non blank line should start with a
        ## line tag (category) or an itemize "bullet".
        if(!is.na(header))
            date <- NA_character_
        else {
            txt <- chunk[1L]
            header <- sub(re_v, "\\6", txt, ignore.case = TRUE)
            date <- if(grepl(re_d, txt, perl = TRUE))
                sub(re_d, "\\1", txt, perl = TRUE)
            else
                NA_character_
        }

        lines <- chunk[-1L]
        s <- .collapse(lines)
        if(grepl("^[[:space:]]*([o*+-])", s)) {
            sep <- sub("^[[:space:]]*([o*+-]).*$", "\\1", s)
            ire <- sprintf("^[[:space:]]*([%s])[[:space:]]+", sep)
            ind <- grepl(ire, lines)
            list(entries =
                 sapply(split(lines, cumsum(ind)),
                        function(s)
                        sub(ire, "", .collapse(sub("^\t?", "", s)))
                        ),
                 header = header,
                 chunk = chunk,
                 date = date)
        } else {
            ## Categories should be non-empty starting in column 1.
            re_c <- "^([[:alpha:]].*)[[:space:]]*$"
            ind <- grepl(re_c, lines)
            ## If we detect neither bullet items nor categories, the
            ## chunk is in a different format than we can recognize.
            ## Return no entries, and have the finisher give the whole
            ## chunk and push it onto the bad chunk list.
            if(!any(ind)) {
                list(entries = character(),
                     header = header,
                     chunk = chunk,
                     date = date)
            } else {
                pos <- cumsum(ind) > 0
                list(entries =
                     Map(do_chunk,
                         split(lines[pos], cumsum(ind)[pos]),
                         sub("[[:punct:]]*$", "",
                             sub(re_c, "\\1", lines[ind]))),
                     header = header,
                     chunk = chunk,
                     date = date)
            }
        }
    }

    out <- lapply(chunks, do_chunk)
    ## Now assemble pieces.
    reporter <- function(x) {
        if(verbose)
            message(gettextf("Cannot process chunk/lines:\n%s",
                             .collapse(x)))
        NULL
    }
    finisher <- function(x) {
        entries <- x$entries
        version <- x$header
        date <- x$date
        if(is.list(entries)) {
            do.call(rbind,
                    lapply(entries,
                           function(x) {
                               entries <- x$entries
                               bad <- if(!length(entries)) {
                                   reporter(x$chunk)
                                   entries <-
                                       sub("^[[:space:]]*", "",
                                           .collapse(x$chunk[-1L]))
                                   TRUE
                               }
                               else FALSE
                               cbind(version, date, x$header, entries,
                                     bad)
                           }))
        }
        else {
            bad <- if(!length(entries)) {
                reporter(x$chunk)
                entries <-
                    sub("^[[:space:]]*", "",
                        .collapse(x$chunk[-1L]))
                TRUE
            }
            else FALSE
            cbind(version, date, NA_character_, entries, bad)
        }
    }

    out <- do.call(rbind, lapply(out, finisher))

    ## Try to remove a common 'exdent' from the entries.
    entries <- out[, 4L]
    exdent <-
        unlist(lapply(gregexpr("\n *", entries), attr, "match.length"))
    exdent <- exdent[exdent > 1L]
    if(length(exdent)) {
        out[, 4L] <-
            gsub(sprintf("\n%s", strrep(" ", min(exdent) - 1L)),
                 "\n", entries)
    }

    .make_news_db(out[, -5L, drop = FALSE], as.logical(out[, 5L]))
}

.make_news_db <-
function(x, bad = NULL, classes = NULL)
{
    ## Expect x to be a character matrix giving at least
    ##   version date category text
    ## in its first 4 columns.
    ## Could of course check for this using
    ##   if(!is.character(x) || ncol(x) < 4L)
    out <- data.frame(x, row.names = NULL, stringsAsFactors = FALSE)
    ## Note that we cannot do
    ##   dimnames(out) <- list(NULL,
    ##                         c("Version", "Date", "Category", "Text"))
    colnames(out)[1L : 4L] <-
        c("Version", "Date", "Category", "Text")
    if(!is.null(bad))
        attr(out, "bad") <- bad
    class(out) <- unique(c(classes, "news_db", "data.frame"))
    out
}

## Transform NEWS.Rd

Rd2txt_NEWS_in_Rd_options <-
    list(sectionIndent = 0L, sectionExtra = 2L,
         minIndent = 4L, code_quote = FALSE,
         underline_titles = FALSE)

Rd2txt_NEWS_in_Rd <-
function(f, out = "") {
    if (endsWith(f, ".rds")) f <- readRDS(f)
    Rd2txt(f, out,
           stages = c("install", "render"),
           outputEncoding = if(l10n_info()[["UTF-8"]]) "" else "ASCII//TRANSLIT",
           options = Rd2txt_NEWS_in_Rd_options,
           macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"))
 }

Rd2HTML_NEWS_in_Rd <-
function(f, out, ...) {
    if (endsWith(f, ".rds")) f <- readRDS(f)
    Rd2HTML(f, out, stages = c("install", "render"),
           macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"), ...)
}

Rd2pdf_NEWS_in_Rd <-
function(f, pdf_file)
{
    if (endsWith(f, ".rds")) f <- readRDS(f)
    f2 <- tempfile()
    ## See the comments in ?texi2dvi about spaces in paths
    f3 <- if(grepl(" ", Sys.getenv("TMPDIR")))
        file.path("/tmp", "NEWS.tex")
    else
        file.path(tempdir(), "NEWS.tex")
    out <- file(f3, "w")
    Rd2latex(f, f2,
             stages = c("install", "render"),
             outputEncoding = "UTF-8", writeEncoding = FALSE,
             macros = file.path(R.home("share"), "Rd", "macros", "system.Rd"))
    cat("\\documentclass[", Sys.getenv("R_PAPERSIZE"), "paper]{book}\n",
        "\\usepackage[ae,hyper]{Rd}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{graphicx}\n",
        "\\setkeys{Gin}{width=0.7\\textwidth}\n",
        "\\graphicspath{{\"", normalizePath(file.path(R.home("doc"), "html"), "/"),
                            "/\"}}\n",
        "\\hypersetup{pdfpagemode=None,pdfstartview=FitH}\n",
        "\\begin{document}\n",
        "\\chapter*{}\\sloppy\n",
        "\\begin{center}\n\\huge\n",
        "NEWS for ", R.version$version.string, "\n",
        "\\end{center}\n",
        sep = "", file = out)
    writeLines(readLines(f2), out)
    writeLines("\\end{document}", out)
    close(out)
    od <- setwd(dirname(f3))
    on.exit(setwd(od))
    ## avoid broken texi2pdf scripts: this is simple LaTeX
    ## and emulation suffices
    texi2pdf("NEWS.tex", quiet = TRUE, texi2dvi = "emulation")
    setwd(od); on.exit()
    invisible(file.copy(file.path(dirname(f3), "NEWS.pdf"),
                        pdf_file, overwrite = TRUE))
}

## Transform old-style plain text NEWS file to Rd.

news2Rd <-
function(file, out = stdout(), codify = FALSE)
{
    ## For add-on packages, the given NEWS file should be in the root
    ## package source directory or its 'inst' subdirectory, so that we
    ## can use the DESCRIPTION metadata to obtain the package name and
    ## encoding.

    format <- "default"

    file <- file_path_as_absolute(file)

    if(file_test("-d", file)) {
        dir <- file
        dfile <- file.path(dir, "DESCRIPTION")
        if(!file_test("-f", dfile))
            stop("DESCRIPTION file not found")
        file <- file.path(dir, "inst", "NEWS")
        if(!file_test("-f", file)) {
            file <- file.path(dir, "NEWS")
            if(!file_test("-f", file))
                stop("NEWS file not found")
        }
    } else {
        dir <- dirname(file)
        dfile <- file.path(dir, "DESCRIPTION")
        if(!file_test("-f", dfile)) {
            if((basename(dir) != "inst") ||
               !file_test("-f",
                          dfile <- file.path(dirname(dir),
                                             "DESCRIPTION")))
                stop("DESCRIPTION file not found")
        }
    }

    ## No longer support taking NEWS files without correponding
    ## DESCRIPTION file as being from R itself (PR #16556).

    meta <- .read_description(dfile)

    wto <- function(x) writeLines(x, con = out, useBytes = TRUE)
    cre <- "(\\W|^)(\"[[:alnum:]_.]*\"|[[:alnum:]_.:]+\\(\\))(\\W|$)"

    if(is.character(out)) {
        out <- file(out, "wt")
        on.exit(close(out))
    }
    if(!isOpen(out, "wt")) {
        open(out, "wt")
        on.exit(close(out))
    }

    if(format == "R") {
        news <- readNEWS(chop = "keepAll")
        if(!length(news))
            stop("No news found in given file using old-style R-like format.")
        wto(c("\\newcommand{\\PR}{\\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}",
              "\\name{NEWS}",
              "\\title{R News}",
              "\\encoding{UTF-8}"))
        for(y in news) {
            for(i in seq_along(y)) {
                wto(sprintf("\\section{CHANGES IN R VERSION %s}{",
                            names(y)[i]))
                z <- y[[i]]
                for(j in seq_along(z)) {
                    wto(c(sprintf("  \\subsection{%s}{", names(z)[j]),
                          "    \\itemize{"))
                    for(chunk in z[[j]]) {
                        chunk <- toRd(paste(chunk, collapse = "\n      "))
                        if(codify) {
                            chunk <- gsub(cre, "\\1\\\\code{\\2}\\3",
                                          chunk)
                        }
                        chunk <- gsub("PR#([[:digit:]]+)", "\\\\PR{\\1}",
                                      chunk)
                        wto(paste("      \\item", enc2utf8(chunk)))
                    }
                    wto(c("    }", "  }"))
                }
                wto("}")
            }
        }
    } else {
        news <- .news_reader_default(file)
        bad <- attr(news, "bad")
        if(!length(bad))
            stop("No news found in given file using package default format.")
        if(any(bad)) {
            bad <- news$Text[bad]
            stop("Could not extract news from the following text chunks:\n",
                 paste(sprintf("\nChunk %s:\n%s",
                               format(seq_along(bad)), bad),
                       collapse = "\n"))
        }

        encoding <- meta["Encoding"]
        package <- meta["Package"]

        texts <- toRd(news$Text)
        if(codify)
            texts <- gsub(cre, "\\1\\\\code{\\2}\\3", texts)
        ## Note that .news_reader_default re-encodes ...
        if(!is.na(encoding))
            texts <- iconv(texts, to = encoding, sub = "byte", mark = FALSE)
        news$Text <- texts

        wto(c("\\name{NEWS}",
              sprintf("\\title{News for Package '%s'}", package)))
        if(!is.na(encoding))
            wto(sprintf("\\encoding{%s}", encoding))

        ## Similar to print.news_db():
        vchunks <- split(news, news$Version)
        ## Re-order according to decreasing version.
        vchunks <- vchunks[order(as.numeric_version(names(vchunks)),
                                 decreasing = TRUE)]
        dates <- sapply(vchunks, function(v) v$Date[1L])
        if(any(ind <- !is.na(dates)))
            names(vchunks)[ind] <-
                sprintf("%s (%s)", names(vchunks)[ind], dates[ind])
        vheaders <- sprintf("\\section{Changes in %s version %s}{",
                            package, names(vchunks))
        for(i in seq_along(vchunks)) {
            wto(vheaders[i])
            vchunk <- vchunks[[i]]
            if(all(!is.na(category <- vchunk$Category)
                   & nzchar(category))) {
                ## need to preserve order of headings.
                cchunks <-
                    split(vchunk,
                          factor(category, levels = unique(category)))
                cheaders <- sprintf("  \\subsection{%s}{",
                                    names(cchunks))
                for(j in seq_along(cchunks)) {
                    wto(c(cheaders[j],
                          "    \\itemize{",
                          paste("      \\item",
                                gsub("\n", "\n        ",
                                     cchunks[[j]]$Text)),
                          "    }",
                          "  }"))
                }
            } else {
                wto(c("  \\itemize{",
                      paste("    \\item",
                            gsub("\n", "\n      ", vchunk$Text)),
                      "  }"))
            }
            wto("}")
        }
    }
}

.build_news_db_from_R_NEWS_Rd <-
function(file = NULL)
{
    x <- if(is.null(file))
        readRDS(file.path(R.home("doc"), "NEWS.rds"))
    else {
        ## Expand \Sexpr et al now because this does not happen when using
        ## fragments.
        macros <- initialRdMacros()
        prepare_Rd(parse_Rd(file, macros = macros), stages = "install")
    }

    db <- .extract_news_from_Rd(x)
    db <- db[db[,1L] != "CHANGES in previous versions",,drop = FALSE]

    ## Squeeze in an empty date column.
    .make_news_db(cbind(sub("^CHANGES IN (R )?(VERSION )?", "", db[, 1L]),
                        NA_character_,
                        db[, 2L],
                        Text = sub("\n*$", "", db[, 3L]),
                        HTML = db[, 4L]),
                  NULL,
                  "news_db_from_Rd")
}

.build_news_db_from_package_NEWS_Rd <-
function(file)
{
    macros <- initialRdMacros()
    x <- prepare_Rd(parse_Rd(file, macros = macros), stages = "install")

    db <- .extract_news_from_Rd(x)

    ## Post-process section names to extract versions and dates.
    re_v <- sprintf(".*version[[:space:]]+(%s).*$",
                    .standard_regexps()$valid_package_version)
    reDt <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
    rEnd <- "[[:punct:][:space:]]*$"
    re_d1 <- sprintf(paste0("^.*(%s)", rEnd), reDt)
    ## or ending with '(YYYY-MM-DD, <note>)'
    re_d2 <- sprintf(paste0("^.*\\((%s)[[:punct:]] .*\\)", rEnd), reDt)
    nms <- db[, 1L]
    ind <- grepl(re_v, nms, ignore.case = TRUE)
    if(!all(ind))
        warning("Cannot extract version info from the following section titles:\n",
		paste(unique(nms[!ind]), collapse = "  "))
    .make_news_db(cbind(ifelse(ind,
			       sub(re_v, "\\1", nms, ignore.case = TRUE),
			       NA_character_),
			ifelse(grepl(re_d1, nms, perl = TRUE),
			       sub(re_d1, "\\1", nms, perl = TRUE),
			       ifelse(grepl(re_d2, nms, perl = TRUE),
				      sub(re_d2, "\\1", nms, perl = TRUE),
				      NA_character_)),
			db[, 2L],
                        Text = sub("\n*$", "", db[, 3L]),
                        HTML = db[, 4L]),
                  NULL,
                  "news_db_from_Rd")
}

.extract_news_from_Rd <-
function(x)
{
    get_section_names <- function(x)
        sapply(x, function(e) .Rd_get_text(e[[1L]]))

    get_item_texts <- function(x) {
        ## Currently, chunks should consist of a single \itemize list
        ## containing the news items.  Notify if there is more than one
        ## such list, and stop if there is none.

        pos <- which(RdTags(x) == "\\itemize")
        if(!length(pos)) {
            stop(gettextf("Malformed NEWS.Rd file:\nChunk starting\n  %s\ncontains no \\itemize.",
                          substring(sub("^[[:space:]]*", "",
                                        .Rd_deparse(x)),
                                    1L, 60L)),
                 domain = NA)
        } else if(length(pos) > 1L) {
            warning(gettextf("Malformed NEWS.Rd file:\nChunk starting\n  %s\ncontains more than one \\itemize.\nUsing the first one.",
                             substring(sub("^[[:space:]]*", "",
                                           .Rd_deparse(x)),
                                       1L, 60L)),
                    domain = NA)
            pos <- pos[1L]
        }

        x <- x[[pos]]

        out <- file()
        on.exit(close(out))

        Rd2txt_options <- Rd2txt_NEWS_in_Rd_options
        Rd2txt_options$width <- 72L

        ## Extract and process \item chunks:
        y <- split(x, cumsum(RdTags(x) == "\\item"))
        y <- y[names(y) != "0"]
        if(!length(y)) {
            warning(gettextf("Malformed NEWS.Rd file:\nChunk starting\n  %s\ncontains no \\item.",
                             substring(sub("^[[:space:]]*", "",
                                           .Rd_deparse(x)),
                                       1L, 60L)),
                    domain = NA)
            return(matrix(character(), 0L, 2L,
                          dimnames = list(NULL, c("Text", "HTML"))))
        }
        do.call(rbind,
                lapply(y,
                       function(e) {
                           ## Drop \item.
                           e <- e[-1L]
                           ## Convert to text.
                           Rd2txt(e, fragment = TRUE, out = out,
                                  options = Rd2txt_options)
                           one <- paste(readLines(out, warn = FALSE),
                                        collapse = "\n")
                           ## Need warn = FALSE to avoid warning about
                           ## incomplete final line for e.g. 'cluster'.
                           ## Convert to HTML.
                           Rd2HTML(e, fragment = TRUE, out = out)
                           two <- paste(readLines(out, warn = FALSE),
                                        collapse = "\n")
                           cbind(Text = one, HTML = two)
                       }))
    }

    cbind_safely <- function(u, v)
        cbind(rep_len(u, NROW(v)), v)

    x <- x[RdTags(x) == "\\section"]
    y <- Map(cbind_safely,
             get_section_names(x),
             lapply(x,
                    function(e) {
                        z <- e[[2L]]
                        ind <- RdTags(z) == "\\subsection"
                        if(any(ind)) {
                            z <- z[ind]
                            do.call(rbind,
                                    Map(cbind_safely,
                                        get_section_names(z),
                                        lapply(z,
                                               function(e)
                                                   get_item_texts(e[[2L]]))))
                        } else {
                            cbind_safely(NA_character_,
                                         get_item_texts(z))
                        }
                    }))
    y <- do.call(rbind, y)
    ## Sanitze HTML.
    s <- trimws(y[, "HTML"])
    i <- startsWith(s, "<p>") & !endsWith(s, "</p>")
    s[i] <- paste0(s[i], "</p>")
    y[, "HTML"] <- s

    y

}

.build_news_db_from_package_NEWS_md <-
function(f)
{
    md <- readLines(f, encoding = "UTF-8", warn = FALSE)

    ## Handle YAML header.
    if(md[1L] == "---") {
        for(pos in seq.int(2L, length(md)))
            if(md[pos] == "---") break
        md[seq_len(pos)] <- ""
    }

    doc <- commonmark::markdown_xml(md,
                                    extensions = TRUE,
                                    sourcepos = TRUE)
    doc <- xml2::xml_ns_strip(xml2::read_xml(doc))

    nodes <- xml2::xml_children(doc)    # Need xml2::xml_root()?

    ## Inline for efficiency.
    .markdown_text <- commonmark::markdown_text
    .markdown_html <- commonmark::markdown_html
    .xml_attr <- xml2::xml_attr
    .xml_name <- xml2::xml_name
    .xml_text <- xml2::xml_text
    
    get_text_and_HTML <- function(sp) {
        ## Sourcepos sp already split into l1 c2 l2 c2, for legibility:
        l1 <- sp[1L]; c1 <- sp[2L]; l2 <- sp[3L]; c2 <- sp[4L]
        txt <- if(l1 < l2) {
                   c(substring(md[l1], c1),
                     md[seq.int(from = l1 + 1L,
                                length.out = l2 - l1 - 1L)],
                     substring(md[l2], 1L, c2))
               } else
                   substring(md[l1], c1, c2)
        c(.markdown_text(txt, width = 72L),
          .markdown_html(txt))
    }

    do_vchunk <- function(nodes) {
        ## Get version and date from heading.
        version <- .xml_text(nodes[[1L]])
        nodes <- nodes[-1L]
        if(!length(nodes))
            return(rbind(c(version, "", "", "")))
        ## Unlike news in Rd where we (currently) insist on all news to
        ## be given as items in itemize lists, for md we only split news
        ## in version chunks according to category.  If the chunks has
        ## headings, we take those with the same level as the first one
        ## to start category chunks, and everything before the first
        ## such heading as a chunk with an empty category (empty instead
        ## of missing to make querying more convenient).  If there are
        ## no headings, we have a single version chunk with no (empty)
        ## category.
        ind <- .xml_name(nodes) == "heading"
        pos <- which(ind)
        if(length(pos)) {
            lev <- .xml_attr(nodes[pos], "level")
            ind[pos] <- (lev == lev[1L])
            if((pos[1L]) > 1L) {
                ini <- seq_len(pos[1L] - 1L)
                out <- list(do_cchunk(nodes[ini], FALSE))
                nodes <- nodes[-ini]
                ind <- ind[-ini]
            } else
                out <- list()
            out <- c(out,
                     lapply(split(nodes, cumsum(ind)),
                            do_cchunk, TRUE))
            cbind(version, do.call(rbind, out))
        } else {
            rbind(c(version,
                    do_cchunk(nodes, FALSE)))
        }

    }

    do_cchunk <- function(nodes, heading) {
        ## See above: if the category chunk has a heading, we extract
        ## the category from it.  Otherwise, the category is empty.
        if(heading) {
            category <- .xml_text(nodes[[1L]])
            nodes <- nodes[-1L]
        } else {
            category <- ""
        }
        
        if(!length(nodes))
            return(c(category, "", ""))

        ## Compute text and HTML by converting everything from the start
        ## of the first sourcepos to the end of the last sourcepos.
        sp <- c(.xml_attr(nodes[[1L]], "sourcepos"),
                .xml_attr(nodes[[length(nodes)]], "sourcepos"))
        ## (If there is one node, nodes[c(1L, length(nodes))] would give
        ## that node only once.  Could also special case ...)
        sp <- as.integer(unlist(strsplit(sp, "[:-]"))[c(1L, 2L, 7L, 8L)])
        
        c(category, get_text_and_HTML(sp))
    }
         
    ind <- .xml_name(nodes) == "heading"
    pos <- which(ind)
    if(!length(pos)) return()

    ## Skip leading headings until we find one from which we can extract
    ## a version number.  Then drop everything ahead of this, and take 
    ## all headings with the same level to start version chunks.
    
    re_v <- sprintf("(^|.*[[:space:]]+)[vV]?(%s).*$",
                    .standard_regexps()$valid_package_version)
    while(length(pos) &&
          !grepl(re_v, .xml_text(nodes[[pos[1L]]])))
        pos <- pos[-1L]
    if(!length(pos)) return()

    lev <- .xml_attr(nodes[pos], "level")
    ind[pos] <- (lev == lev[1L])
    if(pos[1L] > 1L) {
        ini <- seq_len(pos[1L] - 1L)
        nodes <- nodes[-ini]
        ind <- ind[-ini]
    }
    vchunks <- split(nodes, cumsum(ind))
    db <- do.call(rbind, lapply(vchunks, do_vchunk))

    ## Very similar to .build_news_db_from_package_NEWS_Rd() ...

    ## Post-process section names to extract versions and dates.
    reDt <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
    rEnd <- "[[:punct:][:space:]]*$"
    re_d1 <- sprintf(paste0("^.*(%s)", rEnd), reDt)
    ## or ending with '(YYYY-MM-DD, <note>)'
    re_d2 <- sprintf(paste0("^.*\\((%s)[[:punct:]] .*\\)", rEnd), reDt)
    nms <- db[, 1L]
    ind <- grepl(re_v, nms, ignore.case = TRUE)
    if(!all(ind))
        warning("Cannot extract version info from the following section titles:\n",
                paste(unique(nms[!ind]), collapse = "  "))

    .make_news_db(cbind(ifelse(ind,
                               sub(re_v, "\\2", nms, ignore.case = TRUE),
                               NA_character_),
                        ifelse(grepl(re_d1, nms, perl = TRUE),
                               sub(re_d1, "\\1", nms, perl = TRUE),
                               ifelse(grepl(re_d2, nms, perl = TRUE),
                                      sub(re_d2, "\\1", nms, perl = TRUE),
                                      NA_character_)),
                        db[, 2L],
                        Text = sub("\n*$", "", db[, 3L]),
                        HTML = db[, 4L]),
                  NULL,
                  "news_db_from_md")
}

format.news_db_from_md <-
function(x, ...)
{
    do_vchunk <- function(vchunk) {
        z <- unlist(Map(c, vchunk$Category, vchunk$Text),
                    use.names = FALSE)
        z[nzchar(z)]
    }

    vchunks <- split(x, x$Version)
    ## Re-order according to decreasing version.
    vchunks <- vchunks[order(numeric_version(names(vchunks),
                                             strict = FALSE),
                             decreasing = TRUE)]
    if(!length(vchunks))
        return(character())
    
    dates <- sapply(vchunks, function(v) v$Date[1L])
    vheaders <-
        format(sprintf("Changes in version %s%s",
                       names(vchunks),
                       ifelse(is.na(dates), "",
                              sprintf(" (%s)", dates))),
               justify = "centre", width = 72L)

    Map(c, vheaders, lapply(vchunks, do_vchunk),
        USE.NAMES = FALSE)
}

.news_db_has_no_bad_entries <-
function(x)     
{
    (is.null(bad <- attr(x, "bad")) ||
     (length(bad) == NROW(x)) && !any(bad))
}
