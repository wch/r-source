.build_news_db_from_R_NEWS <-
function()
{
    db <- readNEWS(chop = "keepAll")
    ## This currently is a list of x.y lists of x.y.z lists of
    ## categories list of entries.
    flatten <- function(e)
        cbind(rep.int(names(e), sapply(e, length)),
              unlist(lapply(e,
                            function(s) {
                                ## Also remove leading white space and
                                ## trailing blank lines.
                                lapply(s,
                                       function(e)
                                           sub("[[:space:]]*$", "",
                                               paste(sub("^ ", "", e),
                                                     collapse = "\n")))
                            }),
                            use.names = FALSE))
    db <- lapply(Reduce(c, db), flatten)
    db <- do.call(rbind, Map(cbind, names(db), db))
    ## Squeeze in an empty date column.
    .make_news_db(cbind(db[, 1L], NA_character_, db[, -1L]),
                  logical(nrow(db)))
}

.build_news_db <-
function(package, lib.loc = NULL, format = NULL, reader = NULL)
{
    dir <- system.file(package = package, lib.loc = lib.loc)
    ## Or maybe use .find.package()?

    ## Eventually add support for DESCRIPTION
    ##   News/File
    ##   News/Format
    ##   News/Reader
    ##   News/Reader@R
    ## entries.
    ## For now, only look for files
    ##   NEWS inst/NEWS
    ## and ignore
    ##   ChangeLog inst/ChangeLog
    ## in the package directory.
    files <- file.path(dir,
                       c("NEWS",
                         file.path("inst", "NEWS")))
    nfile <- files[file_test("-f", files)][1L]

    if(is.na(nfile)) return(invisible())
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
                    paste("CHANGES? *(IN|FOR).*VERSION *",
                          "CHANGES? *(IN|FOR|TO) *",
                          sep = "|"),
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
        pos <- seq_len(which(ind)[1L] - 1L)
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
            date <- if(grepl(re_d, txt))
                sub(re_d, "\\1", txt)
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
            gsub(sprintf("\n%s",
                         paste(rep.int(" ", min(exdent) - 1L),
                               collapse = "")),
                 "\n", entries)
    }

    .make_news_db(out[, -5L, drop = FALSE], as.logical(out[, 5L]))
}

.make_news_db <-
function(x, bad = NULL)
{
    ## Expect x to be a 4 column
    ##   version date category text
    ## character matrix.
    ## Could of course check for this using
    ##   if(!is.character(x) || ncol(x) != 4L)
    out <- data.frame(x, row.names = NULL, stringsAsFactors = FALSE)
    ## Note that we cannot do
    ##   dimnames(out) <- list(NULL,
    ##                         c("Version", "Date", "Category", "Text"))
    colnames(out) <- c("Version", "Date", "Category", "Text")
    if(!is.null(bad))
        attr(out, "bad") <- bad
    class(out) <- c("news_db", "data.frame")
    out
}

## Transform NEWS.Rd

Rd2txt_NEWS_in_Rd_options <-
    list(sectionIndent = 0L, sectionExtra = 2L,
         minIndent = 4L, code_quote = FALSE,
         underline_titles = FALSE)

Rd2txt_NEWS_in_Rd <-
function(f, out = "") {
    if (grepl("[.]rds$", f)) f <- .readRDS(f)
    tools::Rd2txt(f, out,
                  stages = c("install", "render"),
                  options = Rd2txt_NEWS_in_Rd_options)
 }

Rd2HTML_NEWS_in_Rd <-
function(f, out) {
    if (grepl("[.]rds$", f)) f <- .readRDS(f)
    tools::Rd2HTML(f, out,
                   stages = c("install", "render"))
}

## Transform old-style plain text NEWS file to Rd.

news2Rd <-
function(con = stdout(), codify = FALSE)
{
    out <- function(x) writeLines(x, con = con)

    if(is.character(con)) {
        con <- file(con, "wt")
        on.exit(close(con))
    }
    if(!isOpen(con, "wt")) {
        open(con, "wt")
        on.exit(close(con))
    }

    out(c("\\name{NEWS}",
          "\\title{R News}",
          "\\encoding{UTF-8}"))

    for(y in tools::readNEWS(chop = "keepAll")) {
        for(i in seq_along(y)) {
            out(sprintf("\\section{CHANGES IN R VERSION %s}{",
                        names(y)[i]))
            z <- y[[i]]
            for(j in seq_along(z)) {
                out(c(sprintf("  \\subsection{%s}{", names(z)[j]),
                      "    \\itemize{"))
                for(chunk in z[[j]]) {
                    chunk <- paste(chunk, collapse = "\n      ")
                    ## <FIXME>
                    ## Change to something like
                    ##   chunk <- toRd(chunk)
                    ## eventually ...
                    chunk <- gsub("\\", "\\\\", chunk, fixed = TRUE)
                    chunk <- gsub("{", "\\{", chunk, fixed = TRUE)
                    chunk <- gsub("}", "\\}", chunk, fixed = TRUE)
                    chunk <- gsub("%", "\\%", chunk, fixed = TRUE)
                    ## </FIXME>
                    if(codify) {
                        chunk <- gsub("(\\W|^)(\"[[:alnum:]_.]*\"|[[:alnum:]_.:]+\\(\\))(\\W|$)",
                                      "\\1\\\\code{\\2}\\3", chunk)
                    }
                    chunk <- gsub("PR#([[:digit:]]+)",
                                  "\\\\Sexpr[results=rd]{tools:::Rd_expr_PR(\\1)}",
                                  chunk)
                    out(paste("      \\item", chunk))
                }
                out(c("    }", "  }"))
            }
            out("}")
        }
    }

}

Rd_expr_PR <-
function(x)
{
    baseurl <- "https://bugs.R-project.org/bugzilla3/show_bug.cgi?id"
    sprintf("\\href{%s=%s}{PR#%s}", baseurl, x, x)
}

.build_news_db_from_R_NEWS_Rd <-
function(file = NULL)
{
    ## Need to proceed as follows:
    ## Get \section and respective name
    ## Get \subsection and respective name
    ## Get respective items ...

    .get_Rd_section_names <- function(x)
        sapply(x, function(e) .Rd_deparse(e[[1L]]))

    do_chunk <- function(x) {
        ## <FIXME>
        ## This assumes that the chunk in essence is one \itemize list.
        ## Should be safe to assume this for base R NEWS in Rd: for add
        ## on packages we need to check ...
        ## </FIXME>
        out <- NULL
        zz <- textConnection("out", "w", local = TRUE)
        on.exit(close(zz))
        Rd2txt(x, out = zz, fragment = TRUE,
               options =
               c(Rd2txt_NEWS_in_Rd_options,
                 list(itemBullet = "\036  ")))
        s <- gsub("^    ", "", out)
        s <- gsub("^  \036 ", "\036", s)
        s <- paste(s, collapse = "\n")
        s <- sub("^[[:space:]]*\036", "", s)
        s <- sub("[[:space:]]*$", "", s)
        unlist(strsplit(s, "\n\036", fixed = TRUE))
    }

    x <- if(is.null(file))
        .readRDS(file.path(R.home("doc"), "NEWS.rds"))
    else {
        ## Expand \Sexpr et al now because this does not happen when using
        ## fragments.
        prepare_Rd(parse_Rd(file), stages = "install")
    }

    y <- x[RdTags(x) == "\\section"]
    db <- do.call(rbind,
                  Map(cbind,
                      .get_Rd_section_names(y),
                      lapply(y,
                             function(e) {
                                 z <- e[[2L]]
                                 z <- z[RdTags(z) == "\\subsection"]
                                 do.call(rbind,
                                         Map(cbind,
                                             .get_Rd_section_names(z),
                                             lapply(z,
                                                    function(e)
                                                    do_chunk(e[[2L]]))))
                             })))
    ## Squeeze in an empty date column.
    .make_news_db(cbind(sub("^CHANGES IN R VERSION ", "", db[, 1L]),
                        NA_character_,
                        db[, 2L],
                        sub("\n*$", "", db[, 3L])),
                  logical(nrow(db)))
}
