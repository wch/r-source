#  File src/library/tools/R/Rd.R
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

### * Rd_pp

Rd_pp <-
function(lines)
{
    ## Preprocess lines with Rd markup according to .Platform$OS.type.

    if(!is.character(lines))
        stop("argument 'lines' must be a character vector")

    ## Re-encode if necessary (and possible).
    encoding <-
        .get_Rd_metadata_from_Rd_lines(lines[!is.na(nchar(lines, "c", TRUE))],
                                       "encoding")
    if(length(encoding)) {
        if((Sys.getlocale("LC_CTYPE") != "C")
           && capabilities("iconv")) {
            encoding <- encoding[1L]     # Just making sure ...
            if(.is_ASCII(encoding)) {
                if (!tolower(encoding) %in% c("latin1", "latin2", "utf-8"))
                    warning(gettextf("encoding '%s' is not portable",
                                     encoding), domain = NA)
                lines <- iconv(lines, encoding, "")
            }
        }
    }
    else {
        ## No \encoding metadata.
        ## Determine if ASCII
        if(!all(.is_ASCII(lines))) encoding <- NA
    }
    if(any(is.na(nchar(lines, "c", TRUE)))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        if(capabilities("iconv"))
            lines <- iconv(lines, "latin1", "")
        else
            stop("Found invalid multi-byte character data.", "\n",
                 "Cannot re-encode because iconv is not available.", "\n",
                 "Try running R in a single-byte locale.")
    }

    ## Strip Rd first.
    lines <- .strip_Rd_comments(lines)

    pp_line_indices <- grep("^#(endif|ifn?def[[:space:]]+[[:alnum:]]+)",
                            lines)
    ## <NOTE>
    ## This is based on the Perl code in R::Rdtools::Rdpp().
    ## What should we do with #ifn?def lines not matching the above?
    ## </NOTE>
    n_of_pp_lines <- length(pp_line_indices)
    if(n_of_pp_lines == 0L)
        return(structure(lines, encoding = encoding))

    OS <- .Platform$OS.type
    pp_lines <- lines[pp_line_indices]

    ## Record the preprocessor line type: starts of conditionals with
    ## TRUE/FALSE according to whether they increase the skip level or
    ## not, and NA for ends of conditionals.
    pp_types <- rep.int(NA, n_of_pp_lines)
    if(length(i <- grep("^#ifdef", pp_lines))) {
        pp_types[i] <- gsub("^#ifdef[[:space:]]+([[:alnum:]]+).*",
                           "\\1", pp_lines[i]) != OS
    }
    if(length(i <- grep("^#ifndef", pp_lines))) {
        pp_types[i] <- gsub("^#ifndef[[:space:]]+([[:alnum:]]+).*",
                           "\\1", pp_lines[i]) == OS
    }

    ## Looks stupid, but ... we need a loop to determine the skip list
    ## to deal with nested conditionals.
    skip_list <- integer()
    skip_level <- 0L
    skip_indices <- pp_line_indices
    for(i in seq_along(pp_types)) {
        if(!is.na(skip <- pp_types[i])) {
            if(skip_level == 0L && skip > 0L) {
                skipStart <- pp_line_indices[i]
                skip_level <- 1L
            }
            else
                skip_level <- skip_level + skip
            skip_list <- c(skip, skip_list) # push
        }
        else {
            if(skip_level == 1L && skip_list[1L] > 0L) {
                skip_indices <- c(skip_indices,
                                  seq.int(from = skipStart,
                                          to = pp_line_indices[i]))
                skip_level <- 0L
            }
            else
                skip_level <- skip_level - skip_list[1L]
            skip_list <- skip_list[-1L]    # pop
        }
    }

    structure(lines[-skip_indices], encoding = encoding)
}

### * .strip_Rd_comments

.strip_Rd_comments <-
function(lines)
{
    gsub("(^|[^\\])((\\\\\\\\)*)%.*", "\\1\\2", lines)
}

### * Rdinfo

Rdinfo <-
function(file)
{
    ## <NOTE>
    ## This is based on the Perl code in R::Rd::info().
    ## It seems that matches for aliases and keywords are only single
    ## line.  Hence, as we get the lines from @code{Rd_pp()}, we get
    ## aliases and keywords directly from them before collapsing them to
    ## one string (which also allows us to avoid looping as in the Perl
    ## code).
    ## </NOTE>

    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument 'file' must be a character string or connection")

    lines <- Rd_pp(.read_Rd_lines_quietly(file))

    aliases <- .get_Rd_metadata_from_Rd_lines(lines, "alias")
    concepts <- .get_Rd_metadata_from_Rd_lines(lines, "concept")
    keywords <- .get_Rd_metadata_from_Rd_lines(lines, "keyword")

    ## Could be none or more than one ... argh.
    Rd_type <-
        c(.get_Rd_metadata_from_Rd_lines(lines, "docType"), "")[1L]
    encoding <-
        c(.get_Rd_metadata_from_Rd_lines(lines, "encoding"), "")[1L]

    txt <- paste(lines, collapse = "\n")

    Rd_name <- .get_Rd_name(txt)
    if(!length(Rd_name)) {
        msg <-
            c(gettextf("missing/empty \\name field in '%s'",
                       summary(file)$description),
              gettext("Rd files must have a non-empty \\name."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    Rd_title <- .get_Rd_title(txt)
    if(!length(Rd_title)) {
        msg <-
            c(gettextf("missing/empty \\title field in '%s'",
                       summary(file)$description),
              gettext("Rd files must have a non-empty \\title."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    list(name = Rd_name, type = Rd_type, title = Rd_title,
         aliases = aliases, concepts = concepts, keywords = keywords,
         encoding = encoding)
}

### * Rdcontents

Rdcontents <-
function(RdFiles)
{
    ## Compute contents db from Rd files.

    RdFiles <- path.expand(RdFiles[file_test("-f", RdFiles)])

    if(length(RdFiles) == 0L) {
        out <- data.frame(File = character(),
                          Name = character(),
                          Type = character(),
                          Title = character(),
                          Encoding = character(),
                          stringsAsFactors = FALSE)
        out$Aliases <- list()
        out$Concepts <- list()
        out$Keywords <- list()
        return(out)
    }

    entries <- c("Name", "Type", "Title", "Aliases", "Concepts",
                 "Keywords", "Encoding")
    contents <- vector("list", length(RdFiles) * length(entries))
    dim(contents) <- c(length(RdFiles), length(entries))
    for(i in seq_along(RdFiles)) {
        contents[i, ] <- Rdinfo(RdFiles[i])
    }
    colnames(contents) <- entries

    ## Although R-exts says about the Rd title slot that
    ## <QUOTE>
    ##   This should be capitalized, not end in a period, and not use
    ##   any markup (which would cause problems for hypertext search).
    ## </QUOTE>
    ## some Rd files have LaTeX-style markup, including
    ## * LaTeX-style single and double quotation
    ## * Medium and punctuation dashes
    ## * Escaped ampersand.
    ## Hence we try getting rid of these ...
    title <- unlist(contents[ , "Title"])
    title <- gsub("(``|'')", "\"", title)
    title <- gsub("`", "'", title)
    title <- gsub("([[:alnum:]])--([[:alnum:]])", "\\1-\\2", title)
    title <- gsub("\\\\&", "&", title)
    title <- gsub("---", "--", title)
    ## Also remove leading and trailing whitespace.
    title <- sub("^[[:space:]]+", "", title)
    title <- sub("[[:space:]]+$", "", title)

    out <- data.frame(File = basename(RdFiles),
                      Name = unlist(contents[ , "Name"]),
                      Type = unlist(contents[ , "Type"]),
                      Title = title,
                      Encoding = unlist(contents[ , "Encoding"]),
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)
    out$Aliases <- contents[ , "Aliases"]
    out$Concepts <- contents[ , "Concepts"]
    out$Keywords <- contents[ , "Keywords"]
    out
}

### * .write_contents_as_RDS

.write_contents_as_RDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.

    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    .saveRDS(contents, file = outFile, compress = TRUE)
    ## </NOTE>
}

### * .write_contents_as_DCF

.write_contents_as_DCF <-
function(contents, packageName, outFile)
{
    ## Write a @file{CONTENTS} DCF file from an Rd contents db.
    ## Note that these files currently have @samp{URL:} entries which
    ## contain the package name, whereas @code{Rdcontents()} works on
    ## collections of Rd files which do not necessarily all come from
    ## the same package ...

    ## If the contents is 'empty', return immediately.  (Otherwise,
    ## e.g. URLs would not be right ...)
    if(!NROW(contents)) return()

    ## <NOTE>
    ## This has 'html' hard-wired.
    ## Note that slashes etc. should be fine for URLs.
    URLs <- paste("../../../library/", packageName, "/html/",
                  file_path_sans_ext(contents[ , "File"]),
                  ".html",
                  sep = "")
    ## </NOTE>

    if(is.data.frame(contents))
        contents <-
            cbind(contents$Name,
                  sapply(contents$Aliases, paste, collapse = " "),
                  sapply(contents$Keywords, paste, collapse = " "),
                  contents$Title)
    else
        contents <-
            contents[, c("Name", "Aliases", "Keywords", "Title"),
                     drop = FALSE]

    cat(paste(c("Entry:", "Aliases:", "Keywords:", "Description:",
                "URL:"),
              t(cbind(contents, URLs))),
        sep = c("\n", "\n", "\n", "\n", "\n\n"),
        file = outFile)
}

### * .build_Rd_index

.build_Rd_index <-
function(contents, type = NULL)
{
    ## Build an Rd 'index' containing Rd "names" (see below) and titles,
    ## maybe subscripted according to the Rd type (\docType).

    keywords <- contents[ , "Keywords"]

    if(!is.null(type)) {
        idx <- contents[ , "Type"] %in% type
        ## Argh.  Ideally we only want to subscript according to
        ## \docType.  Maybe for 2.0 ...
        if(type == "data")
            idx <- idx | keywords == "datasets"
        ## (Note: we really only want the Rd objects which have
        ## 'datasets' as their *only* keyword.)
        contents <- contents[idx, , drop = FALSE]
        keywords <- keywords[idx]
    }

    ## Drop all Rd objects marked as 'internal' from the index.
    idx <- is.na(sapply(keywords, function(x) match("internal", x)))

    index <- contents[idx, c("Name", "Title"), drop = FALSE]
    if(nrow(index)) {
        ## If a \name is not a valid \alias, replace it by the first
        ## alias.
        aliases <- contents[idx, "Aliases"]
        bad <- which(!mapply("%in%", index[, 1L], aliases))
        if(any(bad)) {
            ## was [[, but that applies to lists not char vectors
            tmp <- sapply(aliases[bad], "[", 1L)
            tmp[is.na(tmp)] <- ""
            index[bad, 1L] <- tmp
        }
        ## and sort it by name
        index <- index[sort.list(index[, 1L]), ]
    }
    index
}

### * Rdindex

Rdindex <-
function(RdFiles, outFile = "", type = NULL,
         width = 0.9 * getOption("width"), indent = NULL)
{
    ## Create @file{INDEX} or @file{data/00Index} style files from Rd
    ## files.
    ##
    ## R version of defunct @code{R CMD Rdindex} (now removed).

    if((length(RdFiles) == 1L) && file_test("-d", RdFiles)) {
        ## Compatibility code for the former @code{R CMD Rdindex}
        ## interface.
        docsDir <- RdFiles
        if(file_test("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        RdFiles <- list_files_with_type(docsDir, "docs")
    }

    if(outFile == "")
        outFile <- stdout()
    else if(is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if(!inherits(outFile, "connection"))
        stop("argument 'outFile' must be a character string or connection")

    index <- .build_Rd_index(Rdcontents(RdFiles), type = type)

    writeLines(formatDL(index, width = width, indent = indent),
               outFile)
}

### * Rd_db

Rd_db <-
function(package, dir, lib.loc = NULL)
{
    ## Build an Rd 'data base' from an installed package or the unpacked
    ## package sources as a list containing the 'raw' R documentation
    ## objects obtained via readLines().

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        docs_files <- list_files_with_type(docs_dir, "docs")
        db <- list()
        for(f in docs_files) {
            valid_lines <- lines <- .read_Rd_lines_quietly(f)
            valid_lines[is.na(nchar(lines, "c", TRUE))] <- ""
            eof_pos <- grep("^\\\\eof$", valid_lines)
            db <- c(db, split(lines[-eof_pos],
                              rep(seq_along(eof_pos),
                                  times = diff(c(0, eof_pos)))[-eof_pos]))
        }
        ## If this was installed using a recent enough version of R CMD
        ## INSTALL, information on source file names is available, and
        ## we use it for the names of the Rd db.  Otherwise, remove the
        ## artificial names attribute.
        paths <- as.character(sapply(db, "[", 1L))
        names(db) <-
            if(length(paths)
               && all(grepl("^% --- Source file: (.+) ---$", paths)))
                sub("^% --- Source file: (.+) ---$", "\\1", paths)
            else
                NULL
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd sources", dir),
                 domain = NA)
        docs_files <- list_files_with_type(docs_dir, "docs")
        db <- lapply(docs_files, .read_Rd_lines_quietly)
        names(db) <- docs_files
    }

    ## Add package encoding metadata if available and not override by
    ## Rd \encoding entries.
    encoding <-
        .get_package_metadata(dir, !missing(package))["Encoding"]
    ## Should we catch cases where (non-installed) packages have no
    ## DESCRIPTION or DESCRIPTION.in files (then really they cannot be
    ## packages ...)?
    if(!is.na(encoding)) {
        ## For simplicity, always add a package \encoding entry at the
        ## end (so that an explicit \encoding entry in an Rd file always
        ## takes precedence.
        db <- Map(c, db, sprintf("\\encoding{%s}", encoding))
        ## (As we want mapply(SIMPLIFY = FALSE, USE.NAMES = TRUE).)
    }

    db
}

### * Rd_parse

Rd_parse <-
function(file, text = NULL)
{
    ## Arguments similar to the ones in parse(), with 'text' a character
    ## vector with the text to parse (elements are treated as if they
    ## were lines of a file).
    if(!is.null(text))
        lines <- Rd_pp(text)
    else {
        if(is.character(file)) {
            file <- file(file)
            on.exit(close(file))
        }
        if(!inherits(file, "connection"))
            stop("argument 'file' must be a character string or connection")
        lines <- Rd_pp(.read_Rd_lines_quietly(file))
    }

    ## Get metadata (need to agree on what precisely these are), and
    ## remove the corresponding lines (assuming that these entries are
    ## all one-liners).  We mostly do this because \alias (see Paren.Rd)
    ## has non-standard syntax.

    meta <- list(aliases =
                 .get_Rd_metadata_from_Rd_lines(lines, "alias"),
                 concepts =
                 .get_Rd_metadata_from_Rd_lines(lines, "concept"),
                 keywords =
                 .get_Rd_metadata_from_Rd_lines(lines, "keyword"),
                 doc_type =
                 .get_Rd_metadata_from_Rd_lines(lines, "docType"),
                 encoding =
                 .get_Rd_metadata_from_Rd_lines(lines, "encoding"))
    ## Use NA encoding metadata to indicate that we re-encoded a file
    ## not in ISO-8859 as Latin1.
    if(identical(attr(lines, "encoding"), NA))
        meta$encoding <- NA
    ## Remove the metadata lines.
    ## (Use the same regexp as in .get_Rd_metadata_from_Rd_lines().)
    i <- grep(paste("^[[:space:]]*\\\\",
                    "(alias|concept|keyword|docType|encoding)",
                    "\\{[[:space:]]*([^}]*[^}[:space:]])[[:space:]]*\\}.*",
                    sep = ""),
              lines)
    if(length(i)) lines <- lines[-i]
    ## Collapse into one character string.
    txt <- paste(lines, collapse = "\n")
    ## Initialize for extraction loop.
    tag <- ""
    tags <- list()
    rest <- vals <- character()
    ## Note that what we do here is not quite the same as what the code
    ## in R CMD check for checking Rd files does (which e.g. takes all
    ## lines starting with a command tag as top-level).  Also, it is not
    ## clear whether this is what we *really* want (or what Rdconv()
    ## should do).
    ## <NOTE>
    ## We try to catch \non_function{} here, even though it is at least
    ## deprecated.
    ## </NOTE>
    pattern <- "(^|\n)[[:space:]]*\\\\([[:alpha:]]|non_function)+\\{"
    while((pos <- regexpr(pattern, txt)) != -1L) {
        otag <- tag
        start <- substring(txt, 1L, pos + attr(pos, "match.length") - 2L)
        txt <- substring(txt, pos + attr(pos, "match.length") - 1L)
        pos <- regexpr("\\\\([[:alpha:]]|non_function)+$", start)
        tag <- substring(start, pos + 1L)
        start <- substring(start, 1L, pos - 1L)
        pos <- delimMatch(txt)
        if(pos == -1L)
            stop(gettextf("unterminated section '%s'", tag),
                 domain = NA)
        if(tag == "section") {
            tmp <- substring(txt, 2L, attr(pos, "match.length") - 1L)
            txt <- substring(txt, pos + attr(pos, "match.length"))
            ## Should 'txt' now really start with an open brace?
            if(substring(txt, 1L, 1L) != "{")
                stop(gettextf("incomplete section 'section{%s}'", tmp),
                     domain = NA)
            pos <- delimMatch(txt)
            if(pos == -1L)
                stop(gettextf("unterminated section 'section{%s}'", tmp),
                     domain = NA)
            tag <- c(tag, tmp)
        }
        if(!grepl("^[[:space:]]*(^|\n)[[:space:]]*$", start)) {
            names(start) <- paste(otag, collapse = " ")
            rest <- c(rest, start)
        }
        tags <- c(tags, list(tag))
        vals <- c(vals, substring(txt,
                                  pos + 1L,
                                  pos + attr(pos, "match.length") - 2L))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    if(!grepl("^[[:space:]]*(^|\n)[[:space:]]*$", txt)) {
        names(txt) <- paste(tag, collapse = " ")
        rest <- c(rest, txt)
    }
    ## Remove empty sections unless needed for checking ...
    if(!identical(as.logical(Sys.getenv("_R_CHECK_RD_EMPTY_SECTIONS_")),
                  TRUE)) {
        ind <- grepl("^[[:space:]]*$", vals)
        if(any(ind)) {
            vals <- vals[!ind]
            tags <- tags[!ind]
        }
    }
    data <- data.frame(vals = vals, stringsAsFactors = FALSE)
    data$tags <- tags

    list(meta = meta, data = data, rest = rest)
}

### * Rd_aliases

Rd_aliases <-
function(package, dir, lib.loc = NULL)
{
    ## Get the Rd aliases (topics) from an installed package or the
    ## unpacked package sources.

    if(!missing(package)) {
        dir <- .find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            aliases <- .readRDS(rds)$Aliases
            if(length(aliases)) sort(unlist(aliases)) else character()
        } else
            character()
        ## <NOTE>
        ## Alternatively, we could get the aliases from the help index
        ## (and in fact, earlier versions of this code, then part of
        ## undoc(), did so), along the lines of
        ## <CODE>
        ##   help_index <- file.path(dir, "help", "AnIndex")
        ##   all_doc_topics <- if(!file_test("-f", help_index))
        ##       character()
        ##   else
        ##       sort(scan(file = helpIndex, what = list("", ""),
        ##                 sep = "\t", quote = "", quiet = TRUE,
        ##                 na.strings = character())[[1L]])
        ## </CODE>
        ## This gets all topics the same way as index.search() would
        ## find individual ones.
        ## </NOTE>
    }
    else {
        if(file_test("-d", file.path(dir, "man"))) {
            db <- Rd_db(dir = dir)
            db <- lapply(db, Rd_pp)
            aliases <- lapply(db, .get_Rd_metadata_from_Rd_lines, "alias")
            if(length(aliases))
                sort(unique(unlist(aliases, use.names = FALSE)))
            else character()
        }
        else
            character()
    }
}

### .build_Rd_xref_db

.build_Rd_xref_db <-
function(package, dir, lib.loc = NULL)
{
    db <- if(!missing(package))
        Rd_db(package, lib.loc = lib.loc)
    else
        Rd_db(dir = dir)
    db <- lapply(db, function(f) paste(Rd_pp(f), collapse = "\n"))
    lapply(db, .get_Rd_xrefs)
}

### * get_Rd_section

get_Rd_section <-
function(txt, type, predefined = TRUE)
{
    ## Extract Rd section(s) 'type' from (preprocessed) Rd markup in the
    ## character string 'txt'.  Use 'predefined = FALSE' for dealing
    ## with user-defined sections.

    ## <NOTE>
    ## This is *not* vectorized.  As we try extracting *all* top-level
    ## sections of the given type, computations on a single character
    ## string can result in a character vector of arbitray length.
    ## Hence, a vectorized version would return its results similar to
    ## e.g. strsplit(), i.e., a list of character vectors.  Worth the
    ## effort?
    ## </NOTE>

    out <- character()
    if(length(txt) != 1L)
        stop("argument 'txt' must be a character string")
    pattern <- paste("(^|\n)[[:space:]]*\\\\",
                     ifelse(predefined, type,
                            paste("section\\{", type, "\\}",
                                  sep = "")),
                     "\\{",
                     sep = "")
    while((pos <- regexpr(pattern, txt)) != -1L) {
        txt <- substring(txt, pos + attr(pos, "match.length") - 1L)
        pos <- delimMatch(txt)
        if(pos == -1L) {
            if((type == "alias") && predefined) {
                ## \alias entries seem to be special (Paren.Rd).
                ## The regexp below feels wrong, but is based on what is
                ## used in Perl's R::Rdlists::build_index(), sort of.
                pos <- regexpr("\\{([^\n]*)\\}(\n|$)", txt)
            }
            if(pos == -1L)
                stop(gettextf("unterminated section '%s'", type),
                     domain = NA)
            else {
                out <- c(out, sub("\\{([^\n]*)\\}(\n|$).*", "\\1", txt))
                txt <- substring(txt, pos + attr(pos, "match.length"))
                next
            }

        }
        out <- c(out,
                 substring(txt,
                           pos + 1L,
                           pos + attr(pos, "match.length") - 2L))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    out
}

### * get_Rd_items

get_Rd_items <-
function(txt)
{
    ## Extract names of Rd \item{}{} markup in the character string
    ## 'txt'.
    out <- character()
    if(length(txt) != 1L)
        stop("argument 'txt' must be a character string")
    pattern <- "(^|\n)[[:space:]]*\\\\item\\{"
    while((pos <- regexpr(pattern, txt)) != -1L) {
        txt <- substring(txt, pos + attr(pos, "match.length") - 1L)
        if((pos <- delimMatch(txt)) == -1L)
            stop(gettextf("unmatched \\item name in '\\item{%s'",
                          sub("\n.*$", "", txt)),
                 domain = NA,
                 call. = FALSE)
        out <- c(out,
                 substring(txt,
                           pos + 1L,
                           pos + attr(pos, "match.length") - 2L))
        txt <- substring(txt, pos + attr(pos, "match.length"))
        ## The next character should really be a '{'.  Let's be nice
        ## and tolerate whitespace in between ...
        if((pos <- regexpr("^[[:space:]]*\\{", txt)) == -1L)
            stop(gettextf("no \\item description for item '%s'",
                          out[length(out)]),
                 domain = NA,
                 call. = FALSE)
        txt <- substring(txt, pos + attr(pos, "match.length") - 1L)
        if((pos <- delimMatch(txt)) == -1L)
            stop(gettextf("unmatched \\item description for item '%s'",
                          out[length(out)]),
                 domain = NA,
                 call. = FALSE)
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    out
}

### * .get_Rd_metadata_from_Rd_lines

.get_Rd_metadata_from_Rd_lines <-
function(lines, kind) {
    pattern <- paste("^[[:space:]]*\\\\", kind,
                     "\\{[[:space:]]*([^}]*[^}[:space:]])[[:space:]]*\\}.*",
                     sep = "")
    lines <- grep(pattern, lines, value = TRUE)
    lines <- sub(pattern, "\\1", lines)
    lines <- gsub("\\%", "%", lines, fixed = TRUE)
    if(kind == "alias")
        lines <- sub("\\{", "{", lines, fixed = TRUE)
    lines
}

### * .get_Rd_argument_names

.get_Rd_argument_names <-
function(txt)
{
    txt <- get_Rd_section(txt, "arguments")
    txt <- unlist(sapply(txt, get_Rd_items))
    if(!length(txt)) return(character())
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- sub("^[[:space:]]+", "", txt)
    txt <- sub("[[:space:]]+$", "", txt)
    txt <- gsub("\\\\_", "_", txt)
    txt
}

### * .get_Rd_name

.get_Rd_name <-
function(txt)
{
    start <- regexpr("\\\\name\\{[[:space:]]*([^}]+)[[:space:]]*\\}", txt)
    if(start == -1L) return(character())
    Rd_name <- gsub("[[:space:]]+", " ",
                    substr(txt,
                           start + 6L,
                           start + attr(start, "match.length") - 2L))
    Rd_name
}

### * .get_Rd_title

.get_Rd_title <-
function(txt)
{
    start <- regexpr("\\\\title\\{[[:space:]]*([^}]+)[[:space:]]*\\}", txt)
    if(start == -1L) return(character())
    Rd_title <- gsub("[[:space:]]+", " ",
                     substr(txt,
                            start + 7L,
                            start + attr(start, "match.length") - 2L))
    Rd_title
}

### * .get_Rd_example_code

.get_Rd_example_code <-
function(txt)
{
    txt <- get_Rd_section(txt, "examples")
    if(length(txt) != 1L) return(character())

    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\%", "%", txt)

    ## Version of [Perl] R::Rdconv::drop_full_command().
    txt <- .Rd_transform_command(txt, "dontrun",
                                 function(u) NULL)
    ## Version of [Perl] R::Rdconv::undefine_command().
    txt <- .Rd_transform_command(txt, c("dontshow", "testonly"),
                                 function(u) u)
    txt
}

### * .get_Rd_xrefs

.get_Rd_xrefs <-
function(txt)
{
    out <- matrix(character(), nrow = 0L, ncol = 2L)
    if(length(txt) != 1L) return(out)
    while((pos <-
           regexpr("\\\\link(\\[[^[]+\\])?\\{", txt)) != -1L) {
        len <- attr(pos, "match.length")
        opt <- substring(txt, pos + 6L, pos + len - 3L)
        txt <- substring(txt, pos + len - 1L)
        if((pos <- delimMatch(txt)) == -1L)
            stop("unclosed \\link")
        len <- attr(pos, "match.length")
        arg <- substring(txt, 2L, pos + len - 2L)
        txt <- substring(txt, pos + len)
        out <- rbind(out, c(arg, opt))
    }
    colnames(out) <- c("Target", "Anchor")
    out[, 1L] <-  gsub("\\\\%", "%", out[, 1L])
    out
}

### * .Rd_transform_command

.Rd_transform_command <-
function(txt, cmd, FUN)
{
    ## In Rd text, replace markup of the form \cmd{something} by the
    ## result of applying FUN to something.  Covers several separate
    ## functions in the R::Rdconv Perl code:
    ##   drop_full_command      FUN = function(u) NULL
    ##   undefine_command       FUN = function(u) u
    ##   replace_command        FUN = function(u) sprintf("Bef%sAft", u)
    ## Currently, optional arguments to \cmd are not supported.

    if(length(txt) != 1L) return(character())

    ## Vectorized in 'cmd':
    pattern <- sprintf("\\\\(%s)\\{", paste(cmd, collapse = "|"))

    out <- character()
    while((pos <- regexpr(pattern, txt)) != -1L) {
        out <- c(out, substring(txt, 1L, pos - 1L))
        cmd <- substring(txt, pos, pos + attr(pos, "match.length") - 2L)
        txt <- substring(txt, pos + attr(pos, "match.length") - 1L)
        if((pos <- delimMatch(txt)) == -1L)
            stop(sprintf("unclosed \\%s", cmd))
        out <- c(out,
                 FUN(substring(txt, 2L,
                               pos + attr(pos, "match.length") - 2L)))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }

    paste(c(out, txt), collapse = "")
}

### * .apply_Rd_filter_to_Rd_db

.apply_Rd_filter_to_Rd_db <-
function(db, FUN, ...)
{
    db <- lapply(db,
                 function(t) tryCatch(FUN(t, ...), error = identity))
    idx <- as.logical(sapply(db, inherits, "error"))
    if(any(idx)) {
	msg <- gettext("Rd syntax errors found")
	for(i in which(idx))
	    msg <-
		c(msg,
		  gettextf("Syntax error in documentation object '%s':",
			   names(db)[i]),
		  conditionMessage(db[[i]]))
	stop(paste(msg, collapse = "\n"), call. = FALSE, domain = NA)
    }
    db
}

### * .get_Rd_names_from_Rd_db

.get_Rd_names_from_Rd_db <-
function(db)
{
    Rd_names <- lapply(db, .get_Rd_name)
    ## If the Rd db was obtained from an installed package, we know that
    ## all Rd objects must have a \name entry---otherwise, Rdinfo() and
    ## hence installing the package Rd contents db would have failed.
    ## For Rd dbs created from a package source directory, we now add
    ## the Rd file paths as the names attribute, so that we can point to
    ## the files with missing \name entries.
    idx <- as.integer(sapply(Rd_names, length)) == 0L
    if(any(idx)) {
        Rd_paths <- names(db)
        if(is.null(Rd_paths)) {
            ## This should not happen.
            ## We cannot refer to the bad Rd objects because we do not
            ## know their names, and have no idea which file they came
            ## from ...)
            stop("cannot deal with Rd objects with missing/empty names")
        }
        else {
            stop(paste(gettext("missing/empty \\name field in Rd file(s)"),
                       paste(" ", Rd_paths[idx], collapse = "\n"),
                       sep = "\n"),
                 call. = FALSE, domain = NA)
        }
    }
    unlist(Rd_names)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
