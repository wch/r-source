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

### * Rd_info

Rd_info <-
function(file)
{
    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument 'file' must be a character string or connection")

    Rd <- prepare_Rd(file, defines = .Platform$OS.type)

    aliases <- .Rd_get_metadata(Rd, "alias")
    concepts <- .Rd_get_metadata(Rd, "concept")
    keywords <- .Rd_get_metadata(Rd, "keyword")

    ## Could be none or more than one ... argh.
    Rd_type <- c(.Rd_get_metadata(Rd, "docType"), "")[1L]
    encoding <- c(.Rd_get_metadata(Rd, "encoding"), "")[1L]

    Rd_name <- .Rd_get_name(Rd)
    if(!length(Rd_name)) {
        msg <-
            c(gettextf("missing/empty \\name field in '%s'",
                       summary(file)$description),
              gettext("Rd files must have a non-empty \\name."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    Rd_title <- .Rd_get_title(Rd)
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

### * Rd_contents

Rd_contents <-
function(files)
{
    ## Compute contents db from Rd files.

    files <- path.expand(files[file_test("-f", files)])

    if(!length(files)) {
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
    contents <- vector("list", length(files) * length(entries))
    dim(contents) <- c(length(files), length(entries))
    for(i in seq_along(files)) {
        contents[i, ] <- Rd_info(files[i])
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

    out <- data.frame(File = basename(files),
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

### * .write_Rd_contents_as_RDS

.write_Rd_contents_as_RDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.

    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    .saveRDS(contents, file = outFile, compress = TRUE)
    ## </NOTE>
}

### * .write_Rd_contents_as_DCF

.write_Rd_contents_as_DCF <-
function(contents, packageName, outFile)
{
    ## Write a @file{CONTENTS} DCF file from an Rd contents db.
    ## Note that these files currently have @samp{URL:} entries which
    ## contain the package name, whereas @code{Rd_contents()} works on
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

    index <- .build_Rd_index(Rd_contents(RdFiles), type = type)

    writeLines(formatDL(index, width = width, indent = indent),
               outFile)
}

### * Rd_db

Rd_db <-
function(package, dir, lib.loc = NULL)
{
    ## Build an Rd 'data base' from an installed package or the unpacked
    ## package sources as a list containing the parsed Rd objects.

    ## <NOTE>
    ## We actually also process platform conditionals.
    ## If this was to be changed, we could also need to arrange that Rd
    ## objects in *all* platform specific subdirectories are included.
    ## </NOTE>

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
        ## For an installed package, we can either have an old-style
        ##   man/package.Rd.gz
        ## file with suitable concatenated Rd sources, or a new-style
        ##   man/package.rds
        ## file with a list of the parsed (and platform processed, see
        ## above) Rd objects.
        db_file <- file.path(docs_dir, sprintf("%s.rds", package))
        if(file_test("-f", db_file))
            return(.readRDS(db_file))
        db_file <- file.path(docs_dir, sprintf("%s.Rd.gz", package))
        if(file_test("-f", db_file)) {
            lines <- .read_Rd_lines_quietly(db_file)
            eof_pos <-
                grep("^\\\\eof$", lines, perl = TRUE, useBytes = TRUE)
            db <- split(lines[-eof_pos],
                        rep(seq_along(eof_pos),
                            times = diff(c(0, eof_pos)))[-eof_pos])
        } else return(list())
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
        ## Determine package encoding.
        encoding <- .get_package_metadata(dir, TRUE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
        ## <FIXME>
        ## Change back to
        ##   db <- lapply(db, prepare_Rd_from_Rd_lines,
        ##                encoding = encoding,
        ##                defines = .Platform$OS.type,
        ##                stages = "install")
        ## when we no longer need the Rd sources ...
        ## </FIXME>
        db <- Map(function(x, y) structure(x, source = y),
                  lapply(db, prepare_Rd_from_Rd_lines,
                         encoding = encoding,
                         defines = .Platform$OS.type,
                         stages = "install"),
                  if(encoding != "unknown")
                      Map(c, db, sprintf("\\encoding{%s}", encoding))
                  else
                      db)
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
        encoding <- .get_package_metadata(dir, FALSE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
        ## <FIXME>
        ## Change back to
        ##   db <- lapply(docs_files, prepare_Rd,
        ##                encoding = encoding,
        ##                defines = .Platform$OS.type,
        ##                stages = "install")
        ## when we no longer need the Rd sources ...
        ## </FIXME>
        db <- lapply(docs_files, .read_Rd_lines_quietly)
        db <- Map(function(x, y) structure(x, source = y),
                  lapply(docs_files, prepare_Rd,
                     encoding = encoding,
                     defines = .Platform$OS.type,
                     stages = "install"),
                  if(encoding != "unknown")
                      Map(c, db, sprintf("\\encoding{%s}", encoding))
                  else
                      db)
        names(db) <- docs_files
    }

    db

}

prepare_Rd_from_Rd_lines <-
function(x, ...)
{
    con <- textConnection(x, "rt")
    on.exit(close(con))
    prepare_Rd(con, ...)
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
            aliases <- lapply(db, .Rd_get_metadata, "alias")
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
    ## <FIXME Rd2>
    ## Move to new-style code ...
    ## Does not work:
    ##   db <- lapply(db, paste, collapse = "")
    db <- lapply(db,
                 function(f)
                 paste(Rd_pp(attr(f, "source")), collapse = "\n"))
    lapply(db, .Rd_get_xrefs_from_Rd_text)
    ## </FIXME>
}

### * .Rd_get_metadata

.Rd_get_metadata <-
function(x, kind)
{
    x <- x[RdTags(x) == sprintf("\\%s", kind)]
    if(!length(x))
        character()
    else
        .strip_whitespace(sapply(x, as.character))
}

### * .Rd_get_section

.Rd_get_section <-
function(x, which, predefined = TRUE)
{
    x <- if(predefined)
        x[RdTags(x) == paste("\\", which, sep = "")]
    else {
        ## User-defined sections are parsed into lists of length 2, with
        ## the elements the title and the body, respectively.
        ind <- ((RdTags(x) == "\\section") &
                (sapply(x, function(e) .Rd_deparse(e[[1L]]) == which)))
        lapply(x[ind], `[[`, 2L)
    }
    if(!length(x)) x else structure(x[[1L]], class = "Rd")
}

### * .Rd_deparse

.Rd_deparse <-
function(x)
{
    ## <NOTE>
    ## This should eventually get an option controlling whether to
    ## escape Rd special characters as needed (thus providing valid Rd)
    ## or not.
    ## </NOTE>
    paste(as.character.Rd(x), collapse = "")
}

### * .Rd_drop_specials

.Rd_drop_specials <-
function(x)
{
    recurse <- function(e) {
        if(is.list(e))
            lapply(e[RdTags(e) != "\\special"], recurse)
        else
            e
    }
    recurse(x)
}

### * .Rd_get_argument_names

.Rd_get_argument_names <-
function(x)
{
    x <- .Rd_get_section(x, "arguments")
    if(!length(x)) return(character())
    txt <- .Rd_get_item_tags(x)
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\_", "_", txt)
    .strip_whitespace(txt)
}

### * .Rd_get_item_tags

.Rd_get_item_tags <-
function(x)
{
    ## Extract two-arg \item tags at top level ... non-recursive.
    out <- lapply(x[RdTags(x) == "\\item"],
                  function(e) {
                      if(length(e) == 2L) .Rd_deparse(e[[1L]])
                  })
    as.character(unlist(out))
}

### * .Rd_get_example_code

.Rd_get_example_code <-
function(x)
{
    x <- .Rd_get_section(x, "examples")
    if(!length(x)) return(character())
    
    ## Need to remove everything inside \dontrun, and "undefine"
    ## \dontshow and \testonly (which is achieved by changing the Rd tag
    ## to "Rd").  Not clear whether this really needs to be done
    ## recursively ...

    recurse <- function(e) {
        if(!is.null(tag <- attr(e, "Rd_tag"))
           && tag %in% c("\\dontshow", "\\testonly"))
            attr(e, "Rd_tag") <- "Rd"
        if(is.list(e))
            structure(lapply(e[RdTags(e) != "\\dontrun"], recurse),
                      Rd_tag = attr(e, "Rd_tag"))
        else e
    }

    .Rd_deparse(structure(recurse(x), Rd_tag = "Rd"))
}

### * .Rd_get_name

.Rd_get_name <-
function(x)
{
    ## <FIXME Rd2>
    if(is.character(x))
        return(.Rd_get_name_from_Rd_text(x))
    ## </FIXME>
    x <- .Rd_get_section(x, "name")
    ## The name should really be plain text, so as.character() should be
    ## fine as well ...
    if(length(x))
        .strip_whitespace(.Rd_deparse(structure(x, Rd_tag = "Rd")))
    else
        character()
}

### * .Rd_get_title

.Rd_get_title <-
function(x)
{
    ## <FIXME Rd2>
    if(is.character(x))
        return(.Rd_get_title_from_Rd_text(x))
    ## </FIXME>
    x <- .Rd_get_section(x, "title")
    if(length(x))
        .strip_whitespace(.Rd_deparse(structure(x, Rd_tag = "Rd")))
    else
        character()
}

### * .Rd_get_xrefs

.Rd_get_xrefs <-
function(x)
{
    out <- matrix(character(), nrow = 0L, ncol = 2L)
    recurse <- function(e) {
        if(identical(attr(e, "Rd_tag"), "\\link")) {
            arg <- as.character(e[[1L]])
            opt <- attr(e, "Rd_option")
            val <- if(is.null(opt))
                c(arg, "")
            else
                c(arg, as.character(opt))
            out <<- rbind(out, val)
        }
        if(is.list(e)) lapply(e, recurse)
    }
    lapply(x, recurse)
    dimnames(out) <- list(NULL, c("Target", "Anchor"))
    out
}    

### * .Rd_get_names_from_Rd_db

.Rd_get_names_from_Rd_db <-
function(db)
{
    Rd_names <- lapply(db, .Rd_get_name)
    ## If the Rd db was obtained from an installed package, we know that
    ## all Rd objects must have a \name entry---otherwise, Rd_info() and
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

### * Old-style code.

### * Rd_pp

Rd_pp <-
function(lines)
{
    ## Preprocess lines with Rd markup according to .Platform$OS.type.

    if(!is.character(lines))
        stop("argument 'lines' must be a character vector")

    ## Re-encode if necessary (and possible).
    encoding <-
        .Rd_get_metadata_from_Rd_lines(lines[!is.na(nchar(lines, "c", TRUE))],
                                       "encoding")
    if(length(encoding)) {
        if((Sys.getlocale("LC_CTYPE") != "C")) {
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
        lines <- iconv(lines, "latin1", "")
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

.strip_Rd_comments <-
function(lines)
{
    gsub("(^|[^\\])((\\\\\\\\)*)%.*", "\\1\\2", lines)
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
                 .Rd_get_metadata_from_Rd_lines(lines, "alias"),
                 concepts =
                 .Rd_get_metadata_from_Rd_lines(lines, "concept"),
                 keywords =
                 .Rd_get_metadata_from_Rd_lines(lines, "keyword"),
                 doc_type =
                 .Rd_get_metadata_from_Rd_lines(lines, "docType"),
                 encoding =
                 .Rd_get_metadata_from_Rd_lines(lines, "encoding"))
    ## Use NA encoding metadata to indicate that we re-encoded a file
    ## not in ISO-8859 as Latin1.
    if(identical(attr(lines, "encoding"), NA))
        meta$encoding <- NA
    ## Remove the metadata lines.
    ## (Use the same regexp as in .Rd_get_metadata_from_Rd_lines().)
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

### * .Rd_get_metadata_from_Rd_lines

.Rd_get_metadata_from_Rd_lines <-
function(lines, kind)
{
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

### * .Rd_get_section_from_Rd_text

.Rd_get_section_from_Rd_text <-
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

### * .Rd_get_argument_names_from_Rd_text

.Rd_get_argument_names_from_Rd_text <-
function(txt)
{
    txt <- .Rd_get_section_from_Rd_text(txt, "arguments")
    txt <- unlist(sapply(txt, .Rd_get_items_from_Rd_text))
    if(!length(txt)) return(character())
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\_", "_", txt)
    .strip_whitespace(txt)
}

### * .Rd_get_example_code_from_Rd_text

.Rd_get_example_code_from_Rd_text <-
function(txt)
{
    txt <- .Rd_get_section_from_Rd_text(txt, "examples")
    if(length(txt) != 1L) return(character())

    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\%", "%", txt)

    ## Version of [Perl] R::Rdconv::drop_full_command().
    txt <- .Rd_transform_command_in_Rd_text(txt, "dontrun",
                                            function(u) NULL)
    ## Version of [Perl] R::Rdconv::undefine_command().
    txt <- .Rd_transform_command_in_Rd_text(txt,
                                            c("dontshow", "testonly"),
                                            function(u) u)
    txt
}

### * .Rd_get_items_from_Rd_text

.Rd_get_items_from_Rd_text <-
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

### * .Rd_get_name_from_Rd_text

.Rd_get_name_from_Rd_text <-
function(txt)
{
    start <-
        regexpr("\\\\name\\{[[:space:]]*([^}]+)[[:space:]]*\\}", txt)
    if(start == -1L) return(character())
    Rd_name <- gsub("[[:space:]]+", " ",
                    substr(txt,
                           start + 6L,
                           start + attr(start, "match.length") - 2L))
    Rd_name
}

### * .Rd_get_title_from_Rd_text

.Rd_get_title_from_Rd_text <-
function(txt)
{
    start <-
        regexpr("\\\\title\\{[[:space:]]*([^}]+)[[:space:]]*\\}", txt)
    if(start == -1L) return(character())
    Rd_title <- gsub("[[:space:]]+", " ",
                     substr(txt,
                            start + 7L,
                            start + attr(start, "match.length") - 2L))
    Rd_title
}

### * .Rd_get_xrefs_from_Rd_text

.Rd_get_xrefs_from_Rd_text <-
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

### * .Rd_transform_command_in_Rd_text

.Rd_transform_command_in_Rd_text <-
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

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
