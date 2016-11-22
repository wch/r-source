#  File src/library/tools/R/citation.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

## Tools for computing on CITATION info.

.parse_CITATION_file <-
function(cfile, encoding = NULL)
{
    if(is.null(encoding))
        encoding <- "ASCII"

    ## The parser can only read valid strings, but single-byte locales
    ## can mark their encoding.  The following allows latin1 and UTF-8
    ## citation files to be read in UTF-8 and any single-byte locale
    ## (including C).
    ##
    ## FIXME: if parse() could be told to read strings bytewise,
    ## we could simply convert to UTF-8.
    if(encoding %in% c("latin1", "UTF-8") && !l10n_info()$MBCS) {
        parse(file = cfile, encoding = encoding)
    } else if(encoding %in% c("C", "ASCII")) {
        ## We do want to make sure this is ASCII: in single-byte
        ## locales 8-bit chars are likely to be parsed as bytes.
        ## Based on showNonASCII()
        x <- readLines(cfile, warn = FALSE)
        asc <- iconv(x, "latin1", "ASCII")
        if (any(is.na(asc) | asc != x))
            stop("non-ASCII input in a CITATION file without a declared encoding")
        parse(file = cfile)
    } else {
        con <- file(cfile, encoding = encoding)
        on.exit(close(con))
        parse(con)
    }
}

.parse_CITATION_file_in_package <-
function(cfile, installed = FALSE)    
{
    cfile <- file_path_as_absolute(cfile)
    dfile <- file.path(if(installed)
                           dirname(cfile)
                       else
                           dirname(dirname(cfile)),
                       "DESCRIPTION")
    meta <- .read_description(dfile)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- NULL
    .parse_CITATION_file(cfile, encoding)
}
    
BibTeX_entry_field_db <-
    list(Article = c("author", "title", "journal", "year"),
         Book = c("author|editor", "title", "publisher", "year"),
         Booklet = c("title"),
         InBook =
         c("author|editor", "title", "chapter", "publisher", "year"),
         InCollection =
         c("author", "title", "booktitle", "publisher", "year"),
         InProceedings = c("author", "title", "booktitle", "year"),
         Manual = c("title"),
         MastersThesis = c("author", "title", "school", "year"),
         Misc = character(),
         PhdThesis = c("author", "title", "school", "year"),
         Proceedings = c("title", "year"),
         TechReport = c("author", "title", "institution", "year"),
         Unpublished = c("author", "title", "note")
         )
## See e.g. lisp/textmodes/bibtex.el in the GNU Emacs sources.

get_CITATION_entry_fields <-
function(file, encoding = "ASCII")
{
    exprs <- .parse_CITATION_file(file, encoding)

    ## Assume that bibentry() or citEntry() only occur at top level.

    ## Try to detect entry type and field names from the calls.
    FOO1 <- FOO2 <- function() match.call(expand.dots = FALSE)
    formals(FOO1) <- formals(utils::citEntry)
    formals(FOO2) <- formals(utils::bibentry)
    ## Could also hard-wire this, of course.
    get_names_of_nonempty_fields <- function(x) {
        names(x)[sapply(x,
                        function(e) {
                            length(e) &&
                            !(is.character(e) &&
                              all(grepl("^[[:space:]]*$", e)))
                        })]
    }

    out <- lapply(exprs,
           function(e) {
               nm <- as.character(e[[1L]])
               if(nm == "citEntry") {
                   e[[1L]] <- as.name("FOO1")
                   e <- as.list(eval(e))
                   entry <- e$entry
                   fields <- get_names_of_nonempty_fields(e$...)
               }
               else if(nm == "bibentry") {
                   e[[1L]] <- as.name("FOO2")
                   e <- as.list(eval(e))
                   entry <- e$bibtype
                   fields <- get_names_of_nonempty_fields(c(e$...,
                                                            as.list(e$other)[-1L]))
               }
               else return()
               entry <- if(!is.character(entry)) NA_character_ else entry[1L]
               list(entry = entry, fields = as.character(fields))
           })

    out <- Filter(Negate(is.null), out)
    ## If we found nothing return nothing ...
    if(!length(out)) return(NULL)
    entries <- sapply(out, `[[`, 1L)
    fields <- lapply(out, `[[`, 2L)
    out <- data.frame(File = file,
                      Entry = entries,
                      stringsAsFactors = FALSE)
    out$Fields <- fields
    out
}


find_missing_required_BibTeX_fields <-
function(entry, fields)
{
    pos <- match(tolower(entry),
                 tolower(names(BibTeX_entry_field_db)))
    if(is.na(pos)) {
        ## Invalid entry.
        return(NA_character_)
    }
    rfields <- BibTeX_entry_field_db[[pos]]
    if(!length(rfields)) return(character())
    ## Go for legibility/generality rather than efficiency.
    fields <- tolower(fields)
    ok <- sapply(strsplit(rfields, "|", fixed = TRUE),
                 function(f) any(f %in% fields))
    rfields[!ok]
}
