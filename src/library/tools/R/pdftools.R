#  File src/library/tools/R/pdftools.R
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

## See PDF Reference version 1.7 chapter 3:
##   At the most fundamental level, a PDF file is a sequence of 8-bit
##   bytes.
## where subsequently for chapter 3
##   the term character is synonymous with byte and merely refers to a
##   particular 8-bit value ...
##
## The PDF character set is divided into 3 classes:
## White-space characters (table 3.1):
##    0 00 000 Null (NUL)
##    9 09 011 Tab (HT)
##   10 0A 012 Line feed (LF)
##   12 0C 014 Form feed (FF)
##   13 0D 015 Carriage return (CR)
##   32 20 040 Space (SP)
## Delimiter characters:
##   ( ) < > [ ] { } / %
## All characters except white-space characters and delimiters are
## regular characters.

## * Variables

pdf_bytes_whitespaces <-
    c(charToRaw("\t\n\f\r "), as.raw(0L))
pdf_bytes_delimiters <-
    charToRaw("()<>[]{}/%")
pdf_bytes_non_regulars <-
    c(pdf_bytes_whitespaces, pdf_bytes_delimiters)
pdf_bytes_eols <-
    charToRaw("\n\r")
pdf_bytes_digits <-
    charToRaw("0123456789")

pdf_bytes_in_keywords <-
    charToRaw(paste0("*'\"",
                    "0123456789",
                    paste(LETTERS, collapse = ""),
                    paste(letters, collapse = "")))

pdf_bytes_in_numerics_not_digits <-
    charToRaw("+-.")
pdf_bytes_in_numerics <-
    c(pdf_bytes_digits, pdf_bytes_in_numerics_not_digits)

pdf_page_sizes <-
do.call(rbind,
        list("A0" =        c(2384L, 3371L),
             "A1" =        c(1685L, 2384L),
             "A2" =        c(1190L, 1684L),
             "A3" =        c( 842L, 1190L),
             "A4" =        c( 595L,  842L),
             "A5" =        c( 420L,  595L),
             "B4" =        c( 729L, 1032L),
             "B5" =        c( 516L,  729L),
             "letter" =    c( 612L,  792L),
             "tabloid" =   c( 792L, 1224L),
             "ledger" =    c(1224L,  792L),
             "legal" =     c( 612L, 1008L),
             "statement" = c( 396L,  612L),
             "executive" = c( 540L,  720L),
             "folio" =     c( 612L,  936L),
             "quarto" =    c( 610L,  780L),
             "10x14" =     c( 720L, 1008L)))

## * pdf_doc

pdf_doc <-
function(file, cache = TRUE)
{
    if(is.character(file)) {
        ## <FIXME>
        ## When caching, we could read the whole PDF file into memory
        ## and use a raw connection to read this byte stream.  But is
        ## there a way to get the connection closed when the doc object
        ## gets removed?
        ##   if(cache) {
        ##       .bytes <- readBin(file, "raw", file.size(file))
        ##       con <- rawConnection(.bytes)
        ##       keep <- TRUE
        ##   }
        ## </FIXME>
        con <- file(file, "rb")
        on.exit(close(con))
        keep <- FALSE
    } else if(inherits(file, "rawConnection")) {
        con <- file
        file <- NA_character_
        keep <- TRUE
    } else if(inherits(file, "file")) {
        con <- file
        file <- summary(con)$description
        keep <- TRUE
    } else {
        stop(gettextf("%s must be a character string or a file/raw connection",
                      sQuote("file")),
             domain = NA)
    }

    ## Read header.
    .con_seek(con, 0L)
    header <- rawToChar(read_next_bytes_until_whitespace(con))
    if(substring(header, 1L, 5L) != "%PDF-")
        stop("PDF header not found")

    ## Go to the end.
    .con_seek(con, -1L, 3L)

    ## Record file size as number of bytes.
    nbytes <- .con_seek(con) + 1L

    ## Check footer.
    bytes <- raw()
    while(!length(bytes))
        bytes <- read_prev_bytes_after_eols(con)
    if(rawToChar(bytes) != "%%EOF")
        stop("EOF marker not found")

    ## Find startxref entry (the location of the xref table).
    ## See PDF Reference version 1.7 section 3.4.4:
    ##   The last line of the file contains only the end-of-file marker,
    ##   %%EOF.
    ##   The two preceding lines contain the keyword startxref and the
    ##   byte offset from the beginning of the file to the beginning of
    ##   the xref keyword in the last cross-reference section.
    ## But as of 2011-09-12 there is at least one PDF in CRAN's packages
    ## (limSolve/inst/doc/JSS-373-fig1.pdf) which has keyword and offset
    ## in the same line.
    ## Hence, let's be nice, and read back over any white-space and not
    ## just eols.

    bytes <- read_prev_bytes_after_bytes(con, pdf_bytes_whitespaces)
    startxref <- suppressWarnings(as.integer(rawToChar(bytes)))
    bytes <- read_prev_bytes_after_bytes(con, pdf_bytes_whitespaces)
    if(substring(rawToChar(bytes), 1L, 9L) != "startxref")
        stop("cannot find 'startxref' keyword")

    xref_tabs <-
        matrix(integer(), nrow = 0L, ncol = 4L,
               dimnames = list(NULL, c("num", "pos", "gen", "use")))
    xref_objs <-
        matrix(integer(), nrow = 0L, ncol = 3L,
               dimnames = list(NULL, c("num", "str", "idx")))
    trailer <- list()

    ## Some PDFs have the offset to the xref table wrong.  As of
    ## 2011-08-24,
    ## * gdata/inst/doc/gregmisc.pdf points to the char after the x
    ## * gplots/inst/doc/BalloonPlot.pdf points to after xref
    find_xref_start <- function(con) {
        ## Skip backwards over whitespace, then read back until the next
        ## whitespace.
        .con_seek(con, -1L, 2L)
        repeat {
            x <- .con_read_bytes(con, 1L)
            if(!(x %.IN.% pdf_bytes_whitespaces)) break
            .con_seek(con, -2L, 2L)
        }
        repeat {
            .con_seek(con, -2L, 2L)
            x <- .con_read_bytes(con, 1L)
            if(x %.IN.% pdf_bytes_whitespaces) break
        }
        pos <- .con_seek(con)
        if(rawToChar(.con_read_bytes(con, 4L)) == "xref")
            pos
        else
            stop("cannot find xref table")
    }

    ## Load the xref info.
    repeat {
        .con_seek(con, startxref)
        x <- .con_read_bytes(con, 1L)
        if(x == charToRaw("x")) {
            ## A standard cross-reference table, hopefully.
            bytes <- read_next_bytes_until_whitespace(con)
            if(!rawToChar(bytes) == "ref")
                stop("cannot read xref table")
            read_next_non_whitespace_and_seek_back(con)
            repeat {
                bytes <- read_next_bytes_until_whitespace(con)
                num <- suppressWarnings(as.integer(rawToChar(bytes)))
                read_next_non_whitespace_and_seek_back(con)
                bytes <- read_next_bytes_until_whitespace(con)
                size <- suppressWarnings(as.integer(rawToChar(bytes)))
                read_next_non_whitespace_and_seek_back(con)
                cnt <- 0
                entries <- list()
                while(cnt < size) {
                    bytes <- .con_read_bytes(con, 20L)
                    ## Assume that all lines were correctly at 20 bytes.
                    ## (Could also try to be nice ...)
                    entry <- c(num,
                               as.integer(rawToChar(bytes[ 1L : 10L])),
                               as.integer(rawToChar(bytes[12L : 16L])),
                               bytes[18L] == 0x6e) # charToRaw("n") => 6e
                    entries <- c(entries, list(entry))
                    cnt <- cnt + 1L
                    num <- num + 1L
                }
                xref_tabs <- rbind(xref_tabs, do.call(rbind, entries))
                read_next_non_whitespace_and_seek_back(con)
                if(rawToChar(.con_read_bytes(con, 7L)) != "trailer")
                    .con_seek(con, -7L, 2L)
                else break
            }
            ## Read trailer info.
            read_next_non_whitespace_and_seek_back(con)
            new_trailer <- pdf_read_object(con)
            ## Merge with current trailer info.
            trailer[names(new_trailer)] <- new_trailer
            ## If the trailer info has a /Prev key, then redo the above
            ## with the corresponding value the new startxref.
            startxref <- new_trailer[["Prev"]]
            if(is.null(startxref)) break
        } else if(x %.IN.% pdf_bytes_digits) {
            ## PDF 1.5+ cross-reference stream, hopefully.
            .con_seek(con, -1L, 2L)
            pos <- .con_seek(con)
            pdf_read_object_header(con)
            obj <- pdf_read_object(con)
            if(("Type" %notin% names(obj)) ||
               !(obj[["Type"]] == "XRef")) {
                ## Something's wrong.
                ## Try finding the xref table before pos.
                .con_seek(con, pos)
                startxref <- find_xref_start(con)
                next
            }
            size <- obj[["Size"]]
            index <- obj[["Index"]]
            index <- if(is.null(index)) {
                ## Use default [0 Size].
                cbind(0, size)
            } else {
                matrix(unlist(index), ncol = 2L, byrow = TRUE)
            }
            field_sizes <- unlist(obj[["W"]])
            stream <- rawConnection(PDF_Stream_get_data(obj))
            for(i in seq_len(nrow(index))) {
                num <- index[i, 1L]
                cnt <- 0L
                while(cnt < index[i, 2L]) {
                    bytes <- .con_read_bytes(stream, field_sizes[1L])
                    d1 <- strtoi(paste(bytes, collapse = ""), 16L)
                    bytes <- .con_read_bytes(stream, field_sizes[2L])
                    d2 <- strtoi(paste(bytes, collapse = ""), 16L)
                    bytes <- .con_read_bytes(stream, field_sizes[3L])
                    d3 <- strtoi(paste(bytes, collapse = ""), 16L)
                    ## Might actually need to overwrite entries.
                    ## Worry about that lateron ...
                    if(d1 == 1) {
                        xref_tabs <-
                            rbind(xref_tabs, c(num, d2, d3, TRUE))
                    } else if(d1 == 2) {
                        xref_objs <-
                            rbind(xref_objs, c(num, d2, d3))
                    }
                    cnt <- cnt + 1L
                    num <- num + 1L
                }
            }
            close(stream)
            keys <- c("Root", "Encrypt", "Info", "ID")
            pos <- match(keys, names(obj), nomatch = 0L)
            trailer[keys[pos > 0L]] <- obj[pos]
            startxref <- obj[["Prev"]]
            if(is.null(startxref)) break
        } else {
            .con_seek(con, -1L, 2L)
            startxref <- find_xref_start(con)
        }
    }

    ## Determine the "active" objects (all objects in cross-reference
    ## streams and objects in cross-reference tables marked in use and
    ## with a "real" position).
    ind <- (xref_tabs[, "pos"] > 0L) & (xref_tabs[, "use"] > 0L)
    names <-
        c(.ref_to_name(xref_tabs[ind, c("num", "gen"), drop = FALSE]),
          if(length(num <- xref_objs[, "num"]))
          .ref_to_name(cbind(num, 0)))
    ## Build an index of names as lists of object numbers with
    ## corresponding generation numbers in decreasing order.
    nums <- xref_objs[, "num"]
    gens <- rep.int(0L, length(nums))
    nums <- c(nums, xref_tabs[ind, "num"])
    gens <- c(gens, xref_tabs[ind, "gen"])
    gens_by_nums <-
        lapply(split(as.integer(gens), nums), sort, decreasing = TRUE)

    y <- new.env(parent = emptyenv())
    y$file <- file
    y$size <- nbytes
    y$header <- header
    y$xref_tabs <- xref_tabs
    y$xref_objs <- xref_objs
    y$trailer <- trailer
    y$cache <- cache
    y$con <- if(keep) con else NULL
    y$names <- names
    y$length <- length(names)
    y$gens_by_nums <- gens_by_nums
    y$objects <- list()
    class(y) <- "pdf_doc"
    y
}

print.pdf_doc <-
function(x, ...)
{
    writeLines(strwrap(sprintf("PDF document (file \"%s\", %d bytes, %d objects)",
                               x$file, x$size, x$length),
                       exdent = 4L))
    invisible(x)
}

length.pdf_doc <-
function(x)
    x$length

names.pdf_doc <-
function(x)
    x$names

`[.pdf_doc` <-
function(x, i)
{
    if(!inherits(x, "pdf_doc")) stop("wrong class")
    if(missing(i)) return(pdf_doc_get_objects(x))
    i <- as.character(i)                # For now ...
    out <- vector("list", length(i))
    pos <- .match_ids_to_pdf_doc_names(i, x)
    nms <- x$names[pos]
    names(out) <- nms
    ind <- !is.na(pos)
    out[ind] <- pdf_doc_get_objects(x, nms[ind])
    out
}

`[[.pdf_doc` <-
function(x, i)
{
    if(!inherits(x, "pdf_doc")) stop("wrong class")
    if(missing(i) || (!(len <- length(i <- as.character(i)))))
        stop("attempting to select less than one element")
    else if(len > 1L)
        stop("attempting to select more than one element")
    pos <- .match_ids_to_pdf_doc_names(i, x)
    if(is.na(pos)) return(NULL)
    pdf_doc_get_object(x, x$names[pos])
}

## * pdf_catalog

pdf_catalog <-
function(file)
{
    doc <- if(inherits(file, "pdf_doc")) file else pdf_doc(file)
    pdf_doc_get_object(doc, doc$trailer[["Root"]])
}

## * pdf_fonts

## <FIXME>
## Currently only extracts the fonts used in pages (but not, e.g.,
## forms).
## </FIXME>

pdf_fonts <-
function(file)
{
    doc <- if(inherits(file, "pdf_doc"))
        file
    else
        pdf_doc(file, cache = FALSE)

    con <- file(doc$file, "rb")
    on.exit(close(con))

    ## Get the page resources.
    resources <- pdf_doc_get_page_resources(doc, con)
    ## Get the font resources (actually, their references).
    frefs <- list()
    for(res in resources) {
        fonts <- pdf_dereference_maybe(res[["Font"]], doc, con)
        for(ref in fonts) {
            if(is.na(match(list(ref), frefs))) {
                frefs <- c(frefs, list(ref))
            }
        }
    }

    if(!length(frefs)) {
        tab <- data.frame(name = character(),
                          type = character(),
                          emb = logical(),
                          sub = logical(),
                          uni = logical(),
                          enc = logical(),
                          num = integer(),
                          gen = integer(),
                          stringsAsFactors = FALSE)
        tab$enc <- list()
        return(tab)
    }

    ## Now get the referenced font objects and extract some basic
    ## information in the style of pdffonts(1).
    ##   emb   "yes" if the font is embedded in the PDF file
    ##   sub   "yes" if the font is a subset
    ##   uni   "yes" if there is an explicit "ToUnicode" map in the PDF file
    ##         (the absence of a ToUnicode map doesn't necessarily mean that
    ##         the text can't be converted to Unicode)
    ## Re subset, see section 5.5.3 "Font Subsets" in the PDF ref:
    ##   For a font subset, the PostScript name of the font--the value
    ##   of the font's BaseFont entry and the font descriptor's FontName
    ##   entry--begins with a tag followed by a plus sign (+). The tag
    ##   consists of exactly six uppercase letters; the choice of
    ##   letters is arbitrary, but different subsets in the same PDF
    ##   file must have different tags.
    ## Also extract /Encoding information:
    ##   A specification of the font's character encoding if different
    ##   from its built-in encoding. The value of Encoding is either the
    ##   name of a predefined encoding (MacRomanEncoding,
    ##   MacExpertEncoding, or WinAnsiEncoding, as described in Appendix
    ##   D) or an encoding dictionary that specifies differences from
    ##   the font's built-in encoding or from a specified predefined
    ##   encoding (see Section 5.5.5, "Character Encoding").
    tab <- lapply(frefs,
                  function(ref) {
                      obj <- pdf_doc_get_object(doc, ref, con)
                      base <- obj[["BaseFont"]]
                      ## See PDF Reference version 1.7 section 5.5.4.
                      ## Type 3 font dictionaries have no BaseFont entry.
                      ## (Guess they are always embedded?)
                      if(is.null(base))
                          base <- "[none]"
                      else if(inherits(base,
                                       "PDF_Indirect_Reference")) {
                          base <- pdf_doc_get_object(doc, base, con)
                      }
                      list(base,
                           obj[["Subtype"]],
                           ((base == "[none]") ||
                            !is.null(obj[["FontDescriptor"]])),
                           grepl("^[[:upper:]]{6}\\+", base, perl = TRUE),
                           !is.null(obj[["ToUnicode"]]),
                           obj[["Encoding"]],
                           ref["num"],
                           ref["gen"])
                  })

    tab <- as.data.frame(do.call(rbind, tab))
    names(tab) <-
        c("name", "type", "emb", "sub", "uni", "enc", "num", "gen")
    ## <NOTE>
    ## This turns name and type from lists of PDF names to character.
    ## Maybe add methods for combining PDF_Name objects lateron ...
    ind <- names(tab) != "enc"
    tab[ind] <- lapply(tab[ind], unlist)
    ## </NOTE>
    ## <FIXME>
    ## Do something useful to the encoding information.
    ## For now, these are really NULL or indirect references ...
    ## </FIXME>

    class(tab) <- c("pdf_fonts", "data.frame")
    tab
}

format.pdf_fonts <-
function(x, ...)
{
    y <- format.data.frame(x, ...)
    ## For now, simply don't show the encoding information.
    ## Alternatively:
    ##   enc <- character(length(x$enc))
    ##   ind <- vapply(x$enc, is.null, FALSE)
    ##   enc[!ind] <- sapply(x$enc[!ind], format)
    ##   y$enc <- enc
    y$enc <- NULL
    y
}

print.pdf_fonts <-
function(x, ...)
{
    print.data.frame(format(x, ...))
    invisible(x)
}

## * pdf_info

pdf_info <-
function(file)
{
    doc <- if(inherits(file, "pdf_doc"))
        file
    else
        pdf_doc(file, cache = FALSE)

    if(is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    ref <- doc$trailer[["Info"]]
    if(is.null(ref)) {
        info <- list()
    } else {
        info <- unclass(pdf_doc_get_object(doc, ref, con))
        ## Be nice (the PDF Reference does not explicitly say that
        ## values in the Document Information Dictionary may be indirect
        ## references):
        ind <- as.logical(sapply(info, inherits,
                                 "PDF_Indirect_Reference"))
        if(any(ind))
            info[ind] <-
                lapply(info[ind],
                       function(ref) pdf_doc_get_object(doc, ref, con))

        ## Transform text strings.
        ## See PDF Reference version 1.7 section 10.2.1:
        ##   The value associated with any key not specifically
        ##   mentioned in Table 10.2 must be a text string.
        ## If they are not PDF strings, drop them.
        keys <- c("CreationDate", "ModDate", "Trapped")
        pos <- which(is.na(match(names(info), keys)))
        ind <- as.logical(sapply(info[pos], inherits, "PDF_String"))
        info[pos[ind]] <-
            lapply(info[pos[ind]], PDF_Text_String_to_character)
        info[pos[!ind]] <- NULL

        ## Transform trapping information.
        ## Could provide default value "unknown".

        ## Transform dates to POSIXt if possible.
        if(!is.null(dt <- info[["CreationDate"]]))
            info[["CreationDate"]] <- PDF_Date_to_POSIXt(dt)
        if(!is.null(dt <- info[["ModDate"]]))
            info[["ModDate"]] <- PDF_Date_to_POSIXt(dt)
        ## Alternatively, use
        ##   keys <- c("CreationDate", "ModDate")
        ##   ind <- !is.na(match(names(info), keys))
        ##   info[ind] <- lapply(info[ind], PDF_Date_to_POSIXt)
    }

    pages <- pdf_doc_get_page_list(doc, con)
    info[["Pages"]] <- length(pages)

    rectangles <- lapply(pages, `[[`, "MediaBox")
    urx <- unlist(lapply(rectangles, `[[`, 3L))
    ury <- unlist(lapply(rectangles, `[[`, 4L))
    if((length(uurx <- unique(urx)) == 1L) &&
       (length(uury <- unique(ury)) == 1L)) {
        info["Page size"] <- sprintf("%s x %s pts", uurx, uury)
        pos <- which((abs(pdf_page_sizes[, 1L] - uurx) < 1) &
                     (abs(pdf_page_sizes[, 2L] - uury) < 1))
        if(!length(pos)) {
            pos <- which((abs(pdf_page_sizes[, 2L] - uurx) < 1) &
                         (abs(pdf_page_sizes[, 1L] - uury) < 1))
        }
        if(length(pos))
            info["Page size"] <-
                sprintf("%s [%s]",
                        info["Page size"],
                        rownames(pdf_page_sizes)[pos])
    }

    info[["File size"]] <- sprintf("%d bytes", doc$size)

    version <- substring(doc$header, 6L)
    catalog <- pdf_doc_get_object(doc, doc$trailer[["Root"]], con)
    version_in_catalog <- catalog[["Version"]]
    if(!is.null(version_in_catalog)) {
        ## Use version in catalog only if given (as a name) and later
        ## than the version specified in the header.
        if(as.numeric_version(version_in_catalog) > version)
            version <- version_in_catalog
    }
    info[["PDF version"]] <- version

    class(info) <- "pdf_info"
    info
}

format.pdf_info <-
function(x, ...)
{
    formatDL(sprintf("%s:", names(x)),
             sapply(x, format),
             ...)
}

## * Object readers

pdf_read_object <-
function(con, doc = NULL)
{
    if(pdftools_debug_level() > 0L) {
        bytes <- .con_read_bytes(con, 10L)
        message(sprintf("looking at %s", deparse(intToUtf8(bytes))))
        .con_seek(con, -length(bytes), 2L)
    }

    x <- read_next_non_whitespace(con)
    if(!length(x)) return(NA)
    .con_seek(con, -1L, 2L)
    if(x == 0x28)                       # charToRaw("(") => 28
        pdf_read_object_string_literal(con)
    else if(x == 0x2f)                  # charToRaw("/") => 2f
        pdf_read_object_name(con)
    else if(x == 0x5b)                  # charToRaw("[") => 5b
        pdf_read_object_array(con)
    ## <NOTE>
    ## Handled by pdf_read_object_keyword() now.
    ##   else if(x == "n")
    ##     pdf_read_object_null(con)
    ##   else if(x %in% c("t", "f"))
    ##     pdf_read_object_boolean(con)
    ## </NOTE>
    else if(x == 0x3c) {                # charToRaw("<") => 3c
        ## Hexadecimal string or dictionary
        bytes <- .con_read_bytes(con, 2L)
        .con_seek(con, -2L, 2L)
        if(all(bytes == c(0x3c, 0x3c)))
            pdf_read_object_dictionary_or_stream(con, doc)
        else
            pdf_read_object_string_hexadecimal(con)
    }
    else if(x == 0x25) {                # charToRaw("%") => 25
        ## Read until eol.
        repeat {
            x <- .con_read_bytes(con, 1L)
            if(x %.IN.% pdf_bytes_eols) break
        }
        read_next_non_whitespace_and_seek_back(con)
        pdf_read_object(con)
    }
    else if(x %.IN.% pdf_bytes_in_numerics_not_digits)
        pdf_read_object_numeric(con)
    else if(x %.IN.% pdf_bytes_digits) {
        ## Could be a number object or an indirect object reference.
        bytes <- .con_read_bytes(con, 20L)
        .con_seek(con, - length(bytes), 2L)
        ## Cannot simply call rawToChar(bytes) as we might have read nul
        ## bytes.
        if(length(pos <- which(bytes == 0))) {
            bytes <- bytes[seq_len(pos[1L] - 1L)]
        }
        if(grepl("^[[:digit:]]+\\s[[:digit:]]+\\sR[^[:alpha:]]",
                 rawToChar(bytes),
                 useBytes = TRUE))
            pdf_read_object_indirect_reference(con)
        else
            pdf_read_object_numeric(con)
    }
    else {
        ## A keyword, hopefully.
        pdf_read_object_keyword(con)
    }
}

## <NOTE>
## Handled by pdf_read_object_keyword() now.
##
## pdf_read_object_boolean <-
## function(con)
## {
##     x <- rawToChar(.con_read_bytes(con, 1L))
##     if(x == "t") {
##         bytes <- .con_read_bytes(con, 3L)
##         if(rawToChar(bytes) == "rue")
##             return(TRUE)
##     } else if(x == "f") {
##         bytes <- .con_read_bytes(con, 4L)
##         if(rawToChar(bytes) == "alse")
##             return(FALSE)
##     }
##     stop("cannot read boolean object")
## }
##
## </NOTE>

pdf_read_object_numeric <-
function(con)
{
    table <- pdf_bytes_in_numerics

    bytes <- raw()
    while((x <- .con_read_bytes(con, 1L)) %.IN.% table) {
        bytes <- c(bytes, x)
    }
    .con_seek(con, -1L, 2L)

    s <- rawToChar(bytes)
    if(grepl(".", s, fixed = TRUE))
        as.numeric(s)
    else
        as.integer(s)
}

## PDF string objects.

## See PDF Reference version 1.7 section 3.2.3:
## A PDF string simply is a sequence of bytes.
## As this may contain nuls, we cannot unconditionally represent these
## as R character strings (which must not have embedded nulls, even with
## a "bytes" encoding), and conditionalizing the representation seems
## rather awkward.
## Hence, we represent PDF strings as a byte (raw) vector of class
## "PDF_String".
## See PDF Reference version 1.7 section 3.8.1:
## PDF has the notion of a "text string" type
##   used for human-readable characters, such as text annotations,
##   bookmark names, article names, and document information.  These
##   strings are encoded using either PDFDocEncoding or UTF-16BE with a
##   leading byte-order marker.
## with note:
##   This is not a true data type, but a string type that represents
##   data encoded using specific conventions.
## As the context indicates when PDF strings are to be taken as PDF text
## strings (e.g. for the title or author specification in the Document
## Information Dictionary, see PDF Reference version 1.7 section 10.2.1),
## we do not use a subclass of "PDF_String" for text strings: instead,
## we use
##   PDF_Text_String_to_character()
## to convert the byte vector to an R character string encoded in
## UTF-8 when the text string context applies.
## (Maybe PDF_Text_String_to_UTF8() would be a better name?)

pdf_read_object_string_literal <-
function(con)
{
    x <- .con_read_bytes(con, 1L)
    lparen <- charToRaw("(")
    if(x != lparen)
        stop("cannot read literal string object")

    rparen <- charToRaw(")")
    escape <- charToRaw("\\")
    pdf_bytes_escape_tails <- charToRaw("nrtbf()\\")
    pdf_bytes_escape_bytes <- charToRaw("\n\r\t\b\f()\\")
    names(pdf_bytes_escape_bytes) <-
        as.character(pdf_bytes_escape_tails)

    bytes <- raw()
    parens <- 1L
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(!length(x)) break
        if(x == lparen) {
            parens <- parens + 1L
        } else if(x == rparen) {
            parens <- parens - 1L
            if(!parens) break
        } else if(x == escape) {
            x <- .con_read_bytes(con, 1L)
            if(x %.IN.% pdf_bytes_digits) {
                i <- 0L
                while(i < 2L) {
                    y <- .con_read_bytes(con, 1L)
                    if(!(y %.IN.% pdf_bytes_digits)) {
                        .con_seek(con, -1L, 2L)
                        break
                    }
                    x <- c(x, y)
                    i <- i + 1L
                }
                x <- as.raw(strtoi(rawToChar(x), 8L))
            } else if(x %.IN.% pdf_bytes_escape_tails) {
                x <- pdf_bytes_escape_bytes[as.character(x)]
            } else if(x %.IN.% pdf_bytes_eols) {
                x <- .con_read_bytes(con, 1L)
                if(!(x %.IN.% pdf_bytes_eols))
                    .con_seek(con, -1L, 2L)
                x <- raw()
            }
            ## See PDF Reference version 1.7 section 3.2.3.
            ## If the character following the backslash is not not a
            ## special character for an escape sequence, the backslash
            ## is ignored.
        }
        bytes <- c(bytes, x)
    }
    class(bytes) <- "PDF_String"
    bytes
}

pdf_read_object_string_hexadecimal <-
function(con)
{
    x <- .con_read_bytes(con, 1L)
    if(x != 0x3c)                       # charToRaw("<") => 3c
        stop("cannot read hexadecimal string object")

    end <- charToRaw(">")

    ## See PDF Reference version 1.7 section 3.2.3:
    ## Each pair of hexadecimal digits defines one byte of the string.
    ## White-space characters are ignored.
    ## If the final digit of a hexadecimal string is missing (i.e., if
    ## there is an odd number of digits) it is assumed to be 0.
    bytes <- raw()
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(x == end) break
        if(!(x %.IN.% pdf_bytes_whitespaces))
            bytes <- c(bytes, x)
    }

    if(length(bytes) %% 2)
        bytes <- c(bytes, charToRaw("0"))
    n <- length(bytes) %/% 2
    s <- substring(rawToChar(bytes),
                   seq(1L, by = 2L, length.out = n),
                   seq(2L, by = 2L, length.out = n))
    bytes <- as.raw(strtoi(s, 16L))
    class(bytes) <- "PDF_String"
    bytes
}

format.PDF_String <-
function(x, ...)
{
    sprintf("PDF_String(<%s>)", paste(as.character(x), collapse = ""))
}

## PDF name objects.

pdf_read_object_name <-
function(con)
{
    ## See PDF Reference version 1.7 section 3.2.4:
    ##   A slash character (/) introduces a name.  The slash is not part
    ##   of the name but is a prefix indicating that the following
    ##   sequence of characters constitutes a name.
    ##
    ##   The name may include any regular characters, but not delimiter
    ##   or white-space characters.
    ##
    ##   Beginning with PDF 1.2, any character except null (character
    ##   code 0) may be included in a name by writing its 2-digit
    ##   hexadecimal code, preceded by the number sign character (#).

    x <- .con_read_bytes(con, 1L)
    if(x != 0x2f)                       # charToRaw("/") => 2f
        stop("cannot read name object")

    bytes <- raw()
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(!length(x) || (x %.IN.% pdf_bytes_non_regulars)) {
            .con_seek(con, -1L, 2L)
            break
        }
        bytes <- c(bytes, x)
    }

    ## Cf. also URLdecode().
    pos <- which(bytes == 0x23)         # charToRaw("#") => 23
    if(length(pos)) {
        hex <- sapply(pos,
                      function(p) rawToChar(bytes[p + (1L : 2L)]))
        bytes[pos] <- as.raw(strtoi(hex, 16L))
        bytes <- bytes[- c(pos + 1L, pos + 2L)]
    }
    ## Note that we currently leave the leading slash as part of the
    ## name.
    s <- rawToChar(bytes)
    class(s) <- "PDF_Name"
    s
}

print.PDF_Name <-
function(x, ...)
{
    print(noquote(unclass(x)), ...)
    invisible(x)
}

pdf_read_object_array <-
function(con)
{
    x <- .con_read_bytes(con, 1L)
    if(x != 0x5b)                       # charToRaw("[") => 5b
        stop("cannot read array object")

    end <- charToRaw("]")

    y <- list()
    repeat {
        x <- read_next_non_whitespace_and_seek_back(con)
        if(x == end) {
            .con_read_bytes(con, 1L)
            break
        }
        y <- c(y, list(pdf_read_object(con)))
    }
    class(y) <- "PDF_Array"
    y
}

format.PDF_Array <-
function(x, ...)
{
    sprintf("PDF_Array(%d)", length(x))
}

pdf_read_object_dictionary_or_stream <-
function(con, doc = NULL)
{
    bytes <- .con_read_bytes(con, 2L)
    if(!all(bytes == c(0x3c, 0x3c)))
        stop("cannot read dictionary object")

    end <- charToRaw(">")

    y <- list()
    repeat {
        x <- read_next_non_whitespace_and_seek_back(con)
        if(x == end) {
            .con_read_bytes(con, 2L)
            break
        }
        key <- pdf_read_object(con)
        read_next_non_whitespace_and_seek_back(con)
        val <- pdf_read_object(con)
        y[key] <- list(val)
    }
    pos <- .con_seek(con)
    ## Check whether this is in fact a stream object.
    ## Read ahead.
    read_next_non_whitespace_and_seek_back(con)
    if(rawToChar(.con_read_bytes(con, 6L)) == "stream") {
        ## Argh.  Handle the EOL marker assuming compliance: should
        ## check for this.
        eol <- .con_read_bytes(con, 1L)
        if(!(eol %.IN.% pdf_bytes_eols))
            stop("cannot read stream object")
        if(eol == charToRaw("\r"))
            .con_read_bytes(con, 1L)
        ## Need length information in dictionary.
        len <- y[["Length"]]
        if(is.null(len))
            stop("cannot read stream object")
        ## However, the length could be an indirect object reference.
        ## In this case we can only resolve the length for a non-NULL
        ## doc with the cross-reference needed.  Otherwise, we record
        ## the stream data start, and defer reading the bytes.
        if((ref <- inherits(len, "PDF_Indirect_Reference")) &&
           is.null(doc)) {
            y[["__stream_start__"]] <- .con_seek(con)
            y[["__stream_bytes__"]] <- NULL
        } else {
            if(ref) {
                tell <- .con_seek(con)
                len <- pdf_doc_get_object(doc, len, con)
                .con_seek(con, tell)
            }
            y[["__stream_bytes__"]] <- .con_read_bytes(con, len)
            ## Now check if we really hit the end of the stream.
            read_next_non_whitespace_and_seek_back(con)
            bytes <- .con_read_bytes(con, 9L)
            if(rawToChar(bytes) != "endstream")
                stop("cannot read stream object")
        }
        class(y) <- "PDF_Stream"
    } else {
        .con_seek(con, pos)
        class(y) <- "PDF_Dictionary"
    }
    y
}

format.PDF_Dictionary <-
function(x, ...)
{
    sprintf("PDF_Dictionary(<<%s>>)",
            paste(names(x), collapse = ","))
}

format.PDF_Stream <-
function(x, ...)
{
    sprintf("PDF_Stream(<<%s>>)",
            paste(names(x), collapse = ","))
}

## Experimental summary methods.
## Cannot easily make this the print method, because PDF dictionary
## and stream objects can be recursive ...

summary.PDF_Dictionary <-
function(object, ...)
    writeLines(sprintf("%s: %s", names(object), sapply(object, format)))

summary.PDF_Stream <-
function(object, ...)
{
    if(!is.null(bytes <- object[["__stream_bytes__"]]))
        object[["__stream_bytes__"]] <-
            sprintf("%d bytes", length(bytes))
    writeLines(sprintf("%s: %s", names(object), sapply(object, format)))
}

## <NOTE>
## Handled by pdf_read_object_keyword() now.
##
## pdf_read_object_null <-
## function(con)
## {
##     bytes <- .con_read_bytes(con, 4L)
##     if(rawToChar(bytes) != "null")
##         stop("cannot read null object")
##     NULL
## }
##
## </NOTE>

pdf_read_object_indirect_reference <-
function(con)
{
    num <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    gen <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    x <- .con_read_bytes(con, 1L)
    if(x != 0x52)                       # charToRaw("R") => 52
        stop("cannot read indirect reference object")
    y <- c(num = as.integer(rawToChar(num)),
           gen = as.integer(rawToChar(gen)))
    class(y) <- "PDF_Indirect_Reference"
    y
}

format.PDF_Indirect_Reference <-
function(x, ...)
{
    sprintf("PDF_Indirect_Reference(%d,%d)", x["num"], x["gen"])
}

pdf_dereference_maybe <-
function(obj, doc, con = NULL)
{
    if(inherits(obj, "PDF_Indirect_Reference"))
        obj <- pdf_doc_get_object(doc, obj, con)
    obj
}

pdf_read_object_keyword <-
function(con)
{
    bytes <- raw()
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(!length(x) || !(x %.IN.% pdf_bytes_in_keywords)) {
            .con_seek(con, -1L, 2L)
            break
        }
        bytes <- c(bytes, x)
    }

    s <- rawToChar(bytes)
    if(s == "null")
        NULL
    else if(s == "true")
        TRUE
    else if(s == "false")
        FALSE
    else {
        class(s) <- "PDF_Keyword"
        s
    }
}

print.PDF_Keyword <-
function(x, ...)
{
    print(noquote(unclass(x)), ...)
    invisible(x)
}

pdf_read_object_header <-
function(con)
{
    ## Read num and gen.
    read_next_non_whitespace_and_seek_back(con)
    num <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    gen <- read_next_bytes_until_whitespace(con)
    ## Now skip the "obj".
    .con_read_bytes(con, 3L)
    read_next_non_whitespace_and_seek_back(con)
    c(num = suppressWarnings(as.integer(rawToChar(num))),
      gen = suppressWarnings(as.integer(rawToChar(gen))))
}

pdf_read_indirect_object_at_pos <-
function(con, pos, num = NA_integer_, gen = NA_integer_, doc = NULL)
{
    ## Move to pos.
    .con_seek(con, pos)
    ## Read header first.
    hdr <- pdf_read_object_header(con)
    ## Be paranoid.
    if(anyNA(hdr))
        stop(gettextf("cannot find object header at xrefed position %d",
                      pos),
             domain = NA)
    ## Apparently it is feasible to have cross-references to indirect
    ## objects with actually different object and/or generation numbers:
    ## as of 2011-09-27, grImport/inst/doc/Rnewspage27.pdf had both
    ## objects 69 and 70 point to the same offset [providing object 70].
    ## For now, give a message and proceed.
    if(!is.na(num) && (num != hdr["num"]))
        message(gettextf("mismatch in object numbers (given: %d, found: %d)",
                         num, hdr["num"]),
                domain = NA)
    if(!is.na(gen) && (gen != hdr["gen"]))
        message(gettextf("mismatch in generation numbers (given: %d, found: %d)",
                         gen, hdr["gen"]),
                domain = NA)
    ## Read object.
    pdf_read_object(con, doc)
}

pdf_read_stream_bytes <-
function(con, obj, doc = NULL)
{
    len <- obj[["Length"]]
    if(inherits(len, "PDF_Indirect_Reference")) {
        len <- pdf_doc_get_object(doc, len, con)
    }
    pos <- obj[["__stream_start__"]]
    .con_seek(con, pos)
    bytes <- .con_read_bytes(con, len)
    ## Check if we really hit the end of the stream.
    read_next_non_whitespace_and_seek_back(con)
    if(rawToChar(.con_read_bytes(con, 9L)) != "endstream")
        stop("cannot read stream data")
    bytes
}

pdf_doc_get_object <-
function(doc, ref, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    if(is.character(ref)) {
        ref <- as.integer(unlist(strsplit(ref, ".", fixed = TRUE)))
    }

    if(length(ref) == 1L) {
        names(ref) <- "num"
    } else if(length(ref) == 2L) {
        names(ref) <- c("num", "gen")
    }
    num <- ref["num"]
    gen <- ref["gen"]

    ## First look in the object cache.
    if(doc$cache) {
        pos <- match(.ref_to_name(ref), names(doc$objects))
        if(!is.na(pos)) return(doc$objects[[pos]])
    }

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    ## Next look in the xrefs for object streams.
    if((is.na(gen) || (gen == 0L)) &&
       (length(pos <- which(doc$xref_objs[, "num"] == num)))) {
        if(length(pos) > 1L) {
            ## Can this really happen?
            pos <- pos[1L]
        }
        ptr <- doc$xref_objs[pos, ]
        idx <- ptr["idx"]
        obj <- pdf_doc_get_object(doc, ptr["str"], con)
        ## Could check whether
        ##   obj[["Type"]] == "ObjStm"
        n <- obj[["N"]]
        if(idx >= n)
            stop("invalid index in object stream lookup")
        first <- obj[["First"]]
        stream <- rawConnection(PDF_Stream_get_data(obj, doc))
        on.exit(close(stream), add = TRUE)
        i <- 0L
        if(doc$cache) {
            while(i < n) {
                cnum <- pdf_read_object(stream)
                read_next_non_whitespace_and_seek_back(stream)
                cpos <- pdf_read_object(stream)
                read_next_non_whitespace_and_seek_back(stream)
                tell <- .con_seek(stream, first + cpos)
                obj <- pdf_read_object(stream, doc)
                key <- .ref_to_name(cnum)
                doc$objects[key] <- list(obj)
                .con_seek(stream, tell)
                i <- i + 1L
            }
            return(doc$objects[[.ref_to_name(num)]])
        } else {
            while(i <= idx) {
                cnum <- pdf_read_object(stream)
                read_next_non_whitespace_and_seek_back(stream)
                cpos <- pdf_read_object(stream)
                read_next_non_whitespace_and_seek_back(stream)
                i <- i + 1L
            }
            .con_seek(stream, obj[["First"]] + cpos)
            return(pdf_read_object(stream, doc))
        }
    }

    ## Figure out the position to start from.
    if(length(ref) == 1L) {
	pos <- which.max(doc$xref_tabs[, "num"] == ref)
        gen <- doc$xref_tabs[pos, "gen"]
        pos <- doc$xref_tabs[pos, "pos"]
    }
    else {
        ind <- ((doc$xref_tabs[, "num"] == num) &
                (doc$xref_tabs[, "gen"] == gen))
        pos <- doc$xref_tabs[ind, "pos"]
    }

    obj <- pdf_read_indirect_object_at_pos(con, pos, num, gen, doc)
    if(doc$cache)
        doc$objects[.ref_to_name(c(num, gen))] <- list(obj)
    obj
}

pdf_doc_get_objects <-
function(doc, ids = NULL, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    ## Start with the object cache.
    objects <- doc$objects

    ## If this contains as many objects as there are names, all objects
    ## have already been cached.
    if(length(objects) == doc$length) {
        if(!length(ids))
            return(objects)
        else
            return(objects[ids])
    }

    ## Otherwise, we need to get the objects not yet in the cache (which
    ## could be all objects if caching is off, of course).

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    debug <- (pdftools_debug_level() > 0L)

    ## First get the objects from the old-style xref tables.
    tab <- doc$xref_tabs
    str <- .ref_to_name(doc$xref_objs[, "str"], "0")
    ## Determine the active objects needed.
    tab <- tab[((tab[, "pos"] > 0L) & (tab[, "use"] > 0L)), ,
               drop = FALSE]
    ## If ids is NULL (so that we are getting all active objects), we
    ## need those active objects not yet in the cache.
    ind <- is.na(match(.ref_to_name(tab[, c("num", "gen"),
                                        drop = FALSE]),
                       names(objects)))
    ## Otherwise, we only need those active objects not yet in the cache
    ## which are directly matched by ids, or which contain streams with
    ## objects matched by ids.
    if(length(ids)) {
        nms <- .ref_to_name(tab[ind, c("num", "gen"), drop = FALSE])
        wanted_by_direct_match <- !is.na(match(nms, ids))
        if(!length(str)) {
            ind <- ind & wanted_by_direct_match
        } else {
            pos <- match(ids,
                         .ref_to_name(doc$xref_objs[, "num"], "0"),
                         nomatch = 0L)
            str <- str[pos]
            wanted_by_stream_match <- !is.na(match(nms, unique(str)))
            ind <- ind & (wanted_by_direct_match | wanted_by_stream_match)
        }
    }

    for(i in which(ind)) {
        entry <- tab[i, ]
        if(debug)
            message(sprintf("processing %s",
                            paste(names(entry), entry, collapse = " ")))
        pos <- entry["pos"]
        num <- entry["num"]
        gen <- entry["gen"]
        obj <- pdf_read_indirect_object_at_pos(con, pos, num, gen, doc)
        key <- .ref_to_name(c(num, gen))
        if(doc$cache)
            doc$objects[key] <- list(obj)
        objects[key] <- list(obj)
    }
    ## Now for the new-style xref streams objects.
    for(s in unique(str)) {
        obj <- objects[[s]]
        n <- obj[["N"]]
        first <- obj[["First"]]
        stream <- rawConnection(PDF_Stream_get_data(obj, doc))
        tab <- matrix(0, n, 2L)
        ## First read the object numbers and byte offsets.
        i <- 1L
        while(i <= n) {
            tab[i, 1L] <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            tab[i, 2L] <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            i <- i + 1L
        }
        ## Determine the objects still needed.
        pos <- which(is.na(match(.ref_to_name(tab[, 1L], 0),
                                 names(objects))))
        if(length(ids)) {
            pos <- pos[!is.na(match(.ref_to_name(tab[pos, 1L], 0), ids))]
        }
        ## Then read the objects from the stream.
        for(i in pos) {
            .con_seek(stream, first + tab[i, 2L])
            obj <- pdf_read_object(stream, doc)
            key <- .ref_to_name(tab[i, 1L])
            if(doc$cache)
                doc$objects[key] <- list(obj)
            objects[key] <- list(obj)
        }
        close(stream)
    }

    if(length(ids))
        objects <- objects[ids]

    objects
}

## * pdf_doc_get_page_tree

pdf_doc_get_page_tree <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    debug <- (pdftools_debug_level() > 0L)

    catalog <- pdf_doc_get_object(doc, doc$trailer[["Root"]], con)
    ## Pages entry in the catalog dictionary is required and must be an
    ## indirect reference.
    pages <- pdf_doc_get_object(doc, catalog[["Pages"]], con)
    recurse <- function(x) {
        if(!is.null(kids <- x[["Kids"]])) {
            x[["Kids"]] <-
                lapply(kids,
                       function(kid)
                       if(inherits(kid, "PDF_Indirect_Reference")) {
                           if(debug) {
                               message(sprintf("expanding %s",
                                               format(kid)))
                           }
                           recurse(pdf_doc_get_object(doc, kid, con))
                       } else {
                           kid
                       })
            class(x) <- "PDF_Page_Tree"
        } else {
            ## No more kids, should be a leaf node.
            ## Could check on Type being Page.
            class(x) <- "PDF_Page"
        }
        x
    }
    recurse(pages)
}

pdf_doc_get_page_list <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    ## See PDF Reference version 1.7 section 3.6.2.
    ## Page objects can inherit
    ##   Resources MediaBox CropBox Rotate
    ## from its ancestors.
    ## Hence, we flattening out the tree to the list of pages (leaf
    ## nodes of the page tree), propagate these entries.
    inherited_entry_names <-
        c("Resources", "MediaBox", "CropBox", "Rotate")

    pages <- list()
    ## Cannot use rapply() because this only deals with nodes which are
    ## not lists.
    recurse <- function(x) {
        if(!is.null(kids <- x[["Kids"]])) {
            entries <- x[inherited_entry_names]
            for(kid in kids) {
                cls <- class(kid)
                kid <- c(kid, entries[is.na(match(names(entries),
                                                  names(kids)))])
                class(kid) <- cls
                if(inherits(kid, "PDF_Page")) {
                    pages <<- c(pages, list(kid))
                }
                recurse(kid)
            }
        }
    }
    recurse(pdf_doc_get_page_tree(doc, con))
    pages
}

## * pdf_doc_get_content_streams

## <FIXME>
## Most likely this should only extract the Contents entries and let the
## "consumers" do the expansions of indirect object references as needed.
## </FIXME>

pdf_doc_get_page_content_streams <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    pages <- pdf_doc_get_page_list(doc, con)
    ## See PDF Reference version 1.7 section 3.6.2.
    ## A page object may have a Contents entry with value a single
    ## content stream or an array of such streams.
    ## A missing Contents entry means that the page is empty.
    lapply(pages,
           function(p) {
               obj <- p[["Contents"]]
               if(inherits(obj, "PDF_Array"))
                   lapply(obj, pdf_dereference_maybe, doc, con)
               else
                   pdf_dereference_maybe(obj, doc, con)
           })
}

## <FIXME>
## Most likely this should only extract the Resources entries and let the
## "consumers" do the expansions of indirect object references as needed
## (as well as handle inheritance from ancestors).
## </FIXME>

pdf_doc_get_page_resources <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")

    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    pages <- pdf_doc_get_page_list(doc, con)

    ## See PDF Reference version 1.7 section 3.6.2.
    ## A page object may have a Resources entry giving a dictionary
    ## (which apparently could be an indirect object reference).
    ## An empty dictionary means no resources.
    ## A missing Resources entry means that resources are inherited from
    ## an ancestor node in the page tree.
    lapply(pages,
           function(p) {
               pdf_dereference_maybe(p[["Resources"]], doc, con)
           })
}

## * Streams

PDF_Stream_get_data <-
function(obj, doc = NULL)
{
    bytes <- obj[["__stream_bytes__"]]

    if(is.null(bytes)) {
        if(is.null(doc))
            stop("cannot read stream data")
        con <- doc$con
        if(is.null(con)) {
            con <- file(doc$file, "rb")
            on.exit(close(con))
        }
        bytes <- pdf_read_stream_bytes(con, obj, doc)
    }

    filters <- as.list(obj[["Filter"]])
    ## Handle DecodeParms.
    ## The PDF specs say that if there is a single filter, DecodeParms
    ## can be a dictionary object with the parameters, but need not be
    ## given if the defaults are to be used.  If there are multiple
    ## filters and any filter has non-default parameters, DecodeParms
    ## must be an array with one entry for each filter: either a
    ## dictionary with the parameters or the null object.
    parameters <- obj[["DecodeParms"]]
    if(is.null(parameters))
        parameters <- rep.int(list(NULL), length(filters))
    else if(inherits(parameters, "PDF_Dictionary"))
        parameters <- list(parameters)
    for(i in seq_along(filters)) {
        filter <- filters[[i]]
        if(filter == "FlateDecode")
            bytes <- pdf_filter_flate_decode(bytes, parameters[[i]])
        else
            stop(gettextf("unsupported filter %s",
                          sQuote(filter)),
                 domain = NA)
    }
    bytes
}

## * Filters

pdf_filter_flate_decode <-
function(x, params)
{
    ## Need to decompress first
    m <- memDecompress(x, "gzip")
    predictor <- params[["Predictor"]]
    if(is.null(predictor) || (predictor == 1L))
        return(m)
    if((predictor < 10L) && (predictor > 15L)) {
        stop(gettextf("unsupported %s predictor %d",
                      "flatedecode",
                      predictor),
             domain = NA)
    }
    columns <- params[["Columns"]]
    bytes <- raw()
    rowlength <- columns + 1L
    prev_rowdata <- integer(rowlength)
    for(row in seq(0L, length.out = length(m) / rowlength)) {
        ## <FIXME>
        ## Use a rawConnection() instead.
        rowdata <- as.integer(m[seq(row * rowlength + 1L,
                                    (row + 1L) * rowlength)])
        ## </FIXME>
        fb <- rowdata[1L]
        if(fb == 1L) {
            for(i in seq(3L, rowlength))
                rowdata[i] <- (rowdata[i] + rowdata[i - 1L]) %% 256
        } else if(fb == 2L) {
            for(i in seq(2L, rowlength))
                rowdata[i] <- (rowdata[i] + prev_rowdata[i]) %% 256
        } else if(fb != 0L) {
            stop(gettextf("unsupported PNG filter %d", fb),
                 domain = NA)
        }
        prev_rowdata <- rowdata
        bytes <- c(bytes, as.raw(rowdata[-1L]))
    }
    bytes
}

## * Content streams

pdf_content_stream_read_objects <-
function(con, doc = NULL)
{
    objects <- list()
    while(!identical(obj <- pdf_read_object(con, doc), NA))
        objects <- c(objects, list(obj))
    objects
}

## * Common data structures

## ** PDF text strings

PDF_Text_String_to_character <-
function(bytes)
{
    ## PDF text strings can be encoded in PDFDocEncoding ot UTF-16BE.
    if(identical(bytes[c(1L, 2L)], as.raw(c(0xfe, 0xff)))) {
        ## If the first two bytes represent the Unicode byte-order marker
        ## U+FEFF, this is a text string encoded in UTF-16BE.
        bytes <- bytes[-c(1L, 2L)]
        ## <NOTE>
        ##   This can contain the language encoding as
        ##     U+001B (i.e., as.raw(c(0x00, 0x1b)))
        ##     2-byte ISO-639 language code
        ##     2-byte ISO-639 language code [optional]
        ##     U+001B
        ## </NOTE>
        language <- NULL
        pos <- which(bytes == 0x00)
        if(length(pos)) {
            pos <- pos[bytes[pos + 1L] == 0x1b]
            if(length(pos) == 2L) {
                ini <- pos[1L]
                len <- pos[2L] - ini
                if(len == 4L) {
                    language <- rawToChar(bytes[c(ini + 2L, ini + 3L)])
                    bytes <- bytes[- (ini + (0L : 5L))]
                } else if(len == 6L) {
                    ## Use IETF language tag format.
                    language <-
                        paste(rawToChar(bytes[c(ini + 2L, ini + 3L)]),
                              rawToChar(bytes[c(ini + 4L, ini + 5L)]),
                              sep = "-")
                    bytes <- bytes[- (ini + (0L : 7L))]
                }
            }
        }
        s <- intToUtf8(bytes)
        if(!is.null(language))
            attr(s, "Language") <- language
        s
    } else {
        intToUtf8(PDFDocEncoding[as.character(bytes)])
    }
}

## <FIXME>
## Integrate into tools::charset_to_Unicode eventually.
PDFDocEncoding <-
    c(0x0000, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
      0xfffd, 0x0009, 0x000a, 0xfffd, 0x000c, 0x000d, 0xfffd, 0xfffd,
      0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
      0x02d8, 0x02c7, 0x02c6, 0x02d9, 0x02dd, 0x02db, 0x02da, 0x02dc,
      0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
      0x0028, 0x0029, 0x002a, 0x002b, 0x002c, 0x002d, 0x002e, 0x002f,
      0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
      0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f,
      0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
      0x0048, 0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f,
      0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
      0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f,
      0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,
      0x0068, 0x0069, 0x006a, 0x006b, 0x006c, 0x006d, 0x006e, 0x006f,
      0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,
      0x0078, 0x0079, 0x007a, 0x007b, 0x007c, 0x007d, 0x007e, 0xfffd,
      0x2022, 0x2020, 0x2021, 0x2026, 0x2014, 0x2013, 0x0192, 0x2044,
      0x2039, 0x203a, 0x2212, 0x2030, 0x201e, 0x201c, 0x201d, 0x2018,
      0x2019, 0x201a, 0x2122, 0xfb01, 0xfb02, 0x0141, 0x0152, 0x0160,
      0x0178, 0x017d, 0x0131, 0x0142, 0x0153, 0x0161, 0x017e, 0xfffd,
      0x20ac, 0x00a1, 0x00a2, 0x00a3, 0x00a4, 0x00a5, 0x00a6, 0x00a7,
      0x00a8, 0x00a9, 0x00aa, 0x00ab, 0x00ac, 0xfffd, 0x00ae, 0x00af,
      0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x00b4, 0x00b5, 0x00b6, 0x00b7,
      0x00b8, 0x00b9, 0x00ba, 0x00bb, 0x00bc, 0x00bd, 0x00be, 0x00bf,
      0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x00c7,
      0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf,
      0x00d0, 0x00d1, 0x00d2, 0x00d3, 0x00d4, 0x00d5, 0x00d6, 0x00d7,
      0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x00de, 0x00df,
      0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7,
      0x00e8, 0x00e9, 0x00ea, 0x00eb, 0x00ec, 0x00ed, 0x00ee, 0x00ef,
      0x00f0, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7,
      0x00f8, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x00fd, 0x00fe, 0x00ff)
names(PDFDocEncoding) <- format.hexmode(0 : 255)
## </FIXME>

## ** PDF dates

## See PDF Reference version 1.7 section 3.8.3.
## Dates are of the form
##   (D:YYYYMMDDHHmmSSOHH'mm')
## where
## * YYYY MM DD HH mm SS have the usual meanings
## * O is the relationship of local time to Universal Time (UT),
##   denoted by one of the characters +, -, or Z: a plus sign (+) as
##   the value of the O field signifies that local time is later
##   than UT, a minus sign (-) signifies that local time is earlier
##   than UT, and the letter Z signifies that local time is UT.
## * the apostrophe character after HH and mm is part of the syntax
## * all fields after the year are optional
## * the prefix 'D:', although also optional, is strongly recommended
## * the default values for MM and DD are both 01
## * all other numerical fields default to zero values.
PDF_Date_to_POSIXt <-
function(bytes)
{
    ## Strip optional 'D:' prefix.
    s <- sub("^D:", "", rawToChar(bytes))
    ## Strip apostrophes in offset spec.
    s <- gsub("'", "", s)
    if(nchar(s) <= 14L) {
        s <- sprintf("%s%s", s,
                     substring("    0101000000", nchar(s) + 1L, 14L))
        strptime(s, "%Y%m%d%H%M%S")
    } else if(substring(s, 15L, 15L) == "Z") {
        strptime(substring(s, 1L, 14L), "%Y%m%d%H%M%S")
    } else {
        strptime(s, "%Y%m%d%H%M%S%z")
    }
}

## * Utilities

.match_ids_to_pdf_doc_names <-
function(ids, doc)
{
    ids <- as.character(ids)
    ## Exact matches first.
    pos <- match(ids, doc$names)
    if(any(ind <- is.na(pos))) {
        ## Try expanding a num-only id to the one with highest gen.
        num <- ids[ind]
        yep <- match(num, names(doc$gens_by_nums), nomatch = 0L)
        gen <- vapply(doc$gens_by_nums[yep], `[[`, 0L, 1L)
        yep <- (yep > 0L)
        pos[ind][yep] <-
            match(.ref_to_name(num[yep], gen), doc$names)
    }
    pos
}

.ref_to_name <-
function(x, y = NULL)
{
    sep <- "."
    if(!length(x))
        character()
    else if(!is.null(y)) {
        y <- rep(y, length.out = length(x))
        paste(x, y, sep = sep)
    }
    else if(is.matrix(x))
        paste(x[, 1L], x[, 2L], sep = sep)
    else if(length(x) > 1L)
        paste(x[1L], x[2L], sep = sep)
    else
        paste(x, "0", sep = sep)
}

read_next_bytes_until_whitespace <-
function(con)
{
    bytes <- raw()
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(!length(x) || (x %.IN.% pdf_bytes_whitespaces)) break
        bytes <- c(bytes, x)
    }
    bytes
}

read_next_non_whitespace <-
function(con)
{
    repeat {
        x <- .con_read_bytes(con, 1L)
        if(!length(x) || !(x %.IN.% pdf_bytes_whitespaces)) break
    }
    x
}

read_next_non_whitespace_and_seek_back <-
function(con)
{
    x <- read_next_non_whitespace(con)
    .con_seek(con, -1L, 2L)
    x
}

read_prev_bytes_after_bytes <-
function(con, set)
{
    ## Read the previous bytes from con until the first byte in set, and
    ## move point to the first preceding byte not in set.
    bytes <- raw()
    repeat {
        x <- .con_read_bytes(con, 1L)
        .con_seek(con, -2L, 2L)
        if(x %.IN.% set) {
            while(x %.IN.% set) {
                x <- .con_read_bytes(con, 1L)
                .con_seek(con, -2L, 2L)
            }
            .con_seek(con, 1, 2L)
            break
        } else {
            bytes <- c(x, bytes)
        }
    }
    bytes
}

read_prev_bytes_after_eols <-
function(con)
    read_prev_bytes_after_bytes(con, pdf_bytes_eols)

raw_connection_to_bytes_in_file <-
function(file)
{
    bytes <- readBin(file, "raw", file.size(file))
    rawConnection(bytes)
}

## * Utilities to enhance performance

## Test whether a single byte is contained in a sequence of bytes.
`%.IN.%` <-
function(x, bytes)
    any(x == bytes)
## Seems that this is faster than using grepRaw(fixed = TRUE) or
## match().

## Calling seek() is inefficient.
## * 1st, it dispatched to seek.connection().
## * 2nd, this pmatches two of its arguments before calling a .Internal.
## Hence, use the following, where values 1L, 2L, 3L for the origin
## correspond to "start", "current" and "end".

.con_seek <-
function(con, where = NA, origin = 1L)
{
    .Internal(seek(con, as.double(where), as.integer(origin), 0L))
}

## Calling readBin() to read bytes is somewhat inefficient.
## Hence, use the following.

.con_read_bytes <-
function(con, n = 1L)
{
    .Internal(readBin(con, "raw", n, NA_integer_, FALSE, FALSE))
}

## * Utilities for debugging

pdftools_debug_level <-
local({
    level <- 0L
    function(new) {
        if(!missing(new))
            level <<- new
        else
            level
    }
})
