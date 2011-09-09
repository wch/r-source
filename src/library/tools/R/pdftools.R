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
    c(charToRaw("\t\n\f\r "), as.raw(0))
pdf_bytes_delimiters <-
    charToRaw("()<>[]{}/%")
pdf_bytes_non_regulars <-
    c(pdf_bytes_whitespaces, pdf_bytes_delimiters)
pdf_bytes_eols <-
    charToRaw("\n\r")
pdf_bytes_digits <-
    charToRaw("0123456789")

pdf_bytes_in_keywords <-
    charToRaw(paste("*'\"",
                    "0123456789",
                    paste(LETTERS, collapse = ""),
                    paste(letters, collapse = ""),
                    sep = ""))

pdf_bytes_in_numerics_not_digits <-
    charToRaw("+-.")
pdf_bytes_in_numerics <-
    c(pdf_bytes_digits, pdf_bytes_in_numerics_not_digits)

## * pdf_doc

pdf_doc <-
function(file)
{
    if(is.character(file)) {
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
    seek(con, 0L)
    header <- rawToChar(read_next_bytes_until_whitespace(con))
    if(substring(header, 1L, 5L) != "%PDF-")
        stop("PDF header not found")

    ## Go to the end.
    seek(con, -1L, "end")

    ## Record file size as number of bytes.
    nbytes <- seek(con) + 1L

    ## Check footer.
    bytes <- raw()
    while(!length(bytes))
        bytes <- read_prev_bytes_after_eols(con)
    if(rawToChar(bytes) != "%%EOF")
        stop("EOF marker not found")

    ## Find startxref entry (the location of the xref table).
    bytes <- read_prev_bytes_after_eols(con)
    startxref <- suppressWarnings(as.integer(rawToChar(bytes)))
    bytes <- read_prev_bytes_after_eols(con)
    if(substring(rawToChar(bytes), 1L, 9L) != "startxref")
        stop("cannot find startxref")

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
        seek(con, -1L, "current")
        repeat {
            x <- readBin(con, "raw", 1L)
            if(!(x %.IN.% pdf_bytes_whitespaces)) break
            seek(con, -2L, "current")
        }
        repeat {
            seek(con, -2L, "current")
            x <- readBin(con, "raw", 1L)
            if(x %.IN.% pdf_bytes_whitespaces) break
        }
        pos <- seek(con)
        if(rawToChar(readBin(con, "raw", 4L)) == "xref")
            pos
        else
            stop("cannot find xref table")
    }

    ## Load the xref info.
    repeat {
        seek(con, startxref)
        x <- readBin(con, "raw", 1L)
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
                    bytes <- readBin(con, "raw", 20L)
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
                if(rawToChar(readBin(con, "raw", 7L)) != "trailer")
                    seek(con, -7L, "current")
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
            seek(con, -1L, "current")
            pos <- seek(con)
            hdr <- pdf_read_object_header(con)
            obj <- pdf_read_object(con)
            if(!("Type" %in% names(obj)) ||
               !(obj[["Type"]] == "XRef")) {
                ## Something's wrong.
                ## Try finding the xref table before pos.
                seek(con, pos)
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
                    bytes <- readBin(stream, "raw", field_sizes[1L])
                    d1 <- strtoi(paste(bytes, collapse = ""), 16L)
                    bytes <- readBin(stream, "raw", field_sizes[2L])
                    d2 <- strtoi(paste(bytes, collapse = ""), 16L)      
                    bytes <- readBin(stream, "raw", field_sizes[3L])
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
            seek(con, -1L, "current")
            startxref <- find_xref_start(con)
        }
    }

    y <- list(file = file,
              size = nbytes,
              header = header,
              xref_tabs = xref_tabs,
              xref_objs = xref_objs,
              trailer = trailer,
              con = if(keep) con else NULL)
    class(y) <- "pdf_doc"
    y
}

print.pdf_doc <-
function(x, ...)
{
    n <- sum(x$xref_tabs[, "use"]) + NROW(x$xref_objs)
    writeLines(strwrap(sprintf("PDF document (file \"%s\", %d bytes, %d objects)",
                               x$file, x$size, n),
                       exdent = 4L))
    invisible(x)
}

## * pdf_catalog

pdf_catalog <-
function(file)
{
    doc <- if(inherits(file, "pdf_doc")) file else pdf_doc(file)
    pdf_doc_get_object(doc, doc$trailer[["Root"]])
}

## * pdf_fonts

pdf_fonts <-
function(file)
{
    doc <- if(inherits(file, "pdf_doc")) file else pdf_doc(file)

    con <- file(doc$file, "rb")
    on.exit(close(con))

    ## Get the page resources.
    resources <- pdf_doc_get_page_resources(doc, con)
    ## Get the font resources (actually, their references).
    frefs <- list()
    for(res in resources) {
        for(ref in res[["Font"]]) {
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
                      list(base,
                           obj[["Subtype"]],
                           !is.null(obj[["FontDescriptor"]]),
                           grepl("^[[:upper:]]{6}\\+", base),
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
    doc <- if(inherits(file, "pdf_doc")) file else pdf_doc(file)

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
    urx <- lapply(rectangles, `[[`, 3L)
    ury <- lapply(rectangles, `[[`, 4L)
    if((length(uurx <- unique(urx)) == 1L) &&
       (length(uury <- unique(ury)) == 1L))
        info["Page size"] <- sprintf("%s x %s pts", uurx, uury)
    
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

print.pdf_info <-
function(x, ...)
{
    writeLines(format(x, ...))
    invisible(x)
}

## * Object readers

pdf_read_object <-
function(con)
{
    if(pdftools_debug_level() > 0L) {
        bytes <- readBin(con, "raw", 10L)
        message(sprintf("Looking at %s", intToUtf8(bytes)))
        seek(con, -length(bytes), "current")
    }
    
    x <- read_next_non_whitespace(con)
    if(!length(x)) return(NA)
    seek(con, -1L, "current")
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
        bytes <- readBin(con, "raw", 2L)
        seek(con, -2L, "current")
        if(all(bytes == c(0x3c, 0x3c)))
            pdf_read_object_dictionary_or_stream(con)
        else
            pdf_read_object_string_hexadecimal(con)
    }
    else if(x == 0x25) {                # charToRaw("%") => 25
        ## Read until eol.
        repeat {
            x <- readBin(con, "raw", 1L)
            if(x %.IN.% pdf_bytes_eols) break
        }
        read_next_non_whitespace_and_seek_back(con)
        pdf_read_object(con)
    }
    else if(x %.IN.% pdf_bytes_in_numerics_not_digits)
        pdf_read_object_numeric(con)
    else if(x %.IN.% pdf_bytes_digits) {
        ## Could be a number object or an indirect object reference.
        bytes <- readBin(con, "raw", 20L)
        seek(con, - length(bytes), "current")
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
##     x <- rawToChar(readBin(con, "raw", 1L))
##     if(x == "t") {
##         bytes <- readBin(con, "raw", 3L)
##         if(rawToChar(bytes) == "rue")
##             return(TRUE)
##     } else if(x == "f") {
##         bytes <- readBin(con, "raw", 4L)
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
    while((x <- readBin(con, "raw", 1L)) %.IN.% table) {
        bytes <- c(bytes, x)
    }
    seek(con, -1L, "current")
    
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
    x <- readBin(con, "raw", 1L)
    lparen <- charToRaw("(")    
    if(x != lparen)
        stop("cannot read literal string object")
    
    rparen <- charToRaw(")")
    escape <- charToRaw("\\")
    escape_tails <- c("n", "r", "t", "b", "f", "(", ")", "\\")
    pdf_bytes_escape_tails <-
        charToRaw(paste(escape_tails, collapse = ""))
    pdf_bytes_escape_bytes <-
        charToRaw(c("\n\r\t\b\f()\\"))
    ## names(pdf_bytes_escape_bytes) <- escape_tails
    names(pdf_bytes_escape_bytes) <-
        as.character(charToRaw("nrtbf()\\"))

    bytes <- raw()
    parens <- 1L
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x)) break
        if(x == lparen) {
            parens <- parens + 1L
        } else if(x == rparen) {
            parens <- parens - 1L
            if(!parens) break
        } else if(x == escape) {
            x <- readBin(con, "raw", 1L)
            if(x %.IN.% pdf_bytes_digits) {
                i <- 0L
                while(i < 2L) {
                    y <- readBin(con, "raw", 1L)
                    if(!(y %.IN.% pdf_bytes_digits)) {
                        seek(con, -1L, "current")                        
                        break
                    }
                    x <- c(x, y)
                    i <- i + 1L
                }
                x <- as.raw(strtoi(rawToChar(x), 8L))
            } else if(x %.IN.% pdf_bytes_escape_tails) {
                ## x <- pdf_bytes_escape_bytes[rawToChar(x)]
                x <- pdf_bytes_escape_bytes[as.character(x)]
            } else if(x %.IN.% pdf_bytes_eols) {
                x <- readBin(con, "raw", 1L)
                if(!(x %.IN.% pdf_bytes_eols))
                    seek(con, -1L, "current")
                x <- raw()
            } else {
                stop(gettextf("invalid escape sequence %s",
                              sQuote(paste("\\",
                                           rawToChar(x),
                                           collapse = ""))),
                     domain = NA)
            }
        }
        bytes <- c(bytes, x)
    }
    class(bytes) <- "PDF_String"
    bytes
}

pdf_read_object_string_hexadecimal <-
function(con)
{
    x <- readBin(con, "raw", 1L)
    if(x != 0x3c)                       # charToRaw("<") => 3c
        stop("cannot read hexadecimal string object")
    
    end <- charToRaw(">")

    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        if(x == end) break
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

print.PDF_String <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
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

    x <- readBin(con, "raw", 1L)
    if(x != 0x2f)                       # charToRaw("/") => 2f
        stop("cannot read name object")

    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || (x %.IN.% pdf_bytes_non_regulars)) {
            seek(con, -1L, "current")
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
    x <- readBin(con, "raw", 1L)
    if(x != 0x5b)                       # charToRaw("[") => 5b
        stop("cannot read array object")

    end <- charToRaw("]")
    
    y <- list()
    repeat {
        x <- read_next_non_whitespace_and_seek_back(con)
        if(x == end) {
            readBin(con, "raw", 1L)
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

print.PDF_Array <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

pdf_read_object_dictionary_or_stream <-
function(con)
{
    bytes <- readBin(con, "raw", 2L)
    if(!all(bytes == c(0x3c, 0x3c)))
        stop("cannot read dictionary object")

    end <- charToRaw(">")    
    
    y <- list()
    repeat {
        x <- read_next_non_whitespace_and_seek_back(con)
        if(x == end) {
            readBin(con, "raw", 2L)
            break
        }
        key <- pdf_read_object(con)
        read_next_non_whitespace_and_seek_back(con)
        val <- pdf_read_object(con)
        y[[key]] <- val
    }
    pos <- seek(con)
    ## Check whether this is in fact a stream object.
    ## Read ahead.
    read_next_non_whitespace_and_seek_back(con)
    if(rawToChar(readBin(con, "raw", 6L)) == "stream") {
        ## Argh.  Handle the EOL marker assuming compliance: should
        ## check for this.
        eol <- readBin(con, "raw", 1L)
        if(!(eol %.IN.% pdf_bytes_eols))
            stop("cannot read stream object")
        if(eol == charToRaw("\r"))
            readBin(con, "raw", 1L)
        ## Need length information in dictionary.
        len <- y[["Length"]]
        if(is.null(len))
            stop("cannot read stream object")
        ## However, the length could be an indirect object reference.
        ## In this case we cannot resolve the length here (as we only
        ## look at the byte stream without knowing the cross-reference
        ## information).  Hence, record the position where the stream
        ## data start, and have PDF_Stream_get_data() resolve lateron.
        if(inherits(len, "PDF_Indirect_Reference")) {
            y[["__stream_tell__"]] <- seek(con)
            y[["__stream_data__"]] <- NULL
        } else {
            y[["__stream_data__"]] <- readBin(con, "raw", len)
            ## Now check if we really hit the end of the stream.
            read_next_non_whitespace_and_seek_back(con)
            bytes <- readBin(con, "raw", 9L)
            if(rawToChar(bytes) != "endstream")
                stop("cannot read stream object")
        }
        class(y) <- "PDF_Stream"
    } else {
        seek(con, pos)
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

print.PDF_Dictionary <-
print.PDF_Stream <-    
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

## <NOTE>
## Handled by pdf_read_object_keyword() now.
##
## pdf_read_object_null <-
## function(con)
## {
##     bytes <- readBin(con, "raw", 4L)
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
    x <- readBin(con, "raw", 1L)
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

print.PDF_Indirect_Reference <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

pdf_read_object_keyword <-
function(con)
{
    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || !(x %.IN.% pdf_bytes_in_keywords)) {
            seek(con, -1L, "current")
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
    readBin(con, "raw", 3L)
    read_next_non_whitespace_and_seek_back(con)
    c(num = suppressWarnings(as.integer(rawToChar(num))),
      gen = suppressWarnings(as.integer(rawToChar(gen))))
}

pdf_read_indirect_object_at_pos <-
function(con, pos, num = NA_integer_, gen = NA_integer_)
{
    ## Move to pos.
    seek(con, pos)
    ## Read header first.
    hdr <- pdf_read_object_header(con)
    ## Be paranoid.
    if(any(is.na(hdr)))
        stop(gettextf("cannot find object header at xrefed position %d",
                      pos),
             domain = NA)
    if(!is.na(num) && (num != hdr["num"]))
        stop(gettextf("mismatch in object numbers (given: %d, found: %d)",
                      num, hdr["num"]),
             domain = NA)
    if(!is.na(gen) && (gen != hdr["gen"]))
        stop(gettextf("mismatch in generation numbers (given: %d, found: %d)",
                      gen, hdr["gen"]),
             domain = NA)
    ## Read object.
    pdf_read_object(con)
}

pdf_doc_get_object <-
function(doc, ref, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")
    
    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    if(length(ref) == 1L) {
        names(ref) <- "num"
    } else if(length(ref) == 2L) {
        names(ref) <- c("num", "gen")
    }
    num <- ref["num"]
    gen <- ref["gen"]

    ## First look in the xrefs for object streams.
    if((is.na(gen) || (gen == 0L)) &&
       (length(pos <- which(doc$xref_objs[, "num"] == num)))) {
        if(length(pos) > 1L) {
            ## Can this really happen?
            pos <- pos[1L]
        }
        ptr <- doc$xref_objs[pos, ]
        num <- ptr["str"]
        idx <- ptr["idx"]
        obj <- pdf_doc_get_object(doc, num, con)
        ## Could check whether
        ##   obj[["Type"]] == "ObjStm"
        if(idx >= obj[["N"]])
            stop("invalid index in object stream lookup")
        stream <- rawConnection(PDF_Stream_get_data(obj, doc))
        on.exit(close(stream), add = TRUE)
        i <- 0L
        while(i <= idx) {
            cnum <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            cpos <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            i <- i + 1L
        }
        seek(stream, obj[["First"]] + cpos)
        return(pdf_read_object(stream))
    }

    ## Figure out the position to start from.
    if(length(ref) == 1L) {
        ind <- (doc$xref_tabs[, "num"] == ref)
        pos <- doc$xref_tabs[ind, "pos"]
        if(length(pos) > 1L) {
            ## Can this really happen?
            pos <- pos[1L]
        }
    }
    else {
        ind <- ((doc$xref_tabs[, "num"] == num) &
                (doc$xref_tabs[, "gen"] == gen))
        pos <- doc$xref_tabs[ind, "pos"]
    }

    pdf_read_indirect_object_at_pos(con, pos, num, gen)
}

pdf_doc_get_objects <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")
    
    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    debug <- (pdftools_debug_level() > 0L)    

    objects <- list()

    ## First get the objects from the old-style xref tables.
    for(i in seq_len(nrow(doc$xref_tabs))) {
        entry <- doc$xref_tabs[i, ]
        if(!entry["use"]) next
        pos <- entry["pos"]
        if(pos == 0L) next
        if(debug) message(sprintf("processing %s", format(entry)))
        num <- entry["num"]
        gen <- entry["gen"]
        obj <- pdf_read_indirect_object_at_pos(con, pos, num, gen)
        objects[[.ref_to_name(c(num, gen))]] <- obj
    }
    ## Now for the new-style xref streams objects.
    if(length(doc$xref_objs)) {
        for(str in unique(doc$xref_objs[, "str"])) {
            obj <- objects[[.ref_to_name(str)]]
            n <- obj[["N"]]
            first <- obj[["First"]]
            stream <- rawConnection(PDF_Stream_get_data(obj))
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
            ## Then read the objects from the stream.
            i <- 1L
            while(i <= n) {
                seek(stream, first + tab[i, 2L])
                obj <- pdf_read_object(stream)
                objects[[.ref_to_name(tab[i, 1L])]] <- obj
                i <- i + 1L                
            }
            close(stream)
        }
    }
                
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
    
    pages <- list()
    ## Cannot use rapply() because this only deals with nodes which are
    ## not lists.
    recurse <- function(x) {
        if(!is.null(kids <- x[["Kids"]])) {
            for(kid in kids) {
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

pdf_doc_get_page_content_streams <-
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")
    
    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    pages <- pdf_doc_get_page_list(doc, con)
    lapply(pages,
           function(p) pdf_doc_get_object(doc, p[["Contents"]], con))
}

pdf_doc_get_page_resources <- 
function(doc, con = NULL)
{
    if(!inherits(doc, "pdf_doc")) stop("wrong class")
    
    if(is.null(con) && is.null(con <- doc$con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }
    
    pages <- pdf_doc_get_page_list(doc, con)
    lapply(pages,
           function(p) pdf_doc_get_object(doc, p[["Resources"]], con))
}

## * Streams

PDF_Stream_get_data <-
function(obj, doc = NULL)
{
    bytes <- obj[["__stream_data__"]]

    if(is.null(bytes)) {
        if(is.null(doc))
            stop("cannot read stream data")
        con <- doc$con
        if(is.null(con)) {
            con <- file(doc$file, "rb")
            on.exit(close(con))
        }
        len <- obj[["Length"]]
        if(inherits(len, "PDF_Indirect_Reference")) {
            ## <FIXME>
            ## Eventually there should be something like
            ##    pdf_doc_get_object_with_cache()
            ## </FIXME>
            len <- pdf_doc_get_object(doc, len, con)
        }
        ## Now that we know the length as well ...
        seek(con, obj[["__stream_tell__"]])
        bytes <- readBin(con, "raw", len)
        ## Now check if we really hit the end of the stream.
        read_next_non_whitespace_and_seek_back(con)
        if(rawToChar(readBin(con, "raw", 9L)) != "endstream")
            stop("cannot read stream data")
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
        stop(gettextf("unsupported flatedecode predictor %d",
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
function(con)
{
    objects <- list()
    while(!identical(obj <- pdf_read_object(con), NA))
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
        substring(s, nchar(s), 14L) <-
            substring("    0101000000", nchar(s), 14L)
        strptime(s, "%Y%m%d%H%M%S")
    } else if(substring(s, 15L, 15L) == "Z") {
        strptime(substring(s, 1L, 14L), "%Y%m%d%H%M%S")
    } else {
        strptime(s, "%Y%m%d%H%M%S%z")
    }
}

## * Utilities

## Need a better name ...
.ref_to_name <-
function(x)
{
    if(length(x) == 1L)
        paste(x, "0", sep = ",")
    else
        paste(x[1L], x[2L], sep = ",")
}

read_next_bytes_until_whitespace <-
function(con)
{
    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || (x %.IN.% pdf_bytes_whitespaces)) break
        bytes <- c(bytes, x)
    }
    bytes
}

read_next_non_whitespace <-
function(con)
{
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || !(x %.IN.% pdf_bytes_whitespaces)) break
    }
    x
}

read_next_non_whitespace_and_seek_back <-
function(con)
{
    x <- read_next_non_whitespace(con)
    seek(con, -1L, "current")
    x
}

read_prev_bytes_after_eols <-
function(con)
{
    ## Read the previous bytes until the first eol byte, and move point
    ## to the first preceding non-eol byte.
    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        seek(con, -2L, "current")
        if(x %.IN.% pdf_bytes_eols) {
            while(x %.IN.% pdf_bytes_eols) {
                x <- readBin(con, "raw", 1L)
                seek(con, -2L, "current")
            }
            seek(con, 1, "current")
            break
        } else {
            bytes <- c(x, bytes)
        }
    }
    bytes
}

raw_connection_to_bytes_in_file <-
function(file)
{
    bytes <- readBin(file, "raw", file.info(file)$size)
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

.stream_seek <-
function(con, where = NA, origin = 1L)
{
    .Internal(seek(con, as.double(where), as.integer(origin), 0L))
}

## Calling readBin() to read bytes is somewhat inefficient.
## Hence, use the following.

.stream_read_bytes <-
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
