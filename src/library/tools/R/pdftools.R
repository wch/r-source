## Note
##   http://www.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/index.html

## * Variables

pdf_bytes_whitespaces <-
    charToRaw("\n\r\t ")
pdf_bytes_eols <-
    charToRaw("\n\r")
pdf_bytes_delimiters <-
    charToRaw("()<>[]{}/%")
pdf_bytes_digits <-
    charToRaw("0123456789")

## * pdf_file

pdf_file <-
function(file)
{
    if(is.character(file)) {
        con <- file(file, "rb")
        on.exit(close(con))
    } else {
        if(!inherits(file, "file"))
            stop(gettextf("%s must be a character string or a file connection",
                          sQuote("file")),
                 domain = NA)
        con <- file
    }

    ## Start at the end.
    seek(con, -1L, "end")
    bytes <- raw()
    while(!length(bytes))
        bytes <- read_prev_bytes_after_eols(con)
    if(rawToChar(bytes) != "%%EOF")
        stop("EOF marker not found")
    ## Find startxref entry (the location of the xref table).
    bytes <- read_prev_bytes_after_eols(con)
    startxref <- as.integer(rawToChar(bytes))
    bytes <- read_prev_bytes_after_eols(con)
    if(substring(rawToChar(bytes), 1L, 9L) != "startxref")
        stop("cannot find startxref")

    xrefs <- xrefs_obj_streams <- trailer <- list()

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
            if(!(x %in% pdf_bytes_whitespaces)) break
            seek(con, -2L, "current")
        }
        repeat {
            seek(con, -2L, "current")
            x <- readBin(con, "raw", 1L)
            if(x %in% pdf_bytes_whitespaces) break
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
                num <- as.integer(rawToChar(bytes))
                read_next_non_whitespace_and_seek_back(con)
                bytes <- read_next_bytes_until_whitespace(con)
                size <- as.integer(rawToChar(bytes))
                read_next_non_whitespace_and_seek_back(con)            
                cnt <- 0
                while(cnt < size) {
                    bytes <- readBin(con, "raw", 20L)
                    ## Assume that all lines were correctly at 20 bytes.
                    ## (Could also try to be nice ...)
                    entry <- c(num,
                               rawToChar(bytes[ 1L : 10L]),
                               rawToChar(bytes[12L : 16L]),
                               rawToChar(bytes[      18L]))
                    xrefs <- c(xrefs, list(entry))
                    cnt <- cnt + 1L
                    num <- num + 1L
                }
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
            startxref <- new_trailer[["/Prev"]]
            if(is.null(startxref)) break
        } else if(x %in% pdf_bytes_digits) {
            ## PDF 1.5+ cross-reference stream, hopefully.
            seek(con, -1L, "current")
            pos <- seek(con)
            hdr <- pdf_read_object_header(con)
            obj <- pdf_read_object(con)
            if(!("/Type" %in% names(obj)) ||
               !(obj[["/Type"]] == "/XRef")) {
                ## Something's wrong.
                ## Try finding the xref table before pos.
                seek(con, pos)
                startxref <- find_xref_start(con)
                next
            }
            ## return(obj)            
            ## <FIXME>
            ## Add support for /Index lateron, for now only deal with
            ## the default.
            ## </FIXME>
            num <- 0L
            size <- obj[["/Size"]]
            field_sizes <- unlist(obj[["/W"]])
            cnt <- 0L
            xrefs <- list()
            xrefs_obj_streams <- list()
            stream <- rawConnection(pdf_get_stream_data(obj))
            while(cnt < size) {
                bytes <- readBin(stream, "raw", field_sizes[1L])
                d1 <- strtoi(paste(bytes, collapse = ""), 16L)
                bytes <- readBin(stream, "raw", field_sizes[2L])
                d2 <- strtoi(paste(bytes, collapse = ""), 16L)      
                bytes <- readBin(stream, "raw", field_sizes[3L])
                d3 <- strtoi(paste(bytes, collapse = ""), 16L)
                ## Might actually need to overwrite entries.
                ## Worry about that lateron ...
                if(d1 == 1) {
                    xrefs <-
                        c(xrefs,
                          list(c(num, d2, d3, "n")))
                } else if(d1 == 2) {
                    xrefs_obj_streams <-
                        c(xrefs_obj_streams,
                          list(c(num, d2, d3)))
                }
                cnt <- cnt + 1L
                num <- num + 1L
            }
            close(stream)
            keys <- c("/Root", "/Encrypt", "/Info", "/ID")
            pos <- match(keys, names(obj), nomatch = 0L)
            trailer[keys[pos > 0L]] <- obj[pos]
            startxref <- obj[["/Prev"]]
            if(is.null(startxref)) break
        } else {
            seek(con, -1L, "current")
            startxref <- find_xref_start(con)
        }
    }
        
    xrefs <- as.data.frame(do.call(rbind, xrefs),
                           stringsAsFactors = FALSE)
    xrefs[-4L] <- lapply(xrefs[-4L], as.integer)
    names(xrefs) <- c("num", "pos", "gen", "use")

    xrefs_obj_streams <- do.call(rbind, xrefs_obj_streams)
    if(!is.null(xrefs_obj_streams))
        colnames(xrefs_obj_streams) <- c("num", "str", "idx")

    y <- list(file = summary(con)$description,
              xrefs = xrefs,
              xrefs_obj_streams = xrefs_obj_streams,
              trailer = trailer)
    class(y) <- "pdf_file"
    y
}

## * pdf_info

pdf_info <-
function(file)
{
    doc <- pdf_file(file)
    ref <- doc$trailer[["/Info"]]
    if(is.null(ref)) return(NULL)
    pdf_file_get_object(doc, ref)
}


## * Object readers

pdf_read_object <-
function(con) {
    x <- rawToChar(readBin(con, "raw", 1L))
    seek(con, -1L, "current")
    if(x %in% c("t", "f"))
        pdf_read_object_boolean(con)
    else if(x == "(")
        pdf_read_object_string_literal(con)
    else if(x == "/")
        pdf_read_object_name(con)
    else if(x == "[")
        pdf_read_object_array(con)
    else if(x == "n")
        pdf_read_object_null(con)
    else if(x == "<") {
        ## Hexadecimal string or dictionary
        bytes <- readBin(con, "raw", 2L)
        seek(con, -2L, "current")
        if(rawToChar(bytes) == "<<")
            pdf_read_object_dictionary(con)
        else
            pdf_read_object_string_hexadecimal(con)
    }
    else if(x == "%") {
        ## Read until eol.
        repeat {
            x <- readBin(con, "raw", 1L)
            if(x %in% pdf_bytes_eols) break
        }
        read_next_non_whitespace_and_seek_back(con)
        read_pdf_object(con)
    }
    else {
        ## Could be a number object or an indirect object reference.
        if(x %in% charToRaw("+-"))
            return(pdf_read_object_numeric(con))
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
}

pdf_read_object_boolean <-
function(con)
{
    x <- rawToChar(readBin(con, "raw", 1L))
    if(x == "t") {
        bytes <- readBin(con, "raw", 3L)
        if(rawToChar(bytes) == "rue")
            return(TRUE)
    } else if(x == "f") {
        bytes <- readBin(con, "raw", 4L)
        if(rawToChar(bytes) == "alse")
            return(FALSE)
    }
    stop("cannot read boolean object")
}

pdf_read_object_numeric <-
function(con) {
    bytes <- raw()
    table <- charToRaw("+-.0123456789")
    while((x <- readBin(con, "raw", 1L)) %in% table) {
        bytes <- c(bytes, x)
    }
    seek(con, -1L, "current")
    s <- rawToChar(bytes)
    if(grepl(".", s, fixed = TRUE))
        as.numeric(s)
    else
        as.integer(s)
}

pdf_read_object_string_literal <-
function(con) {
    x <- readBin(con, "raw", 1L)
    if(rawToChar(x) != "(")
        stop("cannot read literal string object")
    lparen <- charToRaw("(")
    rparen <- charToRaw(")")
    escape <- charToRaw("\\")
    escape_tails <- c("n", "r", "t", "b", "f", "(", ")", "\\")
    pdf_bytes_escape_tails <-
        charToRaw(paste(escape_tails, collapse = ""))
    pdf_bytes_escape_bytes <-
        charToRaw(c("\n\r\t\b\f()\\"))
    names(pdf_bytes_escape_bytes) <- escape_tails

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
            if(x %in% pdf_bytes_digits) {
                i <- 0L
                while(i < 2L) {
                    y <- readBin(con, "raw", 1L)
                    if(!(y %in% pdf_bytes_digits)) {
                        seek(con, -1L, "current")                        
                        break
                    }
                    x <- c(x, y)
                    i <- i + 1L
                }
                x <- as.raw(strtoi(rawToChar(x), 8L))
            } else if(x %in% pdf_bytes_escape_tails) {
                x <- pdf_bytes_escape_bytes[rawToChar(x)]
            } else if(x %in% pdf_bytes_eols) {
                x <- readBin(con, "raw", 1L)
                if(!(x %in% pdf_bytes_eols))
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
    rawToChar(bytes)
}

pdf_read_object_string_hexadecimal <-
function(con) {
    ## <FIXME>
    ## Quick shot, may need to improve.
    x <- readBin(con, "raw", 1L)
    if(rawToChar(x) != "<")
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
    ## Argh.
    ## These strings can contain embedded nuls, in which case we need to
    ## return a byte sequence.
    s <- as.raw(strtoi(s, 16L))
    if((length(s) > 2L) &&
       identical(head(s, 2L), as.raw(c(254, 255)))) {
        ## If the first two bytes represent the Unicode byte-order marker
        ## U+FEFF, this is a text string encoded in UTF-16BE.
        ## <FIXME>
        ##   This can contain the language encoding as
        ##     U+001B (i.e., as.raw(0, 27))
        ##     2-byte ISO-639 language code
        ##     2-byt eISO-639 language code [optional]
        ##     U+001B
        ##   Could try using grepRaw for this.
        s <- intToUtf8(tail(s, -2L))
        ## </FIXME>
    } else {
        if(all(s != 0)) s <- rawToChar(s)
    }
    s
    ## </FIXME>
}

pdf_read_object_name <-
function(con) {
    x <- readBin(con, "raw", 1L)
    if(rawToChar(x) != "/")
        stop("cannot read name object")
    bytes <- c(pdf_bytes_whitespaces, pdf_bytes_delimiters)
    repeat {
        y <- readBin(con, "raw", 1L)
        if(y %in% bytes) {
            seek(con, -1L, "current")
            break
        }
        x <- c(x, y)
    }
    rawToChar(x)
}

pdf_read_object_array <-
function(con) {
    x <- readBin(con, "raw", 1L)
    if(rawToChar(x) != "[")
        stop("cannot read array object")
    y <- list()
    end <- charToRaw("]")
    repeat {
        x <- read_next_non_whitespace_and_seek_back(con)
        if(x == end) {
            readBin(con, "raw", 1L)
            break
        }
        y <- c(y, pdf_read_object(con))
    }
    y
}

pdf_read_object_dictionary <-
function(con) {
    bytes <- readBin(con, "raw", 2L)
    if(rawToChar(bytes) != "<<")
        stop("cannot read dictionary object")
    y <- list()
    end <- charToRaw(">")
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
        if(!(eol %in% pdf_bytes_eols))
            stop("cannot read stream object")
        if(eol == charToRaw("\r"))
            readBin(con, "raw", 1L)
        ## Need length information in dictionary.
        if(is.null(y[["/Length"]]))
            stop("cannot read stream object")
        ## <FIXME>
        ## /Length info could be and indirect reference?
        ## </FIXME>
        y[["__streamdata__"]] <- readBin(con, "raw", y[["/Length"]])
        ## Now check if we really hit the end of the stream.
        read_next_non_whitespace_and_seek_back(con)
        bytes <- readBin(con, "raw", 9L)
        if(rawToChar(bytes) != "endstream")
            stop("cannot read stream object")
    } else {
        seek(con, pos)
    }
    y
}

pdf_read_object_stream <-
function(con) {
}

pdf_read_object_null <-
function(con)
{
    bytes <- readBin(con, "raw", 4L)
    if(rawToChar(bytes) != "null")
        stop("cannot read null object")
    NULL
}

pdf_read_object_indirect_reference <-
function(con) {
    num <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    gen <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    x <- readBin(con, "raw", 1L)
    if(rawToChar(x) != "R")
        stop("cannot read indirect reference object")
    c(num = as.integer(rawToChar(num)),
      gen = as.integer(rawToChar(gen)))
}

pdf_read_object_header <-
function(con) {
    ## Read num and gen.
    read_next_non_whitespace_and_seek_back(con)    
    num <- read_next_bytes_until_whitespace(con)
    read_next_non_whitespace_and_seek_back(con)
    gen <- read_next_bytes_until_whitespace(con)
    ## Now skip the "obj".
    readBin(con, "raw", 3L)
    read_next_non_whitespace_and_seek_back(con)
    c(num = as.integer(rawToChar(num)),
      gen = as.integer(rawToChar(gen)))
}
    

pdf_file_get_object <-
function(doc, ref, con = NULL)
{
    ## Experimental---need to read the docs first.
    ## Right now, get an indirect object from its reference.

    if(is.null(con)) {
        con <- file(doc$file, "rb")
        on.exit(close(con))
    }

    if(length(ref) == 1L) {
        names(ref) <- "num"
    }
    num <- ref["num"]
    gen <- ref["gen"]

    ## First look in the xrefs for object streams.
    if((is.na(gen) || (gen == 0L)) &&
       (length(pos <- which(doc$xrefs_obj_streams[, "num"] == num)))) {
        if(length(pos) > 1L) {
            ## Can this really happen?
            pos <- pos[1L]
        }
        ptr <- doc$xrefs_obj_streams[pos, ]
        num <- ptr["str"]
        idx <- ptr["idx"]
        obj <- pdf_file_get_object(doc, num, con)
        ## Could check whether
        ##   obj[["/Type"]] == "/ObjStm"
        if(idx >= obj[["/N"]])
            stop("invalid index in object stream lookup")
        stream <- rawConnection(pdf_get_stream_data(obj))
        on.exit(close(stream), add = TRUE)
        i <- 0L
        while(i <= idx) {
            cnum <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            cpos <- pdf_read_object(stream)
            read_next_non_whitespace_and_seek_back(stream)
            i <- i + 1L
        }
        seek(stream, obj[["/First"]] + cpos)
        return(pdf_read_object(stream))
    }

    ## Figure out the position to start from.
    if(length(ref) == 1L) {
        pos <- subset(doc$xrefs, num == ref)$pos
        if(length(pos) > 1L) {
            ## Can this really happen?
            pos <- pos[1L]
        }
    }
    else
        pos <- subset(doc$xrefs,
                      (num == ref["num"]) & (gen == ref["gen"]))$pos

    ## con <- file(doc$file, "rb")
    ## on.exit(close(con))
    
    seek(con, pos)
    ## Read header first.
    hdr <- pdf_read_object_header(con)
    ## Be paranoid.
    if(any(is.na(hdr)))
        stop("cannot find object header at xrefed positions")
    gnum <- ref["num"]
    fnum <- hdr["num"]
    if(gnum != fnum)
        stop(gettextf("mismatch in object numbers (given: %d, found: %d)",
                      gnum, fnum),
             domain = NA)
    ggen <- ref["gen"]
    fgen <- hdr["gen"]
    if(!is.na(ggen) && (ggen != fgen))
        stop(gettextf("mismatch in generation numbers (given: %d, found: %d)",
                      gnum, fnum),
             domain = NA)

    pdf_read_object(con)
}

## * Streams

pdf_get_stream_data <-
function(obj) {
    bytes <- obj[["__streamdata__"]]
    filters <- as.list(obj[["/Filter"]])
    for(filter in filters) {
        if(filter == "/FlateDecode")
            bytes <- pdf_filter_flate_decode(bytes,
                                             obj[["/DecodeParms"]])
        else
            stop(gettextf("unsupported filter %s",
                          sQuote(filter)),
                 domain = NA)
    }
    bytes
}

## * Filters

pdf_filter_flate_decode <-
function(x, params) {
    ## Need to decompress first
    m <- memDecompress(x, "gzip")
    predictor <- params[["/Predictor"]]
    if(is.null(predictor) || (predictor == 1L))
        return(m)
    if((predictor < 10L) && (predictor > 15L)) {
        stop(gettextf("unsupported flatedecode predictor %d",
                      predictor),
             domain = NA)
    }
    columns <- params[["/Columns"]]
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

## * Utilities

read_next_bytes_until_whitespace <-
function(con) {
    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || (x %in% pdf_bytes_whitespaces)) break
        bytes <- c(bytes, x)
    }
    bytes
}

read_next_non_whitespace <-
function(con) {
    repeat {
        x <- readBin(con, "raw", 1L)
        if(!length(x) || !(x %in% pdf_bytes_whitespaces)) break
    }
    x
}

read_next_non_whitespace_and_seek_back <-
function(con) {
    x <- read_next_non_whitespace(con)
    seek(con, -1L, "current")
    x
}

read_prev_bytes_after_eols <-
function(con) {
    ## Read the previous bytes until the first eol byte, and move point
    ## to the first preceding non-eol byte.
    bytes <- raw()
    repeat {
        x <- readBin(con, "raw", 1L)
        seek(con, -2L, "current")
        if(x %in% pdf_bytes_eols) {
            while(x %in% pdf_bytes_eols) {
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
