#  File src/library/utils/R/read.fwf.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

read.fwf <-
function(file, widths, header = FALSE, sep = "\t",
         skip = 0L, row.names, col.names, n = -1L, buffersize = 2000,
         fileEncoding = "", ...)
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[!nzchar(x)] <- NA_character_
        x
    }

    if (is.list(widths)) {
        recordlength <- length(widths)
        widths <- do.call("c", widths)
    } else recordlength <- 1L

    drop <- (widths < 0L)
    cwidths <- cumsum(abs(widths))
    st <- c(1L, 1L+cwidths)
    first <- st[-length(st)][!drop]
    last <- cwidths[!drop]
    outsep <- c(rep_len(sep, length(first) - 1L), "\n")

    buffersize <- (buffersize %/% recordlength) * recordlength

    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME,"a")
    on.exit(close(FILE), add = TRUE)

    if (is.character(file)) {
        file <- if(nzchar(fileEncoding))
            file(file, "rt", encoding = fileEncoding) else file(file, "rt")
        on.exit(close(file), add = TRUE)
    } else if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file), add = TRUE)
    }

    if (skip) readLines(file, n = skip)
    if (header) {
        headerline <- readLines(file, n = 1L)
        cat(file = FILE, headerline, "\n")
    }

    repeat({
        if (n == 0L) break
        thisblock <- if (n == -1L) buffersize
                     else min(buffersize, n*recordlength)
        raw <- readLines(file, n = thisblock)
        nread <- length(raw)
        if (recordlength > 1L &&  nread %% recordlength) {
            raw <- raw[1L:(nread-nread %% recordlength)]
            warning(sprintf(ngettext(nread %% recordlength,
                                     "last record incomplete, %d line discarded",
                                     "last record incomplete, %d lines discarded"),
                            nread %% recordlength), domain = NA)
        }
        if (recordlength > 1L) {
            raw <- matrix(raw, nrow = recordlength)
            raw <- apply(raw, 2L, paste, collapse = "")
        }

        cat(file = FILE, sapply(raw, doone, USE.NAMES=FALSE), sep = outsep)

        if (nread < thisblock) break
        if (n > 0L) n <- n - length(raw)
    })

    close(FILE)
    FILE <- file(FILENAME,"r")
    read.table(file = FILE, header = header, sep = sep,
	       row.names = row.names, col.names = col.names, quote = "", ...)
}
