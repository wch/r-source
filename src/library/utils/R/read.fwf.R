#  File src/library/utils/R/read.fwf.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

read.fwf <-
function(file, widths, header = FALSE, sep = "\t",
         skip = 0, row.names, col.names, n = -1, buffersize = 2000, ...)
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
    widths <- abs(widths)


    buffersize <- (buffersize %/% recordlength) * recordlength

    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME,"a")
    on.exit(close(FILE),add=TRUE)

    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file), add=TRUE)
    } else if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file), add=TRUE)
    }

    if (skip) readLines(file, n=skip)
    if (header) {
        headerline <- readLines(file, n=1L)
        cat(file=FILE, headerline, "\n")
    }

    repeat({
        if (n == 0L) break
        if (n == -1L)
            thisblock <- buffersize
        else
            thisblock <- min(buffersize,n*recordlength)

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
            raw <- matrix(raw, nrow=recordlength)
            raw <- apply(raw, 2L, paste, collapse="")
        }

        st <- c(1L, 1L+cumsum(widths))
        first <- st[-length(st)][!drop]
        last <- cumsum(widths)[!drop]
        cat(file = FILE, sapply(raw, doone),
            sep = c(rep_len(sep, length(first)-1L), "\n"))

        if (nread < thisblock) break
        if (n > 0L) n <- n - length(raw)
    })

    close(FILE)
    FILE <- file(FILENAME,"r")
    read.table(file = FILE, header = header, sep = sep,
	       row.names = row.names, col.names = col.names, quote = "", ...)
}
