#  File src/library/base/R/scan.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

scan <-
function(file = "", what = double(), nmax = -1L, n = -1L, sep = "",
         quote = if(identical(sep, "\n")) "" else "'\"",
         dec = ".", skip = 0L, nlines = 0L,
         na.strings = "NA", flush = FALSE, fill = FALSE,
         strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE,
         multi.line = TRUE, comment.char = "", allowEscapes = FALSE,
         fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)
{
    na.strings <- as.character(na.strings)# allow it to be NULL
    if(!missing(n)) {
        if(missing(nmax))
            nmax <- n / pmax(length(what), 1L)
        else
            stop("either specify 'nmax' or 'n', but not both.")
    }
    if (missing(file) && !missing(text)) {
	file <- textConnection(text, encoding = "UTF-8")
	encoding <- "UTF-8"
	on.exit(close(file))
    }

    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- if(nzchar(fileEncoding))
                file(file, "r", encoding = fileEncoding) else file(file, "r")
            on.exit(close(file))
        }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
                   na.strings, flush, fill, strip.white, quiet,
                   blank.lines.skip, multi.line, comment.char,
                   allowEscapes, encoding, skipNul))
}
