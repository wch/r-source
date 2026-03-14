#  File src/library/utils/R/bitstring.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2026 The R Core Team
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


## User-friendly wrapper for intToBits(), numToBits(), etc., for easy
## inspection of bit-level representation of R atomic types (numeric, integer)


.tobitstring <- function(x, size)
{
    m <- as.integer(x) |> matrix(nrow = size)
    apply(m[size:1, , drop = FALSE], 2, paste, collapse = "")
}

bitstring <- function(x) UseMethod("bitstring")

bitstring.default <- function(x) stop("Unsupported object")

bitstring.raw <- function(x)
{
    structure(rawToBits(x) |> .tobitstring(8),
              class = "bitstring", type = "uint8", nbits = 8L)
}

bitstring.integer <- function(x)
{
    structure(intToBits(x) |> .tobitstring(32),
              class = "bitstring", type = "int32", nbits = c(1L, 31L))
}

bitstring.numeric <- function(x)
{
    structure(numToBits(x) |> .tobitstring(64),
              class = "bitstring", type = "binary64", nbits = c(1L, 11L, 52L))
}

format.bitstring <- function(x, sep = " ", ...)
{
    bpos <- cumsum(c(1, attr(x, "nbits")))
    s <- head(bpos, -1)
    e <- tail(bpos, -1) - 1L
    if (length(s) == 1)
        ans <- NextMethod("format")
    else {
        ans <- substring(x, s[[1]], e[[1]])
        for (i in seq_along(s)[-1]) {
            ans <- paste0(ans, sep, substring(x, s[[i]], e[[i]]))
        }
    }
    ans
}

print.bitstring <- tools::.print.via.format

