#  File src/library/base/R/raw.R
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

raw <- function(length = 0L) .Internal(vector("raw", length))

#as.raw <- function(x) .Internal(as.raw(x))

charToRaw <- function(x) .Internal(charToRaw(x))
rawToChar <- function(x, multiple=FALSE) .Internal(rawToChar(x, multiple))

rawShift <- function(x, n) .Internal(rawShift(x, n))

rawToBits <- function(x) .Internal(rawToBits(x))
intToBits <- function(x) .Internal(intToBits(x))

packBits <- function(x, type=c("raw", "integer"))
{
    type <- match.arg(type)
    .Internal(packBits(x, type))
}

utf8ToInt <- function(x) .Internal(utf8ToInt(x))
intToUtf8 <- function(x, multiple=FALSE) .Internal(intToUtf8(x, multiple))
