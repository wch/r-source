#  File src/library/utils/R/hashtab.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

hashtab <- function(type = c("identical", "address"), size) {
    K <- if (missing(size)) 3 else ceiling(log2(2 * size))
    type <- match.arg(type)
    .External(C_hashtab_Ext, type, K)
}

gethash <- function(h, key, nomatch = NULL)
    .External(C_gethash_Ext, h, key, nomatch)

sethash <- function(h, key, value)
    invisible(.External(C_sethash_Ext, h, key, value))

remhash <- function(h, key)
    invisible(.External(C_remhash_Ext, h, key))

numhash <- function(h)
    .External(C_numhash_Ext, h)

typhash <- function(h)
    .External(C_typhash_Ext, h)

maphash <- function(h, FUN)
    invisible(.External(C_maphash_Ext, h, FUN))

clrhash <- function(h)
    invisible(.External(C_clrhash_Ext, h))

is.hashtab <- function(x) .External(C_ishashtab_Ext, x)

format.hashtab <- function(x, ...) {
    addr <- sub("<.*: (.*)>", "\\1", format(unclass(x)[[1]]))
    num <- numhash(x)
    type <- typhash(x)
    sprintf("<hashtable %s: count = %d, type = \"%s\">", addr, num, type)
}

print.hashtab <- function(x, ...) {
    cat(format(x), "\n", sep = "")
    invisible(x)
}

`[[.hashtab` <- function(h, key, nomatch = NULL, ...)
    gethash(h, key, nomatch)

`[[<-.hashtab` <- function(h, key, ..., value) {
    sethash(h, key, value)
    h
}

as.environment.hashtab <- function(x)
    stop("invalid object for 'as.environment'")

length.hashtab <- function(x) numhash(x)

str.hashtab <- function(object, ...) cat(format(object, ...), "\n")
## or rather
str.hashtab <- function(object, ...) 
    cat(sprintf("class 'hashtab': %d entries; type=\"%s\", addr=%s",
                numhash(object), typhash(object), format(unclass(object)[[1]])), "\n")
