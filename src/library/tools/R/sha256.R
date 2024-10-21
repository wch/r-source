#  File src/library/tools/R/sha256.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

sha256sum <- function(files, bytes) {
    if (!missing(files) && !missing(bytes))
        stop("'files' and 'bytes' are mutually exclusive")
    if (!missing(bytes)) {
        if (!is.raw(bytes)) stop("'bytes' must be a raw vector")
        .Call(C_Rsha256, bytes)
    } else {
        files <- path.expand(files)
        structure(.Call(C_Rsha256, files), names=files)
    }
}

# The following fns are neither used nor exported - for now.

.installSHA256sums <- function(pkgDir, outDir = pkgDir)
{
    dot <- getwd()
    if (is.null(dot))
        stop("current working directory cannot be ascertained")
    setwd(pkgDir)
    x <- sha256sum(dir(".", recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "SHA256"]
    cat(paste(x, names(x), sep=" *"), sep="\n",
        file=file.path(outDir, "SHA256"))
}

checkSHA256sums <- function(package, dir)
{
    if(missing(dir)) dir <- find.package(package, quiet = TRUE)
    if(length(dir) != 1L) return(NA)
    sha256file <- file.path(dir, "SHA256")
    if(!file.exists(sha256file)) return(NA)
    inlines <- readLines(sha256file)
    ## now split on the first space.
    xx <- sub("^([0-9a-fA-F]*)(.*)", "\\1", inlines)
    nmxx <- names(xx) <- sub("^[0-9a-fA-F]* [ |*](.*)", "\\1", inlines)
    dot <- getwd()
    if (is.null(dot))
        stop("current working directory cannot be ascertained")
    setwd(dir)
    x <- sha256sum(dir(dir, recursive = TRUE))
    setwd(dot)
    x <- x[names(x) != "SHA256"]
    nmx <- names(x)
    res <- TRUE
    not.here <- (nmxx %notin% nmx)
    if(any(not.here)) {
        res <- FALSE
        if (sum(not.here) > 1L)
            cat("files", paste(sQuote(nmxx[not.here]), collapse = ", "),
                "are missing\n", sep = " ")
        else
            cat("file", sQuote(nmxx[not.here]), "is missing\n", sep = " ")
    }
    nmxx <- nmxx[!not.here]
    diff <- xx[nmxx] != x[nmxx]
    if(any(diff)) {
        res <- FALSE
        files <- nmxx[diff]
        if(length(files) > 1L)
            cat("files", paste(sQuote(files), collapse = ", "),
                "have the wrong SHA256 checksums\n", sep = " ")
        else cat("file", sQuote(files), "has the wrong SHA256 checksum\n")
    }
    res
}

.hex.chars <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")

hex2raw <- function(x) {
  if (length(x) != 1L) stop("x must be a single string")
  if (!nzchar(x)) return(raw(1L))
  ## pad with 0 to full bytes
  m <- match(strsplit(tolower(x),"")[[1L]], .hex.chars)
  if (any(is.na(m))) stop("invalid hex string")
  if (length(m) %% 2 == 1) m <- c(1L, m) ## add leading 0 for full byte
  as.raw(colSums(matrix(m - 1L, 2) * c(16L, 1L)))
}

.pad <- function(x, n) if (length(x) < n) c(x, raw(n - length(x))) else x

hmac <- function(key, x, hash, block) {
  key <- .pad(if (length(key) > block) hex2raw(hash(key)) else key, block)
  # HMAC := HASH( c( key ^ 0x5c, HASH( c( key ^ 0x36, x ) ) ) )
  hash(c(xor(key, as.raw(0x5c)),
       hex2raw(hash(c(xor(key, as.raw(0x36)), x)))))
}

hmac.sha256 <- function(key, x) hmac(key, x, function(x) sha256sum(bytes=x), 64L)
hmac.md5 <- function(key, x) hmac(key, x, function(x) md5sum(bytes=x), 64L)
