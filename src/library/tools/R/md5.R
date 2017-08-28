#  File src/library/tools/R/md5.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

md5sum <- function(files)
    structure(.Call(C_Rmd5, files), names=files)

.installMD5sums <- function(pkgDir, outDir = pkgDir)
{
    dot <- getwd()
    if (is.null(dot))
        stop("current working directory cannot be ascertained")
    setwd(pkgDir)
    x <- md5sum(dir(".", recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    cat(paste(x, names(x), sep=" *"), sep="\n",
        file=file.path(outDir, "MD5"))
}

checkMD5sums <- function(package, dir)
{
    if(missing(dir)) dir <- find.package(package, quiet = TRUE)
    if(!length(dir)) return(NA)
    md5file <- file.path(dir, "MD5")
    if(!file.exists(md5file)) return(NA)
    inlines <- readLines(md5file)
    ## now split on the first space.
    xx <- sub("^([0-9a-fA-F]*)(.*)", "\\1", inlines)
    nmxx <- names(xx) <- sub("^[0-9a-fA-F]* [ |*](.*)", "\\1", inlines)
    dot <- getwd()
    if (is.null(dot))
        stop("current working directory cannot be ascertained")
    setwd(dir)
    x <- md5sum(dir(dir, recursive = TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
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
                "have the wrong MD5 checksums\n", sep = " ")
        else cat("file", sQuote(files), "has the wrong MD5 checksum\n")
    }
    res
}
