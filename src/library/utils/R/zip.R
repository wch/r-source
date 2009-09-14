#  File src/library/utils/R/zip.R
#  Part of the R package, http://www.R-project.org
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

zip.file.extract <- function(file, zipname = "R.zip",
			     unzip = getOption("unzip"), dir = tempdir())
{
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        if(!is.character(unzip) || length(unzip) != 1L)
            stop("'unzip' must be a single character string")
        if(!nzchar(unzip)) unzip <- "internal"
        if(unzip != "internal") {
            cmd <- paste(unzip, "-oq", shQuote(file.path(path, zipname)),
                         topic, " -d ", dir)
            ## there is an unzip clone about that does not respect -q, so
            ## use redirection here.
            res <- if(.Platform$OS.type == "windows")
                system(cmd, invisible = TRUE) else system(paste(cmd, "> /dev/null"))
            if(!res) file <- file.path(dir, topic)
        } else {
            rc <- .Internal(unzip(file.path(path, zipname), topic, dir,
                                  FALSE, TRUE, FALSE))
            if (rc == 0L)
                file <- file.path(dir, topic)
        }
    }
    file
}

unzip <-
    function(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".")
{
    if(!list && !missing(exdir))
        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
    res <- .Internal(unzip(zipfile, files, exdir, list, overwrite, junkpaths))
    if(list) {
        dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M",  tz="UTC")
        data.frame(Name = res[[1]], Length = res[[2]], Date = dates)
    } else invisible(attr(res, "extracted"))
}

untar <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                  compressed = NA, extra = NULL, verbose = FALSE)
{
    if (is.character(compressed)) {
        switch(match.arg(compressed, c("gzip", "bzip2")),
               "gzip" = "z", "bzip2" = "j")
    } else if (is.logical(compressed)) {
        if (is.na(compressed)) {
            if (grepl("[.](gz|tgz|taz|Z)$", tarfile)) cflag <- "z"
            else if (grepl("[.]bz2$", tarfile)) cflag <- "j"
        } else if (compressed) cflag <- "z"
    } else stop("'compressed' must be logical or character")

    gzOK <- .Platform$OS.type == "windows"
    TAR <- Sys.getenv("TAR")
    if (!nzchar(TAR) && .Platform$OS.type == "windows") {
        res <- try(system("tar.exe --version", intern = TRUE), silent = TRUE)
        TAR <- if (!inherits(res, "try-error")) "tar.exe"
        else {
            TAR <- file.path(R.home(), "bin", "untgz.exe")
            ## This is rather different.
            if (cflag == "j")
                stop("'bzip2 compression is not supported by untgz.exe",
                     domain = NA)
            if (list) {
                cmd <- paste(TAR, "-l", shQuote(tarfile))
                if (verbose) message("untar: using cmd = ", sQuote(cmd))
                return(system(cmd, intern = TRUE))
            } else {
                ## NB only absolute [aths for tarfile will work
                tarfile <- chartr("\\", "/", normalizePath(tarfile))
                if (!missing(exdir)) {
                    dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
                    od <- setwd(exdir)
                    on.exit(setwd(od))
                }
                cmd <- paste(TAR, shQuote(tarfile))
                if (length(files))
                    cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
                if (verbose) message("untar: using cmd = ", sQuote(cmd))
                res <- system(cmd)
                if (res) warning(sQuote(cmd), " returned error code ", res,
                                 domain = NA)
                return(invisible(res))
            }
        }
    }
    if (!nzchar(TAR))
        stop("set evironment variable 'TAR' to point to a GNU-compatible 'tar'")

    if (!gzOK ) {
        ## version info may be sent to stdout or stderr
        tf <- tempfile()
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        if (length(extra)) cmd <- paste(cmd, extra, collapse = " ")
        system(cmd, intern = TRUE)
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        if (length(extra)) cmd <- paste(cmd, extra, collapse = " ")
        system(cmd, intern = TRUE)
        system(paste(TAR, "--version >", tf, "2>&1"))
        if (file.exists(tf)) {
            gzOK <- any(grepl("GNU", readLines(tf), fixed = TRUE))
            unlink(tf)
        }
    }
    tarfile <- path.expand(tarfile)
    cflag <- ""
    if (!gzOK && cflag == "z" && nzchar(ZIP <- Sys.getenv("R_GZIPCMD"))) {
        TAR <- paste(ZIP, "-dc", tarfile, "|", TAR)
        tarfile <- "-"
        cflag <- ""
    }
    if (!gzOK && cflag == "j") {
        TAR <- paste("bzip2 -dc", tarfile, "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (list) {
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        if (length(extra)) cmd <- paste(cmd, extra, collapse = " ")
        system(cmd, intern = TRUE)
    } else {
        cmd <- paste(TAR, " -", cflag, "xf ", shQuote(tarfile), sep = "")
        if (!missing(exdir)) {
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
            cmd <- paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extra)) cmd <- paste(cmd, extra, collapse = " ")
        if (length(files))
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        res <- system(cmd)
        if (res) warning(sQuote(cmd), " returned error code ", res,
                         domain = NA)
        invisible(res)
    }
}
