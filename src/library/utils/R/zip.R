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
                  compressed = NA, extras = NULL, verbose = FALSE)
{
    TAR <- Sys.getenv("TAR")
    if (inherits(tarfile, "connection") || identical(TAR, "internal"))
        return(untar2(tarfile, files, list, exdir))

    if (!nzchar(TAR) && .Platform$OS.type == "windows") {
        res <- try(system("tar.exe --version", intern = TRUE), silent = TRUE)
        if (!inherits(res, "try-error")) TAR <- "tar.exe"
    }
    if (!nzchar(TAR)) return(untar2(tarfile, files, list, exdir))

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
    if (!gzOK ) {
        ## version info may be sent to stdout or stderr
        tf <- tempfile()
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
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
    if (!gzOK && cflag == "j" && nzchar(ZIP <- Sys.getenv("R_BZIPCMD"))) {
        TAR <- paste(ZIP,  "-dc", tarfile, "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (list) {
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        system(cmd, intern = TRUE)
    } else {
        cmd <- paste(TAR, " -", cflag, "xf ", shQuote(tarfile), sep = "")
        if (!missing(exdir)) {
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
            cmd <- paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (length(files))
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) message("untar: using cmd = ", sQuote(cmd))
        res <- system(cmd)
        if (res) warning(sQuote(cmd), " returned error code ", res,
                         domain = NA)
        invisible(res)
    }
}

untar2 <- function(tarfile, files = NULL, list = FALSE, exdir = ".")
{
    ## A tar file is a set of 512 byte records,
    ## a header record followed by file contents (zero-padded).
    ## See http://en.wikipedia.org/wiki/Tar_%28file_format%29
    if(is.character(tarfile)) {
        con <- gzfile(path.expand(tarfile), "rb") # reads compressed formats
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")
    if (!missing(exdir)) {
        dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        od <- setwd(exdir)
        on.exit(setwd(od))
    }
    contents <- character()
    repeat{
        block <- readBin(con, "raw", n = 512L)
        if(!length(block)) break
        if(length(block) < 512L) stop("incomplete block on file")
        if(all(block == 0)) break
        ns <- max(which(block[1:100] > 0))
        name <- rawToChar(block[seq_len(ns)])
        ## mode 8 bytes (including nul) at 101
        mode <- rawToChar(block[101:107])
        ## size 12 bytes at 125, octal, zero/space padded
        size <- 0L
        for(i in 124L+(1:12))  {
            z <- block[i]
            if(!as.integer(z)) break;
            switch(rawToChar(z),
                   " " = {},
                   "0"=,"1"=,"2"=,"3"=,"4"=,"5"=,"6"=,"7"=
                   {size <- 8*size + (as.integer(z)-48)},
                   stop("invalid octal digit in size")
                   )
        }
        ts <- rawToChar(block[137:146])
        ft <- as.POSIXct(as.numeric(ts), origin="1970-01-01")
        type <- block[157L]
        if(type == 48 || type == 0) {
            contents <- c(contents, name)
            remain <- size
            dothis <- !list
            if(dothis && length(files)) dothis <- name %in% files
            if(dothis) {
                dir.create(dirname(name), showWarning = FALSE, recursive = TRUE)
                out <- file(name, "wb")
            }
            for(i in seq_len(ceiling(size/512L))) {
                block <- readBin(con, "raw", n = 512L)
                if(length(block) < 512L)
                    stop("incomplete block on file")
                if (dothis) {
                    writeBin(block[seq_len(min(512L, remain))], out)
                    remain <- remain - 512L
                }
            }
            if(dothis) {
                close(out)
                Sys.chmod(name, mode)
                .Call("R_setFileTime", name, ft)
            }
        } else if(type == 49 || type == 50) { # hard and symbolic links
            contents <- c(contents, name)
            ns <- max(which(block[158:257] > 0))
            name2 <- rawToChar(block[158:ns])
            if(!list) {
                ## this will not work for links to dirs on Windows
                if(.Platform$OS.type == "windows") file.copy(name2, name)
                else file.symlink(name2, name)
            }
        } else if(type == 53) {
            contents <- c(contents, name)
            if(!list) {
                dir.create(name, showWarning = FALSE, recursive = TRUE)
                Sys.chmod(name, mode)
                ## .Call("R_setFileTime", name, ft)
            }
        } else stop("unsupported entry type", sQuote(rawToChar(type)))
    }
    if(list) contents else invisible(contents)
}
