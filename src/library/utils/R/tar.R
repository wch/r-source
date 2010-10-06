#  File src/library/utils/R/tar.R
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

untar <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                  compressed = NA, extras = NULL, verbose = FALSE,
                  tar = Sys.getenv("TAR"))
{
    if (inherits(tarfile, "connection") || identical(tar, "internal"))
        return(untar2(tarfile, files, list, exdir))

    TAR <- tar
    if (!nzchar(TAR) && .Platform$OS.type == "windows") {
        res <- tryCatch(system("tar.exe --version", intern = TRUE),
                        error = identity)
        if (!inherits(res, "error")) TAR <- "tar.exe"
    }
    if (!nzchar(TAR) || TAR == "internal")
        return(untar2(tarfile, files, list, exdir))

    cflag <- ""
    if (is.character(compressed)) {
        ## Any tar which supports -J does not need it for extraction
        switch(match.arg(compressed, c("gzip", "bzip2", "xz")),
               "gzip" = "z", "bzip2" = "j", "xz" = "J")
    } else if (is.logical(compressed)) {
        if (is.na(compressed)) {
            magic <- readBin(tarfile, "raw", n = 3)
            if(all(magic[1:2] == c(0x1f, 0x8b))) cflag <- "z"
            else if(all(magic[1:2] == c(0x1f, 0x9d))) cflag <- "z" # compress
            else if(rawToChar(magic[1:3]) == "BZh") cflag <- "j"
            else if(rawToChar(magic[1:5]) == "\xFD7zXZ") cflag <- "J"
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
    if (!gzOK && cflag == "z" && nzchar(ZIP <- Sys.getenv("R_GZIPCMD"))) {
        TAR <- paste(ZIP, "-dc", shQuote(tarfile), "|", TAR)
        tarfile <- "-"
        cflag <- ""
    }
    if (!gzOK && cflag == "j" && nzchar(ZIP <- Sys.getenv("R_BZIPCMD"))) {
        TAR <- paste(ZIP,  "-dc", shQuote(tarfile), "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (cflag == "J") {
        TAR <- paste("xz -dc", shQuote(tarfile), "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (list) {
        cmd <- paste(TAR, " -", cflag, "tf ", shQuote(tarfile), sep = "")
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) message("untar: using cmd = ", sQuote(cmd), domain = NA)
        system(cmd, intern = TRUE)
    } else {
        cmd <- paste(TAR, " -", cflag, "xf ", shQuote(tarfile), sep = "")
        if (!missing(exdir)) {
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
            cmd <- if(.Platform$OS.type == "windows")
                ## some versions of tar.exe need / here
                paste(cmd, "-C", gsub("\\", "/", exdir, fixed=TRUE))
            else
                paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (length(files))
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) message("untar: using cmd = ", sQuote(cmd), domain = NA)
        res <- system(cmd)
        if (res) warning(sQuote(cmd), " returned error code ", res,
                         domain = NA)
        invisible(res)
    }
}

untar2 <- function(tarfile, files = NULL, list = FALSE, exdir = ".")
{
    getOct <- function(x, offset, len)
    {
        x <- 0L
        for(i in offset + seq_len(len)) {
            z <- block[i]
            if(!as.integer(z)) break; # terminate on nul
            switch(rawToChar(z),
                   " " = {},
                   "0"=,"1"=,"2"=,"3"=,"4"=,"5"=,"6"=,"7"=
                   {x <- 8*x + (as.integer(z)-48)},
                   stop("invalid octal digit")
                   )
        }
        x
    }

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
        on.exit(setwd(od), add = TRUE)
    }
    contents <- character()
    llink <- lname <- NULL
    repeat{
        block <- readBin(con, "raw", n = 512L)
        if(!length(block)) break
        if(length(block) < 512L) stop("incomplete block on file")
        if(all(block == 0)) break
        ns <- max(which(block[1:100] > 0))
        name <- rawToChar(block[seq_len(ns)])
        magic <- rawToChar(block[258:262])
        if ((magic == "ustar") && block[346] > 0) {
            ns <- max(which(block[346:500] > 0))
            prefix <- rawToChar(block[345+seq_len(ns)])
            name <- file.path(prefix, name)
        }
        ## mode zero-padded 8 bytes (including nul) at 101
        ## Aargh: bsdtar has this one incorrectly with 6 bytes+space
        mode <- as.octmode(getOct(block, 100, 8))
        size <- getOct(block, 124, 12)
        ts <- getOct(block, 136, 12)
        ft <- as.POSIXct(as.numeric(ts), origin="1970-01-01", tz="UTC")
        csum <- getOct(block, 148, 6)
        block[149:156] <- charToRaw(" ")
        xx <- as.integer(block)
        checksum <- sum(xx) %% 2^24 # 6 bytes
        if(csum != checksum) {
            ## try it with signed bytes.
            checksum <- sum(ifelse(xx > 127, xx - 128, xx)) %% 2^24 # 6 bytes
            if(csum != checksum)
                warning(gettextf("checksum error for entry '%s'", name),
                        domain = NA)
        }
        type <- block[157L]
        ctype <- rawToChar(type)
        if(type == 0L || ctype == "0") {
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            contents <- c(contents, name)
            remain <- size
            dothis <- !list
            if(dothis && length(files)) dothis <- name %in% files
            if(dothis) {
                dir.create(dirname(name), showWarnings = FALSE,
                           recursive = TRUE)
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
                .Call("R_setFileTime", name, ft, PACKAGE = "base")
            }
        } else if(ctype %in% c("1", "2")) { # hard and symbolic links
            contents <- c(contents, name)
            ns <- max(which(block[158:257] > 0))
            name2 <- rawToChar(block[157L + seq_len(ns)])
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(llink)) {name2 <- llink; llink <- NULL}
            if(!list) {
                ## this will not work for links to dirs on Windows
                if(.Platform$OS.type == "windows") file.copy(name2, name)
                else file.symlink(name2, name)
            }
        } else if(ctype == "5") {
            contents <- c(contents, name)
            if(!list) {
                dir.create(name, showWarnings = FALSE, recursive = TRUE)
                Sys.chmod(name, mode)
                ## not much point, since dir will be populated afterwards
                ## .Call("R_setFileTime", name, ft)
            }
        } else if(ctype %in% c("L", "K")) {
            ## This is a GNU extension that should no longer be
            ## in use, but it is.
            name_size <- 512L * ceiling(size/512L)
            block <- readBin(con, "raw", n = name_size)
            if(length(block) < name_size)
                stop("incomplete block on file")
            ns <- max(which(block > 0)) # size on file may or may not include final nul
            if(ctype == "L")
                lname <- rawToChar(block[seq_len(ns)])
            else
                llink <- rawToChar(block[seq_len(ns)])
        } else stop("unsupported entry type ", sQuote(ctype))
    }
    if(list) contents else invisible(0L)
}

tar <- function(tarfile, files = NULL,
                compression = c("none", "gzip", "bzip2", "xz"),
                compression_level = 6, tar = Sys.getenv("tar"))
{
    if(is.character(tarfile)) {
        TAR <- tar
        if(nzchar(TAR) && TAR != "internal") {
            ## FIXME: could pipe through gzip etc: might be safer for xz
            ## as -J was lzma in GNU tar 1.20:21
            flags <- switch(match.arg(compression),
                            "none" = "cf",
                            "gzip" = "zcf",
                            "bzip2" = "jcf",
                            "xz" = "Jcf")
            cmd <- paste(TAR, flags, shQuote(tarfile),
                         paste(shQuote(files), collapse=" "))
            return(invisible(system(cmd)))
        }
        con <- switch(match.arg(compression),
                      "none" =    file(tarfile, "wb"),
                      "gzip" =  gzfile(tarfile, "wb", compress = compression_level),
                      "bzip2" = bzfile(tarfile, "wb", compress = compression_level),
                      "xz" =    xzfile(tarfile, "wb", compress = compression_level))
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")

    files <- list.files(files, recursive = TRUE, all.files = TRUE,
                        full.names = TRUE)
    ## this omits directories: get back the non-empty ones
    bf <- unique(dirname(files))
    files <- c(bf[!bf %in% c(".", files)], files)

    for (f in unique(files)) {
        info <- file.info(f)
        if(is.na(info$size)) {
            warning(gettextf("file '%s' not found", f), domain = NA)
            next
        }
        header <- raw(512L)
        ## add trailing / to dirs.
        if(info$isdir && !grepl("/$", f)) f <- paste(f, "/", sep = "")
        name <- charToRaw(f)
        if(length(name) > 100L) {
            if(length(name) > 255L) stop("file path is too long")
            s <- max(which(name[1:155] == charToRaw("/")))
            if(is.infinite(s) || s+100 < length(name))
                stop("file path is too long")
            warning("storing paths of more than 100 bytes is not portable:\n  ",
                    sQuote(f), domain = NA)
            prefix <- name[1:(s-1)]
            name <- name[-(1:s)]
            header[345+seq_along(prefix)] <- prefix
        }
        header[seq_along(name)] <- name
        header[101:107] <- charToRaw(sprintf("%07o", info$mode))
        if(!is.na(info$uid))
            header[109:115] <- charToRaw(sprintf("%07o", info$uid))
        if(!is.na(info$gid))
            header[117:123] <- charToRaw(sprintf("%07o", info$gid))
        ## size is 0 for directories and it seems for links.
        size <- ifelse(info$isdir, 0, info$size)
        header[137:147] <- charToRaw(sprintf("%011o", as.integer(info$mtime)))
        if (info$isdir) header[157L] <- charToRaw("5")
        else {
            lnk <- Sys.readlink(f)
            if(is.na(lnk)) lnk <- ""
            header[157L] <- charToRaw(ifelse(nzchar(lnk), "2", "0"))
            if(nzchar(lnk)) {
                ## we could use the GNU extension ...
                if(length(lnk) > 100L) stop("linked path is too long")
                header[157L + seq_len(nchar(lnk))] <- charToRaw(lnk)
                size <- 0
            }
        }
        header[125:135] <- charToRaw(sprintf("%011o", as.integer(size)))
        ## the next two are what POSIX says, not what GNU tar does.
        header[258:262] <- charToRaw("ustar")
        header[264:265] <- charToRaw("0")
        if(!is.na(s <- info$uname)) {
            ns <- nchar(s, "b")
            header[265L + (1:ns)] <- charToRaw(s)
        }
        if(!is.na(s <- info$grname)) {
            ns <- nchar(s, "b")
            header[297L + (1:ns)] <- charToRaw(s)
        }
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header)) %% 2^24 # 6 bytes
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        if(info$isdir || nzchar(lnk)) next
        inf <- file(f, "rb")
        for(i in seq_len(ceiling(info$size/512L))) {
            block <- readBin(inf, "raw", 512L)
            writeBin(block, con)
            if( (n <- length(block)) < 512L) writeBin(raw(512L - n), con)
        }
        close(inf)
    }
    ## trailer is two blocks of nuls.
    block <- raw(512L)
    writeBin(block, con)
    writeBin(block, con)
    invisible(0L)
}
