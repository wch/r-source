#  File src/library/utils/R/tar.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

untar <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                  compressed = NA, extras = NULL, verbose = FALSE,
                  restore_times = TRUE, tar = Sys.getenv("TAR"))
{
    if (inherits(tarfile, "connection") || identical(tar, "internal"))
        return(untar2(tarfile, files, list, exdir, restore_times))

    if (!(is.character(tarfile) && length(tarfile) == 1L))
        stop("invalid 'tarfile' argument")

    TAR <- tar
    if (!nzchar(TAR) && .Platform$OS.type == "windows" &&
        nzchar(Sys.which("tar.exe"))) TAR <- "tar.exe"
    if (!nzchar(TAR) || TAR == "internal")
        return(untar2(tarfile, files, list, exdir))

    cflag <- ""
    if (is.character(compressed)) {
        ## Any tar which supports -J does not need it for extraction
        switch(match.arg(compressed, c("gzip", "bzip2", "xz")),
               "gzip" = "z", "bzip2" = "j", "xz" = "J")
    } else if (is.logical(compressed)) {
        if (is.na(compressed)) {
            magic <- readBin(tarfile, "raw", n = 3L)
            if(all(magic[1:2] == c(0x1f, 0x8b))) cflag <- "z"
            else if(all(magic[1:2] == c(0x1f, 0x9d))) cflag <- "z" # compress
            else if(rawToChar(magic[1:3]) == "BZh") cflag <- "j"
            else if(rawToChar(magic[1:5]) == "\xFD7zXZ") cflag <- "J"
        } else if (compressed) cflag <- "z"
    } else stop("'compressed' must be logical or character")
    if (!restore_times) cflag <- paste0(cflag, "m")

    gzOK <- .Platform$OS.type == "windows"
    if (!gzOK ) {
        ## version info may be sent to stdout or stderr
        tf <- tempfile()
        ## TAR might be a command+flags, so don't quote it
        cmd <- paste0(TAR, " -", cflag, "tf ", shQuote(tarfile))
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
        cmd <- paste0(TAR, " -", cflag, "tf ", shQuote(tarfile))
        if (length(extras)) cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) message("untar: using cmd = ", sQuote(cmd), domain = NA)
        system(cmd, intern = TRUE)
    } else {
        cmd <- paste0(TAR, " -", cflag, "xf ", shQuote(tarfile))
        if (!missing(exdir)) {
            if (!dir.exists(exdir)) {
                if(!dir.create(exdir, showWarnings = TRUE, recursive = TRUE))
                    stop(gettextf("failed to create directory %s", sQuote(exdir)),
                         domain = NA)
            }
            cmd <- if(.Platform$OS.type == "windows")
                ## some versions of tar.exe need / here
                paste(cmd, "-C", shQuote(gsub("\\", "/", exdir, fixed=TRUE)))
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

untar2 <- function(tarfile, files = NULL, list = FALSE, exdir = ".",
                   restore_times = TRUE)
{
    ## might be used with len = 12, so result of more than max int
    getOctD <- function(x, offset, len)
    {
        x <- 0.0
        for(i in offset + seq_len(len)) {
            z <- block[i]
            if(!as.integer(z)) break; # terminate on nul
            switch(rawToChar(z),
                   " " = {},
                   "0"=,"1"=,"2"=,"3"=,"4"=,"5"=,"6"=,"7"=
                   {x <- 8*x + (as.integer(z)-48L)},
                   stop("invalid octal digit")
                   )
        }
        x
    }
    getOct <- function(x, offset, len)
        as.integer(getOctD(x, offset, len))
    mydir.create <- function(path, ...) {
        ## for Windows' sake
        path <- sub("[\\/]$", "", path)
        if(dir.exists(path)) return()
        if(!dir.create(path, showWarnings = TRUE, recursive = TRUE, ...))
           stop(gettextf("failed to create directory %s", sQuote(path)),
                domain = NA)
    }

    warn1 <- character()

    ## A tar file is a set of 512 byte records,
    ## a header record followed by file contents (zero-padded).
    ## See https://en.wikipedia.org/wiki/Tar_%28file_format%29
    if(is.character(tarfile) && length(tarfile) == 1L) {
        con <- gzfile(path.expand(tarfile), "rb") # reads compressed formats
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")
    if (!missing(exdir)) {
        mydir.create(exdir)
        od <- setwd(exdir)
        on.exit(setwd(od), add = TRUE)
    }
    contents <- character()
    llink <- lname <- lsize <- NULL
    repeat{
        block <- readBin(con, "raw", n = 512L)
        if(!length(block)) break
        if(length(block) < 512L) stop("incomplete block on file")
        if(all(block == 0)) break
        ## This should be non-empty, but whole name could be in prefix
        w <- which(block[1:100] > 0)
        ns <- if(length(w)) max(w) else 0
        name <- rawToChar(block[seq_len(ns)])
        magic <- rawToChar(block[258:262])
        if ((magic == "ustar") && block[346L] > 0) {
            ns <- max(which(block[346:500] > 0))
            prefix <- rawToChar(block[345L+seq_len(ns)])
            name <- file.path(prefix, name)
            ns <- nchar(name, "b")
        }
        if (ns <= 0) stop("invalid name field in tarball")
        ## mode zero-padded 8 bytes (including nul) at 101
        ## Aargh: bsdtar has this one incorrectly with 6 bytes+space
        mode <- as.octmode(getOct(block, 100, 8))
        size <- getOctD(block, 124, 12)
        ts <- getOctD(block, 136, 12)
        ft <- as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "UTC")
        csum <- getOct(block, 148, 8)
        block[149:156] <- charToRaw(" ")
        xx <- as.integer(block)
        checksum <- sum(xx) %% 2^24 # 6 bytes
        if(csum != checksum) {
            ## try it with signed bytes.
            checksum <- sum(ifelse(xx > 127L, xx - 128L, xx)) %% 2^24 # 6 bytes
            if(csum != checksum)
                warning(gettextf("checksum error for entry '%s'", name),
                        domain = NA)
        }
        type <- block[157L]
        ctype <- rawToChar(type)
#        message(sprintf("%s, %d: '%s'", ctype, size, name))
        if(type %in% c(0L, 7L) || ctype == "0") {
            ## regular or high-performance file
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(lsize)) {size <- lsize; lsize <- NULL}
            contents <- c(contents, name)
            remain <- size
            dothis <- !list
            if(dothis && length(files)) dothis <- name %in% files
            if(dothis) {
                mydir.create(dirname(name))
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
                Sys.chmod(name, mode, FALSE) # override umask
                if(restore_times) Sys.setFileTime(name, ft)
            }
        } else if(ctype %in% c("1", "2")) {
            ## hard and symbolic links
            contents <- c(contents, name)
            ns <- max(which(block[158:257] > 0))
            name2 <- rawToChar(block[157L + seq_len(ns)])
            if(!is.null(lname)) {name <- lname; lname <- NULL}
            if(!is.null(llink)) {name2 <- llink; llink <- NULL}
            if(!list) {
                if(ctype == "1") {
                    mydir.create(dirname(name))
                    unlink(name)
                    if (!file.link(name2, name)) { # will give a warning
                        ## link failed, so try a file copy
                        if(file.copy(name2, name))
                             warn1 <- c(warn1, "restoring hard link as a file copy")
                        else
                            warning(gettextf("failed to copy %s to %s", sQuote(name2), sQuote(name)), domain = NA)
                    }
                } else {
                    if(.Platform$OS.type == "windows") {
                        ## this will not work for links to dirs
                        mydir.create(dirname(name))
                        from <- file.path(dirname(name), name2)
                        if (!file.copy(from, name))
                            warning(gettextf("failed to copy %s to %s", sQuote(from), sQuote(name)), domain = NA)
                        else
                            warn1 <- c(warn1, "restoring symbolic link as a file copy")
                   } else {
                       mydir.create(dirname(name))
                       od <- setwd(dirname(name))
                       nm <- basename(name)
                       unlink(nm)
                       if(!file.symlink(name2, nm)) { # will give a warning
                        ## so try a file copy: will not work for links to dirs
                        if (file.copy(name2, nm))
                            warn1 <- c(warn1, "restoring symbolic link as a file copy")
                           else
                               warning(gettextf("failed to copy %s to %s", sQuote(from), sQuote(name)), domain = NA)
                       }
                       setwd(od)
                   }
                }
            }
        } else if(ctype %in% c("3", "4")) {
            ## 3 and 4 are devices
            warn1 <- c(warn1, "skipping devices")
        } else if(ctype == "5") {
            ## directory
            contents <- c(contents, name)
            if(!list) {
                mydir.create(name)
                Sys.chmod(name, mode, TRUE) # FIXME: check result
                ## no point is setting time, as dir will be populated later.
            }
        } else if(ctype == "6") {
            ## 6 is a fifo
            warn1 <- c(warn1, "skipping fifos")
       } else if(ctype %in% c("L", "K")) {
            ## These are GNU extensions that are widely supported
            ## They use one or more blocks to store the name of
            ## a file or link or of a link target.
            name_size <- 512L * ceiling(size/512L)
            block <- readBin(con, "raw", n = name_size)
            if(length(block) < name_size)
                stop("incomplete block on file")
            ns <- max(which(block > 0)) # size on file may or may not include final nul
            if(ctype == "L")
                lname <- rawToChar(block[seq_len(ns)])
            else
                llink <- rawToChar(block[seq_len(ns)])
        } else if(ctype == "x") {
            ## pax headers misused by bsdtar.
            isUTF8 <- FALSE
            warn1 <- c(warn1, "using pax extended headers")
            info <- readBin(con, "raw", n = 512L*ceiling(size/512L))
            info <- strsplit(rawToChar(info), "\n", fixed = TRUE)[[1]]
            hcs <- grep("[0-9]* hdrcharset=", info, useBytes = TRUE,
                        value = TRUE)
            if(length(hcs)) {
                hcs <- sub("[0-9]* hdrcharset=", hcs, useBytes = TRUE)
                isUTF8 <- identical(hcs, "ISO-IR 10646 2000 UTF-8")
            }
            path <- grep("[0-9]* path=", info, useBytes = TRUE, value = TRUE)
            if(length(path)) {
                lname <- sub("[0-9]* path=", "", path, useBytes = TRUE)
                if(isUTF8) Encoding(lname) <- "UTF-8"
            }
            linkpath <- grep("[0-9]* linkpath=", info, useBytes = TRUE,
                             value = TRUE)
            if(length(linkpath)) {
                llink <- sub("[0-9]* linkpath=", "", linkpath, useBytes = TRUE)
                if(isUTF8) Encoding(llink) <- "UTF-8"
            }
            size <- grep("[0-9]* size=", info, useBytes = TRUE, value = TRUE)
            if(length(size))
                lsize <- as.integer(sub("[0-9]* size=", "", size))
         } else if(ctype == "g") {
            warn1 <- c(warn1, "skipping pax global extended headers")
            readBin(con, "raw", n = 512L*ceiling(size/512L))
        } else stop("unsupported entry type ", sQuote(ctype))
    }
    if(length(warn1)) {
        warn1 <- unique(warn1)
        for (w in warn1) warning(w, domain = NA)
    }
    if(list) contents else invisible(0L)
}

tar <- function(tarfile, files = NULL,
                compression = c("none", "gzip", "bzip2", "xz"),
                compression_level = 6, tar = Sys.getenv("tar"),
                extra_flags = "")
{
    if(is.character(tarfile)) {
        if(nzchar(tar) && tar != "internal") {
            ## FIXME: could pipe through gzip etc: might be safer for xz
            ## as -J was lzma in GNU tar 1.20:21
            flags <- switch(match.arg(compression),
                            "none" = "-cf",
                            "gzip" = "-zcf",
                            "bzip2" = "-jcf",
                            "xz" = "-Jcf")

            if (grepl("darwin", R.version$os)) {
                ## precaution for Mac OS X to omit resource forks
                ## we can't tell the running OS version from R.version$os
                ## but at least it will not be older
                tar <- paste("COPYFILE_DISABLE=1", tar) # >= 10.5, Leopard
                if (grepl("darwin8", R.version$os)) # 10.4, Tiger
                    tar <- paste("COPY_EXTENDED_ATTRIBUTES_DISABLE=1", tar)
            }
            if (is.null(extra_flags)) extra_flags <- ""
            ## 'tar' might be a command + flags, so don't quote it
            cmd <- paste(tar, extra_flags, flags, shQuote(tarfile),
                         paste(shQuote(files), collapse=" "))
            return(invisible(system(cmd)))
        }
        con <- switch(match.arg(compression),
                      "none" =    file(tarfile, "wb"),
                      "gzip" =  gzfile(tarfile, "wb", compression = compression_level),
                      "bzip2" = bzfile(tarfile, "wb", compression = compression_level),
                      "xz" =    xzfile(tarfile, "wb", compression = compression_level))
        on.exit(close(con))
    } else if(inherits(tarfile, "connection")) con <- tarfile
    else stop("'tarfile' must be a character string or a connection")

    ## FIXME: eventually we should use the pax extension, but
    ## that was first supported in R 2.15.3.
    GNUname <- function(name, link = FALSE)
    {
        header <- raw(512L)
        n1 <- charToRaw("ExtendedName")
        header[seq_along(n1)] <- n1
        header[157L] <- charToRaw(ifelse(link, "K", "L"))
        size <- length(name)
        header[125:135] <- charToRaw(sprintf("%011o", as.integer(size)))
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header)) %% 2^24 # 6 bytes
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        writeBin(name, con)
        ssize <- 512L * ceiling(size/512L)
        if(ssize > size) writeBin(raw(ssize - size), con)
    }
    warn1 <- character()

    files <- list.files(files, recursive = TRUE, all.files = TRUE,
                        full.names = TRUE, include.dirs = TRUE)

    invalid_uid <- invalid_gid <- FALSE
    for (f in unique(files)) {
        info <- file.info(f)
        if(is.na(info$size)) {
            warning(gettextf("file '%s' not found", f), domain = NA)
            next
        }
        header <- raw(512L)
        ## add trailing / to dirs.
        if(info$isdir && !grepl("/$", f)) f <- paste0(f, "/")
        name <- charToRaw(f)
        if(length(name) > 100L) {
            OK <- TRUE
            ## best possible case: 155+/+100
            if(length(name) > 256L) OK <- FALSE
            else {
                ## do not want to split on terminal /
                m <- length(name)
                s <- max(which(name[1:min(156, m - 1L)] == charToRaw("/")))
                if(is.infinite(s) || s + 100L < length(name)) OK <- FALSE
            }
            warning("storing paths of more than 100 bytes is not portable:\n  ",
                    sQuote(f), domain = NA)
            if (OK) {
                prefix <- name[1:(s-1L)]
                name <- name[-(1:s)]
                header[345L+seq_along(prefix)] <- prefix
            } else {
                GNUname(name)
                name <- charToRaw("dummy")
                warn1 <- c(warn1, "using GNU extension for long pathname")
            }
        }
        header[seq_along(name)] <- name
        mode <- info$mode
        ## for use by R CMD build
        if (is.null(extra_flags) && grepl("/(configure|cleanup)$", f) &&
            (mode & "111") != as.octmode("111")) {
            warning(gettextf("file '%s' did not have execute permissions: corrected", f), domain = NA, call. = FALSE)
            mode <- mode | "111"
        }
        header[101:107] <- charToRaw(sprintf("%07o", mode))
        ## Windows does not have uid, gid: defaults to 0, which isn't great
        uid <- info$uid
        ## uids are supposed to be less than 'nobody' (32767)
        ## but it seems there are broken ones around: PR#15436
        if(!is.null(uid) && !is.na(uid)) {
            if(uid < 0L || uid > 32767L) {invalid_uid <- TRUE; uid <- 32767L}
            header[109:115] <- charToRaw(sprintf("%07o", uid))
        }
        gid <- info$gid
        if(!is.null(gid) && !is.na(gid)) {
            if(gid < 0L || gid > 32767L) {invalid_gid <- TRUE; gid <- 32767L}
            header[117:123] <- charToRaw(sprintf("%07o", gid))
	}
        header[137:147] <- charToRaw(sprintf("%011o", as.integer(info$mtime)))
        if (info$isdir) header[157L] <- charToRaw("5")
        else {
            lnk <- Sys.readlink(f)
            if(is.na(lnk)) lnk <- ""
            header[157L] <- charToRaw(ifelse(nzchar(lnk), "2", "0"))
            if(nzchar(lnk)) {
                if(nchar(lnk, "b") > 100L) {
                    ##  stop("linked path is too long")
                    GNUname(charToRaw(lnk), TRUE)
                    warn1 <- c(warn1, "using GNU extension for long linkname")
                    lnk <- "dummy"
                }
                header[157L + seq_len(nchar(lnk))] <- charToRaw(lnk)
                size <- 0
            }
        }
        ## size is 0 for directories and it seems for links.
        size <- ifelse(info$isdir, 0, info$size)
        if(size >= 8^11) stop("file size is limited to 8GB")
        header[125:135] <- .Call(C_octsize, size)
        ## the next two are what POSIX says, not what GNU tar does.
        header[258:262] <- charToRaw("ustar")
        header[264:265] <- charToRaw("0")
        ## Windows does not have uname, grname
        s <- info$uname
        if(!is.null(s) && !is.na(s)) {
            ns <- nchar(s, "b")
            header[265L + (1:ns)] <- charToRaw(s)
        }
        s <- info$grname
        if(!is.null(s) && !is.na(s)) {
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
    if (invalid_uid)
        warning(gettextf("invalid uid value replaced by that for user 'nobody'", uid),
                domain = NA, call. = FALSE)
    if (invalid_gid)
        warning(gettextf("invalid gid value replaced by that for user 'nobody'", uid),
                domain = NA, call. = FALSE)
    ## trailer is two blocks of nuls.
    block <- raw(512L)
    writeBin(block, con)
    writeBin(block, con)
    if(length(warn1)) {
        warn1 <- unique(warn1)
        for (w in warn1) warning(w, domain = NA)
    }
    invisible(0L)
}
