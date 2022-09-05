#  File src/library/utils/R/mach-o.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
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

## This file contains tools to handle Mach Object File Format,
## most commonly used as binary format for libraries and executables
## on Mach-based operating systems such as macOS.
## All functions are currently internal and not exported.

## macDynLoads - list LC_LOAD_DYLIB entries from a Mach-O file.
##
## Supports both fat and thin files and tries to be as silent
## as possible.
## filename - can be a seekable connection or a string.
## arch - used for fat files, if missing, defaults to the
##        architecture of this R and if not found, picks
##        the first one in the arch list. If specified
##        and not matched in the fat file, returns NULL.
## info - if TRUE returns architecture info (either from
##        the fat header or the Mach-O header of a thin file)
##
## Returns NULL if something is wrong (not a Mach-O file,
## errors in the structures, arch not found etc.) or
## a character vector of the load entries.
macDynLoads <- function(filename, arch, info=FALSE) {
    if (inherits(filename, "connection")) {
        if (!isSeekable(f))
            stop("Source must be a seekable connection")
        f <- filename
    } else {
        f <- file(path.expand(filename), "rb")
        on.exit(close(f))
    }
    if (missing(arch))
        arch <- NULL
    
    magic <- readBin(f, 1L, 1L, endian="big")

    magic.fat  <- -889275714L  # 0xcafebabe
    magic.mh   <- -17958194L   # 0xfeedface
    magic.hm   <- -822415874L  # 0xcefaedfe (inv)
    magic.mh64 <- -17958193L   # 0xfeedfacf
    magic.hm64 <- -805638658L  # 0xcffaedfe (inv)

    cpu.types <- c(vax=1L, i386=7L, x86_64=0x1000007L,
                   arm=12L, arm64=0x100000cL, `arm64-32`=0x200000cL,
                   sparc=14L, ppc=18L, ppc64=0x1000012)

    ## convert 32-bit unsigned int to R's double
    i2r <- function(i) ifelse(i < 0, 4294967296 + i, i)

    ## match cpu type to its name
    cpu2name <- function(ct) {
        cpu <- match(ct, cpu.types)
        if (!is.na(cpu)) names(cpu.types)[cpu] else "<unknown>"
    }

    ## fat header - listing archs and offsets
    ## we try to find arch matching this R session, otherwise we use
    ## the first entry
    if (magic == magic.fat) {
        n.archs <- readBin(f, 1L, 1L, endian="big")
        if (n.archs < 0)
            return(list())
        archs <- replicate(n.archs, {
            ai <- i2r(readBin(f, 1L, 5L, endian="big"))
            ## cputype, subtype, offset, size, align
            list(cpu=cpu2name(ai[1L]), type=ai[1:2], offset=ai[3L], size=ai[4L])
        }, FALSE)
        names(archs) <- sapply(archs, function(o)o $cpu)
        if (isTRUE(info))
            return(archs)
        ## find matching arch
        if (is.null(arch)) {
            r.arch <- archs[[R.Version()$arch]]
            ## if not, simply use the first one
            if (is.null(r.arch)) r.arch <- archs[[1L]]
        } else
            r.arch <- archs[[arch]]
        if (is.null(r.arch))
            return(NULL)
        seek(f, r.arch$offset, "start", "read")
        magic <- readBin(f, 1L, 1L, endian="big")
    }

    ## Mach-O file
    if (magic == magic.mh || magic == magic.hm ||
        magic == magic.mh64 || magic == magic.hm64) {
        end <- if (magic == magic.mh || magic == magic.mh64) "big" else "little"
        api <- if (magic == magic.mh || magic == magic.hm) 32L else 64L
        mh <- i2r(readBin(f, 1L, 6L + if (api == 64L) 1L else 0L, endian=end))
        ## cputype, subtype, filetype, ncmds, sizeofcmds, flags[, res (64 only)]
        if (isTRUE(info)) {
            res <- list(list(cpu=cpu2name(mh[1L]), type=mh[1:2]))
            names(res) <- res[[1]]$cpu
            return(res)
        }
        unlist(replicate(mh[4L], {
            ## cmd, size
            lc <- i2r(readBin(f, 1L, 2L, endian=end))
            if (lc[1L] == 12L && lc[2L] > 24L) { ## LC_LOAD_DYLIB
                ld <- i2r(readBin(f, 1L, 4L, endian=end))
                ## name-offset, timestamp, current_ver, compat_ver
                r <- readBin(f, raw(), lc[2L] - 24L)
                o <- ld[1L] - 24
                if (o < length(r) && o > 0)
                    r <- r[seq.int(o + 1L, length(r))]
                if (any(r == 0L))
                    r <- r[1:(which(r == 0L)[1L] - 1L)]
                rawToChar(r)
            } else {
                seek(f, lc[2L] - 8, "current", "read")
                NULL
            }
        }))
    }
}


## takes a path to a Mach-O file that is expected to link to
## X11 and checks whether the libX11 load entry points to an
## existing file. If not, raises an error informing the user
## to install XQuartz.
check_for_XQuartz <- function(DSO)
{
    if (file.exists(DSO)) {
        loads <- macDynLoads(DSO)
        if (length(loads)) {
            ## only consider absolute paths (it could also be @xxx or relative)
            ind <- grep("^/.*libX11[.][0-9]+[.]dylib", loads)
            if (length(ind)) {
                this <- loads[ind]
                if (!file.exists(this))
                    stop("X11 library is missing: install XQuartz from www.xquartz.org", domain = NA)
            }
        }
    }
}
