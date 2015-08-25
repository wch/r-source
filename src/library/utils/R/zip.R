#  File src/library/utils/R/zip.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

unzip <-
    function(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".", unzip = "internal",
             setTimes = FALSE)
{
    if(identical(unzip, "internal")) {
        if(!list && !missing(exdir))
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        res <- .External(C_unzip, zipfile, files, exdir, list, overwrite,
                         junkpaths, setTimes)
        if(list) {
            dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M",  tz="UTC")
            data.frame(Name = res[[1]], Length = res[[2]], Date = dates,
                       stringsAsFactors = FALSE)
        } else invisible(attr(res, "extracted"))
    } else {
        WINDOWS <- .Platform$OS.type == "windows"
        if(!is.character(unzip) || length(unzip) != 1L || !nzchar(unzip))
            stop("'unzip' must be a single character string")
        zipfile <- path.expand(zipfile)
        if (list) {
            res <- if (WINDOWS)
                    system2(unzip, c("-l", shQuote(zipfile)), stdout = TRUE)
                else
                    system2(unzip, c("-l", shQuote(zipfile)), stdout = TRUE,
                            env = c("TZ=UTC"))
            l <- length(res)
            res2 <- res[-c(1,3, l-1, l)]
            con <- textConnection(res2); on.exit(close(con))
            z <- read.table(con, header=TRUE, as.is=TRUE)
            dt <- paste(z$Date, z$Time)
            ## Unzip 6.00 always uses 4-digits years, but any order is
            ## possible and the separator could be - or / (depending
            ## on the locale on Windows).
            ## Unzip 5.52 uses 2-digit years, but default to "%m-%d-%y" on
            ## most platforms (but is locale-dependent on Windows).
            formats <-
                if (max(nchar(z$Date) > 8))
                    c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y") else
                    ## At this point we are guessing: there is no way
                    ## to know what "08-09-10" means.  Take the most common
                    ## default first.
                    c("%m-%d-%y", "%d-%m-%y", "%y-%m-%d")
            slash <- any(grepl("/", z$Date))
            if (slash) formats <- gsub("-", "/", formats)
            formats <- paste(formats, "%H:%M")
            for (f in formats) {
                zz <- as.POSIXct(dt, tz="UTC", format = f)
                if (all(!is.na(zz))) break
            }
            z[, "Date"] <- zz
            z[c("Name", "Length", "Date")]
        } else {
            args <- c("-oq", shQuote(zipfile))
            if (length(files)) args <- c(args, shQuote(files))
            if (exdir != ".") args <- c(args, "-d", shQuote(exdir))
            ## there is an unzip clone about that does not respect -q
            system2(unzip, args, stdout = NULL, stderr = NULL,
                    invisible = TRUE)
            invisible(NULL)
        }
    }
}

zip <- function(zipfile, files, flags = "-r9X", extras = "",
                zip = Sys.getenv("R_ZIPCMD", "zip"))
{
    if (missing(flags) && (!is.character(files) || !length(files)))
        stop("'files' must a character vector specifying one or more filepaths")
    args <- c(flags, shQuote(path.expand(zipfile)),
              shQuote(files), extras)
    if (.Platform$OS.type == "windows")
        invisible(system2(zip, args, invisible = TRUE))
    else invisible(system2(zip, args))
}

