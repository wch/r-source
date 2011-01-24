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
             junkpaths = FALSE, exdir = ".", unzip = "internal")
{
    if(identical(unzip, "internal")) {
        if(!list && !missing(exdir))
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        res <- .Internal(unzip(zipfile, files, exdir, list, overwrite, junkpaths))
        if(list) {
            dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M",  tz="UTC")
            data.frame(Name = res[[1]], Length = res[[2]], Date = dates)
        } else invisible(attr(res, "extracted"))
    } else {
        if(!is.character(unzip) || length(unzip) != 1L || !nzchar(unzip))
            stop("'unzip' must be a single character string")
        if (list) {
            res <- system2(unzip, c("-l", shQuote(zipfile)), stdout = TRUE,
                           env = c("TZ=UTC"))
            l <- length(res)
            res2 <- res[-c(1,3, l-1, l)]
            con <- textConnection(res2); on.exit(close(con))
            z <- read.table(con, header=TRUE)
            z[, "Date"] <- as.POSIXct(paste(z$Date, z$Time),
                                      "%m-%d-%y %H:%M",  tz="UTC")
            z[c("Name", "Length", "Date")]
        } else {
            args <- c("-oq", shQuote(zipfile))
            if (length(files)) args <- c(args, shQuote(files))
            if (exdir != ".") args <- c(args, "-d", shQuote(exdir))
            ## there is an unzip clone about that does not respect -q
            system2(unzip, args, stdout = NULL, stderr = NULL)
            invisible(NULL)
        }
    }
}
