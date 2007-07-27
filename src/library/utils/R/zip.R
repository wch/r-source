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
                             unzip = getOption("unzip"))
{
    if(!is.character(unzip) || length(unzip) != 1)
        stop("'unzip' must be a single character string")
    if(!nzchar(unzip)) unzip <- "internal"
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tmpd <- tempdir()
        if(unzip != "internal") {
            cmd <- paste(unzip, "-oq", shQuote(file.path(path, zipname)),
                         topic, " -d ", tmpd)
            ## there is an unzip clone about that does not respect -q, so
            ## use redirection here.
            res <- if(.Platform$OS.type == "windows")
                system(cmd, invisible = TRUE) else system(paste(cmd, "> /dev/null"))
            if(!res) file <- file.path(tmpd, topic)
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tmpd))
            if (rc == 0)
                file <- file.path(tmpd, topic)
        }
    }
    file
}
