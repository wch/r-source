#  File src/library/utils/R/page.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

page <- function(x, method = c("dput", "print"), ...)
{
    ## local functions to parcel out '...'
    local.file.show <- function(file, title = subx, delete.file = TRUE,
                                pager = getOption("pager"), ...)
        file.show(file, title = title, delete.file = delete.file, pager = pager)
    local.dput <- function(x, file, title, delete.file, pager, ...)
        dput(x, file, ...)
    local.print <- function(x, title, delete.file, pager, ...)
        print(x, ...)

    if(is.character(x) && length(x) == 1L) {
        subx <- x
        parent <- parent.frame()
        if(exists(subx, envir = parent)) # inherits=TRUE is default
            x <- get(subx, envir = parent)
        else
            stop(gettextf("no object named '%s' to show", x), domain = NA)
    } else {
        subx <- deparse(substitute(x))
    }
    file <- tempfile("Rpage.")
    if(match.arg(method) == "dput")
        local.dput(x, file, ...)
    else {
        sink(file)
        local.print(x, ...)
        sink()
    }
    local.file.show(file, ...)
}
