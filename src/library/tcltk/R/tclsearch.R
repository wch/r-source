#  File src/library/tcltk/R/tclsearch.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/


addTclPath <- function(path = ".")
{
    ## Tcl uses Unix-style paths on Windows
    if(.Platform$OS.type == "windows")
        path <- gsub("\\\\", "/", path)
    a <- tclvalue(tcl("set", "auto_path"))
    paths <- strsplit(a, " ", fixed=TRUE)[[1L]]
    if (! path %in% paths)
        tcl("lappend", "auto_path", path)
    invisible(paths)
}

tclRequire <- function(package, warn = TRUE)
{
    a <- tryCatch(tcl("package", "require", package), error = identity)
    if (inherits(a, "error")) {
        if (warn)
            warning(gettextf("Tcl package '%s' not found", package),
                    domain = NA)
        return(FALSE)
    } else return(a)
}
