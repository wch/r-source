#  File src/library/utils/R/menu.R
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

menu <- function(choices, graphics = FALSE, title = NULL)
{
    if(!interactive()) stop("menu() cannot be used non-interactively")
    if(isTRUE(graphics)) {
        if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA"
           ## Tk might not require X11 on macOS, but if DISPLAY is set
           ## this will work for Aqua Tcl/Tk.
           ## OTOH, we do want to check Tk works!
           || (capabilities("tcltk") && capabilities("X11") &&
               suppressWarnings(tcltk::.TkUp))) {
            res <- select.list(choices, multiple = FALSE, title = title,
                               graphics = TRUE)
            return(match(res, choices, nomatch = 0L))
        }
    }
    nc <- length(choices)
    if(length(title) && nzchar(title[1L])) cat(title[1L], "\n")
    op <- paste0(format(seq_len(nc)), ": ", choices)
    if(nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2
        ncol <- getOption("width") %/% nw  # might be 0
        if(ncol > 1L)
            op <- paste0(fop, c(rep("  ", ncol - 1), "\n"), collapse="")
        cat("", op, "", sep="\n")
    } else cat("", op, "", sep="\n")
    repeat {
	ind <- .Call(C_menu, as.character(choices))
	if(ind <= nc) return(ind)
	cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}
