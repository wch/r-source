#  File src/library/utils/R/widgets.R
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

select.list <-
    function(choices, preselect = NULL, multiple = FALSE, title = NULL,
             graphics = getOption("menu.graphics"))
{
    if(!interactive()) stop("select.list() cannot be used non-interactively")
    if(!is.null(title) && (!is.character(title) || length(title) != 1))
        stop("'title' must be NULL or a length-1 character vector")
    if(isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == "AQUA")
        return(.External2(C_selectlist, choices, preselect, multiple, title))
        ## must be Unix here
        ## Tk might not require X11 on macOS, but if DISPLAY is set
        ## this will work for Aqua Tcl/Tk.
        ## OTOH, we do want to check Tk works!
        else if(graphics && capabilities("tcltk") &&
                capabilities("X11") && suppressWarnings(tcltk::.TkUp))
            return(tcltk::tk_select.list(choices, preselect, multiple, title))
    }
    ## simple text-based alternatives.
    if(!multiple) {
        res <- menu(choices, FALSE, title)
        if(res < 1L || res > length(choices)) return("")
        else return(choices[res])
    } else {
        nc <- length(choices)
        if (length(title) && nzchar(title[1L]))
            cat(title, "\n", sep = "")
        def <- if(is.null(preselect)) rep.int(FALSE, nc)
        else choices %in% preselect
        op <- paste0(format(seq_len(nc)), ": ",
                     ifelse(def, "+", " "), " ", choices)
        if(nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2L
            ncol <- getOption("width") %/% nw
            if(ncol > 1L)
                op <- paste0(fop, c(rep.int("  ", ncol - 1L), "\n"),
                             collapse = "")
            cat("", op, sep = "\n")
        } else cat("", op, "", sep = "\n")
        cat(gettext("Enter one or more numbers separated by spaces, or an empty line to cancel\n"))
	repeat {
            res <- tryCatch(scan("", what = 0, quiet = TRUE, nlines = 1),
                            error = identity)
	    if(!inherits(res, "error")) break
	    cat(gettext("Invalid input, please try again\n"))
	}
        if(!length(res) || (length(res) == 1L && !res[1L])) return(character())
        res <- sort(res[1 <= res & res <= nc])
        return(choices[res])
    }
}

flush.console <- function() invisible(.Call(C_flushconsole))

process.events <- function() invisible(.Call(C_processevents))
