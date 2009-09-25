#  File src/library/utils/R/widgets.R
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

select.list <- function(list, preselect=NULL, multiple=FALSE, title=NULL)
{
    if(!interactive()) stop("select.list() cannot be used non-interactively")
    if(.Platform$OS.type == "windows" | .Platform$GUI == "AQUA")
        return(.Internal(select.list(list, preselect, multiple, title)))
    ## simple text-based alternatives.
    if(!multiple) {
        res <- menu(list, , title)
        if(res < 1L || res > length(list)) return("")
        else return(list[res])
    } else {
        nc <- length(list)
        cat(title, "\n")
        def <- if(is.null(preselect)) rep(FALSE, nc)
        else list %in% preselect
        op <- paste(format(seq_len(nc)), ": ",
                    ifelse(def, "+", " "), " ", list, sep="")
        if(nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2
            ncol <- getOption("width") %/% nw
            if(ncol > 1L)
                op <- paste(fop, c(rep("  ", ncol - 1), "\n"),
                            sep ="", collapse="")
            cat("", op, sep="\n")
        } else cat("", op, "", sep="\n")
	cat(gettext("Enter zero or more numbers separated by space\n"))
	repeat {
            res <- tryCatch(scan("", what=0, quiet=TRUE, nlines=1),
                            error = identity)
	    if(!inherits(res, "error")) break
	    cat(gettext("Invalid input, please try again\n"))
	}
        if(!length(res) || (length(res) == 1L && !res[1L])) return(character(0L))
        res <- sort(res[1 <= res && res <= nc])
        return(list[res])
    }
}

flush.console <- function()
    if (.Platform$GUI == "AQUA" || .Platform$OS.type == "windows")
        .Internal(flush.console())
