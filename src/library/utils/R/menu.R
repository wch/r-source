menu <- function(choices, graphics = FALSE, title = "")
{
    if(!interactive()) stop("menu() cannot be used non-interactively")
    if(graphics) {
        if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA")
            return(select.list(choices, multiple=FALSE, title=title))
        else if(.Platform$OS.type == "unix"
                && capabilities("tcltk") && capabilities("X11"))
            return(tcltk::tk_select.list(choices, multiple=FALSE, title=title))
    }
    nc <- length(choices)
    if(length(title) && nchar(title[1])) cat(title[1], "\n")
    op <- paste(format(seq(length=nc)), ": ", choices, sep="")
    if(nc > 10) {
        fop <- format(op)
        nw <- nchar(fop[1], "w") + 2
        ncol <- getOption("width") %/% nw  # might be 0
        if(ncol > 1)
            op <- paste(fop, c(rep("  ", ncol - 1), "\n"), sep="", collapse="")
        cat("", op, "", sep="\n")
    } else cat("", op, "", sep="\n")
    repeat {
	ind <- .Internal(menu(as.character(choices)))
	if(ind <= nc) return(ind)
	cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}
