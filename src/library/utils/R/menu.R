menu <- function(choices, graphics = FALSE, title = "")
{
    nc <- length(choices)
    if(nchar(title[1])) cat(title[1], "\n")
    op <- paste(format(seq(length=nc)), ": ", choices, sep="")
    if(nc > 10) {
        fop <- format(op)
        nw <- nchar(fop[1], "w") + 2
        ncol <- getOption("width") %/% nw  # might be 0
        if(ncol > 1)
            op <- paste(fop, c(rep("  ", ncol - 1), "\n"), collapse="")
        cat("", op, "", sep="\n")
    } else cat("", op, "", sep="\n")
    repeat {
	ind <- .Internal(menu(as.character(choices)))
	if(ind <= nc) return(ind)
	cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}
