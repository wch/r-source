select.list <- function(list, preselect=NULL, multiple=FALSE, title=NULL)
{
    if(!interactive()) stop("select.list() cannot be used non-interactively")
    if(.Platform$OS.type == "windows")
        return(.Internal(select.list(list, preselect, multiple, title)))
    ## simple text-based alternatives.
    if(!multiple) {
        res <- menu(list, , title)
        if(res < 1 || res > length(list)) return("")
        else return(list[res])
    } else {
        nc <- length(list)
        cat(title, "\n")
        def <- if(is.null(preselect)) rep(FALSE, nc)
        else list %in% preselect
        op <- paste(format(seq(length=nc)), ": ",
                    ifelse(def, "+", " "), " ", list, sep="")
        if(nc > 10) {
            fop <- format(op)
            nw <- nchar(fop[1], "w") + 2
            ncol <- getOption("width") %/% nw
            if(ncol > 1)
                op <- paste(fop, c(rep("  ", ncol - 1), "\n"),
                            sep ="", collapse="")
            cat("", op, sep="\n")
        } else cat("", op, "", sep="\n")
        cat(gettext("Enter zero or more numbers separated by space\n"))
        res <- scan("", what=0, quiet=TRUE, nlines=1)
        if(!length(res) || (length(res) == 1 && !res[1])) return(character(0))
        res <- sort(res[1 <= res && res <= nc])
        return(list[res])
    }
}

flush.console <- function()
    if (.Platform$GUI == "AQUA" || .Platform$OS.type == "windows")
        .Internal(flush.console())
