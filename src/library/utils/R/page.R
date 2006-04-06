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

    if(is.character(x) && length(x) == 1) {
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
