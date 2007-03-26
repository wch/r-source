winDialog <- function(type = c("ok", "okcancel", "yesno", "yesnocancel"),
                       message)
{
    type <- match.arg(type)
    res <- .Internal(winDialog(type, message))
    if(res == 10) return(invisible(NULL))
    c("NO", "CANCEL", "YES", "OK")[res+2]
}

winDialogString <- function(message, default)
    .Internal(winDialogString(message, default))

winMenuAdd <- function(menuname)
    invisible(.Internal(winMenuAdd(menuname, NULL, NULL)))

winMenuAddItem <- function(menuname, itemname, action)
    invisible(.Internal(winMenuAdd(menuname, itemname, action)))

winMenuDel <- function(menuname)
    invisible(.Internal(winMenuDel(menuname, NULL)))

winMenuDelItem <- function(menuname, itemname)
    invisible(.Internal(winMenuDel(menuname, itemname)))

winMenuAdd <- function(menuname)
    invisible(.Internal(winMenuAdd(menuname, NULL, NULL)))

winMenuAddItem <- function(menuname, itemname, action) {
    ## If specified menu does not exist, add it
    z <- winMenuNames()
    if (! menuname %in% z)
        winMenuAdd(menuname)

    invisible(.Internal(winMenuAdd(menuname, itemname, action)))
}
winMenuNames <- function() {
    .Internal(winMenuNames())
}

winMenuItems <- function(menuname) {
    .Internal(winMenuItems(menuname))
}
