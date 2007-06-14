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

winMenuDel <- function(menuname)
    invisible(.Internal(winMenuDel(menuname, NULL)))

winMenuDelItem <- function(menuname, itemname)
    invisible(.Internal(winMenuDel(menuname, itemname)))

winMenuAdd <- function(menuname)
    invisible(.Internal(winMenuAdd(menuname, NULL, NULL)))

winMenuAddItem <- function(menuname, itemname, action) {
    ## If specified menu does not exist, add it
    if (! menuname %in% winMenuNames()) winMenuAdd(menuname)

    invisible(.Internal(winMenuAdd(menuname, itemname, action)))
}

winMenuNames <- function() .Internal(winMenuNames())

winMenuItems <- function(menuname) .Internal(winMenuItems(menuname))

