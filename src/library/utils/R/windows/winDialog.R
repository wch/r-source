#  File src/library/utils/R/windows/winDialog.R
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

winDialog <- function(type = c("ok", "okcancel", "yesno", "yesnocancel"),
                       message)
{
    if (!interactive())
        stop("winDialog() cannot be used non-interactively")
    type <- match.arg(type)
    res <- .Internal(winDialog(type, message))
    if(res == 10L) return(invisible(NULL))
    c("NO", "CANCEL", "YES", "OK")[res+2L]
}

winDialogString <- function(message, default)
{
    if (!interactive())
        stop("winDialogString() cannot be used non-interactively")
    .Internal(winDialogString(message, default))
}

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

winProgressBar <- function(title = "R progress bar", label = "",
                           min = 0, max = 1, initial = 0, width = 300)
{
    res <- .Internal(winProgressBar(width, title, label, min, max, initial))
    structure(list(pb=res), class = "winProgressBar")
}

close.winProgressBar <- function(con, ...)
    .Internal(closeWinProgressBar(con$pb))

setWinProgressBar <- function(pb, value, title=NULL, label=NULL)
{
    if(!inherits(pb, "winProgressBar"))
       stop("'pb' is not from class \"winProgressBar\"")
    invisible(.Internal(setWinProgressBar(pb$pb, value, title, label)))
}

getWinProgressBar <- function(pb)
{
    if(!inherits(pb, "winProgressBar"))
       stop("'pb' is not from class \"winProgressBar\"")
    .Internal(setWinProgressBar(pb$pb, NULL, NULL, NULL))
}
