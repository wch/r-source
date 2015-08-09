#  File src/library/utils/R/windows/winDialog.R
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

winDialog <- function(type = c("ok", "okcancel", "yesno", "yesnocancel"),
                       message)
{
    if (!interactive())
        stop("winDialog() cannot be used non-interactively")
    type <- match.arg(type)
    res <- .External2(C_winDialog, type, message)
    if(res == 10L) return(invisible(NULL))
    c("NO", "CANCEL", "YES", "OK")[res+2L]
}

winDialogString <- function(message, default)
{
    if (!interactive())
        stop("winDialogString() cannot be used non-interactively")
    .External2(C_winDialogString, message, default)
}

winMenuDel <- function(menuname)
    invisible(.External2(C_winMenuDel, menuname, NULL))

winMenuDelItem <- function(menuname, itemname)
    invisible(.External2(C_winMenuDel, menuname, itemname))

winMenuAdd <- function(menuname)
    invisible(.External2(C_winMenuAdd, menuname, NULL, NULL))

winMenuAddItem <- function(menuname, itemname, action) {
    ## If specified menu does not exist, add it
    if (! menuname %in% winMenuNames()) winMenuAdd(menuname)

    invisible(.External2(C_winMenuAdd, menuname, itemname, action))
}

winMenuNames <- function() .External2(C_winMenuNames)

winMenuItems <- function(menuname) .External2(C_winMenuItems, menuname)

## There is internal coercion, but using as.xxx here allows method dispatch
winProgressBar <- function(title = "R progress bar", label = "",
                           min = 0, max = 1, initial = 0, width = 300L)
{
    res <- .External2(C_winProgressBar, as.integer(width), as.character(title),
                      as.character(label), as.double(min),
                      as.double(max), as.double(initial))
    structure(list(pb=res), class = "winProgressBar")
}

close.winProgressBar <- function(con, ...)
    .External2(C_closeWinProgressBar, con$pb)

setWinProgressBar <- function(pb, value, title=NULL, label=NULL)
{
    if(!inherits(pb, "winProgressBar"))
       stop(gettextf("'pb' is not from class %s",
                     dQuote("winProgressBar")),
            domain = NA)
    if(!is.null(title)) title <- as.character(title)
    if(!is.null(label)) label <- as.character(label)
    invisible(.External2(C_setWinProgressBar, pb$pb, as.double(value),
                         title, label))
}

getWinProgressBar <- function(pb)
{
    if(!inherits(pb, "winProgressBar"))
        stop(gettextf("'pb' is not from class %s",
                      dQuote("winProgressBar")),
             domain = NA)
    .External2(C_setWinProgressBar, pb$pb, NULL, NULL, NULL)
}
