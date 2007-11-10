#  File src/library/tcltk/R/utils.R
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

tk_select.list <-
    function(list, preselect = NULL, multiple = FALSE, title = NULL)
{
    lvar <- tclVar()
    tclObj(lvar) <- list
    oldmode <- tclServiceMode(FALSE)
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    if(!is.null(title) && nzchar(title)) {
        lab <- tklabel(dlg, text = title, fg = "blue")
        tkpack(lab, side="top")
    }
    onOK <- function() {
        res <- 1+as.integer(tkcurselection(box))
        ans.select_list <<- list[res]
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    buttons <- tkframe(dlg)
    tkpack(buttons, side="bottom")
    OK <- tkbutton(buttons, text = gettext("OK"), width = 6, command = onOK)
    Cancel <- tkbutton(buttons, text = gettext("Cancel"), command = onCancel)
    tkpack(OK, Cancel, side="left", fill="x", padx="2m")

    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200
    ## allow for win furniture and buttons, and for e.g. KDE panel
    ht <- min(length(list), scht %/% 20) # a guess of font height
    box <- tklistbox(dlg, height = ht,
                     listvariable = lvar, bg = "white",
                     selectmode = ifelse(multiple, "multiple", "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font=NULL))
    ## fudge factor here seems to be 1 on Windows, 3 on X11.
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp)))+3
    ht <- min(length(list), scht %/% tmp)
    tkdestroy(box)
    if(ht < length(list)) {
        scr <- tkscrollbar(dlg, repeatinterval = 5,
                           command = function(...) tkyview(box, ...))
        box <- tklistbox(dlg, height = ht, width = 0,
                         listvariable = lvar, bg = "white",
                         selectmode = ifelse(multiple, "multiple", "single"),
                         yscrollcommand = function(...)tkset(scr,...))
        tkpack(box, side="left", fill="both", expand=TRUE)
        tkpack(scr, side="right", fill="y")
    } else {
        box <- tklistbox(dlg, height = ht, width = 0,
                         listvariable = lvar, bg = "white",
                         selectmode = ifelse(multiple, "multiple", "single"))
        tkpack(box, side="left", fill="both")
    }
    preselect <- match(preselect, list)
    ans.select_list <- character(0) # avoid name conflicts
    for(i in preselect[preselect > 0])
        tkselection.set(box, i - 1) # 0-based

    tkbind(dlg, "<Destroy>", onCancel)
    tkfocus(box)
    tclServiceMode(oldmode)
    tkwait.window(dlg)
    if(!multiple && !length(ans.select_list)) ans.select_list <- ""
    ans.select_list
}
