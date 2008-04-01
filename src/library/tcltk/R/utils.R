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
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk) ttkbutton <- tkbutton
    lvar <- tclVar()
    tclObj(lvar) <- list
    oldmode <- tclServiceMode(FALSE)
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    if(!is.null(title) && nzchar(title)) {
        lab <- if(have_ttk) ttklabel(dlg, text = title, foreground = "blue")
        else tklabel(dlg, text = title, fg = "blue")
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
    OK <- ttkbutton(buttons, text = gettext("OK"), width = 6, command = onOK)
    Cancel <- ttkbutton(buttons, text = gettext("Cancel"), command = onCancel)
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
        scr <- if(have_ttk) ttkscrollbar(dlg, command = function(...) tkyview(box, ...))
        else tkscrollbar(dlg, repeatinterval=5, command = function(...) tkyview(box, ...))
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

tkProgressBar <- function(title = "R progress bar", label = "",
                          min = 0, max = 1, initial = 0, width = 300)
{
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk)
        if(as.character(tclRequire("BWidget")) == "FALSE")
            stop("either Tk >= 8.5 or BWidget is required")

    .win <- tktoplevel()
    .val <- initial
    .tkval <- tclVar(0)
    .killed <- FALSE

    tkwm.geometry(.win, sprintf("%dx80", width+40))
    tkwm.title(.win, title)
    fn <- tkfont.create(family="helvetica", size=12)

    .lab <- tklabel(.win, text=label, font = fn)
    tkpack(.lab, side="top")
    tkpack(tklabel(.win, text="", font = fn), side="bottom")

    pBar <- if(have_ttk) ttkprogressbar(.win, length=width, variable=.tkval) else tkwidget(.win, "ProgressBar", width=width, variable=.tkval)
    tkpack(pBar, side="bottom")

    getVal <- function() .val
    up <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        tclvalue(.tkval) <<- 100*(value - min)/(max - min)
    }
    kill <- function() if(!.killed) {tkdestroy(.win); .killed <<- TRUE}
    title <- function(title) tkwm.title(.win, title)
    lab <- function(label) tkconfigure(.lab, text=label)
    tkbind(.win, "<Destroy>", kill)
    up(initial)

    structure(list(getVal=getVal, up=up, title=title, label=lab, kill=kill),
              class = "tkProgressBar")
}

getTkProgressBar <- function(pb)
{
    if(!inherits(pb, "tkProgressBar"))
       stop("'pb' is not from class \"tkProgressBar\"")
    pb$getVal()
}

setTkProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "tkProgressBar"))
       stop("'pb' is not from class \"tkProgressBar\"")
    oldval <- pb$getVal()
    pb$up(value)
    if(!is.null(title)) pb$title(title)
    if(!is.null(label)) pb$label(label)
    tcl("update", "idletasks")
    invisible(oldval)
}

close.tkProgressBar <- function(con, ...)
{
    con$kill()
    invisible(NULL)
}
