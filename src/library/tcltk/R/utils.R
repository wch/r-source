#  File src/library/tcltk/R/utils.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

tk_select.list <-
    function(choices, preselect = NULL, multiple = FALSE, title = NULL)
{
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk) ttkbutton <- tkbutton
    lvar <- tclVar()
    tclObj(lvar) <- choices
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
        res <- 1L + as.integer(tkcurselection(box))
        ans.select_list <<- choices[res]
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

    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200L
    ## allow for win furniture and buttons, and for e.g. KDE panel
    ht <- min(length(choices), scht %/% 20) # a guess of font height
    box <- tklistbox(dlg, height = ht,
                     listvariable = lvar, bg = "white", setgrid = 1,
                     selectmode = ifelse(multiple, "multiple", "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font=NULL))
    ## fudge factor here seems to be 1 on Windows, 3 on X11.
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp)))+3
    ht <- min(length(choices), scht %/% tmp)
    tkdestroy(box)
    if(ht < length(choices)) {
        scr <- if(have_ttk) ttkscrollbar(dlg, command = function(...) tkyview(box, ...))
        else tkscrollbar(dlg, repeatinterval=5, command = function(...) tkyview(box, ...))
        box <- tklistbox(dlg, height = ht, width = 0,
                         listvariable = lvar, bg = "white", setgrid = 1,
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
    preselect <- match(preselect, choices)
    preselect <- preselect[preselect > 0L] - 1L # 0-based
    if(length(preselect)) {
        for(i in preselect) tkselection.set(box, i)
        ## ensure first (and usally only) pre-selection is visible
        tkyview(box, preselect[1L])
    }

    ans.select_list <- character() # avoid name conflicts
    tkbind(dlg, "<Destroy>", onCancel)
    tkbind(dlg, "<Double-ButtonPress-1>", onOK)
    tkfocus(box)
    tclServiceMode(oldmode)
    tkwait.window(dlg)
    Sys.sleep(0.1) # allow time for window to be removed.
    if(!multiple && !length(ans.select_list)) ans.select_list <- ""
    ans.select_list
}

tkProgressBar <- function(title = "R progress bar", label = "",
                          min = 0, max = 1, initial = 0, width = 300)
{
    useText <- FALSE
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if(!have_ttk && as.character(tclRequire("PBar")) == "FALSE") useText <- TRUE


    .win <- tktoplevel()
    .val <- initial
    .killed <- FALSE

    tkwm.geometry(.win, sprintf("%dx80", width+40))
    tkwm.title(.win, title)
    fn <- tkfont.create(family="helvetica", size=12)

    if(useText) {
        ## currently unused
        .lab <- tklabel(.win, text=label, font=fn, padx=20)
        tkpack(.lab, side = "left")
        fn2 <- tkfont.create(family="helvetica", size=16)
       .vlab <- tklabel(.win, text="0%", font=fn2, padx=20)
        tkpack(.vlab, side = "right")
        up <- function(value) {
            if(!is.finite(value) || value < min || value > max) return()
            .val <<- value
             tkconfigure(.vlab,
                         text=sprintf("%d%%",
                         round(100*(value - min)/(max - min))))
        }
    } else {
        .lab <- tklabel(.win, text=label, font=fn, pady=10)
       .tkval <- tclVar(0)
        tkpack(.lab, side="top")
        tkpack(tklabel(.win, text="", font = fn), side="bottom")

        pBar <- if(have_ttk) ttkprogressbar(.win, length=width, variable=.tkval) else tkwidget(.win, "ProgressBar", width=width, variable=.tkval)
        tkpack(pBar, side="bottom")
        up <- function(value) {
            if(!is.finite(value) || value < min || value > max) return()
            .val <<- value
            tclvalue(.tkval) <<- 100*(value - min)/(max - min)
        }
    }
    getVal <- function() .val
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
        stop(gettextf("'pb' is not from class %s",
                      dQuote("tkProgressBar")),
             domain = NA)
    pb$getVal()
}

setTkProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "tkProgressBar"))
        stop(gettextf("'pb' is not from class %s",
                      dQuote("tkProgressBar")),
             domain = NA)
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

tk_choose.files <-
    function(default = '', caption = 'Select files', multi = TRUE,
             filters = NULL, index = 1)
{
    args <- list("tk_getOpenFile", title = caption, multiple = multi)
    if(nzchar(default)) args <- c(args, initialdir = dirname(default),
                                   initialfile = basename(default))
    if(!is.null(filters)) {
        if(!is.character(filters) || length(dim(filters)) != 2 || ncol(filters) != 2)
            stop("'filters' must be a 2-column character matrix")
        f <- filters
        f[] <- paste0("{", filters, "}")
        ff <- apply(f, 1, paste, collapse = " ")
        fff <- paste0("{", ff, "}")
        args <- c(args, filetypes = paste(fff, collapse = " "))
    }
    res <- tclvalue(do.call(tcl, args))
    if(nzchar(res))
        if(multi) {
            ## Filenames with spaces will be surrounded by { }
            ans <- character()
            pat <- "([^{])*\\{([^}]*)\\}(.*)"
            while(grepl(pat, res)) {
                ans <- c(ans, sub(pat, "\\2", res))
                res <- sub(pat, "\\1\\3", res)
            }
            ans <- c(ans, strsplit(res, " ", fixed = TRUE)[[1]])
            ans[nzchar(ans)]
        } else res
    else character()
}


tk_choose.dir <- function(default = '', caption = 'Select directory')
{
    res <- tclvalue(tcl("tk_chooseDirectory", initialdir = default, title = caption))
    if(nzchar(res)) res else NA_character_
}

tk_messageBox <-
    function(type = c("ok", "okcancel", "yesno", "yesnocancel",
                      "retrycancel", "aburtretrycancel"),
             message, caption = "", default = "", ...)
{
    type <- match.arg(type)
    args <- list("tk_messageBox", type=type, message=message,
                 title=caption, ...)
    if(nzchar(default)) args <- c(args, default=default)
    tclvalue(do.call("tcl", args))
}
