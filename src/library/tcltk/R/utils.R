tk_select.list <-
    function(list, preselect = NULL, multiple = FALSE, title = NULL)
{
    lvar <- tclVar()
    tclObj(lvar) <- list
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    if(!is.null(title) && nchar(title)) {
        lab <- tklabel(dlg, text = title, fg = "blue")
        tkgrid.configure(lab, columnspan = 2)
    }
    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 100
                                        # allow for win furniture and buttons
    ht <- min(length(list), scht %/% 20) # a guess of font height
    box <- tklistbox(dlg, height = ht,
                     listvariable = lvar, bg = "white",
                     selectmode = ifelse(multiple, "multiple", "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font=NULL))
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp)))+1
    ht <- min(length(list), scht %/% tmp)
    tkdestroy(box)
    if(ht < length(list)) {
        scr <- tkscrollbar(dlg, repeatinterval = 5,
                           command = function(...) tkyview(box, ...))
        box <- tklistbox(dlg, height = ht,
                         listvariable = lvar, bg = "white",
                         selectmode = ifelse(multiple, "multiple", "single"),
                         yscrollcommand = function(...)tkset(scr,...))
        tkgrid.configure(box, scr, columnspan = 2)
        tkgrid.configure(scr, rowspan = ht, sticky = "nsw")
    } else {
        box <- tklistbox(dlg, height = ht,
                         listvariable = lvar, bg = "white",
                         selectmode = ifelse(multiple, "multiple", "single"))
        tkgrid.configure(box, columnspan = 2)
    }
    preselect <- match(preselect, list)
    ans.select_list <- character(0) # avoid name conflicts
    for(i in preselect[preselect > 0])
        tkselection.set(box, i - 1) # 0-based

    onOK <- function() {
        res <- 1+as.integer(tkcurselection(box))
        ans.select_list <<- list[res]
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        ans <<- character(0)
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    OK <- tkbutton(dlg, text = "OK", width = 6, command = onOK)
    Cancel <- tkbutton(dlg, text = "Cancel", command = onCancel)
    tkgrid(OK, Cancel)
    tkbind(dlg, "<Destroy>", onCancel)
    tkfocus(box)
    tkwait.window(dlg)
    if(!multiple && !length(ans.select_list)) ans.select_list <- ""
    ans.select_list
}
