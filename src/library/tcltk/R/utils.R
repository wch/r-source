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
    winfo <- as.numeric(tclvalue(tkwinfo("screenheight", dlg)))
    ht <- min(length(list), winfo %/% 20) # a guess of font height
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
    tkwait.window(dlg)
    if(!multiple && !length(ans.select_list)) ans.select_list <- ""
    ans.select_list
}
