if(FALSE) {
CRANmirrorWidget <- function(a)
{
    lvar <- tclVar()
    tclObj(lvar) <- a[,1]
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    box <- tklistbox(dlg, height = length(a[, 1]),
                     listvariable = lvar, selectmode = "single")
    onOK <- function() {
        res <- 1+as.integer(tkcurselection(box))
        if(length(res)) {
            repos <- getOption("repos")
            URL <- a[res, "URL"]
            repos["CRAN"] <- gsub("/$", "", a[res, "URL"])
            options(repos = repos)
        }
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    lab <- tklabel(dlg, text = "Select CRAN mirror", fg = "blue")
    tkgrid.configure(lab, columnspan = 2)
    tkgrid.configure(box, columnspan = 2)
    OK <- tkbutton(dlg,     text = "OK", width = 6, command = onOK)
    Cancel <- tkbutton(dlg, text = "Cancel", command = onCancel)
    tkgrid(OK, Cancel)
    tkbind(dlg, "<Destroy>", function() tkgrab.release(dlg))
    tkwait.window(dlg)
    invisible()
}

repositoriesWidget <- function(a)
{
    lvar <- tclVar()
    tclObj(lvar) <- a[,1]
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    box <- tklistbox(dlg, height = length(a[, 1]),
                     listvariable = lvar, selectmode = "multiple")
    for(i in which(a[["default"]])) tkselection.set(box, i-1) # 0-based

    onOK <- function() {
        res <- 1+as.integer(tkcurselection(box))
        repos <- a[["URL"]]
        names(repos) <- row.names(a)
        options(repos = repos[res])
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    lab <- tklabel(dlg, text = "Select repositories", fg = "blue")
    tkgrid.configure(lab, columnspan = 2)
    tkgrid.configure(box, columnspan = 2)
    OK <- tkbutton(dlg, text = "OK", width = 6, command = onOK)
    Cancel <- tkbutton(dlg, text = "Cancel", command = onCancel)
    tkgrid(OK, Cancel)
    tkbind(dlg, "<Destroy>", function() tkgrab.release(dlg))
    tkwait.window(dlg)
    invisible()
}
}

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
                         listvariable = lvar,
                         selectmode = ifelse(multiple, "multiple", "single"),
                         yscrollcommand = function(...)tkset(scr,...))
        tkgrid.configure(box, scr, columnspan = 2)
        tkgrid.configure(scr, rowspan = ht, sticky = "nsw")
    } else {
        box <- tklistbox(dlg, height = ht,
                         listvariable = lvar,
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
