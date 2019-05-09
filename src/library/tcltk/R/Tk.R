#  File src/library/tcltk/R/Tk.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

### ------ Basics ------


.Tcl <- function(...)
    structure(.External(.C_dotTcl, ...), class = "tclObj")
.Tcl.objv <- function(objv)
    structure(.External(.C_dotTclObjv, objv), class = "tclObj")

.Tcl.callback <- function(...)
    .External(.C_dotTclcallback, ...)

.Tcl.args <- function(...) {
    ## Eek! (See .Tcl.args.objv for explanation)
    pframe <- parent.frame(3)
    ## Convert argument tags to option names (i.e. stick "-" in front)
    name2opt <- function(x) if ( x != "") paste0("-", x) else ""

    isCallback <- function(x)
	is.function(x) || is.call(x) || is.expression(x)

    makeAtomicCallback <- function(x, e) {
	if (is.name(x))
	    x <- eval(x, e)
	if (is.call(x)){
	    if(identical(x[[1L]], as.name("break")))
		return("break")
	    if(identical(x[[1L]], as.name("function")))
                x <- eval(x, e)
        }
	.Tcl.callback(x, e)
    }

    makeCallback <- function(x, e) {
	if (is.expression(x))
	    paste(lapply(x, makeAtomicCallback, e), collapse = ";")
	else
	    makeAtomicCallback(x, e)
    }

    ## Convert arguments. Callbacks and windows require special treatment
    ## everything else is converted to strings
    val2string <- function(x) {
        if (is.null(x)) return("")
        if (is.tkwin(x)){ current.win <<- x ; return (.Tk.ID(x)) }
	if (inherits(x,"tclVar")) return(names(unclass(x)$env))
        if (isCallback(x)){
	    # Jump through some hoops to protect from GC...
	    ref <- local({value <- x; envir <- pframe; environment()})
            callback <- makeCallback(get("value", envir = ref),
		                     get("envir", envir = ref))
	    callback <- paste("{", callback, "}")
            assign(callback, ref, envir = current.win$env)
            return(callback)
        }
        ## quoting/escaping hell (much less, since using fixed=TRUE):
        x <- gsub("\\", "\\\\", as.character(x), fixed=TRUE)
        x <- gsub("\"", "\\\"", x, fixed=TRUE)
        x <- gsub("[",  "\\[",  x, fixed=TRUE)
        x <- gsub("$",  "\\$",  x, fixed=TRUE)
        paste0("\"", x, "\"", collapse = " ")
    }

    val <- list(...)
    nm <- names(val)

    if (!length(val)) return("")
    nm <- if (is.null(nm)) rep("", length(val)) else sapply(nm, name2opt)

    ## This is a bit dodgy: we need to ensure that callbacks don't get
    ## garbage collected, so we try registering them with the relevant
    ## window, which is assumed to be the last preceding window
    ## argument during val2string processing if one occurs, or the
    ## "win" variable of the caller (tkwidget calls) or as a last
    ## resort .TkRoot. What a mess!

    current.win <-
        if (exists("win", envir = parent.frame()))
            get("win", envir = parent.frame())
        else .TkRoot

    val <- sapply(val, val2string)
    paste(as.vector(rbind(nm, val)), collapse = " ")
}

.Tcl.args.objv <- function(...) {

    ## Eek! This is broken by design...
    ## The issue is that if a callback is given in the form of an expression,
    ## then we need to ensure that it is evaluated in the proper environment
    ## The typical case is that tkbind() calls tcl() calls  .Tcl.args.objv()
    ## so we grab 3 levels back. This will break direct calls to tcl(), though.

    pframe <- parent.frame(3)

    isCallback <- function(x)
	is.function(x) || is.call(x) || is.expression(x)

    makeAtomicCallback <- function(x, e) {
	if (is.name(x))
	    x <- eval(x, e)
	if (is.call(x)){
	    if(identical(x[[1L]], as.name("break")))
		return("break")
	    if(identical(x[[1L]], as.name("function")))
                x <- eval(x, e)
        }
	.Tcl.callback(x, e)
    }

    makeCallback <- function(x, e) {
	if (is.expression(x))
	    paste(lapply(x, makeAtomicCallback, e), collapse = ";")
	else
	    makeAtomicCallback(x, e)
    }

    ## Convert arguments. Callbacks and windows require special treatment
    ## everything else is converted to strings
    val2obj <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.tkwin(x)){current.win <<- x ; return(as.tclObj(.Tk.ID(x)))}
	if (inherits(x,"tclVar")) return(as.tclObj(names(unclass(x)$env)))
        if (isCallback(x)){
	    # Jump through some hoops to protect from GC...
	    ref <- local({value <- x; envir <- pframe; environment()})
            callback <- makeCallback(get("value", envir = ref),
		                     get("envir", envir = ref))
            assign(callback, ref, envir = current.win$env)
            return(as.tclObj(callback, drop = TRUE))
        }
        as.tclObj(x, drop = TRUE)
    }

    val <- list(...)

    ## This is a bit dodgy: we need to ensure that callbacks don't get
    ## garbage collected, so we try registering them with the relevant
    ## window, which is assumed to be the last preceding window
    ## argument during val2string processing if one occurs,
    ## or as a last resort .TkRoot. What a mess!

    current.win <- .TkRoot

    lapply(val, val2obj)
}


.Tk.ID <- function(win) win$ID

.Tk.newwin <- function(ID) {
    win <- list(ID = ID, env = new.env(parent = emptyenv()))
    win$env$num.subwin <- 0
    class(win) <- "tkwin"
    win
}

.Tk.subwin <- function(parent) {
    ID <- paste(parent$ID, parent$env$num.subwin <- parent$env$num.subwin + 1,
                sep = ".")
    win <- .Tk.newwin(ID)
    assign(ID, win, envir = parent$env)
    assign("parent", parent, envir = win$env)
    win
}

tkdestroy  <- function(win) {
    tcl("destroy", win)
    ID <- .Tk.ID(win)
    env <- get("parent", envir = win$env)$env
    if (exists(ID, envir = env, inherits = FALSE))
        rm(list = ID, envir = env)
}

is.tkwin <- function(x) inherits(x, "tkwin")

tclVar <- function(init = "") {
   n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1L
   name <- paste0("::RTcl", n)
   l <- list(env = new.env())
   assign(name, NULL, envir = l$env)
   reg.finalizer(l$env, function(env) tcl("unset", names(env)))
   class(l) <- "tclVar"
   tclvalue(l) <- init
   l
}

tclObj <- function(x) UseMethod("tclObj")
"tclObj<-" <- function(x, value) UseMethod("tclObj<-")

tclObj.tclVar <- function(x){
    z <- .External(.C_RTcl_ObjFromVar, names(x$env))
    class(z) <- "tclObj"
    z
}

"tclObj<-.tclVar" <- function(x, value){
    value <- as.tclObj(value)
    .External(.C_RTcl_AssignObjToVar, names(x$env), value)
    x
}

tclvalue <- function(x) UseMethod("tclvalue")
"tclvalue<-" <- function(x, value) UseMethod("tclvalue<-")

tclvalue.tclVar <- function(x) tclvalue(tclObj(x))
tclvalue.tclObj <- function(x) .External(.C_RTcl_StringFromObj, x)
print.tclObj <- function(x,...) {
    z <- tclvalue(x)
    if (length(z)) cat("<Tcl>", z, "\n")
    invisible(x)
}

"tclvalue<-.tclVar" <- function(x, value) {
    name <- names(unclass(x)$env)
    tcl("set", name, value)
    x
}

tclvalue.default <- function(x) tclvalue(tcl("set", as.character(x)))

"tclvalue<-.default" <- function(x, value) {
    name <- as.character(x)
    tcl("set", name, value)
    x
}

as.character.tclVar <- function(x, ...) names(unclass(x)$env)

as.character.tclObj <- function(x, ...)
    .External(.C_RTcl_ObjAsCharVector, x)
as.double.tclObj <- function(x, ...)
    .External(.C_RTcl_ObjAsDoubleVector, x)
as.integer.tclObj <- function(x, ...)
    .External(.C_RTcl_ObjAsIntVector, x)
as.logical.tclObj <- function(x, ...)
    as.logical(.External(.C_RTcl_ObjAsIntVector, x))
as.raw.tclObj <- function(x, ...)
    .External(.C_RTcl_ObjAsRawVector, x)

is.tclObj <- function(x) inherits(x, "tclObj")

as.tclObj <- function(x, drop = FALSE) {
    if (is.tclObj(x)) return(x)
    z <- switch(storage.mode(x),
                character = .External(.C_RTcl_ObjFromCharVector, x, drop),
                double = .External(.C_RTcl_ObjFromDoubleVector, x,drop),
                integer = .External(.C_RTcl_ObjFromIntVector, x, drop),
                logical = .External(.C_RTcl_ObjFromIntVector, as.integer(x), drop),
                raw = .External(.C_RTcl_ObjFromRawVector, x),
                stop(gettextf("cannot handle object of mode '%s'",
                              storage.mode(x)), domain = NA)
                )
    class(z) <- "tclObj"
    z
}
# Actually makes .default and .tclVar methods equivalent, the latter
# just saves a level of function dispatching

tclServiceMode <- function(on = NULL)
    .External(.C_RTcl_ServiceMode, as.logical(on))

#----

.TkRoot <- .Tk.newwin("")
tclvar  <- structure(list(), class = "tclvar")
.TkRoot$env$TclVarCount <- 0


# ------ Widgets ------

tkwidget <- function (parent, type, ...) # generic
{
    win <- .Tk.subwin(parent)
    # older version had .Tk.ID(win) here, but this makes for easier
    # current.win handling
    tcl(type, win, ...)
    win
}

tkbutton      <- function(parent, ...) tkwidget(parent, "button", ...)
tkcanvas      <- function(parent, ...) tkwidget(parent, "canvas", ...)
tkcheckbutton <- function(parent, ...) tkwidget(parent, "checkbutton", ...)
tkentry       <- function(parent, ...) tkwidget(parent, "entry", ...)
tkframe       <- function(parent, ...) tkwidget(parent, "frame", ...)
tklabel       <- function(parent, ...) tkwidget(parent, "label", ...)
tklistbox     <- function(parent, ...) tkwidget(parent, "listbox", ...)
tkmenu        <- function(parent, ...) tkwidget(parent, "menu", ...)
tkmenubutton  <- function(parent, ...) tkwidget(parent, "menubutton", ...)
tkmessage     <- function(parent, ...) tkwidget(parent, "message", ...)
tkradiobutton <- function(parent, ...) tkwidget(parent, "radiobutton", ...)
tkscale       <- function(parent, ...) tkwidget(parent, "scale", ...)
tkscrollbar   <- function(parent, ...) tkwidget(parent, "scrollbar", ...)
tktext        <- function(parent, ...) tkwidget(parent, "text", ...)

ttkbutton      <- function(parent, ...) tkwidget(parent, "ttk::button", ...)
ttkcheckbutton <- function(parent, ...) tkwidget(parent, "ttk::checkbutton", ...)
ttkcombobox    <- function(parent, ...) tkwidget(parent, "ttk::combobox", ...)
ttkentry       <- function(parent, ...) tkwidget(parent, "ttk::entry", ...)
ttkframe       <- function(parent, ...) tkwidget(parent, "ttk::frame", ...)
ttklabel       <- function(parent, ...) tkwidget(parent, "ttk::label", ...)
ttklabelframe  <- function(parent, ...) tkwidget(parent, "ttk::labelframe", ...)
ttkmenubutton  <- function(parent, ...) tkwidget(parent, "ttk::menubutton", ...)
ttknotebook    <- function(parent, ...) tkwidget(parent, "ttk::notebook", ...)
ttkpanedwindow <- function(parent, ...) tkwidget(parent, "ttk::panedwindow", ...)
ttkprogressbar <- function(parent, ...) tkwidget(parent, "ttk::progressbar", ...)
ttkradiobutton <- function(parent, ...) tkwidget(parent, "ttk::radiobutton", ...)
ttkscale       <- function(parent, ...) tkwidget(parent, "ttk::scale", ...)
ttkscrollbar   <- function(parent, ...) tkwidget(parent, "ttk::scrollbar", ...)
ttkseparator   <- function(parent, ...) tkwidget(parent, "ttk::separator", ...)
ttksizegrip    <- function(parent, ...) tkwidget(parent, "ttk::sizegrip", ...)
ttkspinbox     <- function(parent, ...) tkwidget(parent, "ttk::spinbox", ...)
ttktreeview    <- function(parent, ...) tkwidget(parent, "ttk::treeview", ...)


tktoplevel    <- function(parent = .TkRoot,...) {
    w <- tkwidget(parent,"toplevel",...)
    ID <- .Tk.ID(w)
    tkbind(w, "<Destroy>",
           function() {
               if (exists(ID, envir = parent$env, inherits = FALSE))
                   rm(list = ID, envir = parent$env)
               tkbind(w, "<Destroy>","")
           })
    utils::process.events()
    w
}
### ------ Window & Geometry managers, widget commands &c ------

tcl <- function(...) .Tcl.objv(.Tcl.args.objv(...))

tktitle <- function(x) tcl("wm", "title", x)

"tktitle<-" <- function(x, value) {
    tcl("wm", "title", x, value)
    x
}

tkbell     <- function(...) tcl("bell", ...)
tkbind     <- function(...) tcl("bind", ...)
tkbindtags <- function(...) tcl("bindtags", ...)
tkfocus    <- function(...) tcl("focus", ...)
tklower    <- function(...) tcl("lower", ...)
tkraise    <- function(...) tcl("raise", ...)


tkclipboard.append <- function(...) tcl("clipboard", "append", ...)
tkclipboard.clear  <- function(...) tcl("clipboard", "clear", ...)


tkevent.add      <- function(...) tcl("event", "add", ...)
tkevent.delete   <- function(...) tcl("event", "delete", ...)
tkevent.generate <- function(...) tcl("event", "generate", ...)
tkevent.info     <- function(...) tcl("event", "info", ...)


tkfont.actual    <- function(...) tcl("font", "actual", ...)
tkfont.configure <- function(...) tcl("font", "configure", ...)
tkfont.create    <- function(...) tcl("font", "create", ...)
tkfont.delete    <- function(...) tcl("font", "delete", ...)
tkfont.families  <- function(...) tcl("font", "families", ...)
tkfont.measure   <- function(...) tcl("font", "measure", ...)
tkfont.metrics   <- function(...) tcl("font", "metrics", ...)
tkfont.names     <- function(...) tcl("font", "names", ...)

tkgrab         <- function(...) tcl("grab", ...)
tkgrab.current <- function(...) tcl("grab", "current", ...)
tkgrab.release <- function(...) tcl("grab", "release", ...)
tkgrab.set     <- function(...) tcl("grab", "set", ...)
tkgrab.status  <- function(...) tcl("grab", "status", ...)

tkimage.create <- function(...) tcl("image", "create", ...)
tkimage.delete <- function(...) tcl("image", "delete", ...)
tkimage.height <- function(...) tcl("image", "height", ...)
tkimage.inuse  <- function(...) tcl("image", "inuse", ...)
tkimage.names  <- function(...) tcl("image", "names", ...)
tkimage.type   <- function(...) tcl("image", "type", ...)
tkimage.types  <- function(...) tcl("image", "types", ...)
tkimage.width  <- function(...) tcl("image", "width", ...)


## NB: some widgets also have a selection.clear command, hence the "X".
## tkselection.clear might be made a generic function instead.
tkXselection.clear  <- function(...) tcl("selection", "clear", ...)
tkXselection.get    <- function(...) tcl("selection", "get", ...)
tkXselection.handle <- function(...) tcl("selection", "handle", ...)
tkXselection.own    <- function(...) tcl("selection", "own", ...)

tkwait.variable  <- function(...) tcl("tkwait", "variable", ...)
tkwait.visibility <- function(...) tcl("tkwait", "visibility", ...)
tkwait.window    <- function(...) tcl("tkwait", "window", ...)

## Standard dialogs
tkgetOpenFile    <- function(...) tcl("tk_getOpenFile", ...)
tkgetSaveFile    <- function(...) tcl("tk_getSaveFile", ...)
tkchooseDirectory <- function(...) tcl("tk_chooseDirectory", ...)
tkmessageBox     <- function(...) tcl("tk_messageBox", ...)
tkdialog         <- function(...) tcl("tk_dialog", ...)
tkpopup          <- function(...) tcl("tk_popup", ...)


## File handling functions

tclfile.tail <- function(...) tcl("file", "tail", ...)
tclfile.dir  <- function(...) tcl("file", "dir", ...)
tclopen      <- function(...) tcl("open", ...)
tclclose     <- function(...) tcl("close", ...)
tclputs      <- function(...) tcl("puts", ...)
tclread      <- function(...) tcl("read", ...)

## Tkwinfo actually has a bazillion subcommands, but it's rarely
## used, so let's be lazy

tkwinfo <- function(...) tcl("winfo", ...)

## Not so with tkwm.

tkwm.aspect          <- function(...) tcl("wm", "aspect", ...)
tkwm.client          <- function(...) tcl("wm", "client", ...)
tkwm.colormapwindows <- function(...) tcl("wm", "colormapwindows", ...)
tkwm.command         <- function(...) tcl("wm", "command", ...)
tkwm.deiconify       <- function(...) tcl("wm", "deiconify", ...)
tkwm.focusmodel      <- function(...) tcl("wm", "focusmodel", ...)
tkwm.frame           <- function(...) tcl("wm", "frame", ...)
tkwm.geometry        <- function(...) tcl("wm", "geometry", ...)
tkwm.grid            <- function(...) tcl("wm", "grid", ...)
tkwm.group           <- function(...) tcl("wm", "group", ...)
tkwm.iconbitmap      <- function(...) tcl("wm", "iconbitmap", ...)
tkwm.iconify         <- function(...) tcl("wm", "iconify", ...)
tkwm.iconmask        <- function(...) tcl("wm", "iconmask", ...)
tkwm.iconname        <- function(...) tcl("wm", "iconname ", ...)
tkwm.iconposition    <- function(...) tcl("wm", "iconposition", ...)
tkwm.iconwindow      <- function(...) tcl("wm", "iconwindow ", ...)
tkwm.maxsize         <- function(...) tcl("wm", "maxsize", ...)
tkwm.minsize         <- function(...) tcl("wm", "minsize", ...)
tkwm.overrideredirect <- function(...) tcl("wm", "overrideredirect", ...)
tkwm.positionfrom    <- function(...) tcl("wm", "positionfrom", ...)
tkwm.protocol        <- function(...) tcl("wm", "protocol", ...)
tkwm.resizable       <- function(...) tcl("wm", "resizable", ...)
tkwm.sizefrom        <- function(...) tcl("wm", "sizefrom", ...)
tkwm.state           <- function(...) tcl("wm", "state", ...)
tkwm.title           <- function(...) tcl("wm", "title", ...)
tkwm.transient       <- function(...) tcl("wm", "transient", ...)
tkwm.withdraw        <- function(...) tcl("wm", "withdraw", ...)


### Geometry managers

tkgrid                 <- function(...) tcl("grid", ...)
tkgrid.bbox            <- function(...) tcl("grid", "bbox", ...)
tkgrid.columnconfigure <- function(...) tcl("grid", "columnconfigure", ...)
tkgrid.configure       <- function(...) tcl("grid", "configure", ...)
tkgrid.forget          <- function(...) tcl("grid", "forget", ...)
tkgrid.info            <- function(...) tcl("grid", "info", ...)
tkgrid.location        <- function(...) tcl("grid", "location", ...)
tkgrid.propagate       <- function(...) tcl("grid", "propagate", ...)
tkgrid.rowconfigure    <- function(...) tcl("grid", "rowconfigure", ...)
tkgrid.remove          <- function(...) tcl("grid", "remove", ...)
tkgrid.size            <- function(...) tcl("grid", "size", ...)
tkgrid.slaves          <- function(...) tcl("grid", "slaves", ...)

tkpack           <- function(...) tcl("pack", ...)
tkpack.configure <- function(...) tcl("pack", "configure", ...)
tkpack.forget    <- function(...) tcl("pack", "forget", ...)
tkpack.info      <- function(...) tcl("pack", "info", ...)
tkpack.propagate <- function(...) tcl("pack", "propagate", ...)
tkpack.slaves    <- function(...) tcl("pack", "slaves", ...)

tkplace           <- function(...) tcl("place", ...)
tkplace.configure <- function(...) tcl("place", "configure", ...)
tkplace.forget    <- function(...) tcl("place", "forget", ...)
tkplace.info      <- function(...) tcl("place", "info", ...)
tkplace.slaves    <- function(...) tcl("place", "slaves", ...)



### Widgets commands

tkactivate      <- function(widget, ...) tcl(widget, "activate", ...)
tkadd           <- function(widget, ...) tcl(widget, "add", ...)
tkaddtag        <- function(widget, ...) tcl(widget, "addtag", ...)
tkbbox          <- function(widget, ...) tcl(widget, "bbox", ...)
tkcanvasx       <- function(widget, ...) tcl(widget, "canvasx", ...)
tkcanvasy       <- function(widget, ...) tcl(widget, "canvasy", ...)
tkcget          <- function(widget, ...) tcl(widget, "cget", ...)
tkcompare       <- function(widget, ...) tcl(widget, "compare", ...)
tkconfigure     <- function(widget, ...) tcl(widget, "configure", ...)
tkcoords        <- function(widget, ...) tcl(widget, "coords", ...)
tkcreate        <- function(widget, ...) tcl(widget, "create", ...)
tkcurselection  <- function(widget, ...) tcl(widget, "curselection", ...)
tkdchars        <- function(widget, ...) tcl(widget, "dchars", ...)
tkdebug         <- function(widget, ...) tcl(widget, "debug", ...)
tkdelete        <- function(widget, ...) tcl(widget, "delete", ...)
tkdelta         <- function(widget, ...) tcl(widget, "delta", ...)
tkdeselect      <- function(widget, ...) tcl(widget, "deselect", ...)
tkdlineinfo     <- function(widget, ...) tcl(widget, "dlineinfo", ...)
tkdtag          <- function(widget, ...) tcl(widget, "dtag", ...)
tkdump          <- function(widget, ...) tcl(widget, "dump", ...)
tkentrycget     <- function(widget, ...) tcl(widget, "entrycget", ...)
tkentryconfigure <- function(widget, ...) tcl(widget, "entryconfigure", ...)
tkfind          <- function(widget, ...) tcl(widget, "find", ...)
tkflash         <- function(widget, ...) tcl(widget, "flash", ...)
tkfraction      <- function(widget, ...) tcl(widget, "fraction", ...)
tkget           <- function(widget, ...) tcl(widget, "get", ...)
tkgettags       <- function(widget, ...) tcl(widget, "gettags", ...)
tkicursor       <- function(widget, ...) tcl(widget, "icursor", ...)
tkidentify      <- function(widget, ...) tcl(widget, "identify", ...)
tkindex         <- function(widget, ...) tcl(widget, "index", ...)
tkinsert        <- function(widget, ...) tcl(widget, "insert", ...)
tkinvoke        <- function(widget, ...) tcl(widget, "invoke", ...)
tkitembind      <- function(widget, ...) tcl(widget, "bind", ...)
tkitemcget      <- function(widget, ...) tcl(widget, "itemcget", ...)
tkitemconfigure <- function(widget, ...) tcl(widget, "itemconfigure", ...)
tkitemfocus     <- function(widget, ...) tcl(widget, "focus", ...)
tkitemlower     <- function(widget, ...) tcl(widget, "lower", ...)
tkitemraise     <- function(widget, ...) tcl(widget, "raise", ...)
tkitemscale     <- function(widget, ...) tcl(widget, "scale", ...)
tkmark.gravity  <- function(widget, ...) tcl(widget, "mark", "gravity", ...)
tkmark.names    <- function(widget, ...) tcl(widget, "mark", "names", ...)
tkmark.next     <- function(widget, ...) tcl(widget, "mark", "next", ...)
tkmark.previous <- function(widget, ...) tcl(widget, "mark", "previous", ...)
tkmark.set      <- function(widget, ...) tcl(widget, "mark", "set", ...)
tkmark.unset    <- function(widget, ...) tcl(widget, "mark", "unset", ...)
tkmove          <- function(widget, ...) tcl(widget, "move", ...)
tknearest       <- function(widget, ...) tcl(widget, "nearest", ...)
tkpost          <- function(widget, ...) tcl(widget, "post", ...)
tkpostcascade   <- function(widget, ...) tcl(widget, "postcascade", ...)
tkpostscript    <- function(widget, ...) tcl(widget, "postscript", ...)
tkscan.dragto   <- function(widget, ...) tcl(widget, "scan", "dragto", ...)
tkscan.mark     <- function(widget, ...) tcl(widget, "scan", "mark", ...)
tksearch        <- function(widget, ...) tcl(widget, "search", ...)
tksee           <- function(widget, ...) tcl(widget, "see", ...)
tkselect        <- function(widget, ...) tcl(widget, "select", ...)
tkselection.adjust   <- function(widget, ...)
    tcl(widget, "selection", "adjust", ...)
tkselection.anchor   <- function(widget, ...)
    tcl(widget, "selection", "anchor", ...)
tkselection.clear    <- function(widget, ...)
    tcl(widget, "selection", "clear", ...)
tkselection.from    <- function(widget, ...)
    tcl(widget, "selection", "from", ...)
tkselection.includes <- function(widget, ...)
    tcl(widget, "selection", "includes", ...)
tkselection.present    <- function(widget, ...)
    tcl(widget, "selection", "present", ...)
tkselection.range    <- function(widget, ...)
    tcl(widget, "selection", "range", ...)
tkselection.set      <- function(widget, ...)
    tcl(widget, "selection", "set", ...)
tkselection.to    <- function(widget,...)
    tcl(widget, "selection", "to", ...)
tkset           <- function(widget, ...) tcl(widget, "set", ...)
tksize          <- function(widget, ...) tcl(widget, "size", ...)
tktoggle        <- function(widget, ...) tcl(widget, "toggle", ...)
tktag.add       <- function(widget, ...) tcl(widget, "tag", "add", ...)
tktag.bind      <- function(widget, ...) tcl(widget, "tag", "bind", ...)
tktag.cget      <- function(widget, ...) tcl(widget, "tag", "cget", ...)
tktag.configure <- function(widget, ...) tcl(widget, "tag", "configure", ...)
tktag.delete    <- function(widget, ...) tcl(widget, "tag", "delete", ...)
tktag.lower     <- function(widget, ...) tcl(widget, "tag", "lower", ...)
tktag.names     <- function(widget, ...) tcl(widget, "tag", "names", ...)
tktag.nextrange <- function(widget, ...) tcl(widget, "tag", "nextrange", ...)
tktag.prevrange <- function(widget, ...) tcl(widget, "tag", "prevrange", ...)
tktag.raise     <- function(widget, ...) tcl(widget, "tag", "raise", ...)
tktag.ranges    <- function(widget, ...) tcl(widget, "tag", "ranges", ...)
tktag.remove    <- function(widget, ...) tcl(widget, "tag", "remove", ...)
tktype          <- function(widget, ...) tcl(widget, "type", ...)
tkunpost        <- function(widget, ...) tcl(widget, "unpost", ...)
tkwindow.cget   <- function(widget, ...) tcl(widget, "window", "cget", ...)
tkwindow.configure <- function(widget, ...) tcl(widget,"window","configure",...)
tkwindow.create  <- function(widget, ...) tcl(widget, "window", "create", ...)
tkwindow.names  <- function(widget, ...) tcl(widget, "window", "names", ...)
tkxview         <- function(widget, ...) tcl(widget, "xview", ...)
tkxview.moveto  <- function(widget, ...) tcl(widget, "xview", "moveto", ...)
tkxview.scroll  <- function(widget, ...) tcl(widget, "xview", "scroll", ...)
tkyposition     <- function(widget, ...) tcl(widget, "ypositions", ...)
tkyview         <- function(widget, ...) tcl(widget, "yview", ...)
tkyview.moveto  <- function(widget, ...) tcl(widget, "yview", "moveto", ...)
tkyview.scroll  <- function(widget, ...) tcl(widget, "yview", "scroll", ...)




tkpager <- function(file, header, title, delete.file)
{
    title <- paste(title, header)
    for ( i in seq_along(file) ) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt,
                   if (length(title)) title[(i-1L) %% length(title)+1L] else "")
###        courier font comes out awfully small on some systems
###        txt <- tktext(tt, bg = "grey90", font = "courier")
        txt <- tktext(tt, bg = "grey90")
        scr <- tkscrollbar(tt, repeatinterval = 5,
                           command = function(...) tkyview(txt,...))
	tkconfigure(txt, yscrollcommand = function(...) tkset(scr,...))
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")

        chn <- tcl("open", zfile)
        tkinsert(txt, "end", gsub("_\b","",tclvalue(tcl("read", chn))))
        tcl("close", chn)

        tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        tkfocus(txt)

        if (delete.file) tcl("file", "delete", zfile)
    }
}



