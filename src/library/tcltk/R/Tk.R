### ------ Basics ------


.Tcl <- function(...).External("dotTcl",...)
.Tcl.callback <- function(...).External("dotTclcallback",...)

.Tcl.args <- function(...) {
    ## Convert argument tags to option names (i.e. stick "-" in front)
    name2opt <- function(x)
        if ( x != "")
            paste("-",x,sep="")
        else ""

    ## Convert arguments. R functions and windows require special treatment
    ## everything else is converted to strings
    val2string <- function(x) {
        if (is.null(x)) return("")
        if (is.tkwin(x)){current.win <<- x ; return (.Tk.ID(x))}
        if (is.function(x)){
            callback <- .Tcl.callback(x)
            assign(callback, .Alias(x), envir=current.win)
            return(callback)
        }
        paste("{", x, "}", sep="", collapse=" ")
    }

    val <- list(...)
    nm <- names(val)

    if (length(val) == 0) return("")
    nm <- if (is.null(nm))
        rep("", length(val))
    else
        sapply(nm, name2opt)

    ## This is a bit dodgy: we need to ensure that callbacks don't get
    ## garbage collected, so we try registering them with the relevant
    ## window, which is assumed to be the last preceding window
    ## argument during val2string processing if one occurs, or the
    ## "win" variable of the caller (tkwidget calls) or as a last
    ## resort .TkRoot. What a mess!

    current.win <-
        if (exists("win", envir=parent.frame()))
            get("win", envir=parent.frame())
        else .TkRoot

    val <- sapply(val, val2string)
    paste(as.vector(rbind(nm, val)), collapse=" ")
}

.Tk.ID <- function(win) get("ID", envir = win)

.Tk.newwin <- function(ID){
    win <- evalq(new.env(), .TkWin)
    assign("ID", ID, envir=win)
    class(win) <- "tkwin"
    win
}

.Tk.subwin <- function(parent) {
    ID <- evalq({
        num.subwin<-num.subwin+1
        paste(ID, num.subwin, sep=".")
    }, parent)
    win<-.Tk.newwin(ID)
    assign(ID, win, envir=parent)
    assign("parent", parent, envir=win)
    win
}

tkdestroy  <- function(win) {
    tkcmd("destroy", win)
    rm(list=.Tk.ID(win), envir=get("parent", envir=win))
}

is.tkwin <- function(x) inherits(x, "tkwin")

"$.tclvar" <- function(x, name) .Tcl(paste("set", name))
"$<-.tclvar" <- function(x, name, value) {
    .Tcl(paste("set ", name, " {", value,"}", sep=""))
    x
}


.TkWin  <- local({num.subwin<-0 ; environment()})
.TkRoot <- .Tk.newwin("")
tclvar  <- structure(NULL,class="tclvar")


# ------ Widgets ------

tkwidget <- function (parent, type, ...) # generic
{
    win <- .Tk.subwin(parent)
    .Tcl(paste(type, .Tk.ID(win), .Tcl.args(...)))
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
tktoplevel    <- function(parent=.TkRoot,...) tkwidget(parent,"toplevel",...)


### ------ Window & Geometry managers, widget commands &c ------


tkcmd <- function(...) .Tcl(.Tcl.args(...)) # generic "catchall"


tktitle <- function(x) tkcmd("wm", "title", x)

"tktitle<-" <- function(x, value) {
    tkcmd("wm", "title", x, value)
    x
}

tkbell     <- function(...) tkcmd("bell", ...)
tkbind     <- function(...) tkcmd("bind", ...)
tkbindtags <- function(...) tkcmd("bindtags", ...)
tkfocus    <- function(...) tkcmd("focus", ...)
tklower    <- function(...) tkcmd("lower", ...)
tkraise    <- function(...) tkcmd("raise", ...)


tkclipboard.append <- function(...) tkcmd("clipboard", "append", ...)
tkclipboard.clear  <- function(...) tkcmd("clipboard", "clear", ...)


tkevent.add      <- function(...) tkcmd("event", "add", ...)
tkevent.delete   <- function(...) tkcmd("event", "delete", ...)
tkevent.generate <- function(...) tkcmd("event", "generate", ...)
tkevent.info     <- function(...) tkcmd("event", "info", ...)


tkfont.actual    <- function(...) tkcmd("font", "actual", ...)
tkfont.configure <- function(...) tkcmd("font", "configure", ...)
tkfont.create    <- function(...) tkcmd("font", "create", ...)
tkfont.delete    <- function(...) tkcmd("font", "delete", ...)
tkfont.families  <- function(...) tkcmd("font", "families", ...)
tkfont.measure   <- function(...) tkcmd("font", "measure", ...)
tkfont.metrics   <- function(...) tkcmd("font", "metrics", ...)
tkfont.names     <- function(...) tkcmd("font", "names", ...)

tkgrab         <- function(...) tkcmd("grab", ...)
tkgrab.current <- function(...) tkcmd("grab", "current", ...)
tkgrab.release <- function(...) tkcmd("grab", "release", ...)
tkgrab.set     <- function(...) tkcmd("grab", "set", ...)
tkgrab.status  <- function(...) tkcmd("grab", "status", ...)

## NB: some widgets also have a selection.clear command, hence the "X".
## tkselection.clear might be made a generic function instead.
tkXselection.clear  <- function(...) tkcmd("selection", "clear", ...)
tkXselection.get    <- function(...) tkcmd("selection", "get", ...)
tkXselection.handle <- function(...) tkcmd("selection", "handle", ...)
tkXselection.own    <- function(...) tkcmd("selection", "own", ...)

tkwait.variable  <- function(...) tkcmd("tkwait", "variable", ...)
tkwait.visibility<- function(...) tkcmd("tkwait", "visibility", ...)
tkwait.window    <- function(...) tkcmd("tkwait", "window", ...)

## Tkwinfo actually has a bazillion subcommands, but it's rarely
## used, so let's be lazy

tkwinfo <- function(...) tkcmd("raise", ...)

## Not so with tkwm.

tkwm.aspect          <- function(...) tkcmd("wm", "aspect", ...)
tkwm.client          <- function(...) tkcmd("wm", "client", ...)
tkwm.colormapwindows <- function(...) tkcmd("wm", "colormapwindows", ...)
tkwm.command         <- function(...) tkcmd("wm", "command", ...)
tkwm.deiconify       <- function(...) tkcmd("wm", "deiconify", ...)
tkwm.focusmodel      <- function(...) tkcmd("wm", "focusmodel", ...)
tkwm.frame           <- function(...) tkcmd("wm", "frame", ...)
tkwm.geometry        <- function(...) tkcmd("wm", "geometry", ...)
tkwm.grid            <- function(...) tkcmd("wm", "grid", ...)
tkwm.group           <- function(...) tkcmd("wm", "group", ...)
tkwm.iconbitmap      <- function(...) tkcmd("wm", "iconbitmap", ...)
tkwm.iconify         <- function(...) tkcmd("wm", "iconify", ...)
tkwm.iconmask        <- function(...) tkcmd("wm", "iconmask", ...)
tkwm.iconname        <- function(...) tkcmd("wm", "iconname ", ...)
tkwm.iconposition    <- function(...) tkcmd("wm", "iconposition", ...)
tkwm.iconwindow      <- function(...) tkcmd("wm", "iconwindow ", ...)
tkwm.maxsize         <- function(...) tkcmd("wm", "maxsize", ...)
tkwm.minsize         <- function(...) tkcmd("wm", "minsize", ...)
tkwm.overrideredirect<- function(...) tkcmd("wm", "overrideredirect", ...)
tkwm.positionfrom    <- function(...) tkcmd("wm", "positionfrom", ...)
tkwm.protocol        <- function(...) tkcmd("wm", "protocol", ...)
tkwm.resizable       <- function(...) tkcmd("wm", "resizable", ...)
tkwm.sizefrom        <- function(...) tkcmd("wm", "sizefrom", ...)
tkwm.state           <- function(...) tkcmd("wm", "state", ...)
tkwm.title           <- function(...) tkcmd("wm", "title", ...)
tkwm.transient       <- function(...) tkcmd("wm", "transient", ...)
tkwm.withdraw        <- function(...) tkcmd("wm", "withdraw", ...)


### Geometry managers

tkgrid                 <- function(...) tkcmd("grid", ...)
tkgrid.bbox            <- function(...) tkcmd("grid", "bbox", ...)
tkgrid.columnconfigure <- function(...) tkcmd("grid", "columnconfigure", ...)
tkgrid.configure       <- function(...) tkcmd("grid", "configure", ...)
tkgrid.forget          <- function(...) tkcmd("grid", "forget", ...)
tkgrid.info            <- function(...) tkcmd("grid", "info", ...)
tkgrid.location        <- function(...) tkcmd("grid", "location", ...)
tkgrid.propagate       <- function(...) tkcmd("grid", "propagate", ...)
tkgrid.rowconfigure    <- function(...) tkcmd("grid", "rowconfigure", ...)
tkgrid.remove          <- function(...) tkcmd("grid", "remove", ...)
tkgrid.size            <- function(...) tkcmd("grid", "size", ...)
tkgrid.slaves          <- function(...) tkcmd("grid", "slaves", ...)

tkpack           <- function(...) tkcmd("pack", ...)
tkpack.configure <- function(...) tkcmd("pack", "configure", ...)
tkpack.forget    <- function(...) tkcmd("pack", "forget", ...)
tkpack.info      <- function(...) tkcmd("pack", "info", ...)
tkpack.propagate <- function(...) tkcmd("pack", "propagate", ...)
tkpack.slaves    <- function(...) tkcmd("pack", "slaves", ...)

tkplace           <- function(...) tkcmd("place", ...)
tkplace.configure <- function(...) tkcmd("place", "configure", ...)
tkplace.forget    <- function(...) tkcmd("place", "forget", ...)
tkplace.info      <- function(...) tkcmd("place", "info", ...)
tkplace.slaves    <- function(...) tkcmd("place", "slaves", ...)



### Widgets commands 

tkactivate  <- function(widget, ...) tkcmd(widget, "activate", ...)
tkadd       <- function(widget, ...) tkcmd(widget, "add", ...)
tkbbox      <- function(widget, ...) tkcmd(widget, "box", ...)
tkcompare   <- function(widget, ...) tkcmd(widget, "compare", ...)
tkconfigure <- function(widget, ...) tkcmd(widget, "configure", ...)
tkconfigure.entry<-function(widget, ...) tkcmd(widget, "entryconfigure", ...)
tkcget      <- function(widget, ...) tkcmd(widget, "cget", ...)
tkcget.entry<- function(widget, ...) tkcmd(widget, "entrycget", ...)
tkcoords    <- function(widget, ...) tkcmd(widget, "coords", ...)
tkcurselection<-function(widget,...) tkcmd(widget, "curselection", ...)
tkdebug     <- function(widget, ...) tkcmd(widget, "debug", ...)
tkdelete    <- function(widget, ...) tkcmd(widget, "delete", ...)
tkdelta     <- function(widget, ...) tkcmd(widget, "delta", ...)
tkdeselect  <- function(widget, ...) tkcmd(widget, "deselect", ...)
tkdlineinfo <- function(widget, ...) tkcmd(widget, "dlineinfo", ...)
tkdump      <- function(widget, ...) tkcmd(widget, "dump", ...)
tkflash     <- function(widget, ...) tkcmd(widget, "flash", ...)
tkfraction  <- function(widget, ...) tkcmd(widget, "fraction", ...)
tkget       <- function(widget, ...) tkcmd(widget, "get", ...)
tkicursor   <- function(widget, ...) tkcmd(widget, "icursor", ...)
tkidentify  <- function(widget, ...) tkcmd(widget, "identify", ...)
tkimage.cget     <- function(widget, ...) tkcmd(widget,"image","cget",...)
tkimage.configure<- function(widget, ...) tkcmd(widget,"image","configure",...)
tkimage.create   <- function(widget, ...) tkcmd(widget,"image","create",...)
tkimage.names    <- function(widget, ...) tkcmd(widget,"image","names",...)
tkindex     <- function(widget, ...) tkcmd(widget, "index", ...)
tkinsert    <- function(widget, ...) tkcmd(widget, "insert", ...)
tkinvoke    <- function(widget, ...) tkcmd(widget, "invoke", ...)
tkmark.gravity  <- function(widget, ...) tkcmd(widget, "mark", "gravity", ...)
tkmark.names    <- function(widget, ...) tkcmd(widget, "mark", "names", ...)
tkmark.next     <- function(widget, ...) tkcmd(widget, "mark", "next", ...)
tkmark.previous <- function(widget, ...) tkcmd(widget, "mark", "previous", ...)
tkmark.set      <- function(widget, ...) tkcmd(widget, "mark", "set", ...)
tkmark.unset    <- function(widget, ...) tkcmd(widget, "mark", "unset", ...)
tknearest   <- function(widget, ...) tkcmd(widget, "nearest", ...)
tkpost      <- function(widget, ...) tkcmd(widget, "post", ...)
tkpostcascade<-function(widget, ...) tkcmd(widget, "postcascade", ...)
tkscan.mark <- function(widget, ...) tkcmd(widget, "scan", "mark", ...)
tkscan.dragto<-function(widget, ...) tkcmd(widget, "scan", "dragto", ...)
tksearch    <- function(widget, ...) tkcmd(widget, "search", ...)
tksee       <- function(widget, ...) tkcmd(widget, "see", ...)
tkselect    <- function(widget, ...) tkcmd(widget, "select", ...)
tkselection.adjust   <- function(widget, ...)
    tkcmd(widget, "selection", "adjust", ...)
tkselection.anchor   <- function(widget, ...)
    tkcmd(widget, "selection", "anchor", ...)
tkselection.clear    <- function(widget, ...)
    tkcmd(widget, "selection", "clear", ...)
tkselection.from    <- function(widget, ...)
    tkcmd(widget, "selection", "from", ...)
tkselection.includes <- function(widget, ...)
    tkcmd(widget, "selection", "includes", ...)
tkselection.present    <- function(widget, ...)
    tkcmd(widget, "selection", "present", ...)
tkselection.range    <- function(widget, ...)
    tkcmd(widget, "selection", "range", ...)
tkselection.set      <- function(widget, ...)
    tkcmd(widget, "selection", "set", ...)
tkselection.to    <- function(widget,...)
    tkcmd(widget, "selection", "to", ...)
tkset       <- function(widget, ...) tkcmd(widget, "set", ...)
tksize      <- function(widget, ...) tkcmd(widget, "size", ...)
tktoggle    <- function(widget, ...) tkcmd(widget, "toggle", ...)
tktag.add   <- function(widget, ...) tkcmd(widget, "tag", "add", ...)
tktag.bind      <- function(widget, ...) tkcmd(widget, "tag", "bind", ...)
tktag.cget      <- function(widget, ...) tkcmd(widget, "tag", "cget", ...)
tktag.configure <- function(widget, ...) tkcmd(widget, "tag", "configure", ...)
tktag.delete    <- function(widget, ...) tkcmd(widget, "tag", "delete", ...)
tktag.lower     <- function(widget, ...) tkcmd(widget, "tag", "lower", ...)
tktag.names     <- function(widget, ...) tkcmd(widget, "tag", "names", ...)
tktag.nextrange <- function(widget, ...) tkcmd(widget, "tag", "nextrange", ...)
tktag.prevrange <- function(widget, ...) tkcmd(widget, "tag", "prevrange", ...)
tktag.raise     <- function(widget, ...) tkcmd(widget, "tag", "raise", ...)
tktag.ranges    <- function(widget, ...) tkcmd(widget, "tag", "ranges", ...)
tktag.remove    <- function(widget, ...) tkcmd(widget, "tag", "remove", ...)
tktype      <- function(widget, ...) tkcmd(widget, "type", ...)
tkunpost    <- function(widget, ...) tkcmd(widget, "unpost", ...)
tkwindow.cget     <-function(widget, ...)tkcmd(widget, "window", "cget", ...)
tkwindow.configure<-function(widget, ...)tkcmd(widget,"window","configure",...)
tkwindow.create   <-function(widget, ...)tkcmd(widget, "window", "create", ...)
tkwindow.names    <-function(widget, ...)tkcmd(widget, "window", "names", ...)
tkxview     <- function(widget, ...) tkcmd(widget, "xview", ...)
tkxview.moveto<-function(widget, ...)tkcmd(widget, "xview", "moveto", ...)
tkxview.scroll<-function(widget, ...)tkcmd(widget, "xview", "scroll", ...)
tkyposition <- function(widget, ...) tkcmd(widget, "ypositions", ...)
tkyview     <- function(widget, ...) tkcmd(widget, "yview", ...)
tkyview.moveto<-function(widget, ...)tkcmd(widget, "yview", "moveto", ...)
tkyview.scroll<-function(widget, ...)tkcmd(widget, "yview", "scroll", ...)





