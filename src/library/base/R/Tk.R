# ------ Basics ------

.Tcl.args <- function() {
    # convert argument tags to option names (stick "-" in front)
    name2opt <- function(x)
        if ( x != "")
            paste("-",x,sep="")
        else ""
    # convert arguments. R functions and windows require special treatment
    # everything else is converted to strings
    val2string <- function(x) {
        if (is.function(x)) return(.Tcl.callback(x))
        if (is.tkwin(x)) return (.Tk.ID(x))
        if (x != "") paste("{", x, "}", sep="", collapse=" ")
    }
    m <- evalq(match.call(expand.dots=F), parent.frame())$...
    nm <- names(m)
    val <- evalq(list(...), parent.frame())
    if (length(val) == 0) return("")
    nm <- if (is.null(nm)) rep("", length(val)) else sapply(nm, name2opt)
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
    .Tk.newwin(ID)
}

## tkinit should die when we get tcl/tk as a package. It is really
## only present because local() doesn't work during loadup.

tkinit <- function() {
    # Cannot run this during loading of base lib
    .TkWin <<- local({num.subwin<-0 ; environment()})
    .TkRoot <<- .Tk.newwin("")
    tclvar <<- structure(NULL,class="tclvar")
}

is.tkwin <- function(x) inherits(x, "tkwin")

"$.tclvar" <- function(x, name) .Tcl(paste("set", name))
"$<-.tclvar" <- function(x, name, value) {.Tcl(paste("set", name, value)); x}


# ------ Widgets ------

tkwidget <- function (parent, type, ...) # generic
{
    win <- .Tk.subwin(parent)
    .Tcl(paste(type, .Tk.ID(win), .Tcl.args()))
    win
}

tkbutton <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("button", .Tk.ID(win), .Tcl.args()))
    win
}

tkcanvas <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("canvas", .Tk.ID(win), .Tcl.args()))
    win
}

tkcheckbutton <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("checkbutton", .Tk.ID(win), .Tcl.args()))
    win
}

tkentry <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("entry", .Tk.ID(win), .Tcl.args()))
    win
}

tkframe <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("frame", .Tk.ID(win), .Tcl.args()))
    win
}

tklabel <- function(parent, ...) {
    win <- .Tk.subwin(parent)
    .Tcl(paste("label", .Tk.ID(win), .Tcl.args()))
    win
}

tklistbox <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("listbox", .Tk.ID(win), .Tcl.args()))
    win
}

tkmenu <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("tkmenu", .Tk.ID(win), .Tcl.args()))
    win
}

tkmenubutton <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("menubutton", .Tk.ID(win), .Tcl.args()))
    win
}

tkradiobutton <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("radiobutton", .Tk.ID(win), .Tcl.args()))
    win
}

tkscale <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("scale", .Tk.ID(win), .Tcl.args()))
    win
}

tkscrollbar <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("scrollbar", .Tk.ID(win), .Tcl.args()))
    win
}

tktext <- function(parent, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("text", .Tk.ID(win), .Tcl.args()))
    win
}

tktoplevel <- function(parent = .TkRoot, ...)
{
    win <- .Tk.subwin(parent)
    .Tcl(paste("toplevel", .Tk.ID(win), .Tcl.args()))
    win
}

# ------ Window & Geometry managers, widget commands &c ------


tkcmd <- function(...) .Tcl(.Tcl.args()) # generic "catchall"

tkpack <- function(...) .Tcl(paste("pack", .Tcl.args()))

tkconfigure <- function(widget, ...)
    .Tcl(paste(widget, "configure", .Tcl.args()))

tktitle <- function(x) .Tcl(paste("wm title",.Tk.ID(x)))

"tktitle<-" <- function(x, value) {
    .Tcl(paste("wm title",.Tk.ID(x),"{",value,"}"))
    x
}

