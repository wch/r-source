# ------ Basics ------

TkOpt <- function() {
    name2opt <- function(x) if ( x != "") paste("-",x,sep="") else ""
    val2string <- function(x) {
        if (is.function(x)) return(.Tkcallback(x))
        if (is.environment(x)) return (TkID(x))
        if (x != "") paste("{",x,"}",sep="", collapse=" ")
    }
    m <- evalq(match.call(expand.dots=F),parent.frame())$...
    nm <- names(m)
    val <- evalq(list(...), parent.frame())
    if (length(val)==0) return("")
    nm <- if(is.null(nm)) rep("",length(val)) else sapply(nm, name2opt)
    val <- sapply(val, val2string)
    paste(as.vector(t(cbind(nm,val))), collapse=" ")
}

TkID <- function(win) get("ID", envir = win)

TkNew.win <- function(ID){
    win <- evalq(new.env(), TkWin)
    assign("ID", ID, envir=win)
    win
}

TkNew.subwin <- function(parent) {
    ID <- evalq({
        num.subwin<-num.subwin+1
        paste(ID, num.subwin, sep=".")
    }, parent)
    TkNew.win(ID)
}

TkInit <- function() {
    # Cannot run this during loading of base lib
    TkWin <<- local({num.subwin<-0 ; environment()})
    .TkRoot <<- TkNew.win("")
    TkVar <<- structure(NULL,class="tkvar")
}

"$.tkvar" <- function(x, name) .Tk(paste("set", name))
"$<-.tkvar" <- function(x, name, value) { .Tk(paste("set", name, value)) ; x }


# ------ Widgets ------

TkWidget <- function (parent, type, ...)
{
    win <- TkNew.subwin(parent)
    .Tk(paste(type, TkID(win), TkOpt()))
    win
}

TkToplevel <- function(parent = .TkRoot, ...)
{
    win <- TkNew.subwin(parent)
    .Tk(paste("toplevel", TkID(win), TkOpt()))
    win
}

TkLabel <- function(parent, ...) {
    win <- TkNew.subwin(parent)
    .Tk(paste("label", TkID(win), TkOpt()))
    win
}

TkButton <- function(parent, ...)
{
    win <- TkNew.subwin(parent)
    .Tk(paste("button", TkID(win), TkOpt()))
    win
}

TkRadioButton <- function(parent, ...)
{
    win <- TkNew.subwin(parent)
    .Tk(paste("radiobutton", TkID(win), TkOpt()))
    win
}

# ------ Window & Geometry managers &c ------


TkCmd <- function(...).Tk(TkOpt()) # generic "catchall"

TkPack <- function(...) {
    .Tk(paste("pack",TkOpt()))
}

"TkTitle<-" <- function(x, value) {
    .Tk(paste("wm title",TkID(x),"{",value,"}"))
    x
}

