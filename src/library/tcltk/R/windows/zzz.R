.First.lib <- function(lib, pkg)
{
    if(!nchar(getenv("TCL_LIBRARY"))) stop("TCL_LIBRARY is not set")
    library.dynam("TclTk", pkg, lib)
    invisible(.C("tcltk_start", PACKAGE="TclTk"))
}

.Last.lib <- function(libpath) {
    if(is.loaded(symbol.C("tcltk_end"))) {
        .C("tcltk_end", PACKAGE="TclTk")
        dyn.unload(file.path(libpath, "libs", "TclTk.dll"))
        num <- match("TclTk", .Dyn.libs)
        assign(".Dyn.libs", .Dyn.libs[-num], envir=.AutoloadEnv)
    }
}
