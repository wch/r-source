.First.lib <- function(lib, pkg)
{
    library.dynam("TclTk", pkg, lib)
    if(!nchar(getenv("TCL_LIBRARY"))) stop("TCL_LIBRARY is not set")
    invisible(.C("tcltk_start", PACKAGE="TclTk"))
}

.Last.lib <- function(libpath) {
    .C("tcltk_end", PACKAGE="TclTk")
    dyn.unload(file.path(libpath, "libs", "TclTk.dll"))
}
