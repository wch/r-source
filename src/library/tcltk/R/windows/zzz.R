.First.lib <- function(lib, pkg)
{
    library.dynam("TclTk", pkg, lib)
    if(!nchar(getenv("TCL_LIBRARY"))) stop("TCL_LIBRARY is not set")
    invisible(.C("tcltk_start"))
}
