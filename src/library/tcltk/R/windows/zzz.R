.First.lib <- function(lib, pkg)
{
    if(!nchar(Sys.getenv("TCL_LIBRARY"))) stop("TCL_LIBRARY is not set")
    library.dynam("tcltk", pkg, lib)
    .C("tcltk_start", PACKAGE="tcltk")
    extra <- system.file("exec", package = "tcltk")
    extra <- gsub("\\\\", "/", extra)
    invisible(addTclPath(extra))
}

.Last.lib <- function(libpath) {
    if(is.loaded(symbol.C("tcltk_end"))) {
        .C("tcltk_end", PACKAGE="tcltk")
        dyn.unload(file.path(libpath, "libs", "tcltk.dll"))
        num <- match("tcltk", .Dyn.libs)
        assign(".Dyn.libs", .Dyn.libs[-num], envir=.AutoloadEnv)
    }
}
