.First.lib <- function(lib, pkg)
{
    if(!nchar(Sys.getenv("TCL_LIBRARY"))) stop("TCL_LIBRARY is not set")
    library.dynam("tcltk", pkg, lib)
    .C("tcltk_start", PACKAGE="tcltk")
    bringToTop(-1)
    extra <- system.file("exec", package = "tcltk")
    extra <- gsub("\\\\", "/", extra)
    bringToTop(-1) # restore focus to console
    invisible(addTclPath(extra))
}

.Last.lib <- function(libpath) {
    if(is.loaded(symbol.C("tcltk_end"))) {
        .C("tcltk_end", PACKAGE="tcltk")
        dyn.unload(file.path(libpath, "libs", "tcltk.dll"))
        ## <NOTE>
        ## Versions of R prior to 1.4.0 had .Dyn.libs in .AutoloadEnv
        ## (and did not always ensure getting it from there).
        ## Until 1.6.0, we consistently used the base environment.        
        ## Now we have a dynamic variable instead.
        ## </NOTE>
        .Dyn.libs <- .dynLibs()
        num <- match("tcltk", .Dyn.libs)
        .dynLibs(.Dyn.libs[-num])
    }
}
