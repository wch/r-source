.onLoad <- function(lib, pkg)
{
    packageStartupMessage("Loading Tcl/Tk interface ...", domain = "R-tcltk",
                          appendLF = FALSE)
    if(nchar(Sys.getenv("MY_TCLTK"))) {
        library.dynam("tcltk", pkg, lib)
    } else {
        if(!file.exists(file.path(R.home(), "Tcl")))
            stop("Tcl/Tk support files were not installed", call.=FALSE)
        Sys.setenv("TCL_LIBRARY"=file.path(R.home(), "Tcl/lib/tcl8.4"))
        opath <-  Sys.getenv("PATH")
        tclbin <- file.path(R.home(), "Tcl/bin")
        Sys.setenv(PATH=paste(tclbin, opath, sep=";"))
        library.dynam("tcltk", pkg, lib)
        Sys.setenv(PATH=opath)
    }
    .C("tcltk_start", PACKAGE="tcltk")
    extra <- system.file("exec", package = "tcltk")
    extra <- gsub("\\\\", "/", extra)
    addTclPath(extra)
    packageStartupMessage(" ", "done", domain = "R-tcltk")
    invisible()
}

.onUnload <- function(libpath) {
    if(is.loaded(symbol.C("tcltk_end"))) {
        .C("tcltk_end", PACKAGE="tcltk")
## unloading the DLL used to work, but it seems Tcl/Tk 8.4.1 does
## not like being reinitialized
        ## library.dynam.unload("tcltk", libpath)
    }
}
