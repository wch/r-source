.onLoad <- function(lib, pkg) {
    ## Use local=FALSE to allow easy loading of Tcl extensions
    cat("Loading Tcl/Tk interface ... ") 
    library.dynam("tcltk", pkg, lib, local=FALSE)
    .C("tcltk_init", PACKAGE="tcltk")
    addTclPath(system.file("exec", package = "tcltk"))
    cat("done\n") 
    ## This kind of stuff could be added to build a more extensive GUI ##
    ##   not a good idea to do this unconditionally                    ##
###    userpager <- getOption("tkpager")
###    options(pager=if (is.null(userpager)) tkpager else userpager)
}

.onUnload <- function(libpath) {
###    options(pager=NULL)
    .C("delTcl", PACKAGE="tcltk")
    ## library.dynam.unload("tcltk", libpath)
}
