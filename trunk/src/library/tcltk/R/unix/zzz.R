.onLoad <- function(lib, pkg)
{
    ## This will get interrupted if there is no display,
    ## so we choose to have the space here.
    packageStartupMessage("Loading Tcl/Tk interface ...", " ",
                          domain = "R-tcltk", appendLF = FALSE)

    ## Use local=FALSE to allow easy loading of Tcl extensions
    library.dynam("tcltk", pkg, lib, local=FALSE)
    .C("tcltk_init", PACKAGE="tcltk")
    addTclPath(system.file("exec", package = "tcltk"))
    packageStartupMessage("done", domain = "R-tcltk")

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
