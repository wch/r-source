.First.lib <- function(lib, pkg) {
    if (.Platform$GUI=="GNOME")
	stop("Tcl/Tk library does not work with GNOME interface")
    library.dynam("tcltk", pkg, lib)
    .C("tcltk_init", PACKAGE="tcltk")
    addTclPath(system.file("exec", package = "tcltk"))
    ## This kind of stuff could be added to build a more extensive GUI ##
    ##   not a good idea to do this unconditionally                    ##
###    userpager <- getOption("tkpager")
###    options(pager=if (is.null(userpager)) tkpager else userpager)
}

.Last.lib <- function(libpath) {
###    options(pager=NULL)
    .C("delTcl", PACKAGE="tcltk")
###    dyn.unload(file.path(libpath, "libs",
###                         paste("tcltk", .Platform$"dynlib.ext", sep="")))
}
