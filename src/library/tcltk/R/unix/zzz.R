.First.lib <- function(lib, pkg) {
    library.dynam("tcltk", pkg, lib)
    .C("tcltk_init", PACKAGE="tcltk")
}

.Last.lib <- function(libpath) {
    .C("delTcl", PACKAGE="tcltk")
#    dyn.unload(file.path(libpath, "libs",
#                         paste("tcltk", .Platform$"dynlib.ext", sep="")))
}
