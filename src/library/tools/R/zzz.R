.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
    .C("tools_init", file.path(libname, pkgname), PACKAGE="tools")

.onUnload <- function(libpath)
    library.dynam.unload("tools", libpath)
