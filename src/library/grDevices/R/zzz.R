.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
    .C("grDevices_init", file.path(libname, pkgname), PACKAGE="grDevices")
