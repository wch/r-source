.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    library.dynam("splines", pkgname, libname)
    .C("spline_init", file.path(libname, pkgname), PACKAGE="splines")
}

.onUnload <- function(libpath)
    library.dynam.unload("splines", libpath)
