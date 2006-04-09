.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    library.dynam("splines", pkgname, libname)
}

.onUnload <- function(libpath)
    library.dynam.unload("splines", libpath)
