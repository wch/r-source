.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    options(ts.S.compat = FALSE)
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
