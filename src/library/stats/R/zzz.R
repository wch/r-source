.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    options(ts.S.compat = FALSE)
    .C("stats_init", file.path(libname, pkgname), PACKAGE="stats")
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
