.noGenerics <- TRUE

.onLoad <- function(lib, pkg)
{
    options(ts.S.compat = FALSE)
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
