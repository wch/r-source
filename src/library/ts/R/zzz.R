.onLoad <- function(lib, pkg)
{
    options(ts.S.compat = FALSE)
}

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("ts", libpath)
