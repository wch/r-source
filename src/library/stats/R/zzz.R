.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
