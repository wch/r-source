.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lqs", libpath)
