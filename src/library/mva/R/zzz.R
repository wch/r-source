.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("mva", libpath)
