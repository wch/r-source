.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("tools", libpath)
