.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("splines", libpath)
