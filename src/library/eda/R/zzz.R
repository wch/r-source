.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("eda", libpath)
