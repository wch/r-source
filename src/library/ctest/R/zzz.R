.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("ctest", libpath)
