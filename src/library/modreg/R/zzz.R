.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("modreg", libpath)
