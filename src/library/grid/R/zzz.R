## environment used for evaluation in the C code
## assigned here to protect from GC, but otherwise unused at R level
.GridEvalEnv <- new.env()

.onLoad <- function(lib, pkg)
{
    library.dynam( "grid", pkg, lib )
    ## want eval in C code to see unexported objects
    environment(.GridEvalEnv) <- asNamespace("grid")
    .Call("L_initGrid", .GridEvalEnv, PACKAGE="grid")
    .grid.loaded <<- TRUE
}

.onUnload <- function(libpath)
{
    if (.grid.loaded) {
        ## Kill all existing devices to avoid replay
        ## of display list which tries to run grid code
        ## Not very friendly to other registered graphics systems
        ## but its safety first for now
        graphics.off()
        .Call("L_killGrid", PACKAGE="grid")
    }
    library.dynam.unload("grid", libpath)
}
