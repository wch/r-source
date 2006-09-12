## environment used for evaluation in the C code
## assigned here to protect from GC, but otherwise unused at R level
.GridEvalEnv <- new.env()

# This should be the only grid global variable(?)
# It contains the list of state structures corresponding to the
# state for each device.
# The state structures are stored in here so that they do not
# get garbage collected.
assign(".GRID.STATE", vector("list", 64), envir = .GridEvalEnv)
## 64 comes from the maximum number of R devices allowed to be open at
## one time, see R_MaxDevices in Graphics.h.

.noGenerics <- TRUE

.onLoad <- function(lib, pkg)
{
    ## want eval in C code to see unexported objects
    environment(.GridEvalEnv) <- asNamespace("grid")
    .Call(L_initGrid, .GridEvalEnv)
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
        .Call(L_killGrid)
    }
    library.dynam.unload("grid", libpath)
}

## .gridplot.hook <- function()
## {
##     pushViewport(viewport(width=unit(1, "npc") - unit(1, "lines"),
## 			  x=0, just="left"))
##     grid.text(paste("help(", ..nameEx, ")"),
## 	      x=unit(1, "npc") + unit(0.5, "lines"),
## 	      y=unit(0.8, "npc"), rot=90,
## 	      gp=gpar(col="orchid"))
## }
