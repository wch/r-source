#  File src/library/grid/R/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## environment used for evaluation in the C code
## assigned here to protect from GC, but otherwise unused at R level
.GridEvalEnv <- new.env()

# This should be the only grid global variable(?)
# It contains the list of state structures corresponding to the
# state for each device.
# The state structures are stored in here so that they do not
# get garbage collected.
assign(".GRID.STATE", vector("list", 64L), envir = .GridEvalEnv)
## 64 comes from the maximum number of R devices allowed to be open at
## one time, see R_MaxDevices in Graphics.h.

.noGenerics <- TRUE

utils::globalVariables(c("n", "vp", "path"))

.onLoad <- function(libname, pkgname)
{
    ## want eval in C code to see unexported objects
    environment(.GridEvalEnv) <- asNamespace("grid")
    .Call(C_initGrid, .GridEvalEnv)
    .grid.loaded <<- TRUE
}

.onUnload <- function(libpath)
{
    if (.grid.loaded) {
        ## Kill all existing devices to avoid replay
        ## of display list which tries to run grid code
        ## Not very friendly to other registered graphics systems
        ## but its safety first for now
        if(length(.Devices) > 1L)
            warning("shutting down all devices when unloading 'grid' namespace",
                    call. = FALSE)
        graphics.off()
        .Call(C_killGrid)
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
