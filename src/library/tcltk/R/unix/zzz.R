#  File src/library/tcltk/R/unix/zzz.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

.TkUp <- FALSE

.onLoad <- function(libname, pkgname)
{
    ## Use local = FALSE to allow easy loading of Tcl extensions
    library.dynam("tcltk", pkgname, libname, local = FALSE)
    routines <- getDLLRegisteredRoutines("tcltk", addNames = FALSE)
    ns <- asNamespace(pkgname)
    for(i in c(1,4)) # .C and .External
        lapply(routines[[i]],
               function(sym) assign(paste0(".C_", sym$name), sym, envir = ns))
    .TkUp <<- .C(.C_tcltk_init, 0L)[[1L]] == 1L
    addTclPath(system.file("exec", package = "tcltk"))
    invisible()
}

## .onUnload <- function(libpath) {
##     ## precaution in case the DLL has been unloaded without the namespace
##     if(is.loaded("delTcl", PACKAGE="tcltk")) {
##         .C("delTcl", PACKAGE="tcltk")
##         ## if we unload the DLL, get a segfault if we try to use tcltk again.
##         library.dynam.unload("tcltk", libpath)
##     }
## }
