#  File src/library/tcltk/R/windows/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

.TkUp <- TRUE

.onLoad <- function(libname, pkgname)
{
    if(!nzchar(tclbin <- Sys.getenv("MY_TCLTK"))) {
        tclbin <- file.path(R.home(), "Tcl",
                            if(.Machine$sizeof.pointer == 8) "bin64" else "bin")
        if(!file.exists(tclbin))
            stop("Tcl/Tk support files were not installed", call.=FALSE)
        if(.Machine$sizeof.pointer == 8) {
            lib64 <- gsub("\\", "/", file.path(R.home(), "Tcl", "lib64"),
                          fixed=TRUE)
            Sys.setenv(TCLLIBPATH = lib64)
        } else Sys.unsetenv("TCLLIBPATH") # in case called from a 64-bit process
    }
    library.dynam("tcltk", pkgname, libname, DLLpath = tclbin)
    routines <- getDLLRegisteredRoutines("tcltk", addNames = FALSE)
    ns <- asNamespace(pkgname)
    for(i in c(1,4))
        lapply(routines[[i]],
               function(sym) assign(paste0(".C_", sym$name), sym, envir = ns))
    .C(.C_tcltk_start)
    addTclPath(system.file("exec", package = "tcltk"))
    invisible()
}


.onUnload <- function(libpath)
{
    ## precaution in case the DLL has been unloaded without the namespace
    if(is.loaded("tcltk_end", PACKAGE="tcltk"))
        .C("tcltk_end", PACKAGE="tcltk")
}


## .onUnload <- function(libpath) {
##     ## precaution in case the DLL has been unloaded without the namespace
##     if(is.loaded("tcltk_end", PACKAGE="tcltk")) {
##         .C("tcltk_end", PACKAGE="tcltk")
##         ## unloading the DLL used to work with 8.3,
##         ## but it seems Tcl/Tk >= 8.4 does not like being reinitialized
##         ## library.dynam.unload("tcltk", libpath)
##     }
## }
