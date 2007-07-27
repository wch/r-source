#  File src/library/tcltk/R/unix/zzz.R
#  Part of the R package, http://www.R-project.org
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

.onLoad <- function(lib, pkg)
{
    ## This will get interrupted if there is no display,
    ## so we choose to have the space here.
    packageStartupMessage("Loading Tcl/Tk interface ...", " ",
                          domain = "R-tcltk", appendLF = FALSE)

    ## Use local=FALSE to allow easy loading of Tcl extensions
    library.dynam("tcltk", pkg, lib, local=FALSE)
    .C("tcltk_init", PACKAGE="tcltk")
    addTclPath(system.file("exec", package = "tcltk"))
    packageStartupMessage("done", domain = "R-tcltk")

    ## This kind of stuff could be added to build a more extensive GUI ##
    ##   not a good idea to do this unconditionally                    ##
###    userpager <- getOption("tkpager")
###    options(pager=if (is.null(userpager)) tkpager else userpager)
}

.onUnload <- function(libpath) {
###    options(pager=NULL)
    .C("delTcl", PACKAGE="tcltk")
    ## library.dynam.unload("tcltk", libpath)
}
