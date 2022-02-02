#  File src/library/graphics/R/units.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

xinch <- function(x=1, warn.log=TRUE) {
    if(warn.log && par("xlog")) warning("x log scale:  xinch() is nonsense")
    x * diff(par("usr")[1L:2])/par("pin")[1L]
}
yinch <- function(y=1, warn.log=TRUE) {
    if(warn.log && par("ylog")) warning("y log scale:  yinch() is nonsense")
    y * diff(par("usr")[3:4])/par("pin")[2L]
}

xyinch <- function(xy=1, warn.log=TRUE) {
    if(warn.log && (par("xlog") || par("ylog")))
	warning("log scale:  xyinch() is nonsense")
    u <- par("usr"); xy * c(u[2L]-u[1L], u[4L]-u[3L]) / par("pin")
}
