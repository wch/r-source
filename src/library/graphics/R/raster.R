#  File src/library/graphics/R/rect.R
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

rasterImage <- function (image, xleft, ybottom, xright, ytop,
                         angle = 0, interpolate = TRUE, ...)
{
    .External.graphics(C_raster,
                       if (inherits(image, "nativeRaster")) image else as.raster(image),
                       as.double(xleft), as.double(ybottom),
                       as.double(xright), as.double(ytop),
                       as.double(angle), as.logical(interpolate), ...)
    invisible()
}
