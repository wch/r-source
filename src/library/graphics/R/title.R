#  File src/library/graphics/R/title.R
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

title <- function(main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                  line=NA, outer=FALSE, ...)
{
    main <- as.graphicsAnnot(main)
    sub <- as.graphicsAnnot(sub)
    xlab <- as.graphicsAnnot(xlab)
    ylab <- as.graphicsAnnot(ylab)
    .External.graphics(C_title, main, sub, xlab, ylab, line, outer, ...)
    invisible()
}
