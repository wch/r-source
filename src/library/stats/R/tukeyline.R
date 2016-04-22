#  File src/library/stats/R/tukeyline.R
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

line <- function(x, y = NULL)
{
    xy <- xy.coords(x, y, setLab = FALSE)
    ok <- complete.cases(xy$x,xy$y)
    Call <- sys.call()
    structure(.Call(C_tukeyline, as.double(xy$x[ok]), as.double(xy$y[ok]),
                    Call), class = "tukeyline")
}
residuals.tukeyline <- residuals.lm
print.tukeyline <- print.lm
