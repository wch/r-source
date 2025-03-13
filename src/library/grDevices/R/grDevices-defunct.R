#  File src/library/grDevices/R/grDevices-defunct.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2025 The R Core Team
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

## <entry>
## Deprecated in 4.4.0
## Defunct    in 4.5.0
xfig <- function (file = if(onefile) "Rplots.fig" else "Rplot%03d.fig",
                  onefile = FALSE, encoding = "none",
                  paper = "default", horizontal = TRUE,
                  width = 0, height = 0, family = "Helvetica",
                  pointsize = 12, bg = "transparent", fg = "black",
                  pagecentre = TRUE,
                  defaultfont = FALSE, textspecial = FALSE)
{
    msg <- gettextf("'%s' was removed in R 4.5.0.\n", "xfig")
    msg <- paste(msg, "Consider an SVG device instead.")
    .Defunct(msg = msg)
}
## <entry/>
