#  File src/library/utils/R/object.size.R
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

object.size <- function(x)
    structure(.Call(C_objectSize, x), class = "object_size")

print.object_size <-
    function(x, quote = FALSE, units = "b", ...)
{
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb",
                                "B", "KB", "MB", "GB"))
    if (units == "auto") {
        if (x >= 1024^3) units <- "Gb"
        else if (x >= 1024^2) units <- "Mb"
        else if (x >= 1024) units <- "Kb"
        else units <- "b"
    }
    y <- switch(units,
                "b" =, "B" = paste(x, "bytes"),
                "Kb" =, "KB" = paste(round(x/1024, 1L), "Kb"),
                "Mb" =, "MB" = paste(round(x/1024^2, 1L), "Mb"),
                "Gb" =, "GB" = paste(round(x/1024^3, 1L), "Gb")
                )
    if(quote) print.default(y, ...) else cat(y, "\n", sep = "")
    invisible(x)
}
