#  File src/library/utils/R/object.size.R
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

object.size <- function(x)
    structure(.Call(C_objectSize, x), class = "object_size")

format.object_size <- function(x, units = "b", ...)
{
    units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb", "Tb", "Pb",
				"B", "KB", "MB", "GB", "TB", "PB",
				"KiB", "MiB", "GiB", "TiB",
				"PiB", "EiB", "ZiB", "YiB"))
    if (units == "auto")
	units <-
	    if      (x >= 1024^4) "Tb"
            else if (x >= 1024^3) "Gb"
	    else if (x >= 1024^2) "Mb"
	    else if (x >= 1024  ) "Kb" else "b"
    switch(units,
	   "b" =, "B" = paste(x, "bytes"),
	   "Kb" =, "KB" = paste(round(x/1024  , 1L), "Kb"),
	   "Mb" =, "MB" = paste(round(x/1024^2, 1L), "Mb"),
	   "Gb" =, "GB" = paste(round(x/1024^3, 1L), "Gb"),
	   "Tb" =, "TB" = paste(round(x/1024^3, 1L), "Tb"),
	   "Pb" =, "PB" = paste(round(x/1024^3, 1L), "Pb"),
	   "KiB" = paste(round(x/1024  , 1L), "KiB"),
	   "MiB" = paste(round(x/1024^2, 1L), "MiB"),
	   "GiB" = paste(round(x/1024^3, 1L), "GiB"),
	   "TiB" = paste(round(x/1024^4, 1L), "TiB"),
	   "PiB" = paste(round(x/1024^5, 1L), "PiB"),
	   "EiB" = paste(round(x/1024^6, 1L), "EiB"),
	   "ZiB" = paste(round(x/1024^7, 1L), "ZiB"),
	   "YiB" = paste(round(x/1024^8, 1L), "YiB")
	   )
}

print.object_size <-
    function(x, quote = FALSE, units = "b", ...)
{
    y <- format.object_size(x, units = units)
    if(quote) print.default(y, ...) else cat(y, "\n", sep = "")
    invisible(x)
}
