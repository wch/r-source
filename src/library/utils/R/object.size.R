#  File src/library/utils/R/object.size.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

format.object_size <- function(x, units = "b", standard = "auto", digits = 1L, ...)
{
    known_bases <- c(legacy = 1024, IEC = 1024, SI = 1000)
    known_units <- list(
	SI     = c("B", "kB",  "MB",  "GB", "TB", "PB",  "EB",  "ZB",  "YB"),
	IEC    = c("B", "KiB", "MiB", "GiB","TiB","PiB", "EiB", "ZiB", "YiB"),
	legacy = c("b", "Kb",  "Mb",  "Gb", "Tb", "Pb"),
	LEGACY = c("B", "KB",  "MB",  "GB", "TB", "PB") # <- only for "KB"
    )

    units <- match.arg(units,
		       c("auto", unique(unlist(known_units), use.names = FALSE)))
    standard <- match.arg(standard, c("auto", names(known_bases)))

    if (standard == "auto") { ## infer 'standard' from 'units':
	standard <- "legacy" # default; may become "SI"
	if (units != "auto") {
	    if (grepl("iB$", units))
		standard <- "IEC"
	    else if (grepl("b$", units))
		standard <- "legacy"   ## keep when "SI" is the default
	    else if (units == "kB")
		## SPECIAL: Drop when "SI" becomes the default
		stop("For SI units, specify 'standard = \"SI\"'")
	}
    }
    base      <- known_bases[[standard]]
    units_map <- known_units[[standard]]

    if (units == "auto") {
	power <- if (x <= 0) 0L else min(as.integer(log(x, base = base)),
					 length(units_map) - 1L)
    } else {
	power <- match(toupper(units), toupper(units_map)) - 1L
	if (is.na(power))
	    stop(gettextf("Unit \"%s\" is not part of standard \"%s\"",
			  sQuote(units), sQuote(standard)), domain = NA)
    }
    unit <- units_map[power + 1L]
    ## SPECIAL: Use suffix 'bytes' instead of 'b' for 'legacy' (or always) ?
    if (power == 0 && standard == "legacy") unit <- "bytes"

    paste(round(x / base^power, digits=digits), unit)
}

print.object_size <-
    function(x, quote = FALSE, units = "b", standard = "auto", digits = 1L, ...)
{
    y <- format.object_size(x, units=units, standard=standard, digits=digits)
    if(quote) print.default(y, ...) else cat(y, "\n", sep = "")
    invisible(x)
}
