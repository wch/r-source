#  File src/library/base/R/utils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

shQuote <- function(string, type = c("sh", "csh", "cmd"))
{
    cshquote <- function(x) {
        xx <- strsplit(x, "'", fixed = TRUE)[[1L]]
        paste(paste0("'", xx, "'"), collapse="\"'\"")
    }
    if(missing(type) && .Platform$OS.type == "windows") type <- "cmd"
    type <- match.arg(type)
    if(type == "cmd") {
        paste0('"', gsub('"', '\\\\"', string), '"')
    } else {
	if(!length(string))
	    ""
	else if(!any(grepl("'", string))) # has single quote
	    paste0("'", string, "'")
	else if(type == "sh")
	    paste0('"', gsub('(["$`\\])', "\\\\\\1", string), '"')
	else if(!any(grepl("([$`])", string)))
	    paste0('"', gsub('(["!\\])' , "\\\\\\1", string), '"')
	else
	    vapply(string, cshquote, "")
    }
}

.standard_regexps <-
function()
{
    list(valid_package_name = "[[:alpha:]][[:alnum:].]*[[:alnum:]]",
         valid_package_version = "([[:digit:]]+[.-]){1,}[[:digit:]]+",
         valid_R_system_version =
         "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+",
         valid_numeric_version = "([[:digit:]]+[.-])*[[:digit:]]+")
}
