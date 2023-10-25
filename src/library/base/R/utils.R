#  File src/library/base/R/utils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

## A pearl from Ruby -> ggplot2 et al.
`%||%` <- function(x, y) if(is.null(x)) y else x

shQuote <- function(string, type = c("sh", "csh", "cmd", "cmd2"))
{
    if(missing(type) && .Platform$OS.type == "windows") type <- "cmd"
    type <- match.arg(type)
    if(type == "cmd") {
        # Prepare the string for parsing by Microsoft C startup code as
        # described for non-argv[0] arguments:
        #   https://docs.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments?view=msvc-160

        # Backslashes before a double quote have a special meaning, so
        # replace any series of backslashes followed by a double quote with
        # twice as many backslashes, and an escaped double quote.
        string <- gsub("(\\\\*)\"", "\\1\\1\\\\\"", string)

        # Double trailing backslashes if any, because of the final double
        # quote we are appending.
        string <- sub("(\\\\+)$", "\\1\\1", string)
        paste0("\"", string, "\"", recycle0 = TRUE)
    } else if (type == "cmd2")
        gsub('([()%!^"<>&|])', "^\\1", string)
    else if(!any(grepl("'", string))) # has no single quotes
	paste0("'", string, "'", recycle0 = TRUE)
    else if(type == "sh")
	paste0('"', gsub('(["$`\\])', "\\\\\\1", string), '"')
    else if(!any(grepl("([$`])", string)))
	paste0('"', gsub('(["!\\])' , "\\\\\\1", string), '"')
    else
	paste0("'", gsub("'", "'\"'\"'", string, fixed = TRUE), "'")
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
