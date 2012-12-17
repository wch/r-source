#  File src/library/methods/R/methods-deprecated.R
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


## <entry>
## Deprecated in 2.10.0
## Defunct in 2.11.0
## Removed in 3.0.0
## trySilent <- function(expr) .Defunct("try(silent = TRUE)")
## </entry>

## <entry>
## Defunct in 3.0.0
traceOn <- function(what, tracer = browseAll, exit = NULL) {
    browseAll <- function() .Defunct()
    .Defunct("trace")
}
traceOff <- function(whatL) .Defunct("untrace")
## </entry>
