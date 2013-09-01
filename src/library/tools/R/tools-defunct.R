#  File src/library/tools/R/tools-defunct.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

## Defunct 2009-08-19
## Removed for 3.0.0
## Rd_parse <-function(file, text = NULL) .Defunct("parse_Rd")

## Deprecated for 3.0.2
## Defunct for 3.1.0 (and there is no such 'file')
readNEWS <- function(file = file.path(R.home(), "NEWS"),
                     trace = FALSE, chop = c("first", "1", "par1", "keepAll"))
    .Defunct()

checkNEWS <- function(file = file.path(R.home(), "NEWS"))
    .Defunct()
