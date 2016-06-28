#  File src/library/utils/R/hasName.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2016 The R Core Team
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

# "hasName(x, name)" is defined to be the same as "name %in% names(x)"

hasName <- function(x, name) match(names(x), name, nomatch = 0L) > 0L
