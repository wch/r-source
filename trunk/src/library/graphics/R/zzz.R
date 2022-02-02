#  File src/library/graphics/R/zzz.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

## <entry>
## Deprecated in 1.5.0
## Defunct in 1.6.0
piechart <- function(x, labels = names(x), edges = 200, radius = 0.8,
                     density = NULL, angle = 45, col = NULL, main = NULL, ...)
    .Defunct("pie")
## </entry>


.noGenerics <- TRUE

# not yet
# .onUnload <- function(libpath) .Call(RunregisterBase);

