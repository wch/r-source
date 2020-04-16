#  File src/library/tools/R/tools-deprecated.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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


## keep one entry (possibly commented) here :

## <entry>
## Deprecated in 3.6.0 (r75... (12 Sep 2018)) -- should have belonged to prev.
## vignetteDepends <-
##     function(vignette, recursive = TRUE, reduce = TRUE,
##              local = TRUE, lib.loc = NULL)
## {
##     .Deprecated("vignetteInfo()$depends or package_dependencies()")

##     if (length(vignette) != 1L)
##         stop("argument 'vignette' must be of length 1")
##     if (!nzchar(vignette)) return(invisible()) # lets examples work.
##     if (!file.exists(vignette))
##         stop(gettextf("file '%s' not found", vignette), domain = NA)

##     vigDeps <- vignetteInfo(vignette)$depends

##     depMtrx <- getVigDepMtrx(vigDeps)
##     instPkgs <- utils::installed.packages(lib.loc=lib.loc)
##     getDepList(depMtrx, instPkgs, recursive, local, reduce)
## }
## </entry>
