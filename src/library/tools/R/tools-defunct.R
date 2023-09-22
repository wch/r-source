#  File src/library/tools/R/tools-defunct.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

## Defunct 2009-08-19
## Removed for 3.0.0
## Rd_parse <-function(file, text = NULL) .Defunct("parse_Rd")

## Deprecated for 3.0.2
## Defunct for 3.1.0 (and there is no such 'file')
## Removed for 4.1.0
## readNEWS <- function(file = file.path(R.home(), "NEWS"),
##                      trace = FALSE, chop = c("first", "1", "par1", "keepAll"))
##     .Defunct()

## checkNEWS <- function(file = file.path(R.home(), "NEWS"))
##     .Defunct()


## <entry>
## Deprecated in 3.3.0 (r70156 (Sat, 13 Feb 2016))
## Defunct    in 4.1.0
package.dependencies <-
    function(x, check = FALSE, depLevel = c("Depends", "Imports", "Suggests"))
{
    .Defunct("package_dependencies")
}

pkgDepends <- function(pkg, recursive=TRUE, local=TRUE,
                       reduce=TRUE, lib.loc=NULL) {
    .Defunct("package_dependencies()")# or also dependsOnPkgs() ?
}

getDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                       local=TRUE, reduce=TRUE, lib.loc=NULL)
{
    .Defunct("dependsOnPkgs() or package_dependencies()")
}

installFoundDepends <- function(depPkgList, ...) {
    .Defunct()
}
## <entry/>

## <entry>
## Deprecated in 3.6.0 (r75... (12 Sep 2018)) -- should have belonged to prev.
vignetteDepends <-
    function(vignette, recursive = TRUE, reduce = TRUE,
             local = TRUE, lib.loc = NULL)
{
    .Defunct("vignetteInfo()$depends or package_dependencies()")
}
## </entry>
