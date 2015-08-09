#  File src/library/utils/R/utils-defunct.R
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
## Deprecated in 1.9.0
## Defunct in 2.0.0
## Removed in 3.0.0
## package.contents <- function(pkg, lib.loc=NULL) .Defunct(package="utils")
## </entry>

## <entry>
## Deprecated in 2.12.2
## Defunct in 2.14.0
## Removed in 3.0.0
## zip.file.extract <- function(file, zipname = "R.zip",
## 			     unzip = getOption("unzip"), dir = tempdir())
## .Defunct("unzip")
## </entry>

## <entry>
## Deprecated in 2.2.0
## Defunct in 3.0.0
CRAN.packages <- function(CRAN = getOption("repos"), method,
                          contriburl = contrib.url(CRAN))
    .Defunct("available.packages")
## </entry>
