#  File src/library/base/R/converters.R
#  Part of the R package, http://www.R-project.org
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

getNumCConverters <- function()
    .Internal(getNumRtoCConverters())


getCConverterDescriptions <- function()
 .Internal(getRtoCConverterDescriptions())


getCConverterStatus <- function()
{
    v <- .Internal(getRtoCConverterStatus())
    names(v) <- getCConverterDescriptions()
    v
}


setCConverterStatus <- function(id, status)
  .Internal(setToCConverterActiveStatus(id, as.logical(status)))


removeCConverter <- function(id)
  .Internal(removeToCConverterActiveStatus(id))

