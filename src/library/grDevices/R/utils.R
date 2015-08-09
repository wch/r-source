#  File src/library/grDevices/R/utils.R
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

n2mfrow <- function(nr.plots)
{
  if      (nr.plots <=  3)  c(nr.plots,1) # 1, 2, 3
  else if (nr.plots <=  6)  c((nr.plots+1)%/%2,2)#-- n.. = 4,5,6
  else if (nr.plots <= 12)  c((nr.plots+2)%/%3,3)
  else c(nrow <- ceiling(sqrt(nr.plots)),
         ceiling( nr.plots / nrow))
}

extendrange <- function(x, r = range(x, na.rm = TRUE), f = 0.05)
{
    ## Purpose: extend a range by a factor 'f' - on each side
    if(!missing(r) && length(r) != 2)
        stop("'r' must be a \"range\", hence of length 2")
    r + c(-f,f) * diff(r)

}

trans3d <- function(x,y,z, pmat) {
    tr <- cbind(x,y,z,1) %*% pmat
    list(x = tr[,1]/tr[,4],
	 y = tr[,2]/tr[,4])
}
