#  File src/library/grDevices/R/utils.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

n2mfrow <- function(nr.plots, asp = 1)
{
  nr.plots <- as.integer(nr.plots)
  if(asp == 1 && nr.plots <= 12L) { # if only for back compatibility
      if   (nr.plots <=  3L)
          c(nr.plots, 1L)        # nr.p = 1, 2, 3
      else if (nr.plots <=  6L)
           c((nr.plots+1L) %/% 2L, 2L)# = 4, 5, 6
      else c((nr.plots+2L) %/% 3L, 3L)# = 7, 8,.., 12
  }
  else {
      rc <- c(nrow <- ceiling(sqrt(nr.plots / asp)),
              ceiling(nr.plots / nrow))
      ## improve (if easy):
      do <- TRUE
      while(do && prod(rc) > nr.plots) {
          if(do <- prod(n <- rc - 0:1) >= nr.plots)
              rc <- n
          else if(do <- prod(n <- rc - 1:0) >= nr.plots)
              rc <- n
      }
      rc
  }
}

extendrange <- function(x, r = range(x, na.rm = TRUE), f = 0.05)
{
    ## Purpose: extend a range by a factor 'f' - on each side
    if(!missing(r) && length(r) != 2)
        stop("'r' must be a \"range\", hence of length 2")
    f <- if(length(f) == 1L) c(-f,f) else c(-f[1L], f[2L])
    r + f * diff(r)
}

trans3d <- function(x,y,z, pmat) {
    tr <- cbind(x,y,z,1, deparse.level=0L) %*% pmat
    list(x = tr[,1]/tr[,4],
	 y = tr[,2]/tr[,4])
}
