#  File src/library/stats/R/fft.R
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

fft <- function(z, inverse=FALSE) .Call(C_fft, z, inverse)

mvfft <- function(z, inverse=FALSE) .Call(C_mvfft, z, inverse)

nextn <- function(n, factors=c(2,3,5)) .Call(C_nextn, n, factors)

convolve <- function(x, y, conj=TRUE,
                     type=c("circular", "open", "filter", "1+open", "1+filter"))
{
    type <- match.arg(type)
    n <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.numeric(y)
    ## switch(type, circular = ..., )
    if(type == "circular") {
        if(ny != n)
            stop("length mismatch in convolution")
    }
    else { ## "open" or "filter": Pad with zeros to common length
        if((do.filt <- grepl("filter", type)) && ny > n)
	    return(if(Real) numeric(0) else complex(0))
	oneP <- grepl("^1\\+", type)
	n1 <- if(oneP) ny else ny - 1L
	x <- c(rep.int(0, n1), x)
	n <- length(y <- c(y, rep.int(0, if(oneP) n else n - 1L)))# n = nx+ny {-1}
    }
    x <- if(Real) Re(fft(fft(x) * (if(conj) Conj(fft(y)) else fft(y)), inverse=TRUE))
         else        fft(fft(x) * (if(conj) Conj(fft(y)) else fft(y)), inverse=TRUE)
    switch(type,
           "filter"   = x[(n1+1L):(n-n1)],
           "1+filter" = x[(n1+1L):(n-n1+1L)],
           "circular" = x,
           "open"     = x,
           "1+open"   = x[-1L]) / n
}

