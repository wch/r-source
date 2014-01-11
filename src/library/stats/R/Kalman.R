#  File src/library/stats/R/Kalman.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2002-14 The R Core Team
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


KalmanLike <- function(y, mod, nit = 0L, fast = TRUE)
{
    ## next call changes objects a, P, Pn if fast == TRUE: beware!
    x <- .Call(C_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), FALSE, fast = fast)
    s2 <- x[1L]/length(y)
    list(Lik = 0.5*(log(x[1L]/length(y)) + x[2L]/length(y)), s2 = s2)
}

KalmanRun <- function(y, mod, nit = 0L, fast = TRUE)
{
    ## next call changes objects a, P, Pn if fast == TRUE: beware!
    z <- setNames(.Call(C_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
			mod$Pn, as.integer(nit), TRUE, fast=fast),
		  c("values", "resid", "states"))
    x <- z$values
    s2 <- x[1L]/length(y)
    z[[1L]] <- c(Lik = 0.5*(log(x[1L]/length(y)) + x[2L]/length(y)), s2 = s2)
    z
}

KalmanForecast <- function(n.ahead = 10L, mod, fast = TRUE)
{
    a <- numeric(p <- length(mod$a))
    P <- matrix(0, p, p)
    a[] <- mod$a
    P[] <- mod$P
    ## next call changes objects a, P if fast==TRUE
    setNames(.Call(C_KalmanFore, as.integer(n.ahead), mod$Z, a, P,
		   mod$T, mod$V, mod$h, fast=fast),
	     c("pred", "var"))
}

KalmanSmooth <- function(y, mod, nit = 0L)
{
    z <- setNames(.Call(C_KalmanSmooth, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
			mod$Pn, as.integer(nit)),
		  c("smooth", "var"))
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1L, 2L, 2L)]
    z
}
