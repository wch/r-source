#  File src/library/stats/R/Kalman.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2002-2014 The R Core Team
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


## There is a bare-bones version of this in StructTS.
KalmanLike <- function(y, mod, nit = 0L, update = FALSE)
{
    x <- .Call(C_KalmanLike, y, mod, nit, FALSE, update)
    z <- list(Lik = 0.5*(log(x[1L]) + x[2L]), s2 = x[1L])
    if(update) attr(z, "mod") <- attr(x, "mod")
    z
}

KalmanRun <- function(y, mod, nit = 0L, update = FALSE)
{
    z <- .Call(C_KalmanLike, y, mod, nit, TRUE, update)
    x <- z$values
    z[[1L]] <- c(Lik = 0.5*(log(x[1L]) + x[2L]), s2 = x[1L])
    z
}

## used by predict.Arima
KalmanForecast <- function(n.ahead = 10L, mod, update = FALSE)
    .Call(C_KalmanFore, as.integer(n.ahead), mod, update)


KalmanSmooth <- function(y, mod, nit = 0L)
{
    z <- .Call(C_KalmanSmooth, y, mod, as.integer(nit))
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1L, 2L, 2L)]
    z
}
