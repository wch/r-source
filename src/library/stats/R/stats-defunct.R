#  File src/library/stats/R/stats-defunct.R
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
## Deprecated in 1.4.0
## Defunct in 1.5.0
## reshapeWide <- function(x, i, j, val, jnames = levels(j)) .Defunct("reshape")
## reshapeLong <- function(x,jvars,  ilev = row.names(x),
##                         jlev = names(x)[jvars], iname = "reshape.i",
##                         jname = "reshape.j", vname = "reshape.v")
##     .Defunct("reshape")
## </entry>

## <entry>
## Deprecated in 1.8.0
## Defunct in 1.9.0
# removed in 3.0.0 to avoid confusion as a method
## print.coefmat <- function(x, digits=max(3, getOption("digits") - 2),
##               signif.stars = getOption("show.signif.stars"),
##               dig.tst = max(1, min(5, digits - 1)),
##               cs.ind, tst.ind, zap.ind = integer(0L),
##               P.values = NULL,
##               has.Pvalue,
##               eps.Pvalue = .Machine$double.eps,
##               na.print = "", ...) .Defunct()
## anovalist.lm <- function (object, ..., test = NULL) .Defunct()
## lm.fit.null <- function(x, y, method = "qr", tol = 1e-07, ...)
##     .Defunct("lm.fit")
## lm.wfit.null <- function(x, y, w, method = "qr", tol = 1e-07, ...)
##     .Defunct("lm.wfit")
## glm.fit.null <- function(x, y, weights , start = NULL,
##              etastart = NULL, mustart = NULL, offset,
##              family = gaussian(), control = glm.control(),
##              intercept = FALSE)
##     .Defunct("glm.fit")
## </entry>

## <entry>
## Deprecated in 2.2.1
## Defunct in 2.4.0
## mauchley.test <- function(...) .Defunct("mauchly.test")
## </entry>

## <entry>
## Deprecated in 2.10.0
## Defunct in 2.11.0
## clearNames <- function( object ) .Defunct("unname")
## </entry>

### all of the above stubs removed in 3.0.0.
