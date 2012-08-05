#  File src/library/stats/R/birthday.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/


qbirthday <- function(prob = 0.5, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    p <- prob
    if (p <= 0) return(1)
    if (p >= 1) return(c*(k-1)+1)
    ## We need smallest n with pbirthday(n, c, k) >= prob
    ## This is a crude inversion of Diaconis & Mosteller expression (7.5),
    ## usually an underestimate.
    N <- exp(((k-1)*log(c) + lgamma(k+1) + log(-log1p(-p)))/k)
    N <- ceiling(N)
    if(pbirthday(N, c, k) < prob) {
        N <- N+1
        while(pbirthday(N, c, k) < prob) N <- N+1
    } else if (pbirthday(N-1, c, k) >= prob) {
        N <- N-1
        while(pbirthday(N-1, c, k) >= prob) N <- N-1
    }
    N
}

pbirthday <- function(n, classes = 365, coincident = 2)
{
    k <- coincident
    c <- classes
    if (k < 2) return(1)
    if (k == 2) return( 1 - prod((c:(c-n+1))/rep(c, n)) )
    if (k > n) return(0)
    if (n > c*(k-1)) return(1)
    ## use Diaconis & Mosteller expression (7.5) on log scale
    LHS <- n * exp(-n/(c*k))/(1 - n/(c*(k+1)))^(1/k)
    lxx <- k*log(LHS) - (k-1)*log(c) - lgamma(k+1)
    -expm1(-exp(lxx))
}

