#  File src/library/base/R/files.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2007 R Core Team
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

.TAOCP1997init <- function(seed)
{
    KK <- 100L; LL <- 37L; MM <- as.integer(2^30)
    KKK <- KK + KK - 1L; KKL <- KK - LL
    ss <- seed - (seed %% 2L) + 2L
    X <- integer(KKK)
    for(j in 1L:KK) {
        X[j] <- ss
        ss <- ss+ss
        if(ss >= MM) ss <- ss - MM + 2L
    }
    X[2L] <- X[2L] + 1L
    ss <- seed
    T <- 69L
    while(T > 0) {
        for(j in KK:2L) X[j + j - 1L] <- X[j]
        for(j in seq(KKK, KKL + 1L, -2L))
            X[KKK - j + 2L] <- X[j] - (X[j] %% 2L)
        for(j in KKK:(KK+1L))
            if(X[j] %% 2L == 1L) {
                X[j - KKL] <- (X[j - KKL] - X[j]) %% MM
                X[j - KK] <- (X[j - KK] - X[j]) %% MM
            }
        if(ss %% 2L == 1L) {
            for(j in KK:1L) X[j + 1L] <- X[j]
            X[1L] <- X[KK + 1L]
            if(X[KK + 1L] %% 2L == 1L)
                X[LL + 1L] <- (X[LL + 1L] - X[KK + 1L]) %% MM
        }
        if(ss) ss <- ss %/% 2L else T <- T - 1L
    }
    rs <- c(X[(LL+1L):KK], X[1L:LL])
    invisible(rs)
}
