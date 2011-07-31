#  File src/library/parallel/R/RngStream.R
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

## The .Random.seed for the L'Ecuyer generator is
## c(kind, a1, a2, a3, b1, b2, b3)

A1p127 <- matrix(c(2427906178, 3580155704,  949770784,
                   226153695, 1230515664, 3580155704,
                   1988835001,  986791581, 1230515664),
                 3, 3, byrow = TRUE)
A2p127 <- matrix(c(1464411153,  277697599, 1610723613,
                   32183930, 1464411153, 1022607788,
                   2824425944,   32183930, 2093834863),
                 3, 3, byrow = TRUE)
A1p76 <- matrix(c(82758667, 1871391091, 4127413238,
                  3672831523,   69195019, 1871391091,
                  3672091415, 3528743235,   69195019),
                3, 3, byrow = TRUE)
A2p76 <- matrix(c(1511326704, 3759209742, 1610795712,
                  4292754251, 1511326704, 3889917532,
                  3859662829, 4292754251, 3708466080),
                3, 3, byrow = TRUE)

.uint <- function(x) x + ifelse(x < 0 , 2^32, 0)
.int <- function(x) as.integer(ifelse(x >= 2^31, x - 2^32, x))
nextRNGStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
        stop("invalid value of 'seed'")
    a <- seed[2:4]; b <- seed[5:7]
    a <- (A1p127 %*% .uint(a)) %% 4294967087
    b <- (A2p127 %*% .uint(b)) %% 4294944443
    c(seed[1L], .int(a), .int(b))
}

nextRNGSubStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
        stop("invalid value of 'seed'")
    a <- seed[2:4]; b <- seed[5:7]
    a <- (A1p76 %*% .uint(a)) %% 4294967087
    b <- (A2p76 %*% .uint(b)) %% 4294944443
    c(seed[1L], .int(a), .int(b))
}

