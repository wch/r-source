#  File src/library/stats/R/runmed.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2003-2017 The R Foundation

#  Copyright (C) 1995 Berwin A. Turlach
#  Ported to R, added interface to Stuetzle's code and further enhanced
#  by Martin Maechler,
#  Copyright (C) 1996-2002 Martin Maechler
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


runmed <- function(x, k, endrule = c("median","keep","constant"),
                   algorithm = NULL, print.level = 0)
{
    n <- as.integer(length(x))
    if(is.na(n)) stop(gettextf("invalid value of %s", "length(x)"), domain = NA)
    k <- as.integer(k)
    if(is.na(k)) stop(gettextf("invalid value of %s", "'k'"), domain = NA)
    if(k < 0L) stop("'k' must be positive")
    if(k %% 2L == 0L)
        warning(gettextf("'k' must be odd!  Changing 'k' to %d",
                         k <- as.integer(1+ 2*(k %/% 2))), domain = NA)
    if(n == 0L) {
	x <- double(); attr(x, "k") <- k
	return(x)
    }
    if (k > n)
        warning(gettextf("'k' is bigger than 'n'!  Changing 'k' to %d",
                         k <- as.integer(1+ 2*((n - 1)%/% 2))), domain = NA)
    algorithm <-
        if(missing(algorithm)) { ## use efficient default
            ## This is too primitive, MM knows better :
            if(k < 20L || n < 300L) "Stuetzle" else "Turlach"
        }
        else match.arg(algorithm, c("Stuetzle", "Turlach"))
    endrule <- match.arg(endrule)# including error.check
    iend <- switch(endrule,
                   ## "median" will be treated at the end
                   "median" =, "keep" = 0L,
                   "constant" = 1L)
    if(print.level)
        cat("runmed(*, endrule=", endrule,", algorithm=",algorithm,
            ", iend=",iend,")\n")
    res <- switch(algorithm,
                  Turlach = .Call(C_runmed, as.double(x), 1, k, iend, print.level),
                  Stuetzle = .Call(C_runmed, as.double(x), 0, k, iend, print.level))
    if(endrule == "median") res <- smoothEnds(res, k = k)

    ## Setting attribute has the advantage that the result immediately plots
    attr(res,"k") <- k
    res
}

### All the following is from MM:

smoothEnds <- function(y, k = 3)
{
    ## Purpose: Smooth end values---typically after runmed()
    ##-- (C) COPYRIGHT 1994,  Martin Maechler <maechler@stat.math.ethz.ch>

    med3 <- function(a,b,c)
    {
        ## med3(a,b,c) == median(a,b,c)
        m <- b
        if (a < b) {
            if (c < b) m <- if (a >= c) a  else  c
        } else {## a >= b
            if (c > b) m <- if (a <= c) a  else  c
        }
        m
    }

    med.odd <- function(x, n = length(x))
    {
        ##  == median(x[1L:n]) IFF n is odd, slightly more efficient
        half <- (n + 1) %/% 2
        sort(x, partial = half)[half]
    }

    k <- as.integer(k)
    if (k < 0L || k %% 2L == 0L)
        stop("bandwidth 'k' must be >= 1 and odd!")
    k <- k %/% 2L
    if (k < 1L) return(y)
    ## else: k >= 1L: do something
    n <- length(y)
    sm <- y
    if (k >= 2L) {
        sm [2L]  <- med3(y[1L],y [2L], y [3L])
        sm[n-1L] <- med3(y[n],y[n-1L],y[n-2L])

        ## Here, could use Stuetzle's strategy for MUCH BIGGER EFFICIENCY
        ##	(when k>=3 , k >> 1):
        ## Starting with the uttermost 3 points,
        ## always 'adding'  2 new ones, and determine the new median recursively
        ##
        if (k >= 3L) {
            for (i in 3:k) {
                j <- 2L*i - 1L
                sm  [i]    <- med.odd( y[1L:j]      , j) #- left border
                sm[n-i+1L] <- med.odd( y[(n+1L-j):n], j) #- right border
            }
        }
    }

    ##--- For the very first and last pt.:  Use Tukey's end-point rule: ---
    ## Ysm[1L]:= Median(Ysm[2L],X1,Z_0), where Z_0 is extrapol. from Ysm[2L],Ysm[3L]
    sm[1L] <- med3(y[1L], sm [2L] , 3*sm [2L]  - 2*sm [3L])
    sm[n]  <- med3(y[n],  sm[n-1L], 3*sm[n-1L] - 2*sm[n-2L])
    sm
}
