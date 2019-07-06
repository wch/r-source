#  File src/library/stats/R/runmed.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2003-2019 The R Foundation
#  Copyright (C) 1995      Berwin A. Turlach
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
                   algorithm = NULL,
                   na.action = c("+Big_alternate", "-Big_alternate", "na.omit", "fail"),
                   print.level = 0)
{
    n <- length(x)
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
    na.actions <- eval(formals()$na.action, NULL, baseenv())
    iNAct <- if(missing(na.action)) 1L else pmatch(na.action, na.actions)
    ## now,	na.action <- na.actions[[ iNAct ]]
    ## is equivalent to the original
    ##		na.action <- match.arg(na.action)
    ## which is here the same as  match.arg(na.action), choices=na.actions) :
    ## na.action <- match.arg(na.action, choices=na.actions)
    ## iNAct <- match(na.action, na.actions)
    if(print.level)
        cat(sprintf(paste0(
	    "runmed(x, k=%d, endrule='%s' ( => iend=%d), algorithm='%s',\n",
	    "       na.*='%s' ( => iNAct=%d))\n"),
		    k, endrule, iend, algorithm, na.actions[[iNAct]], iNAct))
    res <- switch(algorithm,
                  Turlach  = .Call(C_runmed, as.double(x), 1, k, iend, iNAct, print.level),
                  Stuetzle = .Call(C_runmed, as.double(x), 0, k, iend, iNAct, print.level))
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

    ## med3(a,b,c) := median(a,b,c) - assuming no NA's in {a,b,c}
    med3 <- function(a,b,c)
    {
        m <- b
        if (a < b) {
            if (c < b) m <- if (a >= c) a  else  c
        } else {## a >= b
            if (c > b) m <- if (a <= c) a  else  c
        }
        m
    }
    med.3 <- function(x) { ## med.3(x) := median(x, na.rm=TRUE);  {length(x) == 3}
        if(anyNA(x))
            mean.default(x[!is.na(x)], na.rm=TRUE)
        else med3(x[[1L]], x[[2L]], x[[3L]])
    }
    med3i <- function(a,b,c) {
        if(anyNA(x <- c(a,b,c)))
            mean.default(x[!is.na(x)], na.rm=TRUE)
        else med3(a, b, c)
    }

    med.odd <- function(x, n = length(x))
    {
        ##  == median(x[1L:n]) IFF n is odd, slightly more efficient
        if(anyNA(x)) n <- length(x <- x[!is.na(x)])
        if(half <- (n + 1L) %/% 2L) # not empty
            sort(x, partial = half)[half]
        else x[1L] # NA, *not* empty
    }

    k <- as.integer(k)
    if (k < 0L || k %% 2L == 0L)
        stop("bandwidth 'k' must be >= 1 and odd!")
    k <- k %/% 2L
    if (k < 1L) return(y)
    ## else: k >= 1L: do something
    n <- length(y)
    n_1 <- n-1L; n_2 <- n-2L
    sm <- y
    if (k >= 2L) {
        sm [2L] <- med.3(y[1:3])
        sm[n_1] <- med.3(y[c(n,n_1,n_2)])

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
    sm[1L] <- med3i(y[1L], sm [2L], 3*sm [2L] - 2*sm [3L])
    sm[n]  <- med3i(y[n],  sm[n_1], 3*sm[n_1] - 2*sm[n_2])
    sm
}
