###  Copyright (C) 1995 Berwin A. Turlach <berwin@alphasun.anu.edu.au>
###
###  Ported to R, added interface to Stuetzle's code and further enhanced
###  by Martin Maechler,
###  Copyright (C) 1996--2002 Martin Maechler <maechler@stat.math.ethz.ch>
###  Copyright (C) 2003       The R Foundation

###  This program is free software; you can redistribute it and/or modify
###  it under the terms of the GNU General Public License as published by
###  the Free Software Foundation; either version 2 of the License, or
###  (at your option) any later version.
###
###  This program is distributed in the hope that it will be useful,
###  but WITHOUT ANY WARRANTY; without even the implied warranty of
###  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###  GNU General Public License for more details.
###
### A copy of the GNU General Public License is available via WWW at
### http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
### writing to the Free Software Foundation, Inc., 59 Temple Place,
### Suite 330, Boston, MA  02111-1307  USA.

runmed <- function(x, k, endrule = c("median","keep","constant"),
                   algorithm = NULL, print.level = 0)
{
    n <- length(x)
    k <- as.integer(k)
    if(k%%2 == 0)
        warning("'k' must be odd!  Changing 'k' to ",
                k <- as.integer(1+ 2*(k %/% 2)))
    if (k > n)
        warning("'k' is bigger than 'n'!  Changing 'k' to ",
                k <- as.integer(1+ 2*((n - 1)%/% 2)))
    algorithm <-
        if(missing(algorithm)) { ## use efficient default
            ## This is too primitive, MM knows better :
            if(k < 20 || n < 300) "Stuetzle" else "Turlach"
        }
        else {
            match.arg(algorithm, c("Stuetzle", "Turlach"))
        }
    endrule <- match.arg(endrule)# including error.check
    iend <- switch(endrule,
                   ## "median" will be treated at the end
                   "median" =, "keep" = 0,
                   "constant" = 1)
    if(print.level)
        cat("runmed(*, endrule=", endrule,", algorithm=",algorithm,
            ", iend=",iend,")\n")
    res <- switch(algorithm,
                  Turlach = {
                      .C("Trunmed",
                         n,
                         k,
                         as.double(x),
                         rmed = double(n),	# median[] -> result
                         tmp1 = integer(k+1),	# outlist[]
                         tmp2 = integer(2*k +1),# nrlist []
                         tmp3 = double (2*k +1),# window []
                         as.integer(iend),
                         as.integer(print.level),
                         PACKAGE = "stats",
                         DUP = FALSE)$ rmed
                  },
                  Stuetzle = {
                      .C("Srunmed",
                         as.double(x),
                         smo = double(n),
                         n,
                         k,
                         as.integer(iend),
                         debug = (print.level > 0),
                         PACKAGE = "stats",
                         DUP = FALSE)$ smo
                  })
    if(endrule == "median")
        res <- smoothEnds(res, k = k)

    ## list(rmed=res$rmed, k=k)
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
        ##  == median(x[1:n]) IFF n is odd, slightly more efficient
        half <- (n + 1) %/% 2
        sort(x, partial = half)[half]
    }

    k <- as.integer(k)
    if (k < 0 || k%%2 == 0)
        stop("bandwidth 'k' must be >= 1 and odd!")
    k <- k %/% 2
    if (k < 1) return(y)
    ## else: k >= 1: do something
    n <- length(y)
    sm <- y
    if (k >= 2) {
        sm [2]  <- med3(y[1],y [2], y [3])
        sm[n-1] <- med3(y[n],y[n-1],y[n-2])

        ## Here, could use Stuetzle's strategy for MUCH BIGGER EFFICIENCY
        ##	(when k>=3 , k >> 1):
        ## Starting with the uttermost 3 points,
        ## always 'adding'  2 new ones, and determine the new median recursively
        ##
        if (k >= 3) {
            for (i in 3:k) {
                j <- 2*i - 1
                sm  [i]   <- med.odd( y [1:j] ,     j) #- left border
                sm[n-i+1] <- med.odd( y[(n+1-j):n], j) #- right border
            }
        }
    }

    ##--- For the very first and last pt.:  Use Tukey's end-point rule: ---
    ## Ysm[1]:= Median(Ysm[2],X1,Z_0), where Z_0 is extrapol. from Ysm[2],Ysm[3]
    sm[1] <- med3(y[1], sm [2] , 3*sm [2]  - 2*sm [3])
    sm[n] <- med3(y[n], sm[n-1], 3*sm[n-1] - 2*sm[n-2])
    return(sm)
}
