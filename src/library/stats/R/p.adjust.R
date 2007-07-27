#  File src/library/stats/R/p.adjust.R
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

p.adjust.methods <-
    c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

p.adjust <-
    function(p, method = p.adjust.methods, n = length(p))
{
    ## Methods 'Hommel', 'BH', 'BY' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>.
    method <- match.arg(method)
    if(method == "fdr") method <- "BH" # back compatibility

    p0 <- p
    if(all(nna <- !is.na(p))) nna <- TRUE
    p <- as.vector(p[nna])
 ## n <- length(p) ## -- maybe deprecate `n' argument ?
    stopifnot(n >= length(p))
    if (n <= 1) return(p0)
    if (n == 2 && method == "hommel") method <- "hochberg"

    p0[nna] <-
      switch(method,
             bonferroni = pmin(1, n * p),
             holm = {
               i <- 1:n
               o <- order(p)
               ro <- order(o)
               pmin(1, cummax( (n - i + 1) * p[o] ))[ro]
             },
             hommel = { ## needs n-1 >= 2 in for() below
               i <- 1:n
               o <- order(p)
               p <- p[o]
               ro <- order(o)
               q <- pa <- rep.int( min(n*p/(1:n)), n)
               for (j in (n-1):2) {
                 q1 <- min(j*p[(n-j+2):n]/(2:j))
                 q[1:(n-j+1)] <- pmin( j*p[1:(n-j+1)], q1)
                 q[(n-j+2):n] <- q[n-j+1]
                 pa <- pmax(pa,q)
               }
               pmax(pa,p)[ro]
             },
             hochberg = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( (n - i + 1) * p[o] ))[ro]
             },
             BH = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( n / i * p[o] ))[ro]
             },
             BY = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               q <- sum(1/(1:n))
               pmin(1, cummin(q * n / i * p[o]))[ro]
             },
             none = p)
    p0
}
