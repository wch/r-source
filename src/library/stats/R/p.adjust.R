p.adjust.methods <-
    c("holm", "hochberg", "hommel", "bonferroni", "yh", "bh", "fdr", "none")

p.adjust <-
    function(p, method = p.adjust.methods, n = length(p))
{
    ## Methods 'Hommel', 'BH', 'YH' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>.

    method <- match.arg(method)
    if(method == "fdr") method <- "bh"
    p0 <- p
    if(all(nna <- !is.na(p))) nna <- TRUE
    p <- as.vector(p[nna])
 ## n <- length(p)
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
             hommel = { ## needs n-1 >= 2, i.e. n > 2 in for() below
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
             bh = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( n / i * p[o] ))[ro]
             },
             yh = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               q <- sum(1/(1:n))
               pmin(1, cummin(q * n / i * p[o]))[ro]
             },
             none = p)
    p0
}
