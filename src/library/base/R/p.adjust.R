p.adjust.methods <-
    c("holm", "hochberg", "hommel", "bonferroni", "fdr", "none")

p.adjust <- function(p, method = p.adjust.methods, n = length(p)) {
    ## Methods 'Hommel' and 'FDR' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>.

    if (n == 1) return(p)
    method <- match.arg(method)
    switch(method,
           holm = {
               i <- 1:n
               o <- order(p)
               ro <- order(o)
               pmin(1, cummax( (n - i + 1) * p[o] ))[ro]
           },
           hochberg = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( (n - i + 1) * p[o] ))[ro]
           },
           hommel = {
               i <- 1:n
               s <- sort(p, index = TRUE)
               p <- s$x
               ro <- order(s$ix)
               q <- pa <- rep.int( min(n*p/(1:n)), n)
               for (j in (n-1):2) {
                   q1 <- min(j*p[(n-j+2):n]/(2:j))
                   q[1:(n-j+1)] <- pmin( j*p[1:(n-j+1)], q1)
                   q[(n-j+2):n] <- q[n-j+1]
                   pa <- pmax(pa,q)
               }
               pmax(pa,p)[ro]
           },
           fdr = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( n / i * p[o] ))[ro]
           },
           bonferroni = pmin(n * p, 1),
           none = p)
}
