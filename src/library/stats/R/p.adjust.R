p.adjust.methods <-
    c("holm", "hochberg", "hommel", "bonferroni", "fdr", "none")

p.adjust <-
    function(p, method = p.adjust.methods, n = length(p), na.rm = FALSE)
{
    ## Methods 'Hommel' and 'FDR' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>  (w/o NA treatment yet)

    method <- match.arg(method)
    if(na.rm && any(ina <- is.na(p))) {
        pp <- p
        p <- pp[!ina] # for n's default
        pp[!ina] <- p.adjust(p, method = method, n = n, na.rm = FALSE)
        return(pp)
    }
    if (n <= 1) return(p)
    switch(method,
           bonferroni = pmin(1, n * p),
           holm = {
               i <- 1:n
               o <- order(p)
               ro <- order(o)
               pmin(1, cummax( (n - i + 1) * p[o] ))[ro]
           },
           hochberg = ,
           fdr = {
               i <- n:1
               o <- order(p, decreasing = TRUE, na.last = FALSE)# NA's first
               ro <- order(o)
               p <- (if(method == "fdr") n / i else n - i + 1) * p[o]
               p <- if(has.na <- is.na(p[1])) {
                   nna <- sum(is.na(p))
                   c(rep.int(NA, nna), cummin(p[(nna+1):n]))
               } else cummin(p)
               pmin(1, p)[ro]
           },
           hommel = { ## needs n-1 >= 2, i.e. n > 2 in for() below
               if (n <= 2) return(p)
               i <- 1:n
               o <- order(p)
               p <- p[o] # NA's last
               ro <- order(o)
               q <- pa <- rep.int( min(n*p/(1:n), na.rm=TRUE), n)
               ok.p <- !is.na(p)
               for (j in (n-1):2) {
                   qq <- j*p[1:(n-j+1)]
                   if(ok.p[n-j+2]) {
                       q1 <- min(j*p[(n-j+2):n]/(2:j), na.rm=TRUE)
                       qq <- pmin(qq, q1)
                   }
                   q[1:(n-j+1)] <- qq
                   q[(n-j+2):n] <- q[n-j+1]
                   pa <- pmax(pa,q)
               }
               pmin(1, pmax(pa,p)[ro])
           },
           none = p)
}
