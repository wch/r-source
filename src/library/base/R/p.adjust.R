p.adjust.methods<- c("holm", "hochberg", "bonferroni","none")

p.adjust <- function(p, method = p.adjust.methods, n = length(p)) {
    method <- match.arg(method)
    if ( n == 1 ) return(p)
    switch (method,
            hochberg = {
                r <- rank(p)
                index <- order(p)
                qi <- p*(n+1-r)
                for (i in (length(p)-1):1)
                    qi[index[i]] <- min(qi[index[i]], qi[index[i+1]])
                qi
            },
            holm = {
                r <- rank(p)
                index <- order(p)
                qi <- p*(n+1-r)
                for (i in 2:length(p))
                    qi[index[i]] <- max(qi[index[i]], qi[index[i-1]])
                pmin(qi, 1)
            },
            bonferroni = pmin(n * p, 1),
            none = p)
}
