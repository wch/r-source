replicate <- function(n, expr, simplify = TRUE) 
        sapply(integer(n), 
           eval.parent(substitute(function(x)expr)), simplify = simplify)
