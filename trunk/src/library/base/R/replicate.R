replicate <- function(n, expr, simplify = TRUE) 
        sapply(integer(n), 
           eval.parent(substitute(function(...)expr)), simplify = simplify)
