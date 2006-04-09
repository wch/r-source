all.names <- function(expr, functions = TRUE, max.names = 200, unique = FALSE)
    .Internal(all.names(expr, functions, max.names, unique))

all.vars <- function(expr, functions = FALSE, max.names = 200, unique = TRUE)
    .Internal(all.names(expr, functions, max.names, unique))
