options <- function(...) .Internal(options(...))

getOption <- function(x) options(x)[[1]]

