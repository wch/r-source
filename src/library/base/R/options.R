options <- function(...) .Internal(options(...))

getOption <- function(x) options(x)[[1]]

## initial options settings (others are done in C code in InitOptions)
options(defaultPackages = c("methods", "ctest"))

