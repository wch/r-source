rev <- function(x) UseMethod("rev")

rev.default <- function(x) if (length(x) > 0) x[length(x):1] else x
