ts.union <- function(..., dframe = FALSE)
    .cbind.ts(..., dframe = dframe, union = TRUE)
ts.intersect <- function(..., dframe = FALSE)
    .cbind.ts(..., dframe = dframe, union = FALSE)
