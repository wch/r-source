max.col <- function(m)
{
    m <- as.matrix(m)
    n <- nrow(m)
    .C("R_max_col",
       as.double(m),
       n,
       ncol(m),
       rmax = integer(n),
       NAOK = TRUE,
       DUP  = FALSE,
       PACKAGE = "base")$rmax
}

