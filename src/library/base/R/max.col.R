# Originally file MASS/max.col.q
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#
# nrow() & ncol() are guaranteed to return integer
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

