# Originally file MASS/max.col.q
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#
max.col <- function(m)
{
    m <- as.matrix(m)
    n <- nrow(m)
    .C("R_max_col",
       as.double(m),
       as.integer(n),
       as.integer(ncol(m)),
       rmax = integer(n),
       NAOK=TRUE
       )$rmax
}

