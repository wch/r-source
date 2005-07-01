max.col <- function(m, ties.method=c("random", "first", "last"))
{
    ties.method <- match.arg(ties.method)
    m <- as.matrix(m)
    n <- nrow(m)
    .C("R_max_col",
       as.double(m),
       n,
       ncol(m),
       rmax = integer(n),
       tieM = which(ties.method == eval(formals()[["ties.method"]])),
       NAOK = TRUE,
       DUP  = FALSE,
       PACKAGE = "base")$rmax
}

