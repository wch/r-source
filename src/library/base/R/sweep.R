sweep <- function(x, MARGIN, STATS, FUN = "-", ...)
{
    FUN <- match.fun(FUN)
    dims <- dim(x)
    perm <- c(MARGIN, (1:length(dims))[ - MARGIN])
    FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...)
}
