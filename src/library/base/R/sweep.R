sweep <- function(x, MARGIN, STATS, FUN = "-", ...)
{
    FUN <- match.fun(FUN)
    dims <- dim(x)
    MARGIN <- iMargin(MARGIN, dimnames(x))
    perm <- c(MARGIN, (1:length(dims))[ - MARGIN])
    FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...)
}
