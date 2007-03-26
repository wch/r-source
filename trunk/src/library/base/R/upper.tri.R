upper.tri <- function(x, diag = FALSE)
{
    x <- as.matrix(x)
    if(diag) row(x) <= col(x)
    else row(x) < col(x)
}
