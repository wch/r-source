## called by var(.):
cov <- function (x, y=NULL, use="all.obs")
{
    na.method <- pmatch(use, c("all.obs", "complete.obs",
			       "pairwise.complete.obs"))
    if(is.data.frame(x)) x <- as.matrix(x)
    if(is.data.frame(y)) y <- as.matrix(y)
    if(!is.matrix(x) && is.null(y))
        stop("supply both x and y or a matrix-like x")
    .Internal(cov(x, y, na.method))
}
