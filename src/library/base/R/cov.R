## called by var(.):
cov <- function (x, y=NULL, use="all.obs")
{
    na.method <- pmatch(use, c("all.obs", "complete.obs",
			       "pairwise.complete.obs"))
    if(is.data.frame(x)) x <- as.matrix(x)
    if(is.data.frame(y)) y <- as.matrix(y)
    .Internal(cov(x, y, na.method))
}
