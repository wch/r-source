ave <- function (x, ..., FUN = mean)
{
    n <- length(l <- list(...))
    if (n) {
	g <- interaction(...)
	split(x,g) <- lapply(split(x, g), FUN)
    } else 
        x[] <- FUN(x)
    x
}
