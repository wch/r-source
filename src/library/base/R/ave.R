ave <- function (x, ..., FUN = mean)
{
    n <- length(l <- list(...))
    x[] <- if (n) {
        g <- 1
        nlv <- 1
        for (i in 1:n) {
            l[[i]] <- li <- as.factor(l[[i]])
            g <- g + nlv * (as.numeric(li) - 1)
            nlv <- nlv * length(levels(li))
        }
        unlist(lapply(split(x, g), FUN))[g]
    } else FUN(x)
    x
}
