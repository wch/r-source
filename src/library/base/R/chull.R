chull <- function(x, y=NULL)
{
    X <- xy.coords(x, y, recycle = TRUE)
    x <- cbind(X$x, X$y)
    n <- nrow(x)
    z <- .C("chull",
            n=as.integer(n),
            as.double(x),
            as.integer(n),
            as.integer(1:n),
            integer(n),
            integer(n),
            ih=integer(n),
            nh=integer(1),
            il=integer(n))
    rev(z$ih[1:z$nh])
}
