lowess <- function(x, y=NULL, f=2/3, iter=3, delta=.01*diff(range(xy$x[o]))) {
    xy <- xy.coords(x,y)
    if(length(xy$x) != length(xy$y)) stop("x and y lengths differ")
    n <- length(xy$x)
    o <- order(xy$x)
    .C("lowess",
       x=as.double(xy$x[o]),
       as.double(xy$y[o]),
       n,
       as.double(f),
       as.integer(iter),
       as.double(delta),
       y=double(n),
       double(n),
       double(n))[c("x","y")]
}
