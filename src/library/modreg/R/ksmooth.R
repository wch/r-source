# file modreg/R/ksmooth.R
# copyright (C) 1998 B. D. Ripley
#
ksmooth <-
  function(x, y, kernel=c("box", "normal"), bandwidth=0.5, range.x=range(x),
           n.points=max(100, length(x)), x.points)
{
# box is [-0.5, 0.5]. normal is sd = 1.4826/4
  if(missing(y))
    stop("y must be supplied.\nFor density estimation use density()")
  kernel <- match.arg(kernel)
  krn <- switch(kernel, "box" = 1, "normal" = 2)
  if(missing(x.points))
    x.points <- seq(range.x[1], range.x[2], len=n.points)
  else {
    n.points <- length(x.points)
    x.points <- sort(x.points)
  }
  ord <- order(x)
  z <- .C("BDRksmooth",
          as.double(x[ord]),
          as.double(y[ord]),
          as.integer(length(x)),
          xp=as.double(x.points),
          yp=double(n.points),
          as.integer(n.points),
          as.integer(krn),
          as.double(bandwidth)
          )
  list(x=z$xp, y=z$yp)
} 

