mahalanobis <- function(x, center, cov, inverted=FALSE)
{
  x <- if(is.vector(x)) matrix(x, ncol=length(x)) else as.matrix(x)
  x <- sweep(x, 2, center)# = (x - center)
  if(!inverted)
    cov <- solve(cov)
  retval <- apply((x%*%cov) * x, 1, sum)
  names(retval) <- rownames(x)
  retval
}
