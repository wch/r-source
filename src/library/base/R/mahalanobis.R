mahalanobis <- function(x, center, cov, inverted=FALSE)
{  

  if(is.vector(x)){
    x <- matrix(x, ncol=length(x))
  }
  else {
    x <- as.matrix(x)
  }
  
  x <- sweep(x, 2, center)

  if(!inverted){
    cov <- solve(cov)
  }
  
  retval <- apply((x%*%cov) * x, 1, sum)
  names(retval) <- rownames(x)
  retval
}
