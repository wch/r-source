mahalanobis <- function(x, center, cov, inverted=FALSE)
{  

  if(is.vector(x)){
    x <- matrix(x, ncol=length(x))
  }
  else {
    x <- as.matrix(x)
  }

  if(missing(center)){
    center <- rep(0, length=ncol(x))
  }
  
  if(missing(cov)){
    cov <- diag(ncol(x))
  }
  else if((!inverted) && (!is.qr(cov))){
    cov <- solve(cov)
  }

  x <- sweep(x, 1, center)
  retval <- apply((x%*%cov) * x, 1, sum)
  names(retval) <- rownames(x)
  retval
}
