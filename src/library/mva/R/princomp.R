princomp <- function(x, cor=FALSE, scores=TRUE,
                     subset=rep(TRUE, nrow(as.matrix(x)))) {
  z<- as.matrix(x)[subset,, drop=F]
  N <- nrow(z)
  if(cor)
    cv <- get("cor",envir=.GlobalEnv)(z)
  else
    cv <- cov(z)
  ##  (svd can be used but gives different signs for some vectors)
  edc <- eigen(cv)
  cn <- paste("Comp.", 1:ncol(cv), sep="")
  names(edc$values) <- cn
  dimnames(edc$vectors) <- list(dimnames(x)[[2]], cn)
  scr<- NULL
  if (cor) {
    sdev <- sqrt(edc$values)
    sc <- (apply(z,2,var)*(N-1)/N)^0.5
    if (scores)
      scr<-(scale(z,center=T,scale=T) %*% edc$vectors)*sqrt(N/(N-1))
  } else {
    sdev <- sqrt(edc$values*(N-1)/N)
    sc <- rep(1, ncol(z))
    if (scores)
      scr<- (scale(z,center=T,scale=F) %*% edc$vectors)
  }
  names(sc) <- dimnames(x)[[2]]
  edc <-list(sdev=sdev, loadings=edc$vectors,
             center=apply(z,2,mean), scale=sc, n.obs=N, scores=scr)
  ## The splus function also return list elements factor.sdev,
  ## correlations and coef, but these are not documented in the
  ## help. coef seems to equal load.  The splus function also return
  ## list elements call and terms which are not supported here.
  class(edc) <- "princomp"
  edc
}
