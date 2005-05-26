
qbirthday<-function(prob=0.5,classes=365,coincident=2){
  k<-coincident
  c<-classes
  p<-prob
  if (p<=0) return(1)
  if (p>=1) return(c*(k-1)+1)
  if ((k-1)*log(c)>8){
      lNapprox<-((k-1)*log(c)+lgamma(k+1)+log(-log(1-p)))/k
      N<-exp(lNapprox)
  } else{
      N<-(c^(k-1)*gamma(k+1)*log(1/(1-p)))^(1/k)
  }
  round(N)
}

pbirthday<-function(n,classes=365,coincident=2){
    k<-coincident
    c<-classes
    if (coincident<2) return(1)
    if (coincident>n) return(0)
    if (n>classes*(coincident-1)) return(1)
    eps<-1e-14
    if (qbirthday(1-eps,classes,coincident)<=n)
        return(1-eps)
    f<-function(p) qbirthday(p,c,k)-n
    lower<-0
    upper<-min(n^k/(c^(k-1)),1)
    nmin<-uniroot(f,c(lower,upper),tol=eps)
    nmin$root
}

