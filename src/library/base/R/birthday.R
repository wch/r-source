
qbirthday<-function(prob=0.5,classes=365,coincident=2){
  k<-coincident
  c<-classes
  p<-prob
  Napprox<-(c^(k-1)*gamma(k+1)*log(1/(1-p)))^(1/k)
  N<-Napprox*((1-Napprox/(c*(k+1)))^(1/k))*exp(Napprox/(c*k))
  round(N)
}

pbirthday<-function(n,classes=365,coincident=2){
    k<-coincident
    c<-classes
    if (coincident<2) return(1)
    f<-function(p){
        Napprox<-(c^(k-1)*gamma(k+1)*log(1/(1-p)))^(1/k)
        Napprox-n/(((1-n/(c*(k+1)))^(1/k))*exp(n/(c*k)))
    }
    eps<-1e-12
    lower<-min(n/(c^(k-1)),1-100*eps)
    upper<-min(n^k/(c^(k-1)),1-eps)
    nmin<-uniroot(f,c(lower,upper))
    nmin$root
}

