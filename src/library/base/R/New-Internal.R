##vector <- function(mode = "logical", length = 0).Internal(vector(mode,length))
comment <- function(x).Internal(comment(x))
"comment<-" <- function(x,cmt).Internal("comment<-"(x,cmt))

round <- function(x, digits = 0).Internal(round(x,digits))
signif <- function(x, digits = 6).Internal(signif(x,digits))
log <- function(x, base=exp(1))
	if(missing(base)) .Internal(log(x)) else .Internal(log(x,base))

atan2 <- function(y, x).Internal(atan2(y, x))

 beta <- function(a, b).Internal( beta(a, b))
lbeta <- function(a, b).Internal(lbeta(a, b))

 gamma <- function(x).Internal( gamma(x))
lgamma <- function(x).Internal(lgamma(x))
   digamma <- function(x).Internal(   digamma(x))
  trigamma <- function(x).Internal(  trigamma(x))
tetragamma <- function(x).Internal(tetragamma(x))
pentagamma <- function(x).Internal(pentagamma(x))

choose <- function(n,k).Internal(choose(n,k))
lchoose <- function(n,k).Internal(lchoose(n,k))
