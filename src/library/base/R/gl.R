## gl function of GLIM:
gl <- function (n, k, length = n*k, labels=1:n, ordered=FALSE)
    factor(rep(rep(1:n,rep(k,n)), length=length),
	   labels=labels, ordered=ordered)
