## gl function of GLIM
gl <- function (n, k, length = n*k, labels=1:n, ordered=FALSE)
    factor(rep(rep.int(1:n, rep.int(k,n)), length.out=length),
	   levels=1:n, labels=labels, ordered=ordered)
