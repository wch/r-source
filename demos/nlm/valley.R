#### -*- R -*-
					# Helical Valley Function
					# Page 362 Dennis + Schnabel

theta <- function(x1,x2) {
    if(x1 > 0)
	return((0.5/pi)*atan(x2/x1))
    else
	return((0.5/pi)*atan(x2/x1)+0.5)
}

f <- function(x) {
    f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
    f2 <- 10*(sqrt(x[1]^2+x[2]^2)-1)
    f3 <- x[3]
    return(f1^2+f2^2+f3^2)
}

nlm(f, c(-1,0,0), hessian=TRUE, print=0)
