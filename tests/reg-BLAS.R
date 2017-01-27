
## PR#4582 %*% with NAs
stopifnot(is.na(NA %*% 0), is.na(0 %*% NA))
## depended on the BLAS in use.


## found from fallback test in slam 0.1-15
## most likely indicates an inaedquate BLAS.
x <- matrix(c(1, 0, NA, 1), 2, 2)
y <- matrix(c(1, 0, 0, 2, 1, 0), 3, 2)
(z <- tcrossprod(x, y))
stopifnot(identical(z, x %*% t(y)))
stopifnot(is.nan(log(0) %*% 0))
## depended on the BLAS in use: some (including the reference BLAS)
## had z[1,3] == 0 and log(0) %*% 0 as as.matrix(0).

## matrix products with doubles

m <- matrix(c(1,2,3,4), ncol=2)
v <- c(11,12)
rv <- v; dim(rv) <- c(1,2)
cv <- v; dim(cv) <- c(2,1)

stopifnot(identical(m %*% m, matrix(c(7,10,15,22), 2, 2) ))
stopifnot(identical(m %*% cv, matrix(c(47,70), 2, 1) ))
stopifnot(identical(m %*% v, matrix(c(47,70), 2, 1) ))
stopifnot(identical(rv %*% m, matrix(c(35,81), 1, 2) ))
stopifnot(identical(v %*% m, matrix(c(35,81), 1, 2) ))
stopifnot(identical(rv %*% cv, matrix(265,1,1) ))
stopifnot(identical(cv %*% rv, matrix(c(121,132,132,144), 2, 2) ))
stopifnot(identical(v %*% v, matrix(265,1,1) ))

stopifnot(identical(crossprod(m, m), matrix(c(5, 11, 11, 25), 2, 2) ))
stopifnot(identical(crossprod(m, cv), matrix(c(35, 81), 2, 1) ))
stopifnot(identical(crossprod(m, v), matrix(c(35, 81), 2, 1) ))
stopifnot(identical(crossprod(cv, m), matrix(c(35, 81), 1, 2) ))
stopifnot(identical(crossprod(v, m), matrix(c(35, 81), 1, 2) ))
stopifnot(identical(crossprod(cv, cv), matrix(265, 1, 1) ))
stopifnot(identical(crossprod(v, v), matrix(265, 1, 1) ))
stopifnot(identical(crossprod(rv, rv), matrix(c(121, 132, 132, 144), 2, 2) ))

stopifnot(identical(tcrossprod(m, m), matrix(c(10, 14, 14, 20), 2, 2) ))
stopifnot(identical(tcrossprod(m, rv), matrix(c(47, 70), 2, 1) ))
stopifnot(identical(tcrossprod(rv, m), matrix(c(47, 70), 1, 2) ))
stopifnot(identical(tcrossprod(v, m), matrix(c(47, 70), 1, 2) ))
stopifnot(identical(tcrossprod(rv, rv), matrix(265, 1, 1) ))
stopifnot(identical(tcrossprod(cv, cv), matrix(c(121, 132, 132, 144), 2, 2) ))
stopifnot(identical(tcrossprod(v, v), matrix(c(121, 132, 132, 144), 2, 2) ))

## non-square matrix, with Inf

m1 <- matrix(c(1,2,Inf,4,5,6), ncol=2)
m2 <- matrix(c(1,2,3,4), ncol=2)

v <- c(11,12)
rv <- v; dim(rv) <- c(1,2)
cv <- v; dim(cv) <- c(2,1)

v1 <- c(11,12,13)
rv1 <- v1; dim(rv1) <- c(1,3)
cv1 <- v1; dim(cv1) <- c(3,1)

stopifnot(identical(m1 %*% m2, matrix(c(9,12,Inf,19,26,Inf), 3, 2) ))
stopifnot(identical(m1 %*% cv, matrix(c(59,82,Inf), 3, 1) ))
stopifnot(identical(rv1 %*% m1, matrix(c(Inf,182), 1, 2) ))

stopifnot(identical(crossprod(m1, m1), matrix(c(Inf,Inf,Inf,77), 2, 2) ))
stopifnot(identical(crossprod(m1, cv1), matrix(c(Inf, 182), 2, 1) ))
stopifnot(identical(crossprod(cv1, m1), matrix(c(Inf, 182), 1, 2) ))

stopifnot(identical(tcrossprod(m1, m1), matrix(c(17,22,Inf,22,29,Inf,Inf,Inf,Inf), 3,3) ))
stopifnot(identical(tcrossprod(m2, m1), matrix(c(13,18,17,24,Inf,Inf), 2, 3) ))
stopifnot(identical(tcrossprod(rv, m1), matrix(c(59,82,Inf), 1, 3) ))
stopifnot(identical(tcrossprod(m1, rv), matrix(c(59,82,Inf), 3, 1) ))

