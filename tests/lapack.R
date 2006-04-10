## tests of R functions based on the lapack module

##    -------  examples from ?svd using La.svd ---------

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
Eps <- 100 * .Machine$double.eps

X <- hilbert(9)[,1:6]
str(s <- La.svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% s$vt) < Eps)#  X = U D V'
stopifnot(abs(D - t(s$u) %*% X %*% t(s$vt)) < Eps)#  D = U' X V

X <- cbind(1, 1:7)
str(s <- La.svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% s$vt) < Eps)#  X = U D V'
stopifnot(abs(D - t(s$u) %*% X %*% t(s$vt)) < Eps)#  D = U' X V

# test nu and nv
La.svd(X, nu = 0)
(s <- La.svd(X, nu = 7))
stopifnot(dim(s$u) == c(7,7))
La.svd(X, nv = 0)

# test of complex case

X <- cbind(1, 1:7+(-3:3)*1i)
str(s <- La.svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% s$vt) < Eps)
stopifnot(abs(D - Conj(t(s$u)) %*% X %*% Conj(t(s$vt))) < Eps)

# in this case svd calls La.svd
str(s <- svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% Conj(t(s$v))) < Eps)
stopifnot(abs(D - Conj(t(s$u)) %*% X %*% s$v) < Eps)



##  -------  tests of random real and complex matrices ------

##			       100  may cause failures here.
eigenok <- function(A, E, Eps=1000*.Machine$double.eps)
{
    V <- E$vect; lam <- E$values
    stopifnot(abs(A %*% V - V %*% diag(lam)) < Eps,
              abs(lam[length(lam)]/lam[1]) < Eps || # this one not for singular A :
              abs(A - V %*% diag(lam) %*% t(V)) < Eps)
}

Ceigenok <- function(A, E, Eps=1000*.Machine$double.eps)
{
    V <- E$vect; lam <- E$values
    stopifnot(Mod(A %*% V - V %*% diag(lam)) < Eps,
              Mod(A - V %*% diag(lam) %*% Conj(t(V))) < Eps)
}

## failed for some 64bit-Lapack-gcc combinations:
sm <- cbind(1, 3:1, 1:3)
eigenok(sm, eigen(sm))
eigenok(sm, eigen(sm, sym=FALSE))

set.seed(123)
sm <- matrix(rnorm(25), 5, 5)
sm <- 0.5 * (sm + t(sm))
eigenok(sm, eigen(sm))
eigenok(sm, eigen(sm, sym=FALSE))

sm[] <- as.complex(sm)
Ceigenok(sm, eigen(sm))
Ceigenok(sm, eigen(sm, sym=FALSE))

sm[] <- sm + rnorm(25) * 1i
sm <- 0.5 * (sm + Conj(t(sm)))
Ceigenok(sm, eigen(sm))
Ceigenok(sm, eigen(sm, sym=FALSE))
