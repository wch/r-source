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

# test of complex case

X <- cbind(1, 1:7+(-3:3)*1i)
str(s <- La.svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% s$vt) < Eps)
stopifnot(abs(D - Conj(t(s$u)) %*% X %*% Conj(t(s$vt))) < Eps)

# in this case svd calls La.svd
str(s <- svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u %*% D %*% Conj(t(s$v))) < Eps)
stopifnot(abs(D - Conj(t(s$u)) %*% X %*% s$v) < Eps)


##    -------  examples from ?eigen using La.eigen ---------

La.eigen(cbind(c(1,-1),c(-1,1)))
La.eigen(cbind(c(1,-1),c(-1,1)), symmetric = FALSE)

La.eigen(cbind(1,c(1,-1)), only.values = TRUE)
La.eigen(cbind(-1,2:1)) # complex values
La.eigen(print(cbind(c(0,1i), c(-1i,0))))# Hermite ==> real eigenvalues
## 3 x 3:
La.eigen(cbind( 1,3:1,1:3))
La.eigen(cbind(-1,c(1:2,0),0:2)) # complex values

Meps <- .Alias(.Machine$double.eps)
m <- matrix(round(rnorm(25),3), 5,5)
sm <- m + t(m) #- symmetric matrix
em <- La.eigen(sm); V <- em$vect
print(lam <- em$values) # ordered DEcreasingly

stopifnot(
 abs(sm %*% V - V %*% diag(lam))          < 60*Meps,
 abs(sm       - V %*% diag(lam) %*% t(V)) < 60*Meps)

## symmetric = FALSE

em <- La.eigen(sm, symmetric = FALSE); V2 <- em$vect
print(lam2 <- em$values) # ordered decreasingly in ABSolute value !
print(i <- rev(order(lam2)))
stopifnot(abs(lam - lam2[i]) < 60 * Meps)

zapsmall(Diag <- t(V2) %*% V2) # orthonormal
print(norm2V <- apply(V2 * V2, 2, sum))
stopifnot( abs(1- norm2V / diag(Diag)) < 60*Meps)

stopifnot(abs(sm %*% V2 - V2 %*% diag(lam2))            < 60*Meps,
          abs(sm        - V2 %*% diag(lam2) %*% t(V2)) < 60*Meps)


##  -------  tests of random real and complex matrices ------

# 100 may cause failures here.
eigenok <- function(A, E, Eps=1000*.Machine$double.eps)
{
    V <- E$vect; lam <- E$values
    stopifnot(abs(A %*% V - V %*% diag(lam)) < Eps,
              abs(A - V %*% diag(lam) %*% t(V)) < Eps)
}

Ceigenok <- function(A, E, Eps=1000*.Machine$double.eps)
{
    V <- E$vect; lam <- E$values
    stopifnot(Mod(A %*% V - V %*% diag(lam)) < Eps,
              Mod(A - V %*% diag(lam) %*% Conj(t(V))) < Eps)
}

set.seed(123)
sm <- matrix(rnorm(25), 5, 5)
sm <- 0.5 * (sm + t(sm))
eigenok(sm, eigen(sm))
eigenok(sm, La.eigen(sm))
eigenok(sm, La.eigen(sm, sym=FALSE))

sm[] <- as.complex(sm)
Ceigenok(sm, eigen(sm))
Ceigenok(sm, La.eigen(sm))
Ceigenok(sm, La.eigen(sm, sym=FALSE))

sm[] <- sm + rnorm(25) * 1i
sm <- 0.5 * (sm + Conj(t(sm)))
Ceigenok(sm, eigen(sm))
Ceigenok(sm, La.eigen(sm))
Ceigenok(sm, La.eigen(sm, sym=FALSE))
