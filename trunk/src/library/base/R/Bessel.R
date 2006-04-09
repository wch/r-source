gammaCody <- function(x) .Internal(gammaCody(x))

besselI <- function(x, nu, expon.scaled = FALSE)
{
    .Internal(besselI(x,nu, 1+ as.logical(expon.scaled)))
}
besselK <- function(x, nu, expon.scaled = FALSE)
{
    .Internal(besselK(x,nu, 1+ as.logical(expon.scaled)))
}
besselJ <- function(x, nu) .Internal(besselJ(x,nu))
besselY <- function(x, nu) .Internal(besselY(x,nu))
