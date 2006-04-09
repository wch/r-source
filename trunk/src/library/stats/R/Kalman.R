
KalmanLike <- function(y, mod, nit = 0, fast=TRUE)
{
    ## next call changes objects a, P, Pn if fast==TRUE: beware!
    x <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), FALSE, fast=fast, PACKAGE = "stats")
    names(x) <- c("ssq", "sumlog")
    s2 <- x[1]/length(y)
    list(Lik = 0.5*(log(x[1]/length(y)) + x[2]/length(y)), s2 = s2)
}

KalmanRun <- function(y, mod, nit = 0, fast=TRUE)
{
    ## next call changes objects a, P, Pn if fast==TRUE: beware!
    z <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), TRUE, fast=fast, PACKAGE = "stats")
    names(z) <- c("values", "resid", "states")
    x <- z$values
    s2 <- x[1]/length(y)
    z[[1]] <- c(Lik = 0.5*(log(x[1]/length(y)) + x[2]/length(y)), s2 = s2)
    z
}

KalmanForecast <- function(n.ahead = 10, mod, fast=TRUE)
{
    a <- numeric(p <- length(mod$a))
    P <- matrix(0, p, p)
    a[] <- mod$a
    P[] <- mod$P
    ## next call changes objects a, P if fast==TRUE
    x <- .Call("KalmanFore", as.integer(n.ahead), mod$Z, a, P,
               mod$T, mod$V, mod$h, fast=fast, PACKAGE = "stats")
    names(x) <- c("pred", "var")
    x
}

KalmanSmooth <- function(y, mod, nit = 0)
{
    z <- .Call("KalmanSmooth", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), PACKAGE = "stats")
    names(z) <- c("smooth", "var")
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1, 2, 2)]
    z
}
