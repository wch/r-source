
KalmanLike <- function(y, mod, nit = 0)
{
    ## next call changes objects a, P, Pn: beware!
    x <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), FALSE, PACKAGE = "ts")
    names(x) <- c("ssq", "sumlog")
    s2 <- x[1]/length(y)
    list(Lik = 0.5*(log(x[1]/length(y)) + x[2]/length(y)), s2 = s2)
}

KalmanRun <- function(y, mod, nit = 0)
{
    ## next call changes objects a, P, Pn: beware!
    z <- .Call("KalmanLike", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), TRUE, PACKAGE = "ts")
    names(z) <- c("values", "resid", "states")
    x <- z$values
    s2 <- x[1]/length(y)
    z[[1]] <- c(Lik = 0.5*(log(x[1]/length(y)) + x[2]/length(y)), s2 = s2)
    z
}

KalmanForecast <- function(n.ahead = 10, mod)
{
    a <- an <- numeric(p <- length(mod$a))
    P <- Pn <- matrix(0, p, p)
    a[] <- mod$a
    P[] <- mod$P
    ## next call changes objects a, P
    x <- .Call("KalmanFore", as.integer(n.ahead), mod$Z, a, P,
               mod$T, mod$V, mod$h, PACKAGE = "ts")
    names(x) <- c("pred", "var")
    x
}

KalmanSmooth <- function(y, mod, nit = 0)
{
    z <- .Call("KalmanSmooth", y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h,
               mod$Pn, as.integer(nit), PACKAGE = "ts")
    names(z) <- c("smooth", "var")
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1, 2, 2)]
    z
}
