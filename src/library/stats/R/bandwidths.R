#### copyright (C) 1994-2001 W. N. Venables and B. D. Ripley

#====           bandwidth selection rules              ====

bw.nrd0 <- function (x)
{
    if(length(x) < 2) stop("need at least 2 data points")
    hi <- sd(x)
    if(!(lo <- min(hi, IQR(x)/1.34)))# qnorm(.75) - qnorm(.25) = 1.34898
        (lo <- hi) || (lo <- abs(x[1])) || (lo <- 1.)
    0.9 * lo * length(x)^(-0.2)
}

bw.nrd <- function (x)
{
    if(length(x) < 2) stop("need at least 2 data points")
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2] - r[1])/1.34
    1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}

bw.SJ <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                  method = c("ste", "dpi"))
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")
    storage.mode(x) <- "double"
    method <- match.arg(method)

    fSD <- function(h, x, alph2, c1, n, d)
        (c1/SDh(x, alph2 * h^(5/7), n, d))^(1/5) - h
    SDh <- function(x, h, n, d)
        .C("band_phi4_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u
    TDh <- function(x, h, n, d)
        .C("band_phi6_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u

    Z <- .C("band_den_bin",
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb),
            PACKAGE="base")
    d <- Z$d; cnt <- as.integer(Z$cnt)
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    scale <- min(sqrt(var(x)), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2*sqrt(pi)*n)
    TD  <- -TDh(cnt, b, n, d)
    if(!is.finite(TD) || TD <= 0)
        stop("sample is too sparse to find TD")
    if(method == "dpi")
        res <- (c1/SDh(cnt,(2.394/(n * TD))^(1/7) , n, d))^(1/5)
    else {
        alph2 <- 1.357*(SDh(cnt, a, n, d)/TD)^(1/7)
        if(!is.finite(alph2))
            stop("sample is too sparse to find alph2")
        if (fSD(lower, cnt, alph2, c1, n, d) *
            fSD(upper, cnt, alph2, c1, n, d) > 0)
            stop("no solution in the specified range of bandwidths")
        res <- uniroot(fSD, c(lower, upper), tol=0.1*lower,
                       x=cnt, alph2=alph2, c1=c1, n=n, d=d)$root
    }
    res
}


bw.ucv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax)
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fucv <- function(h, x, n, d)
        .C("band_ucv_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C("band_den_bin",
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb),
            PACKAGE="base"
            )
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fucv, c(lower, upper), tol=0.1*lower,
                  x=cnt, n=n, d=d)$minimum
    if(h < 1.1*lower | h > upper-0.1*lower)
        warning("minimum occurred at one end of the range")
    h
}

bw.bcv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax)
{
    if((n <- length(x)) < 2) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fbcv <- function(h, x, n, d)
        .C("band_bcv_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C("band_den_bin",
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb),
            PACKAGE="base"
            )
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fbcv, c(lower, upper), tol=0.1*lower,
                  x=cnt, n=n, d=d)$minimum
    if(h < 1.1*lower | h > upper-0.1*lower)
        warning("minimum occurred at one end of the range")
    h
}
