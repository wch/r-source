density <-
    function(x, bw = "nrd0", adjust = 1,
             kernel = c("gaussian", "epanechnikov", "rectangular",
             "triangular", "biweight", "cosine", "optcosine"),
             window = kernel, width,
             give.Rkern = FALSE,
             n = 512, from, to, cut = 3, na.rm = FALSE)
{
    if(!missing(window) && missing(kernel))
        kernel <- window
    kernel <- match.arg(kernel)
    if(give.Rkern)
        ##-- sigma(K) * R(K), the scale invariant canonical bandwidth:
        return(switch(kernel,
                      gaussian = 1/(2*sqrt(pi)),
                      rectangular = sqrt(3)/6,
                      triangular  = sqrt(6)/9,
                      epanechnikov= 3/(5*sqrt(5)),
                      biweight    = 5*sqrt(7)/49,
                      cosine      = 3/4*sqrt(1/3 - 2/pi^2),
                      optcosine   = sqrt(1-8/pi^2)*pi^2/16
                      ))

    if (!is.numeric(x))
        stop("argument must be numeric")
    name <- deparse(substitute(x))
    x <- as.vector(x)
    x.na <- is.na(x)
    if (any(x.na)) {
        if (na.rm) x <- x[!x.na]
        else stop("x contains missing values")
    }
    N <- nx <- length(x)
    x.finite <- is.finite(x)
    if(any(!x.finite)) {
        x <- x[x.finite]
        nx <- sum(x.finite)
    }
    n.user <- n
    n <- max(n, 512)
    if (n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

    if (missing(bw) && !missing(width)) {
        if(is.numeric(width)) bw <- width/4
        if(is.character(width)) bw <- width
    }
    if (is.character(bw)) {
        bw <- switch(tolower(bw),
                     nrd0 = bw.nrd0(x),
                     nrd = bw.nrd(x),
                     ucv = bw.ucv(x),
                     bcv = bw.bcv(x),
                     sj = , "sj-ste" = bw.SJ(x, method="ste"),
                     "sj-dpi" = bw.SJ(x, method="dpi"),
                     stop("unknown bandwidth rule"))
# width adjustments from V&R 1994 p.137.
#        if(kernel != "gaussian")
#         bw <- bw * switch(kernel,
#                       rectangular = 1/1.15,
#                       triangular  = 1.393/1.15,
#                       epanechnikov= 1.272/1.15,
#                       biweight    = NA,
#                       cosine      = NA,
#                       optcosine   = NA
#                       )
    }
    if (!is.finite(bw)) stop("non-finite `bw'")
    bw <- adjust * bw
    if (bw <= 0) stop("`bw' is not positive.")

    if (missing(from))
        from <- min(x) - cut * bw
    if (missing(to))
	to   <- max(x) + cut * bw
    if (!is.finite(from)) stop("non-finite `from'")
    if (!is.finite(to)) stop("non-finite `to'")
    lo <- from - 4 * bw
    up <- to + 4 * bw
    y <- .C("massdist",
	    x = as.double(x),
	    nx = nx,
	    xlo = as.double(lo),
	    xhi = as.double(up),
	    y = double(2 * n),
	    ny = as.integer(n),
	    PACKAGE = "base")$y * (nx/N)
    kords <- seq(0, 2*(up-lo), length = 2 * n)
    kords[(n + 2):(2 * n)] <- -kords[n:2]
    kords <- switch(kernel,
		    gaussian = dnorm(kords, sd = bw),
                    ## In the following, a := bw / sigma(K0), where
                    ##	K0() is the unscaled kernel below
		    rectangular = {
                        a <- bw*sqrt(3)
                        ifelse(abs(kords) < a, .5/a, 0) },
		    triangular = {
                        a <- bw*sqrt(6) ; ax <- abs(kords)
                        ifelse(ax < a, (1 - ax/a)/a, 0) },
		    epanechnikov = {
                        a <- bw*sqrt(5) ; ax <- abs(kords)
                        ifelse(ax < a, 3/4*(1 - (ax/a)^2)/a, 0) },
		    biweight = { ## aka quartic
                        a <- bw*sqrt(7) ; ax <- abs(kords)
                        ifelse(ax < a, 15/16*(1 - (ax/a)^2)^2/a, 0) },
		    cosine = {
                        a <- bw/sqrt(1/3 - 2/pi^2)
                        ifelse(abs(kords) < a, (1+cos(pi*kords/a))/(2*a),0)},
		    optcosine = {
                        a <- bw/sqrt(1-8/pi^2)
                        ifelse(abs(kords) < a, pi/4*cos(pi*kords/(2*a))/a, 0)}
                    )
    kords <- fft( fft(y)* Conj(fft(kords)), inv=TRUE)
    kords <- Re(kords)[1:n]/length(y)
    xords <- seq(lo, up, length = n)
    keep <- (xords >= from) & (xords <= to)
    x <- seq(from, to, length = n.user)
    structure(list(x = x, y = approx(xords, kords, x)$y, bw = bw, n = N,
		   call=match.call(), data.name=name, has.na = FALSE),
	      class="density")
}

plot.density <- function(x, main=NULL, xlab=NULL, ylab="Density", type="l",
			 zero.line = TRUE, ...)
{
    if(is.null(xlab))
	xlab <- paste("N =", x$n, "  Bandwidth =", formatC(x$bw))
    if(is.null(main)) main <- deparse(x$call)
    plot.default(x, main=main, xlab=xlab, ylab=ylab, type=type, ...)
    if(zero.line) abline(h=0, lwd=0.1, col = "gray")
    invisible(NULL)
}

print.density <- function(x, digits=NULL, ...)
{
    cat("\nCall:\n\t",deparse(x$call),
	"\n\nData: ",x$data.name," (",x$n," obs.);",
	"\tBandwidth 'bw' = ",formatC(x$bw,digits=digits), "\n\n",sep="")
    print(summary(as.data.frame(x[c("x","y")])), digits=digits, ...)
    invisible(x)
}

#====           bandwidth selection rules              ====

bw.nrd0 <- function (x)
{
    hi <- sd(x)
    if(!(lo <- min(hi, IQR(x)/1.34)))# qnorm(.75) - qnorm(.25) = 1.34898
        (lo <- hi) || (lo <- abs(x[1])) || (lo <- 1.)
    0.9 * lo * length(x)^(-0.2)
}

bw.nrd <- function (x)
{
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2] - r[1])/1.34
    1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}

bw.SJ <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                  method = c("ste", "dpi"))
{
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

    method <- match.arg(method)
    if(!is.numeric(x) || !length(x))
        stop("invalid x")
    n <- length(x)
    storage.mode(x) <- "double"
    n <- length(x)
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
    alph2 <- 1.357*(SDh(cnt, a, n, d)/TD)^(1/7)
    if(method == "dpi")
        res <- (c1/SDh(cnt,(2.394/(n * TD))^(1/7) , n, d))^(1/5)
    else {
        if (fSD(lower, cnt, alph2, c1, n, d) *
            fSD(upper, cnt, alph2, c1, n, d) > 0)
            stop("No solution in the specified range of bandwidths")
        res <- uniroot(fSD, c(lower, upper), tol=0.1*lower,
                       x=cnt, alph2=alph2, c1=c1, n=n, d=d)$root
    }
    res
}


bw.ucv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax)
{
    fucv <- function(h, x, n, d)
        .C("band_ucv_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u

    if(!is.numeric(x) || !length(x))
        stop("invalid x")
    n <- length(x)
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
    fbcv <- function(h, x, n, d)
        .C("band_bcv_bin",
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1),
           PACKAGE="base")$u

    if(!is.numeric(x) || !length(x))
        stop("invalid x")
    n <- length(x)
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
    h<- optimize(fbcv, c(lower, upper), tol=0.1*lower,
                 x=cnt, n=n, d=d)$minimum
    if(h < 1.1*lower | h > upper-0.1*lower)
        warning("minimum occurred at one end of the range")
    h
}
