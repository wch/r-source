density <- function(x, ...) UseMethod("density")

density.default <-
    function(x, bw = "nrd0", adjust = 1,
	     kernel = c("gaussian", "epanechnikov", "rectangular",
	     "triangular", "biweight", "cosine", "optcosine"),
	     weights = NULL, window = kernel, width,
	     give.Rkern = FALSE,
	     n = 512, from, to, cut = 3, na.rm = FALSE, ...)
{
    if(length(list(...)) > 0)
	warning("non-matched further arguments are disregarded")
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
        stop("argument 'x' must be numeric")
    name <- deparse(substitute(x))
    x <- as.vector(x)
    x.na <- is.na(x)
    if (any(x.na)) {
        if (na.rm) x <- x[!x.na]
        else stop("'x' contains missing values")
    }
    N <- nx <- length(x)
    x.finite <- is.finite(x)

    ## Handle 'weights'
    if(is.null(weights))  {
        weights <- rep.int(1/nx, nx)
        wsum <- 1
    }
    else {
        if(length(weights) != nx)
            stop("'x' and 'weights' have unequal length")
        if(!all(is.finite(weights)))
            stop("'weights' must all be finite")
        if(any(weights < 0))
            stop("'weights' must not be negative")
        wsum <- sum(weights)
        ## FIXME: Shouldn't we rather signal an error?
        if (!isTRUE(all.equal(1, wsum)))
            warning("sum(weights) != 1  -- will not get true density")
    }

    if(any(!x.finite)) {
        x <- x[x.finite]
        weights <- weights[x.finite]
        nx <- length(x) # == sum(x.finite)
        wsum.fini <- sum(weights)
    } else wsum.fini <- wsum

    n.user <- n
    n <- max(n, 512)
    if (n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

    if (missing(bw) && !missing(width)) {
        if(is.numeric(width)) {
            ## S has width equal to the length of the support of the kernel
            ## except for the gaussian where it is 4 * sd.
            ## R has bw a multiple of the sd.
            fac <- switch(kernel,
                          gaussian = 4,
                          rectangular = 2*sqrt(3),
                          triangular = 2 * sqrt(6),
                          epanechnikov = 2 * sqrt(5),
                          biweight = 2 * sqrt(7),
                          cosine = 2/sqrt(1/3 - 2/pi^2),
                          optcosine = 2/sqrt(1-8/pi^2)
                          )
            bw <- width / fac
        }
        if(is.character(width)) bw <- width
    }
    if (is.character(bw)) {
        if(nx < 2)
            stop("need at least 2 points to select a bandwidth automatically")
        bw <- switch(tolower(bw),
                     nrd0 = bw.nrd0(x),
                     nrd = bw.nrd(x),
                     ucv = bw.ucv(x),
                     bcv = bw.bcv(x),
                     sj = , "sj-ste" = bw.SJ(x, method="ste"),
                     "sj-dpi" = bw.SJ(x, method="dpi"),
                     stop("unknown bandwidth rule"))
    }
    if (!is.finite(bw)) stop("non-finite 'bw'")
    bw <- adjust * bw
    if (bw <= 0) stop("'bw' is not positive.")

    if (missing(from))
        from <- min(x) - cut * bw
    if (missing(to))
	to   <- max(x) + cut * bw
    if (!is.finite(from)) stop("non-finite 'from'")
    if (!is.finite(to)) stop("non-finite 'to'")
    lo <- from - 4 * bw
    up <- to + 4 * bw
    y <- .C("massdist",
	    x = as.double(x),
            xmass = as.double(weights),
	    nx = nx,
	    xlo = as.double(lo),
	    xhi = as.double(up),
	    y = double(2 * n),
	    ny = as.integer(n),
            PACKAGE = "base" )$y * (wsum.fini/wsum)
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
#    keep <- (xords >= from) & (xords <= to)
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
