density <- function(x, bw, adjust = 1,
                    kernel=c("gaussian", "rectangular", "triangular", "cosine"),
                    window = kernel,
		    n = 512, width, from, to, cut = 3, na.rm = FALSE)
{
    if (!is.numeric(x))
	stop("argument must be numeric")
    name <- deparse(substitute(x))
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
    kernel <- match.arg(kernel)
    n.user <- n
    n <- max(n, 512)
    if (n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

    if (missing(bw))
	bw <-
	    if(missing(width))
		adjust * 0.9 * min(sd (x), IQR(x)/1.34) * N^(-0.2)
	    else 0.25 * width
    if (missing(from))
	from <- min(x) - cut * bw
    if (missing(to))
	to   <- max(x) + cut * bw
    if (!is.finite(from)) stop("non-finite `from'")
    if (!is.finite(to)) stop("non-finite `to'")
    if (!is.finite(bw)) stop("non-finite `bw'")
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
		    rectangular = {
                        a <- bw/0.2886751
                        ifelse(abs(kords) < 0.5 * a, 1/a, 0) },
		    triangular = {
                        a <- bw/0.4082483
                        ifelse(abs(kords) < a, (1 - abs(kords)/a)/a, 0) },
		    cosine = {
                        a <- bw/1.135724
                        ifelse(abs(kords) < a*pi, (1+cos(kords/a))/(2*pi*a), 0)}
		    )
    kords <- convolve(y, kords, type = "circular", conj = TRUE)[1:n]
    xords <- seq(lo, up, length = n)
    keep <- (xords >= from) & (xords <= to)
    x <- seq(from, to, length = n.user)
    structure(list(x = x, y = approx(xords, kords, x)$y, bw = bw, n = N,
		   call=match.call(), data.name=name, has.na = FALSE),
	      class="density")
}

plot.density <- function(s, main=NULL, xlab=NULL, ylab="Density", type="l",
			 zero.line = TRUE, ...)
{
    if(is.null(xlab))
	xlab <- paste("N =", s$n, "  Bandwidth =", formatC(s$bw))
    if(is.null(main)) main <- deparse(s$call)
    plot.default(s, main=main, xlab=xlab, ylab=ylab, type=type, ...)
    if(zero.line) abline(h=0, lwd=0.1, col = "gray")
}

print.density <- function(x, digits=NULL, ...)
{
    cat("\nCall:\n\t",deparse(x$call),
	"\n\nData: ",x$data.name," (",x$n," obs.);",
	"\tBandwidth 'bw' = ",formatC(x$bw,digits=digits), "\n\n",sep="")
    print(summary(as.data.frame(x[c("x","y")])), digits=digits, ...)
    invisible(x)
}
