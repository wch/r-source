density <- function(x, bw, adjust = 1, kernel="gaussian", window = kernel,
		    n = 512, width, from, to, cut = 3, na.rm = FALSE)
{
    if (!is.numeric(x))
	stop("argument must be numeric")
    name <- deparse(substitute(x))
    x.na <- is.na(x)
    if(na.rm) x <- x[!x.na]
    has.na <- !na.rm && any(x.na)
    N <- length(x)
    k.list <- c("gaussian", "rectangular", "triangular", "cosine")
    method <- pmatch(kernel, k.list)
    if(is.na(method))
	stop(paste("kernel must be a 'pmatch' of",
		   paste(k.list,collapse=', ')))
    n.user <- n
    n <- max(n, 512)
    if(n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

    if (missing(bw))
	bw <-
	    if(missing(width))
		adjust * 0.9 * min(sd (x, na.rm=has.na),
				   IQR(x, na.rm=has.na)/1.34) * N^-0.2
	    else 0.25 * width
    if (missing(from))
	from <- min(x, na.rm = has.na) - cut * bw
    if (missing(to))
	to   <- max(x, na.rm = has.na) + cut * bw
    lo <- from - 4 * bw
    up <- to + 4 * bw
    y <- .C("massdist",
	    x = as.double(x),
	    nx= N,
	    xlo = as.double(lo),
	    xhi = as.double(up),
	    y = double(2 * n),
	    ny= as.integer(n),
	    NAOK = has.na) $ y
    xords <- seq(lo, up + (up-lo), length = 2 * n)
    kords <- xords - lo
    kords[(n + 2):(2 * n)] <- -kords[n:2]
    kords <- switch(method,
		    dnorm(kords, sd = bw),# 1
		{ a <- bw/0.2886751
		  ifelse(abs(kords) < 0.5 * a, 1/a, 0) },# 2
		{ a <- bw/0.4082483
		  ifelse(abs(kords) < a, (1 - abs(kords)/a)/a, 0) },# 3
		{ a <- bw/1.135724
		  ifelse(abs(kords) < a*pi,
			 (1+cos(kords/a))/(2*pi*a), 0)}# 4
		    )
    kords <- convolve(y, kords)[1:n]
    xords <- seq(lo, up, length = n)
    keep <- (xords >= from) & (xords <= to)
    x <- seq(from, to, length = n.user)
    structure(list(x = x, y = approx(xords, kords, x)$y, bw = bw, n = N,
		   call=match.call(), data.name=name, has.na = has.na),
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
