density <-
function(x, bw, adjust = 1, kernel="gaussian", window = kernel,
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
	##if(! method %in% 1:4) stop("unknown density estimation kernel")
	if(n > 512) n <- 2^ceiling(log2(n)) #- to be fast with FFT

	if (missing(bw))
	 bw <-
	  if(missing(width))
		adjust * 1.06 * min(sd (x, na.rm=has.na),
                                    IQR(x, na.rm=has.na)/1.34) * N^-0.2
	  else 0.25 * width
	if (missing(from))
		from <- min(x, na.rm = has.na) - cut * bw
	if (missing(to))
		to   <- max(x, na.rm = has.na) + cut * bw
	y <- .C("massdist",
		x = as.double(x),
		nx= N,
		xlo = as.double(from),
		xhi = as.double(to),
		y = double(2 * n),
		ny= as.integer(n),
                NAOK = has.na) $ y
	xords <- seq(from, by = (to - from)/(n - 1), length = 2 * n)
	kords <- xords - from
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
	xords <- seq(from, by = (to - from)/(n - 1), length = n)
	structure(list(x = xords, y = kords, bw = bw, n = N,
                       call=match.call(), data.name=name, has.na = has.na),
                  class="density")
}

plot.density <-
function(s, main="", xlab=NULL, ylab="Density", type="l", ...)
{
	if(is.null(xlab)) xlab <- paste("Bandwidth =", s$bw)
	plot.default(s, main=main, xlab=xlab, ylab=ylab, type=type, ...)
}

print.density <-
function(x, digits=NULL, ...)
{
	cat("\nCall:\n\t",deparse(x$call),
	    "\n\nData: ",x$data.name," (",x$n," obs.);",
	    "\tBandwidth 'bw' =",formatC(x$bw,digits=digits), "\n\n",sep="")
	print(summary(as.data.frame(x[c("x","y")])), digits=digits, ...)
	invisible(x)
}
