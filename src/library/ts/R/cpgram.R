# from MASS library: (C) 1994-9 W. N. Venables and B. D. Ripley
#
cpgram <- function(ts, taper=0.1,
   main=paste("Series: ", deparse(substitute(ts))), ci.col="blue")
{
    eval(main)
    if(NCOL(ts) > 1)
        stop("only implemented for univariate time series")
    x <- as.vector(ts)
    x <- x[!is.na(x)]
    x <- spec.taper(scale(x, TRUE, FALSE), p=taper)
    y <- Mod(fft(x))^2/length(x)
    y[1] <- 0
    n <- length(x)
    x <- (0:(n/2))*frequency(ts)/n
    if(length(x)%%2==0) {
        n <- length(x)-1
        y <- y[1:n]
        x <- x[1:n]
    } else y <- y[seq(along=x)]
    xm <- frequency(ts)/2
    mp <- length(x)-1
    crit <- 1.358/(sqrt(mp)+0.12+0.11/sqrt(mp))
    oldpty <- par("pty")
    on.exit(par(oldpty))
    par(pty="s")
    plot(x, cumsum(y)/sum(y), type="s", xlim=c(0, xm),
         ylim=c(0, 1), xaxs="i", yaxs="i", xlab="frequency",
         ylab="")
    lines(c(0, xm*(1-crit)), c(crit, 1), col = ci.col, lty = 2)
    lines(c(xm*crit, xm), c(0, 1-crit), col = ci.col, lty = 2)
    title(main = main)
    invisible()
}
