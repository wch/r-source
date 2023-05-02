## Explore the old.coords TRUE --> FALSE change:
options(warn = 1, nwarnings = 1e4)

chkDens <- function(x, n=512, verbose=TRUE, plot=verbose) {
    stopifnot(n == as.integer(n), length(n) == 1L, n > 1)
    xnam <- deparse(substitute(x))
    den  <- density(x, n=n) # -> grid of n = 512 points
    den0 <- density(x, n=n, old.coords = TRUE, bw=den$bw)
    N <- length(x)
    if(verbose) withAutoprint({
        cat(xnam,":\n",strrep("-",nchar(xnam)), "\n N=", N, "; grid n=", n, "\n", sep="")
        summary(den0$y / den$y) # 1.001 ... 1.011
        summary(    den0$y / den$y - 1) # ~= 1/(2n-2)
        summary(1/ (den0$y / den$y - 1))# ~=    2n-2 = 1022
    })
    corr0 <- 1 - 1/(2*n-2)
    ## difference depends on N (decreasing with N); assuming log(N) ~ N(m_(N), sd_N(log(N)))
    ## in additions to models above, use lots of fudge...
    mN  <-   0.0012 * N^ -0.346
    sdN <-            N^ -0.2033
    tolN  <- mN * exp(3.5 * sdN)
    tolN2 <- tolN * (2 +  6*sdN)
    if(verbose)
        cat("tolN,t..2 = ", format(tolN), format(tolN2), "\n")
    if(plot) {
        plot(den$x, den0$y/den$y - 1, type='o', cex=1/4)
        rug(x, col=adjustcolor("black", pmin(1/2, 150/den$n)))
        title(sprintf("Rel.differ. density(%s, old.coords = TRUE/FALSE)", xnam))
        abline(h = 1/(2*n-2), v = range(x), lty=2)
    }
    stopifnot(exprs = {
        identical(den0$x, den$x)
        any(inI <- min(x) <= den$x  &  den$x <= max(x))
        all.equal(den$y[inI], den0$y[inI]*corr0, tol = tolN )   # 5.878e-5
        all.equal(den$y     , den0$y     *corr0, tol = tolN2) # 9.48 e-5
    })

    ## exact integration to 1 : .. compute density further out:
    n <- 1024+1 # to give exact delta x (with ext=0 *and* if (fr,to) == (-4, 4)
    rx <- range(x)
    fr <- min(-4, rx[1]); to <- max(+4, rx[2])
    repeat {
        den  <- density(x, bw=1/8, from=fr, to=to, n=n, ext=0)
        den0 <- density(x, bw=1/8, from=fr, to=to, n=n, ext=0, old.coords = TRUE)
        if(do1 <- den$y[1] > 0 || den0$y[1] > 0) fr <- fr - (to-fr)/64
        if(do2 <- den$y[n] > 0 || den0$y[n] > 0) to <- to + (to-fr)/64
        if(!do1 && !do2)
            break
        if(verbose) cat(sprintf(" .. extended [fr,to] to [%g, %g]\n", fr, to))
    }
    h <- mean(diff(den$x))
    ## Use Simpson's rule to numerically integrate; weights = h/3 (1 4 2 4 2 .... 2 4 1)
    M <- sum(ip <- den$y*den0$y > 0)
    if(M %% 2 == 0) { ## need odd M
        ip[min(which(ip))-1] <- TRUE
        M <- sum(ip)
    }
    w <- c(1, rep.int(c(4,2), (M-3)/2), 4, 1)
    int.f <- sum(h/3 * w * den$y [ip]) # 0.9999998
    intof <- sum(h/3 * w * den0$y[ip]) # 1.000244
    if(verbose) {
        cat("integral [Simpson approx]: ", int.f,"= 1 -", signif(1-int.f,3), "\n")
        cat("integral [old.c; Simpson]: ", intof,"= 1 -", signif(1-intof,3), "\n")
    }
    stopifnot(abs(1 - int.f) < 5e-7) # see 2.38e-7 whereas old coords gave 0.000244
}

chkDens(-3:3) # integral error: 2.38e-7
chkDens(sqrt(lynx)/10)

set.seed(7)
system.time(
 for(run in 1:20) {
   chkDens(runif(2^12+run, -3.5, 3.5), verbose=FALSE) # integral error: 2.38e-7  "always"
   chkDens(rnorm(950 +run),            verbose=FALSE) # (ditto now)
 }
) #
