stl <- function(x, s.window = NULL, s.degree = 0, t.window = NULL,
                t.degree = 1, robust = FALSE, na.action = na.fail)
{
    nextodd <- function(x){
        x <- round(x)
        if(x%%2==0) x <- x+1
        as.integer(x)
    }

    x <- na.action(as.ts(x))
    if(is.matrix(x)) stop("only univariate series are allowed")
    n <- length(x)
    period <- frequency(x)
    if(is.null(s.window)) stop("s.window is missing with no default")
    if(period < 2 || n <= 2 * period)
        stop("series is not periodic or has less than two periods")
    periodic <- FALSE
    if(is.character(s.window))
        if(is.na(pmatch(s.window, "periodic")))
            stop("unknown value for s.window")
        else {
            periodic <- TRUE
            s.window <- 10 * n + 1
            s.degree <- 0
        }
    s.degree <- as.integer(s.degree)
    if(s.degree < 0 || s.degree > 1) stop("s.degree must be 0 or 1")
    t.degree <- as.integer(t.degree)
    if(t.degree < 0 || t.degree > 1) stop("t.degree must be 0 or 1")
    if(robust) {
        ni <- 1
        niter <- 15
    } else {
        ni <- 2
        niter <- 0
    }
    l.degree <- t.degree
    if(is.null(t.window))
        t.window <- nextodd(ceiling((1.5*period) / (1-(1.5/s.window))))
    l.window <- nextodd(period)
    z <- .Fortran("stl",
                  as.double(x),
                  as.integer(n),
                  as.integer(period),
                  as.integer(s.window),
                  as.integer(t.window),
                  as.integer(l.window),
                  s.degree, t.degree, l.degree,
                  nsjump = as.integer(ceiling(s.window/10)),
                  ntjump = as.integer(ceiling(t.window/10)),
                  nljump = as.integer(ceiling(l.window/10)),
                  as.integer(ni),
                  niter = as.integer(niter), weights = double(n),
                  seasonal = double(n),
                  trend = double(n),
                  double((n+2*period)*5), PACKAGE="ts")
    if(periodic) {
        ## make seasonal part exactly periodic
        which.cycle <- cycle(x)
        z$seasonal <- tapply(z$seasonal, which.cycle, mean)[which.cycle]
    }
    remainder <- as.vector(x) - z$seasonal - z$trend
    y <- cbind(seasonal=z$seasonal, trend=z$trend, remainder=remainder)
    res <- list(time.series = ts(y, start=start(x), frequency = period),
                weights=z$weights, call=match.call())
    class(res) <- "stl"
    res
}

print.stl <- function(x, ...)
{
    cat(" Call:\n")
    cat(" ")
    dput(x$call)
    cat("\nComponents\n")
    print(x$time.series)
    invisible(x)
}

plot.stl <- function(x, labels = colnames(X), ...)
{
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    nplot <- ncomp + 1
    oldpar <- par("mar", "oma", "mfrow", "tck")
    on.exit(par(oldpar))
    par(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0), tck = -0.01)
    par(mfrow = c(nplot, 1))
    for(i in 1:nplot) {
        plot(X[, i], type = if(i < nplot) "l" else "h",
             xlab = "", ylab = "", axes = FALSE, ...)
        if(i == nplot) abline(h=0)
        box()
        right <- i %% 2 == 0
        axis(2, labels = !right)
        axis(4, labels = right)
        mtext(labels[i], 2, 3)
    }
    axis(1, labels = TRUE)
    axis(3, labels = FALSE)
    mtext("time", 1, 3)
    invisible()
}
