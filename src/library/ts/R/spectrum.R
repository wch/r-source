## based on code by Martyn Plummer
spectrum <- function (..., method = c("pgram", "ar"))
{
    switch(match.arg(method),
           pgram = spec.pgram(...),
           ar = spec.ar(...)
           )
}

## spec.taper based on code by Kurt Hornik
spec.taper <- function (x, p = 0.1)
{
    if (any(p < 0) || any(p > 0.5))
        stop("p must be between 0 and 0.5")
    x <- as.ts(x)
    a <- attributes(x)
    x <- as.matrix(x)
    nc <- ncol(x)
    if (length(p) == 1)
        p <- rep(p, nc)
    else if (length(p) != nc)
        stop("length of p must be 1 or equal the number of columns of x")
    nr <- nrow(x)
    for (i in 1:nc) {
        m <- floor(nr * p[i])
        if(m == 0) next
        w <- 0.5 * (1 - cos(pi * seq(1, 2 * m - 1, by = 2)/(2 * m)))
        x[, i] <- c(w, rep(1, nr - 2 * m), rev(w)) * x[, i]
    }
    attributes(x) <- a
    x
}

spec.ar <- function(x, n.freq, order = NULL, plot = TRUE,
                    na.action = na.fail, ...)
{
    ## can be called with a ts or a result of an AR fit.
    if(!is.list(x)) {
        series <- deparse(substitute(x))
        xfreq <- frequency(x)
        x <- na.action(x)
        n <- NROW(x)
        nser <- NCOL(x)
        x <- ar(x, is.null(order), order, na.action=na.action)
    } else {
        cn <- match(c("ar", "var.pred", "order"), names(x))
        if(any(is.na(cn)))
            stop("x must be a time series or an ar() fit")
        series <- x$series
        xfreq <- x$frequency
        if(is.array(x$ar)) nser <- dim(x$ar)[2] else nser <- 1
    }
    n <- x$n.used
    order <- x$order
    if(missing(n.freq)) n.freq <- 500
    freq <- seq(0, 0.5, length = n.freq)
    if (nser == 1) {
        coh <- phase <- NULL
        cs <- outer(freq, 1:order, function(x, y) cos(2*pi*x*y)) %*% x$ar
        sn <- outer(freq, 1:order, function(x, y) sin(2*pi*x*y)) %*% x$ar
        spec <- x$var.pred/(2*pi*xfreq*((1 - cs)^2 + sn^2))
    } else .NotYetImplemented()
    spg.out <- list(freq = freq, spec = spec, coh = coh, phase = phase,
                    n.used = nrow(x), series = series,
                    method = paste("AR (", order, ") spectrum ", sep="")
                    )
    class(spg.out) <- "spec"
    if(plot) {
	plot(spg.out, ci = 0, ...)
        return(invisible(spg.out))
    } else return(spg.out)
}

spec.pgram <-
    function (x, spans = NULL, kernel = NULL, taper = 0.1,
              pad = 0, fast = TRUE,
              demean = FALSE, detrend = TRUE,
              plot = TRUE, na.action = na.fail, ...)
{
    ## Estimate spectral density from (smoothed) periodogram.
    series <- deparse(substitute(x))
    xfreq <- frequency(x)
    x <- na.action(x)
    x <- as.matrix(x)
    N <- nrow(x)
    nser <- ncol(x)
    if(!is.null(spans)) kernel <- modified.daniell.kernel(spans %/% 2)
    if(!is.null(kernel) && !is.kernel(kernel))
        stop("must specify spans or a valid kernel")
    if (detrend) {
        t <- 1:N - (N + 1)/2
        sumt2 <- N * (N^2 - 1)/12
        for (i in 1:ncol(x))
            x[, i] <- x[, i] - mean(x[, i]) - sum(x[, i] * t) * t/sumt2
    }
    else if (demean) {
        x <- sweep(x, 2, apply(x, 2, mean))
    }
    x <- spec.taper(x, taper)
    ## to correct for tapering: Bloomfield (1976, p. 194)
    u2 <- (1 - (5/8)*taper)
    u4 <- (1 - (93/128)*taper)
    if (pad > 0) {
        x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
        N <- nrow(x)
    }
    NewN <- if(fast) nextn(N) else N
    x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
    N <- nrow(x)
    Nspec <- floor(N/2)
    freq <- seq(from = xfreq/N, by = xfreq/N, length = Nspec)
    xfft <- mvfft(x)[2:(N - 1), , drop = FALSE]
    pgram <- array(NA, dim = c(nrow(x) - 2, ncol(x), ncol(x)))
    for (i in 1:ncol(x)) {
        for (j in 1:ncol(x)) {
            pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N*2*pi*xfreq)
        }
    }
    if(length(spans) > 0) {
        filter.list <- vector("list", length(spans))
        for (i in 1:length(spans)) {
            m <- floor(spans[i]/2)
            spans[i] <- 2 * m + 1
            filter.list[[i]] <-
                if (m > 0) c(0.5, rep(1, 2 * m - 1), 0.5)/(2 * m) else 1
        }
        filter <- filter.list[[1]]
        if (length(spans) > 1)
            for (i in 2:length(spans)) filter <- convolve(filter.list[[i]],
                                                          filter, type="open")
        if (length(filter) > 1) {
            ndiff <- nrow(pgram) - length(filter)
            m <- floor(length(filter)/2)
            if (ndiff < 0)
                stop("filter too long!")
            else for (i in 1:ncol(x)) for (j in 1:ncol(x)) {
                pgram[, i, j] <- convolve(pgram[, i, j],
                                          c(filter[(m + 1):(2 * m + 1)],
                                            rep(0, ndiff), filter[1:m]))
            }
        }
        df <- 2/(sum(filter^2) * u4/u2^2)
        m <- floor(length(filter)/2)
        bandwidth <- sqrt(sum((1/12 + (-m:m)^2) * filter)) * xfreq/N
    } else if(!is.null(kernel)) {
        for (i in 1:ncol(x)) for (j in 1:ncol(x))
                pgram[, i, j] <- apply.kernel(pgram[, i, j], kernel,
                                              circular = TRUE)
        df <- df.kernel(kernel)/(u4/u2^2)
        bandwidth <- bandwidth.kernel(kernel) * xfreq/N
    } else {
        df <- 2/(u4/u2^2)
        bandwidth <- sqrt(1/12) * xfreq/N
    }
    spec <- matrix(NA, nrow = Nspec, ncol = nser)
    for (i in 1:nser) spec[, i] <- Re(pgram[1:Nspec, i, i])
    if (nser == 1) {
        coh <- phase <- NULL
    } else {
        coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser * (nser - 1)/2)
        for (i in 1:(nser - 1)) {
            for (j in (i + 1):nser) {
                coh[, i + (j - 1) * (j - 2)/2] <-
                    Mod(pgram[1:Nspec, i, j])^2/(spec[, i] * spec[, j])
                phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[1:Nspec, i, j])
            }
        }
    }
    ## correct for tapering
    for (i in 1:nser) spec[, i] <- spec[, i]/u2
    spec <- drop(spec)
    spg.out <-
        list(freq = freq, spec = spec, coh = coh, phase = phase,
             kernel = kernel, df = df,
             bandwidth = bandwidth, n.used = nrow(x),
             series = series,
             method = ifelse(!is.null(kernel), "Smoothed Periodogram",
                             "Raw Periodogram"),
             taper = taper, pad = pad, detrend = detrend, demean = demean)
    class(spg.out) <- "spec"
    if(plot) {
	plot(spg.out, ...)
        return(invisible(spg.out))
    } else return(spg.out)
}

plot.spec <-
    function (x, add = FALSE, ci = 0.95, log = TRUE,
              xlab = "frequency",
              ylab = if(log) "spectrum (dB)" else "spectrum",
              type = "l", main = NULL, sub = NULL, ...)
{
    if(log) x$spec <- 10 * log10(x$spec)
    matplot(x$freq, x$spec, xlab = xlab, ylab = ylab, type = type,
        add = add, ...)
    is.ar <- !is.na(pmatch("AR", x$method))
    if (ci <= 0 || add || !log || is.ar) {
        #No confidence limits
        ci.text <- ""
    }
    else {
        # The position of the error bar has no meaning: only the width
        # and height. It is positioned in the top right hand corner.
        #
        conf.lim <- spec.ci(x, coverage = ci)
        conf.y <- max(x$spec) - conf.lim[2]
        conf.x <- max(x$freq) - x$bandwidth
        lines(rep(conf.x, 2), conf.y + conf.lim)
        lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y,
            2))
        ci.text <- paste("95% C.I. is (", paste(format(conf.lim,
            digits = 3), collapse = ","), ")dB")
    }
    if (is.null(main))
        main <- paste(paste("Series:", x$series), x$method, sep = "\n")
    if (is.null(sub) && !is.ar)
        sub <- paste("bandwidth=", format(x$bandwidth, digits = 3), ci.text)
    title(main = main, sub = sub)
    invisible(x)
}

spec.ci <- function (spec.obj, coverage = 0.95)
{
    # A utility function for plot.spec which calculates the confidence
    # interval (centred around zero). We use a conditional argument to
    # ensure that the ci always contains zero.
    #
    if (coverage < 0 || coverage >= 1)
        stop("coverage probability out of range [0,1)")
    df <- spec.obj$df
    limits <- numeric(2)
    upper.quantile <- 1 - (1 - coverage) * (1 - pchisq(df, df))
    lower.quantile <- (1 - coverage) * pchisq(df, df)
    -10 * log10(qchisq(c(upper.quantile, lower.quantile), df)/df)
}
