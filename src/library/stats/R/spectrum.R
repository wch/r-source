#  File src/library/stats/R/spectrum.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#  Copyright (C) 1999-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## based on code by Martyn Plummer, plus kernel code by Adrian Trapletti
spectrum <- function (x, ..., method = c("pgram", "ar"))
{
    switch(match.arg(method),
	   pgram = spec.pgram(x, ...),
	   ar	 = spec.ar(x, ...)
	   )
}

## spec.taper based on code by Kurt Hornik
spec.taper <- function (x, p = 0.1)
{
    if (any(p < 0) || any(p > 0.5))
        stop("'p' must be between 0 and 0.5")
    a <- attributes(x)
    x <- as.matrix(x)
    nc <- ncol(x)
    if (length(p) == 1L)
        p <- rep(p, nc)
    else if (length(p) != nc)
        stop("length of 'p' must be 1 or equal the number of columns of 'x'")
    nr <- nrow(x)
    for (i in 1L:nc) {
        m <- floor(nr * p[i])
        if(m == 0) next
        w <- 0.5 * (1 - cos(pi * seq.int(1, 2 * m - 1, by = 2)/(2 * m)))
        x[, i] <- c(w, rep_len(1, nr - 2 * m), rev(w)) * x[, i]
    }
    attributes(x) <- a
    x
}

spec.ar <- function(x, n.freq, order = NULL, plot = TRUE,
                    na.action = na.fail, method = "yule-walker", ...)
{
    ## can be called with a ts or a result of an AR fit.
    if(!is.list(x)) {
        series <- deparse(substitute(x))
        x <- na.action(as.ts(x))
        xfreq <- frequency(x)
        nser <- NCOL(x)
        x <- ar(x, is.null(order), order, na.action=na.action, method=method)
    } else { ## result of ar()
        cn <- match(c("ar", "var.pred", "order"), names(x))
        if(anyNA(cn))
            stop("'x' must be a time series or an ar() fit")
        series <- x$series
        xfreq <- x$frequency
        if(is.array(x$ar)) nser <- dim(x$ar)[2L] else nser <- 1
    }
    order <- x$order
    if(missing(n.freq)) n.freq <- 500
    freq <- seq.int(0, 0.5, length.out = n.freq)
    if (nser == 1) {
        coh <- phase <- NULL
        var.p <- as.vector(x$var.pred)
        spec <-
            if(order >= 1) {
                cs <- outer(freq, 1L:order, function(x, y) cos(2*pi*x*y)) %*% x$ar
                sn <- outer(freq, 1L:order, function(x, y) sin(2*pi*x*y)) %*% x$ar
                var.p/(xfreq*((1 - cs)^2 + sn^2))
            } else rep.int(var.p/xfreq, length(freq))
    } else .NotYetImplemented()
    spg.out <- list(freq = freq*xfreq, spec = spec, coh = coh, phase = phase,
                    n.used = nrow(x), series = series,
                    method = paste0("AR (", order, ") spectrum ")
                    )
    class(spg.out) <- "spec"
    if(plot) {
	plot(spg.out, ci = 0, ...)
        invisible(spg.out)
    } else spg.out
}

spec.pgram <-
    function (x, spans = NULL, kernel = NULL, taper = 0.1,
              pad = 0, fast = TRUE,
              demean = FALSE, detrend = TRUE,
              plot = TRUE, na.action = na.fail, ...)
{
    ## Estimate spectral density from (smoothed) periodogram.
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    xfreq <- frequency(x)
    x <- as.matrix(x)
    N <- N0 <- nrow(x)
    nser <- ncol(x)
    if(!is.null(spans)) # allow user to mistake order of args
        kernel <- {
            if(is.tskernel(spans)) spans else
            kernel("modified.daniell", spans %/% 2)
        }
    if(!is.null(kernel) && !is.tskernel(kernel))
        stop("must specify 'spans' or a valid kernel")
    if (detrend) {
        t <- 1L:N - (N + 1)/2
        sumt2 <- N * (N^2 - 1)/12
        for (i in 1L:ncol(x))
            x[, i] <- x[, i] - mean(x[, i]) - sum(x[, i] * t) * t/sumt2
    }
    else if (demean) {
	x <- sweep(x, 2, colMeans(x), check.margin=FALSE)
    }
    ## apply taper:
    x <- spec.taper(x, taper)
    ## to correct for tapering: Bloomfield (1976, p. 194)
    ## Total taper is taper*2
    u2 <- (1 - (5/8)*taper*2)
    u4 <- (1 - (93/128)*taper*2)
    if (pad > 0) {
        x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
        N <- nrow(x)
    }
    NewN <- if(fast) nextn(N) else N
    x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
    N <- nrow(x)
    Nspec <- floor(N/2)
    freq <- seq.int(from = xfreq/N, by = xfreq/N, length.out = Nspec)
    xfft <- mvfft(x)
    pgram <- array(NA, dim = c(N, ncol(x), ncol(x)))
    for (i in 1L:ncol(x)) {
        for (j in 1L:ncol(x)) { # N0 = #{non-0-padded}
            pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N0*xfreq)
            ## value at zero is invalid as mean has been removed, so interpolate:
            pgram[1, i, j] <- 0.5*(pgram[2, i, j] + pgram[N, i, j])
        }
    }
    if(!is.null(kernel)) {
	for (i in 1L:ncol(x)) for (j in 1L:ncol(x))
	    pgram[, i, j] <- kernapply(pgram[, i, j], kernel, circular = TRUE)
	df <- df.kernel(kernel)
	bandwidth <- bandwidth.kernel(kernel)
    } else { # raw periodogram
	df <- 2
	bandwidth <- sqrt(1/12)
    }
    df <- df/(u4/u2^2)
    df <- df  * (N0 / N) ## << since R 1.9.0
    bandwidth <- bandwidth * xfreq/N
    pgram <- pgram[2:(Nspec+1),,, drop=FALSE]
    spec <- matrix(NA, nrow = Nspec, ncol = nser)
    for (i in 1L:nser) spec[, i] <- Re(pgram[1L:Nspec, i, i])
    if (nser == 1) {
        coh <- phase <- NULL
    } else {
        coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser * (nser - 1)/2)
        for (i in 1L:(nser - 1)) {
            for (j in (i + 1):nser) {
                coh[, i + (j - 1) * (j - 2)/2] <-
                    Mod(pgram[, i, j])^2/(spec[, i] * spec[, j])
                phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[, i, j])
            }
        }
    }
    ## correct for tapering
    for (i in 1L:nser) spec[, i] <- spec[, i]/u2
    spec <- drop(spec)
    spg.out <-
        list(freq = freq, spec = spec, coh = coh, phase = phase,
             kernel = kernel, df = df,
             bandwidth = bandwidth, n.used = N, orig.n = N0,# "n.orig" = "n..."
             series = series, snames = colnames(x),
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
    function (x, add = FALSE, ci = 0.95, log = c("yes", "dB", "no"),
              xlab = "frequency", ylab = NULL,
              type = "l", ci.col = "blue", ci.lty = 3,
              main = NULL, sub = NULL,
              plot.type = c("marginal", "coherency", "phase"), ...)
{
    spec.ci <- function (spec.obj, coverage = 0.95)
    {
        ## A utility function for plot.spec which calculates the confidence
        ## interval (centred around zero). We use a conditional argument to
        ## ensure that the ci always contains zero.

        if (coverage < 0 || coverage >= 1)
            stop("coverage probability out of range [0,1)")
        tail <- (1 - coverage)
        df <- spec.obj$df
        upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
        lower.quantile <- tail * pchisq(df, df)
        1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
    }

    plot.type <- match.arg(plot.type)
    log <- match.arg(log)
    m <- match.call()
    if(plot.type == "coherency") {
        ## need stats:: for non-standard evaluation
        m[[1L]] <- quote(stats::plot.spec.coherency)
        m$plot.type <- m$log <- m$add <- NULL
        return(eval(m, parent.frame()))
    }
    if(plot.type == "phase") {
        ## need stats:: for non-standard evaluation
        m[[1L]] <- quote(stats::plot.spec.phase)
        m$plot.type <- m$log <- m$add <- NULL
        return(eval(m, parent.frame()))
    }
    if(is.null(ylab))
        ylab <- if(log == "dB") "spectrum (dB)" else "spectrum"
    if(is.logical(log))
        log <- if(log) "yes" else "no"
    if(missing(log) && getOption("ts.S.compat")) log <- "dB"
    log <- match.arg(log)
    ylog <- ""
    if(log=="dB") x$spec <- 10 * log10(x$spec)
    if(log=="yes") ylog <- "y"
    dev.hold(); on.exit(dev.flush())
    if(add) {
        matplot(x$freq, x$spec, type = type, add=TRUE, ...)
    } else {
        matplot(x$freq, x$spec, xlab = xlab, ylab = ylab, type = type,
                log = ylog, ...)
        if (ci <= 0 || !is.numeric(x$df) || log == "no") {
            ## No confidence limits
            ci.text <- ""
        } else {
            ## The position of the error bar has no meaning: only the width
            ## and height. It is positioned in the top right hand corner.
            ##
            conf.lim <- spec.ci(x, coverage = ci)
            if(log=="dB") {
                conf.lim <- 10*log10(conf.lim)
                conf.y <- max(x$spec) - conf.lim[2L]
                conf.x <- max(x$freq) - x$bandwidth
                lines(rep(conf.x, 2), conf.y + conf.lim, col=ci.col)
                lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y, 2),
                      col=ci.col)
                ci.text <- paste0(", ", round(100*ci, 2),  "% C.I. is (",
                                  paste(format(conf.lim, digits = 3),
                                        collapse = ","),
                                  ")dB")
            } else {
                ci.text <- ""
                conf.y <- max(x$spec) / conf.lim[2L]
                conf.x <- max(x$freq) - x$bandwidth
                lines(rep(conf.x, 2), conf.y * conf.lim, col=ci.col)
                lines(conf.x + c(-0.5, 0.5) * x$bandwidth, rep(conf.y, 2),
                      col=ci.col)
            }
        }
        if (is.null(main))
            main <- paste(if(!is.null(x$series)) paste("Series:", x$series)
                          else "from specified model",
                          x$method, sep = "\n")
        if (is.null(sub) && is.numeric(x$bandwidth))
             sub <- paste0("bandwidth = ", format(x$bandwidth, digits = 3),
                           ci.text)
        title(main = main, sub = sub)
    }
    invisible(x)
}

## based on code in Venables & Ripley
plot.spec.coherency <-
    function(x, ci = 0.95,
             xlab = "frequency", ylab = "squared coherency", ylim=c(0,1),
             type = "l", main = NULL, ci.col="blue",  ci.lty = 3, ...)
{
    nser <- NCOL(x$spec)
    ## Formulae from Bloomfield (1976, p.225)
    gg <- 2/x$df
    se <- sqrt(gg/2)
    z <- -qnorm((1-ci)/2)
    if (is.null(main))
        main <- paste(paste("Series:", x$series),
                      "Squared Coherency", sep = " --  ")
    if(nser == 2) {
        plot(x$freq, x$coh, type=type, xlab=xlab, ylab=ylab, ylim=ylim, ...)
        coh <- pmin(0.99999, sqrt(x$coh))
        lines(x$freq, (tanh(atanh(coh) + z*se))^2, lty=ci.lty, col=ci.col)
        lines(x$freq, (pmax(0, tanh(atanh(coh) - z*se)))^2,
              lty=ci.lty, col=ci.col)
        title(main)
    } else {
        dev.hold(); on.exit(dev.flush())
        opar <- par(mfrow = c(nser-1, nser-1), mar = c(1.5, 1.5, 0.5, 0.5),
                    oma = c(4, 4, 6, 4))
        on.exit(par(opar), add = TRUE)
        plot.new()
        for (j in 2:nser) for (i in 1L:(j-1)) {
            par(mfg=c(j-1,i, nser-1, nser-1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$coh[, ind], type=type, ylim=ylim, axes=FALSE,
                 xlab="", ylab="", ...)
            coh <- pmin(0.99999, sqrt(x$coh[, ind]))
            lines(x$freq, (tanh(atanh(coh) + z*se))^2, lty=ci.lty, col=ci.col)
            lines(x$freq, (pmax(0, tanh(atanh(coh) - z*se)))^2,
                  lty=ci.lty, col=ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab=x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab=x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5,
                  cex = par("cex.main"), font = par("font.main"))
        }
    }
    invisible()
}

plot.spec.phase <-
    function(x, ci = 0.95,
             xlab = "frequency", ylab = "phase", ylim=c(-pi, pi),
             type = "l", main = NULL, ci.col = "blue", ci.lty = 3, ...)
{
    nser <- NCOL(x$spec)
    ## Formulae from Bloomfield (1976, p.225)
    gg <- 2/x$df
    if (is.null(main))
        main <- paste(paste("Series:", x$series),
                      "Phase spectrum", sep = "  -- ")
    if(nser == 2) {
        plot(x$freq, x$phase, type=type, xlab=xlab, ylab=ylab, ylim=ylim, ...)
        coh <- sqrt(x$coh)
        cl <- asin( pmin( 0.9999, qt(ci, 2/gg-2)*
                         sqrt(gg*(coh^{-2} - 1)/(2*(1-gg)) ) ) )
        lines(x$freq, x$phase + cl, lty=ci.lty, col=ci.col)
        lines(x$freq, x$phase - cl, lty=ci.lty, col=ci.col)
        title(main)
    } else {
        dev.hold(); on.exit(dev.flush())
        opar <- par(mfrow = c(nser-1, nser-1), mar = c(1.5, 1.5, 0.5, 0.5),
                    oma = c(4, 4, 6, 4))
        on.exit(par(opar), add = TRUE)
        plot.new()
        for (j in 2:nser) for (i in 1L:(j-1)) {
            par(mfg=c(j-1,i, nser-1, nser-1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$phase[, ind], type=type, ylim=ylim, axes=FALSE,
                 xlab="", ylab="", ...)
            coh <- sqrt(x$coh[, ind])
            cl <- asin( pmin( 0.9999, qt(ci, 2/gg-2)*
                             sqrt(gg*(coh^{-2} - 1)/(2*(1-gg)) ) ) )
            lines(x$freq, x$phase[, ind] + cl, lty=ci.lty, col=ci.col)
            lines(x$freq, x$phase[, ind] - cl, lty=ci.lty, col=ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab=x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab=x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5,
                  cex = par("cex.main"), font = par("font.main"))
        }
    }
    invisible()
}
