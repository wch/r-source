#  File src/library/stats/R/HoltWinters.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2002-2013 The R Core Team
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

# Originally contributed by David Meyer

HoltWinters <-
function (x,

          # smoothing parameters
          alpha    = NULL, # level
          beta     = NULL, # trend
          gamma    = NULL, # seasonal component
          seasonal = c("additive", "multiplicative"),
          start.periods = 2,

          # starting values
          l.start  = NULL, # level
          b.start  = NULL, # trend
          s.start  = NULL, # seasonal components vector of length `period'

          # starting values for optim
          optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
          optim.control = list()
          )
{
    x <- as.ts(x)
    seasonal <- match.arg(seasonal)
    f <- frequency(x)

    if(!is.null(alpha) && (alpha == 0))
        stop ("cannot fit models without level ('alpha' must not be 0 or FALSE)")
    if(!all(is.null(c(alpha, beta, gamma))) &&
        any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
        stop ("'alpha', 'beta' and 'gamma' must be within the unit interval")
    if((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x == 0))
            stop ("data must be non-zero for multiplicative Holt-Winters")
        if (start.periods < 2)
            stop ("need at least 2 periods to compute seasonal start values")
    }

    ## initialization
    if(!is.null(gamma) && is.logical(gamma) && !gamma) {
        ## non-seasonal Holt-Winters
        expsmooth <- !is.null(beta) && is.logical(beta) && !beta
        if(is.null(l.start))
            l.start <- if(expsmooth) x[1L] else x[2L]
        if(is.null(b.start))
            if(is.null(beta) || !is.logical(beta) || beta)
                b.start <- x[2L] - x[1L]
        start.time <- 3 - expsmooth
        s.start    <- 0
    } else {
        ## seasonal Holt-Winters
        start.time <- f + 1
        wind       <- start.periods * f

        ## decompose series
        st <- decompose(ts(x[1L:wind], start = start(x), frequency = f),
                        seasonal)

        ## level & intercept
        dat <- na.omit(st$trend)
        m   <- lm(dat ~ seq_along(dat))

        if (is.null(l.start)) l.start <- as.vector(coef(m)[1L])
        if (is.null(b.start)) b.start <- as.vector(coef(m)[2L])
        if (is.null(s.start)) s.start <- st$figure
    }

    ## Call to filtering loop
    lenx <- as.integer(length(x))
    if (is.na(lenx)) stop("invalid length(x)")

    len <- lenx - start.time + 1
    hw <- function(alpha, beta, gamma)
        .C(C_HoltWinters,
           as.double(x),
           lenx,
           as.double(max(min(alpha, 1), 0)),
           as.double(max(min(beta, 1), 0)),
           as.double(max(min(gamma, 1), 0)),
           as.integer(start.time),
           ## no idea why this is so: same as seasonal != "multiplicative"
           as.integer(! + (seasonal == "multiplicative")),
           as.integer(f),
           as.integer(!is.logical(beta) || beta),
           as.integer(!is.logical(gamma) || gamma),

           a = as.double(l.start),
           b = as.double(b.start),
           s = as.double(s.start),

	   ## return values
           SSE = as.double(0),
           level = double(len + 1L),
           trend = double(len + 1L),
           seasonal = double(len + f)
           )

    ## if alpha and/or beta and/or gamma are omitted, use optim to find the
    ## values minimizing the squared prediction error
    if (is.null(gamma)) {
        ## optimize gamma
        if (is.null(alpha)) {
            ## optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize alpha, beta, and gamma
                error <- function (p) hw(p[1L], p[2L], p[3L])$SSE
                sol   <- optim(optim.start, error, method = "L-BFGS-B",
                               lower = c(0, 0, 0), upper = c(1, 1, 1),
                               control = optim.control)
                if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
                    if (sol$convergence > 50) {
                        warning(gettextf("optimization difficulties: %s",
                                         sol$message), domain = NA)
                    } else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                beta  <- sol$par[2L]
                gamma <- sol$par[3L]
            } else {
                ## !optimize beta
                ## --> optimize alpha and gamma
                error <- function (p) hw(p[1L], beta, p[2L])$SSE
                sol   <- optim(c(optim.start["alpha"], optim.start["gamma"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
                    if (sol$convergence > 50) {
                        warning(gettextf("optimization difficulties: %s",
                                         sol$message), domain = NA)
                    } else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                gamma <- sol$par[2L]
            }
        } else {
            ## !optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize beta and gamma
                error <- function (p) hw(alpha, p[1L], p[2L])$SSE
                sol   <- optim(c(optim.start["beta"], optim.start["gamma"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
                    if (sol$convergence > 50) {
                        warning(gettextf("optimization difficulties: %s",
                                         sol$message), domain = NA)
                    } else stop("optimization failure")
                }
                beta  <- sol$par[1L]
                gamma <- sol$par[2L]
            } else {
                ## !optimize beta
                ## --> optimize gamma
                error <- function (p) hw(alpha, beta, p)$SSE
                gamma <- optimize(error, lower = 0, upper = 1)$minimum
            }
        }
    } else {
        ## !optimize gamma
        if (is.null(alpha)) {
            ## optimize alpha
            if (is.null(beta)) {
                ## optimize beta
                ## --> optimize alpha and beta
                error <- function (p) hw(p[1L], p[2L], gamma)$SSE
                sol   <- optim(c(optim.start["alpha"], optim.start["beta"]),
                               error, method = "L-BFGS-B",
                               lower = c(0, 0), upper = c(1, 1),
                               control = optim.control)
                if(sol$convergence || any(sol$par < 0 | sol$par > 1)) {
                    if (sol$convergence > 50) {
                        warning(gettextf("optimization difficulties: %s",
                                         sol$message), domain = NA)
                    } else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                beta  <- sol$par[2L]
            } else {
                ## !optimize beta
                ## --> optimize alpha
                error <- function (p) hw(p, beta, gamma)$SSE
                alpha <- optimize(error, lower = 0, upper = 1)$minimum
            }
        } else {
            ## !optimize alpha
            if(is.null(beta)) {
                ## optimize beta
                ## --> optimize beta
                error <- function (p) hw(alpha, p, gamma)$SSE
                beta <- optimize(error, lower = 0, upper = 1)$minimum
            } ## else optimize nothing!
        }
    }

    ## get (final) results
    final.fit <- hw(alpha, beta, gamma)

    ## return fitted values and estimated coefficients along with parameters used
    fitted <- ts(cbind(xhat   = final.fit$level[-len-1],
                       level  = final.fit$level[-len-1],
                       trend  = if (!is.logical(beta) || beta)
                           final.fit$trend[-len-1],
                       season = if (!is.logical(gamma) || gamma)
                           final.fit$seasonal[1L:len]),
                 start = start(lag(x, k = 1 - start.time)),
                 frequency  = frequency(x)
                 )
    if (!is.logical(beta) || beta) fitted[,1] <- fitted[,1] + fitted[,"trend"]
    if (!is.logical(gamma) || gamma)
      fitted[,1] <- if (seasonal == "multiplicative")
        fitted[,1] * fitted[,"season"]
      else
        fitted[,1] + fitted[,"season"]
    structure(list(fitted    = fitted,
                   x         = x,
                   alpha     = alpha,
                   beta      = beta,
                   gamma     = gamma,
                   coefficients = c(a = final.fit$level[len + 1],
                                    b = if (!is.logical(beta) || beta) final.fit$trend[len + 1],
                                    s = if (!is.logical(gamma) || gamma) final.fit$seasonal[len + 1L:f]),
                   seasonal  = seasonal,
                   SSE       = final.fit$SSE,
                   call      = match.call()
                   ),
              class = "HoltWinters"
              )
}

## Predictions, optionally with prediction intervals
predict.HoltWinters <-
    function (object, n.ahead = 1L, prediction.interval = FALSE,
              level = 0.95, ...)
{
    f <- frequency(object$x)

    vars <- function(h) {
        psi <- function(j)
            object$alpha * (1 + j * object$beta) +
                (j %% f == 0) * object$gamma * (1 - object$alpha)
        var(residuals(object)) * if (object$seasonal == "additive")
            sum(1, (h > 1) * sapply(1L:(h-1), function(j) crossprod(psi(j))))
        else {
            rel <- 1 + (h - 1) %% f
            sum(sapply(0:(h-1), function(j) crossprod (psi(j) * object$coefficients[2 + rel] / object$coefficients[2 + (rel - j) %% f])))
        }
    }

    ## compute predictions
    # level
    fit <- rep(as.vector(object$coefficients[1L]) ,n.ahead)
    # trend
    if (!is.logical(object$beta) || object$beta)
        fit <- fit + as.vector((1L:n.ahead)*object$coefficients[2L])
        # seasonal component
    if (!is.logical(object$gamma) || object$gamma)
        if (object$seasonal == "additive")
            fit <- fit + rep(object$coefficients[-(1L:(1+(!is.logical(object$beta) || object$beta)))],
                             length.out=length(fit))
        else
            fit <- fit * rep(object$coefficients[-(1L:(1+(!is.logical(object$beta) || object$beta)))],
                             length.out=length(fit))

    ## compute prediction intervals
    if (prediction.interval)
      int <- qnorm((1 + level) / 2) * sqrt(sapply(1L:n.ahead,vars))
    ts(
       cbind(fit = fit,
             upr = if(prediction.interval) fit + int,
             lwr = if(prediction.interval) fit - int
             ),
       start = end(lag(fitted(object)[,1], k = -1)),
       frequency  = frequency(fitted(object)[,1])
       )
}

residuals.HoltWinters <- function (object, ...) object$x - object$fitted[,1]

plot.HoltWinters <-
    function (x, predicted.values = NA, intervals = TRUE, separator = TRUE,
              col = 1, col.predicted = 2, col.intervals = 4, col.separator = 1,
              lty = 1, lty.predicted = 1, lty.intervals = 1, lty.separator = 3,
              ylab = "Observed / Fitted", main = "Holt-Winters filtering",
              ylim = NULL, ...)
{
    if (is.null(ylim))
      ylim <- range(na.omit(c(fitted(x)[,1], x$x, predicted.values)))

    preds <- length(predicted.values) > 1 || !is.na(predicted.values)

    dev.hold(); on.exit(dev.flush())
    ## plot fitted/predicted values
    plot(ts(c(fitted(x)[,1], if(preds) predicted.values[,1]),
            start = start(fitted(x)[,1]),
            frequency = frequency(fitted(x)[,1])),
         col = col.predicted,
         ylim = ylim,
         ylab = ylab, main = main,
         lty = lty.predicted,
         ...
         )

    ## plot prediction interval
    if(preds && intervals && ncol(predicted.values) > 1) {
        lines(predicted.values[,2], col = col.intervals, lty = lty.intervals)
        lines(predicted.values[,3], col = col.intervals, lty = lty.intervals)
    }

    ## plot observed values
    lines(x$x, col = col, lty = lty)

    ## plot separator
    if (separator && preds)
        abline (v = time(x$x)[length(x$x)], lty = lty.separator, col = col.separator)
}

## print function
print.HoltWinters <- function (x, ...)
{
    cat("Holt-Winters exponential smoothing",
        if (is.logical(x$beta) && !x$beta) "without" else "with", "trend and",
        if (is.logical(x$gamma) && !x$gamma) "without" else
        paste0(if (is.logical(x$beta) && !x$beta) "with ", x$seasonal),
        "seasonal component.")
    cat("\n\nCall:\n", deparse (x$call), "\n\n", sep = "")
    cat("Smoothing parameters:\n")
    cat(" alpha: ", x$alpha, "\n", sep = "")
    cat(" beta : ", x$beta, "\n", sep = "")
    cat(" gamma: ", x$gamma, "\n\n", sep = "")

    cat("Coefficients:\n")
    print(t(t(x$coefficients)))
    invisible(x)
}

# decompose additive/multiplicative series into trend/seasonal figures/noise
decompose <-
function (x, type = c("additive", "multiplicative"), filter = NULL)
{
    type <- match.arg(type)
    l <- length(x)
    f <- frequency(x)
    if (f <= 1 || length(na.omit(x)) < 2 * f)
        stop("time series has no or less than 2 periods")

    ## filter out seasonal components
    if (is.null(filter))
        filter <- if (!f %% 2)
            c(0.5, rep_len(1, f - 1), 0.5) / f
        else
            rep_len(1, f) / f
    trend <- filter(x, filter)

    ## compute seasonal components
    season <- if (type == "additive")
        x - trend
    else
        x / trend

    ## average seasonal figures
    periods <- l %/% f
    index <- seq.int(1L, l, by = f) - 1L
    figure <- numeric(f)
    for (i in 1L:f)
        figure[i] <- mean(season[index + i], na.rm = TRUE)

    ## normalize figure
    figure <- if (type == "additive")
        figure - mean(figure)
    else figure / mean(figure)

    seasonal <- ts(rep(figure, periods+1)[seq_len(l)],
                   start = start(x), frequency = f)

    ## return values
    structure(list(x = x,
                   seasonal = seasonal,
                   trend = trend,
                   random = if (type == "additive")
                       x - seasonal - trend
                   else
                       x / seasonal / trend,
                   figure = figure,
                   type = type),
              class = "decomposed.ts")
}

plot.decomposed.ts <- function(x, ...)
{
    xx <- x$x # added in 2.14.0
    if(is.null(xx))
        xx <- with(x,  if (type == "additive") random + trend + seasonal
                       else random * trend * seasonal)
    plot(cbind(observed = xx,
               trend    = x$trend,
               seasonal = x$seasonal,
               random   = x$random
               ),
         main = paste("Decomposition of", x$type, "time series"),
         ...)
}

