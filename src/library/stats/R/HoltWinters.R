HoltWinters <-
function (x,

          # smoothing parameters
          alpha    = NULL, # level
          beta     = NULL, # trend
          gamma    = NULL, # seasonal component
          seasonal = c("additive", "multiplicative"),
          start.periods = 3,

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

    if(!is.null(alpha) && alpha==0)
        stop ("cannot fit models without level ('alpha' must not be 0).")
    if(!all(is.null(c(alpha, beta, gamma))) &&
        any(c(alpha, beta, gamma) < 0 || c(alpha, beta, gamma) > 1))
        stop ("'alpha', 'beta' and 'gamma' must be within the unit interval.")
    if((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x <= 0))
            stop ("data must be strictly non-negative for multiplicative Holt-Winters")
        if (start.periods < 3)
            stop ("need at least 3 periods to compute seasonal start values")
    }

    ## initialization
    if(!is.null(gamma) && gamma == 0) {
        ## non-seasonal Holt-Winters
        expsmooth <- !is.null(beta) && (beta == 0)
        if(is.null(l.start))
            l.start <- if(expsmooth) x[1] else x[2]
        if(is.null(b.start))
            if(is.null(beta) || beta > 0) b.start <- x[2] - x[1]
        start.time <- 3 - expsmooth
        s.start    <- 0
    } else {
        ## seasonal Holt-Winters
        start.time <- f + 1
        wind       <- start.periods * f

        ## decompose series
        st <- decompose(ts(x[1:wind], start = start(x), frequency = f),
                        seasonal)

        ## level & intercept
        dat <- na.omit(st$trend)
        m   <- lm(dat ~ c(1:length(dat)))

        if (is.null(l.start)) l.start <- as.vector(coef(m)[1])
        if (is.null(b.start)) b.start <- as.vector(coef(m)[2])
        if (is.null(s.start)) s.start <- st$figure
    }

    ## Call to filtering loop
    len <- length(x) - start.time + 1
    hw <- function(alpha, beta, gamma)
        .C("HoltWinters",
           as.double(x),
           as.integer(length(x)),
           as.double(alpha),
           as.double(beta),
           as.double(gamma),
           as.integer(start.time),
           as.integer(! + (seasonal == "multiplicative")),
           as.integer(f),
           a = as.double(l.start),
           b = as.double(b.start),
           s = as.double(s.start),

	   ## return values
           SSE = as.double(0),
           level = double(len + 1),
           trend = double(len + 1),
           seasonal = double(len + f),

	   PACKAGE = "stats"
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
          error <- function (p) hw(p[1], p[2], p[3])$SSE
          sol   <- optim(optim.start, error, method = "L-BFGS-B",
                         lower = c(0, 0, 0), upper = c(1, 1, 1),
                         control = optim.control)
          alpha <- sol$par[1]
          beta  <- sol$par[2]
          gamma <- sol$par[3]
        } else {
        ## !optimize beta
          ## --> optimize alpha and gamma
          error <- function (p) hw(p[1], beta, p[2])$SSE
          sol   <- optim(c(optim.start["alpha"], optim.start["gamma"]),
                         error, method = "L-BFGS-B",
                         lower = c(0, 0), upper = c(1, 1),
                         control = optim.control)
          alpha <- sol$par[1]
          gamma <- sol$par[2]
        }
      } else {
      ## !optimize alpha
        if (is.null(beta)) {
        ## optimize beta
          ## --> optimize beta and gamma
          error <- function (p) hw(alpha, p[1], p[2])$SSE
          sol   <- optim(c(optim.start["beta"], optim.start["gamma"]),
                         error, method = "L-BFGS-B",
                         lower = c(0, 0), upper = c(1, 1),
                         control = optim.control)
          beta  <- sol$par[1]
          gamma <- sol$par[2]
        } else {
        ## !optimize beta
          ## --> optimize gamma
          error <- function (p) hw(alpha, beta, p)$SSE
          sol   <- optim(optim.start["gamma"],
                         error, method = "L-BFGS-B",
                         lower = 0, upper = 1,
                         control = optim.control)
          gamma <- sol$par
        }
      }
    } else {
    ## !optimize gamma
      if (is.null(alpha)) {
      ## optimize alpha
        if (is.null(beta)) {
        ## optimize beta
          ## --> optimize alpha and beta
          error <- function (p) hw(p[1], p[2], gamma)$SSE
          sol   <- optim(c(optim.start["alpha"], optim.start["beta"]),
                         error, method = "L-BFGS-B",
                         lower = c(0, 0), upper = c(1, 1),
                         control = optim.control)
          alpha <- sol$par[1]
          beta  <- sol$par[2]
        } else {
        ## !optimize beta
          ## --> optimize alpha
          error <- function (p) hw(p, beta, gamma)$SSE
          sol   <- optim(optim.start["alpha"], error, method = "L-BFGS-B",
                         lower = 0, upper = 1, control = optim.control)
          alpha <- sol$par
        }
      } else {
      ## !optimize alpha
        if(is.null(beta)) {
        ## optimize beta
          ## --> optimize beta
          error <- function (p) hw(alpha, p, gamma)$SSE
          sol   <- optim(optim.start["beta"], error, method = "L-BFGS-B",
                         lower = 0, upper = 1, control = optim.control)
          beta  <- sol$par
        } ## else optimize nothing!
      }
    }

    ## get (final) results
    final.fit <- hw(alpha, beta, gamma)

    ## return fitted values and estimated coefficients along with parameters used
    fitted <- ts(cbind(xhat   = final.fit$level[-len-1],
                       level  = final.fit$level[-len-1],
                       trend  = if (beta > 0) final.fit$trend[-len-1],
                       season = if (gamma > 0) final.fit$seasonal[1:len]),
                 start = start(lag(x, k = 1 - start.time)),
                 freq  = frequency(x)
                 )
    if (beta > 0) fitted[,1] <- fitted[,1] + fitted[,"trend"]
    if (gamma > 0)
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
                                    b = if (beta > 0) final.fit$trend[len + 1],
                                    s = if (gamma > 0) final.fit$season[len + 1:f]),
                   seasonal  = seasonal,
                   SSE       = final.fit$SSE,
                   call      = match.call()
                   ),
              class = "HoltWinters"
              )
}

## Predictions, optionally with prediction intervals
predict.HoltWinters <-
    function (object, n.ahead = 1, prediction.interval = FALSE,
              level = 0.95, ...)
{
    f <- frequency(object$x)

    vars <- function(h) {
        psi <- function(j)
            object$alpha * (1 + j * object$beta) +
                (j %% f == 0) * object$gamma * (1 - object$alpha)
        var(residuals(object)) * if (object$seasonal == "additive")
            sum(1, (h > 1) * sapply(1:(h-1), function(j) crossprod(psi(j))))
        else {
            rel <- 1 + (h - 1) %% f
            sum(sapply(0:(h-1), function(j) crossprod (psi(j) * object$coefficients[2 + rel] / object$coefficients[2 + (rel - j) %% f])))
        }
    }

    ## compute predictions
    # level
    fit <- rep(as.vector(object$coefficients[1]),n.ahead)
    # trend
    if (object$beta > 0)
        fit <- fit + as.vector((1:n.ahead)*object$coefficients[2])
        # seasonal component
    if (object$gamma > 0)
        if (object$seasonal == "additive")
            fit <- fit + rep(object$coefficients[-(1:(1+(object$beta>0)))],
                             length.out=length(fit))
        else
            fit <- fit * rep(object$coefficients[-(1:(1+(object$beta>0)))],
                             length.out=length(fit))

    ## compute prediction intervals
    if (prediction.interval)
      int <- qnorm((1 + level) / 2) * sqrt(sapply(1:n.ahead,vars))
    ts(
       cbind(fit = fit,
             upr = if(prediction.interval) fit + int,
             lwr = if(prediction.interval) fit - int
             ),
       start = end(lag(fitted(object)[,1], k = -1)),
       freq  = frequency(fitted(object)[,1])
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
    cat ("Holt-Winters exponential smoothing",
         if (x$beta == 0) "without" else "with", "trend and",
         if (x$gamma == 0) "without" else
         paste(if (x$beta==0) "with ", x$seasonal, sep=""),
         "seasonal component.\n")
    cat ("\nCall:\n", deparse (x$call), "\n\n")
    cat ("Smoothing parameters:\n")
    cat (" alpha: ", x$alpha, "\n")
    cat (" beta : ", x$beta, "\n")
    cat (" gamma: ", x$gamma, "\n\n")

    cat ("Coefficients:\n")
    print(t(t(x$coefficients)))
}

# decompose additive/multiplicative series into trend/seasonal figures/noise
decompose <- function (x, type = c("additive", "multiplicative"), filter = NULL)
{
    type <- match.arg(type)
    l <- length(x)
    f <- frequency(x)
    if (f <= 1 || length(na.omit(x)) < 3 * f)
      stop ("time series has no or less than 3 periods")

    ## filter out seasonal components
    if (is.null(filter))
      filter <- if (!f %% 2)
        c(0.5, rep(1, f - 1), 0.5) / f
      else
        rep(1, f) / f

    trend <- filter(x, filter)

    ## compute seasonal components
    season <- if (type == "additive") x - trend else x / trend

    ## remove incomplete seasons at beginning/end
    season <- window(season, start(x) + c(1, 0), c(end(x)[1] - 1, f))

    ## average seasonal figures
    periods <- l %/% f
    index <- c(0, cumsum(rep (f, periods - 3)))
    figure <- numeric(f)
    for (i in 1:f) figure[i] <- mean(season[index + i])

    ## normalize figure
    figure <- if (type == "additive") figure - mean(figure)
    else figure / mean(figure)

    ## return values
    seasonal <- ts(rep(figure, periods), start = start(x), frequency = f)

    structure(
              list(seasonal = seasonal,
                   trend    = trend,
                   random   = if (type == "additive") 
                      x - seasonal - trend
                   else 
                      x / seasonal / trend,
                   figure   = figure,
                   type     = type
                   ),
              class = "decomposed.ts"
              )
}

plot.decomposed.ts <- function(x, ...)
{
    plot(cbind(
               observed = x$random +
               if (x$type == "additive") x$trend + x$seasonal
               else x$trend * x$seasonal,
               trend    = x$trend,
               seasonal = x$seasonal,
               random   = x$random
               ),
         main = paste("Decomposition of", x$type, "time series"),
         ...)
}

