# file modreg/R/smspline.R
# copyright (C) 1998 B. D. Ripley
#
smooth.spline <-
  function(x, y, w = rep(1, length(x)), df = 5, spar = 0, cv = FALSE,
           all.knots = FALSE, df.offset = 0, penalty = 1)
{
  sknotl <- function(x)
  {
    a1 <- log(50, 2)
    a2 <- log(100, 2)
    a3 <- log(140, 2)
    a4 <- log(200, 2)
    n <- length(x)
    if(n < 50) nk <- n
    else if (n >= 50  && n < 200) nk <- 2^(a1+(a2-a1)*(n-50)/150) 
    else if (n >= 200 && n < 800) nk <- 2^(a2+(a3-a2)*(n-200)/600)
    else if (n >= 800 && n < 3200)nk <-  2^(a3+(a4-a3)*(n-800)/2400)
    else if (n >= 3200) nk <- 200 + (n-3200)^0.2
    c(rep(x[1], 3), x[seq(1,n,len=trunc(nk))], rep(x[n], 3))
  }
  if(missing(y)) {
    if(is.list(x)) {
      if(any(is.na(match(c("x", "y"), names(x)))))
        stop("cannot find x and y in list")
      y <- x$y
      x <- x$x
    } else if(is.matrix(x) && ncol(x) == 2) {
      y <- x[, 2]
      x <- x[, 1]
    } else {
      y <- x
      x <- time(x)
    }
  }
  n <- length(x)
  if(n != length(y)) stop("lengths of x and y must match")
  if(missing(w)) w <- rep(1, n)
  else {
    if(length(x) != length(w)) stop("lengths of x and w must match")
    if(any(w < 0)) stop("all weights should be non-negative")
    w <- (w * sum(w > 0))/sum(w)
  }
  if(missing(spar)) ispar <- 0
  else if(spar < 1.01e-15) ispar <- 0 else ispar <- 1
  if(cv)  icrit <- 2 else  icrit <- 1
  dfinfo <- df.offset
  if(!missing(df)) {
    if(df > 1 & df < n) {
      icrit <- 3
      dfinfo <- df
    } else warning("you must supply 1 < df < n")
  }
  n <- as.integer(n)
  x <- signif(x, 6)
  ux <- unique(sort(x))
  ox <- match(x, ux)
  tmp <- matrix(unlist(tapply(seq(along=y), ox,
                              function(x,y,w) c(mean(y[x]), sum(w[x])),
                              y = y, w = w)), , 2, byrow=T)
  ybar <- tmp[, 1]
  wbar <- tmp[, 2]
  nx <- length(ux)
  xbar <- (ux - ux[1])/(ux[nx] - ux[1])
  if(all.knots) {
    knot <- c(rep(xbar[1], 3), xbar, rep(xbar[nx], 3))
    nk <- nx + 2
  } else {
    knot <- sknotl(xbar)
    nk <- length(knot) - 4
  }
  low.parm <- 0
  high.parm <- 1.5
  fit <- .Fortran("qsbart",
                  as.double(penalty),
                  as.double(dfinfo),
                  x = as.double(xbar),
                  y = as.double(ybar),
                  w = as.double(wbar),
                  as.integer(nx),
                  as.double(knot),
                  as.integer(nk),
                  coef = double(nk),
                  ty = double(nx),
                  lev = double(nx),
                  crit = double(1),
                  as.integer(c(icrit, ispar)),
                  spar = as.double(spar),
                  as.double(c(0, 1.5, 0.001)),
                  as.integer(0),
                  double((17 + nk) * nk),
                  as.integer(4),
                  as.integer(1),
                  ier = as.integer(1))
  if(fit$ier > 0) {
    warning("smoothing parameter value too small or too large")
    fit$ty <- rep(mean(y), nx)
  }
  lev <- fit$lev
  df <- sum(lev)
  if(cv) {
    ww <- wbar
    ww[!(ww > 0)] <- 1
    cv.crit <- weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
  } else cv.crit <- weighted.mean((y - fit$ty[ox])^2, w)/
    (1 - (df.offset + penalty * df)/sum(wbar))^2
  pen.crit <- sum(wbar * (ybar - fit$ty) * ybar)
  fit.object <- list(knot = knot, nk = nk, min = ux[1],
                     range = ux[nx] - ux[1], coef = fit$coef)
  class(fit.object) <- "smooth.spline.fit"
  object <- list(x = ux, y = fit$ty, w = wbar, yin = ybar, 
                 lev = lev, cv.crit = cv.crit, pen.crit = pen.crit, df = df, 
                 spar = fit$spar, fit = fit.object, call = match.call())
  class(object) <- "smooth.spline"
  object
}

predict.smooth.spline <- function(object, x, deriv = 0)
{
  if(missing(x)) return(object[c("x", "y")])
  fit <- object$fit
  if(is.null(fit)) stop("not a valid smooth.spline object")
  else predict(fit, x, deriv)
}

print.smooth.spline <- function(x, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
  }
  if(is.null(cv <- cl$cv)) cv <- F
  cat("\nSmoothing Parameter (Spar):", format(x$spar), "\n")
  cat("Equivalent Degrees of Freedom (Df):", format(x$df), "\n")
  cat("Penalized Criterion:", format(x$pen.crit), "\n")
  crss <- if(cv) "PRESS:" else "GCV:"
  cat(crss, format(x$cv.crit), "\n")
  invisible(x)
}

predict.smooth.spline.fit <- function(object, x, deriv = 0)
{
  if(missing(x))
    x <- seq(from = object$min, to = object$min + object$range, 
             length = length(object$coef) - 4)
  xs <- (x - object$min)/object$range
  extrap.left <- xs < 0
  extrap.right <- xs > 1
  interp <- !(extrap.left | extrap.right)
  y <- xs
  if(any(interp))
     y[interp] <- .Fortran("bvalus",
                           as.integer(sum(interp)),
                           as.double(object$knot),
                           as.double(object$coef),
                           as.integer(object$nk),
                           as.double(xs[interp]),
                           s = double(sum(interp)),
                           as.integer(deriv))$s
  if(any(!interp)) {
    xrange <- c(object$min, object$min + object$range)
    if(deriv == 0) {
      end.object <- Recall(object, xrange)$y
      end.slopes <- Recall(object, xrange, 1)$y * object$range
      if(any(extrap.left))
        y[extrap.left] <- end.object[1] + end.slopes[1] * (xs[extrap.left] - 0)
      if(any(extrap.right))
        y[extrap.right] <- end.object[2] + end.slopes[2] * (xs[extrap.right] - 1)
    } else if(deriv == 1) {
      end.slopes <- Recall(object, xrange, 1)$y * object$range
      y[extrap.left] <- end.slopes[1]
      y[extrap.right] <- end.slopes[2]
    }
    else y[!interp] <- 0
  }
  if(deriv > 0) y <- y/(object$range^deriv)
  list(x = x, y = y)
}

supsmu <-
  function(x, y, wt = rep(1, length(y)), span = "cv", periodic = F, bass = 0)
{
  if(span == "cv") span <- 0
  n <- length(y)
  if(length(x) != n) stop("number of observations in x and y must match.")
  if(length(wt) != n)
    stop("number of weights must match number of observations.")
  if(span < 0 || span > 1) stop("span must be between 0 and 1.")
  if(periodic) {
    iper <- 2
    xrange <- range(x)
    if(xrange[1] < 0 || xrange[2] > 1)
      stop("x must be between 0 and 1 for periodic smooth")
  } else iper <- 1
  okay <- is.finite(x + y + wt)
  ord <- order(x[okay], y[okay])
  ord <- cumsum(!okay)[okay][ord] + ord
  xo <- x[ord]
  leno <- length(ord)
  if(diff <- n - leno)
    warning(paste(diff, "observation(s) with NAs, NaNs and/or Infs deleted"))
  .Fortran("bdrsetsmu")
  smo <- .Fortran("bdrsupsmu",
                       as.integer(leno),
                       as.double(xo),
                       as.double(y[ord]),
                       as.double(wt[ord]),
                       as.integer(iper),
                       as.double(span),
                       as.double(bass),
                       smo=double(leno),
                       double(n*7), double(1))$smo
  #eliminate duplicate xsort values and corresponding smoothed values
  dupx <- duplicated(xo)
  list(x = xo[!dupx], y = smo[!dupx])
}
