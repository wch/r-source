### $Id: splines.R,v 1.2 1999/11/20 03:35:40 bates Exp $

bs <-
  function(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
           Boundary.knots = range(x))
{
  nx <- names(x)
  x <- as.vector(x)
  nax <- is.na(x)
  if(nas <- any(nax))
    x <- x[!nax]
  if(!missing(Boundary.knots)) {
    Boundary.knots <- sort(Boundary.knots)
    outside <- (ol <- x < Boundary.knots[1]) | (or <- x > Boundary.knots[2])
  }
  else outside <- FALSE #rep(FALSE, length = length(x))

  ord <- 1 + (degree <- as.integer(degree))
  if(ord <= 1) stop("degree must be integer >= 1")
  if(!missing(df) && missing(knots)) {
    nIknots <- df - ord + (1 - intercept)
    if(nIknots < 0) {
      nIknots <- 0
      warning(paste("df was too small; have used ", ord - (1 - intercept)))
    }
    knots <-
    if(nIknots > 0) {
      knots <- seq(from = 0, to = 1, length = nIknots + 2)[-c(1, nIknots + 2)]
        quantile(x[!outside], knots)
    }
  }
  Aknots <- sort(c(rep(Boundary.knots, ord), knots))
  if(any(outside)) {
    warning("Some x values beyond boundary knots may cause ill-conditioned bases")
    derivs <- 0:degree
    scalef <- gamma(1:ord)# factorials
    basis <- array(0, c(length(x), length(Aknots) - degree - 1))
    if(any(ol)) {
      k.pivot <- Boundary.knots[1]
      xl <- cbind(1, outer(x[ol] - k.pivot, 1:degree, "^"))
      tt <- spline.des(Aknots, rep(k.pivot, ord), ord, derivs)$design
      basis[ol,  ] <- xl %*% (tt/scalef)
    }
    if(any(or)) {
      k.pivot <- Boundary.knots[2]
      xr <- cbind(1, outer(x[or] - k.pivot, 1:degree, "^"))
      tt <- spline.des(Aknots, rep(k.pivot, ord), ord, derivs)$design
      basis[or,  ] <- xr %*% (tt/scalef)
    }
    if(any(inside <- !outside))
      basis[inside,  ] <- spline.des(Aknots, x[inside], ord)$design
  }
  else basis <- spline.des(Aknots, x, ord)$design
  if(!intercept)
    basis <- basis[, -1 , drop = FALSE]
  n.col <- ncol(basis)
  if(nas) {
    nmat <- matrix(NA, length(nax), n.col)
    nmat[!nax,  ] <- basis
    basis <- nmat
  }
  dimnames(basis) <- list(nx, 1:n.col)
  a <- list(degree = degree, knots = knots, Boundary.knots = 
            Boundary.knots, intercept = intercept, class = c("bs", "basis"))
  attributes(basis) <- c(attributes(basis), a)
  basis
}

ns <-
  function(x, df = NULL, knots = NULL, intercept = FALSE,
           Boundary.knots = range(x))
{
  nx <- names(x)
  x <- as.vector(x)
  nax <- is.na(x)
  if(nas <- any(nax))
    x <- x[!nax]
  if(!missing(Boundary.knots)) {
    Boundary.knots <- sort(Boundary.knots)
    outside <- (ol <- x < Boundary.knots[1]) | (or <- x > Boundary.knots[2])
  }
  else outside <- FALSE # rep(FALSE, length = length(x))
  if(!missing(df) && missing(knots)) {
    ## df = number(interior knots) + 1 + intercept
    nIknots <- df - 1 - intercept
    if(nIknots < 0) {
      nIknots <- 0
      warning(paste("df was too small; have used ", 1 + intercept))
    }
    if(nIknots > 0) {
      knots <- seq(from = 0, to = 1, length = nIknots + 2)[-c(1, nIknots + 2)]
      knots <- quantile(x[!outside], knots)
    }
    else knots <- NULL
  } else nIknots <- length(knots)
  Aknots <- sort(c(rep(Boundary.knots, 4), knots))
  if(any(outside)) {
    basis <- array(0, c(length(x), nIknots + 4))
    if(any(ol)) {
      k.pivot <- Boundary.knots[1]
      xl <- cbind(1, x[ol] - k.pivot)
      tt <- spline.des(Aknots, rep(k.pivot, 2), 4, c(0, 1))$design
      basis[ol,  ] <- xl %*% tt
    }
    if(any(or)) {
      k.pivot <- Boundary.knots[2]
      xr <- cbind(1, x[or] - k.pivot)
      tt <- spline.des(Aknots, rep(k.pivot, 2), 4, c(0, 1))$design
      basis[or,  ] <- xr %*% tt
    }
    if(any(inside <- !outside))
      basis[inside,  ] <- spline.des(Aknots, x[inside], 4)$design
  }
  else basis <- spline.des(Aknots, x, 4)$design
  const <- spline.des(Aknots, Boundary.knots, 4, c(2, 2))$design
  if(!intercept) {
    const <- const[, -1 , drop = FALSE]
    basis <- basis[, -1 , drop = FALSE]
  }
  qr.const <- qr(t(const))
  basis <- as.matrix((t(qr.qty(qr.const, t(basis))))[,  - (1:2)])
  n.col <- ncol(basis)
  if(nas) {
    nmat <- matrix(NA, length(nax), n.col)
    nmat[!nax,  ] <- basis
    basis <- nmat
  }
  dimnames(basis) <- list(nx, 1:n.col)
  a <- list(degree = 4, knots = knots, Boundary.knots = Boundary.knots, 
            intercept = intercept, class = c("ns", "basis"))
  attributes(basis) <- c(attributes(basis), a)
  basis
}

predict.bs <-
  function(object, newx, ...)
{
  if(missing(newx))
    return(object)
  a <- c(list(x = newx), attributes(object)[
                c("degree", "knots", "Boundary.knots", "intercept")])
  do.call("bs", a)
}

predict.ns <-
  function(object, newx, ...)
{
  if(missing(newx))
    return(object)
  a <- c(list(x = newx), attributes(object)[
                c("knots", "Boundary.knots", "intercept")])
  do.call("ns", a)
}

# interp.spline <-
#   function(x, y, ord = 4)
# {
#   x <- as.numeric(x)
#   lenx <- length(x)
#   y <- as.numeric(y)
#   leny <- length(y)
#   if(leny!=lenx)
#     stop("Lengths of x and y must be equal")
#   ind <- order(x)
#   x <- x[ind]
#   y <- y[ind]
#   ordm1 <- ord - 1
#   knots <- c(x[1:ordm1] + x[1] - x[ord], x,
#              x[lenx + (1:ordm1) - ordm1] + x[lenx] - x[lenx - ordm1])
#   derivs <- c(2, integer(lenx), 2)
#   x <- c(x[1], x, x[lenx])
#   sys.mat <- spline.des(knots, x, ord = ord, derivs = derivs)$design
#   coeff <- solve(sys.mat, c(0, y, 0))
#   list(knots = knots, coeff = coeff, order = ord)
# }

# periodic.spline <-
#   function(x, y, knots, period = 2 * pi, ord = 4)
# {
#   x <- as.vector(x, "double")
#   y <- as.vector(y, "double")
#   lenx <- length(x)
#   if(lenx!=length(y))
#     stop("Lengths of x and y must match")
#   ind <- order(x)
#   x <- x[ind]
#   y <- y[ind]
#   if(any((x[-1] - x[ - lenx]) <= 0))
#     stop("Values of x must be strictly increasing")
#   if(!missing(knots))
#     period <- knots[length(knots) + 1 - ord] - knots[1]
#   else knots <- c(x[(lenx - (ord - 2)):lenx] - period, x, x[1:ord] + 
#                   period)
#   if((x[lenx] - x[1]) >= period)
#     stop("The range of x values exceeds one period")
#   coeff.mat <- spline.des(knots, x, ord)$design
#   sys.mat <- coeff.mat[, (1:lenx)]
#   sys.mat[, 1:(ord - 1)] <-
#     sys.mat[, 1:(ord - 1)] + coeff.mat[, lenx + (1:(ord - 1))]
#   qrstr <- qr(sys.mat)
#   coeff <- qr.coef(qrstr, y)
#   coeff <- c(coeff, coeff[1:(ord - 1)])
#   list(knots = knots, coeff = coeff, order = ord,
#        period = period, m = coeff.mat)
# }

# spline.value <-
#   function(bspstr, x = seq(knots[ord], knots[ncoeff + 1], len = lenx),
#            deriv = 0, lenx = length(x))
# {
#   if(is.null(bspstr$knots))
#     stop("bspstr must be a B-spline structure")
#   if(missing(x) && missing(lenx))
#     stop("either x or lenx must be given to spline.value")
#   knots <- bspstr$knots
#   ord <- bspstr$order
#   ncoeff <- length(bspstr$coeff)
#   x.orig <- x
#   if(!is.null(period <- bspstr$period)) {
#     ind <- x < knots[ord]
#     if(any(ind))
#       x[ind] <- x[ind] + period * (1 + (knots[ord] - x[ind]) %/% period)
#     ind <- x > knots[ncoeff + 1]
#     if(any(ind))
#       x[ind] <- x[ind] - period * (1 + (x[ind] - knots[ncoeff + 1]) %/% period)
#   }
#   xind <- order(x)
#   x <- x[xind]
#   rind <- order(xind)
#   z <- .C("spline_value",
#           as.double(bspstr$knots),
#           as.double(bspstr$coeff),
#           as.integer(length(bspstr$coeff)),
#           as.integer(bspstr$order),
#           as.double(x),
#           as.integer(lenx),
#           as.integer(deriv),
#           y = double(lenx))
#   list(x = x.orig, y = z$y[rind])
# }

spline.des <- function(knots, x, ord = 4, derivs = integer(nx))
{
  ## "Design matrix" for a collection of B-splines.  `The' basic function.
  knots <- sort(as.vector(knots))
  x <- as.vector(x)
  nk <- length(knots)
  nx <- length(x)
  ind <- order(x)
  sortx <- x[ind]
  ind <- order(ind)
  if(sortx[1] < knots[ord] || sortx[nx] > knots[nk + 1 - ord])
    stop(paste("The x data must be in the range", knots[ord], "to",
               knots[nk + 1 - ord]))
  if(length(derivs)!=nx)
    stop("length of derivs must match length of x")
  ncoef <- nk - ord
  temp <- .C("spline_basis",
             as.double(knots),
             as.integer(ncoef),
             as.integer(ord),
             as.double(sortx),
             as.integer(derivs),
             as.integer(nx),
             design = array(0, c(ord, nx)),
             offsets = integer(nx))
  design <- array(0, c(nx, ncoef))
  d.ind <- array(c(rep(1:nx, rep(ord, nx)),
                   outer(1:ord, temp$offsets, "+")), c(nx * ord, 2))
  design[d.ind] <- temp$design
  list(knots = knots, order = ord, derivs = derivs, design = design[ind,  ])
}

# linear.interp <-
#   function(x, y, x0 = seq(x[1], x[lenx], len = lenx0), lenx0 = length(x0))
# {
#   x <- as.numeric(x)
#   y <- as.numeric(y)
#   lenx <- length(x)
#   if(length(y)!=lenx)
#     stop("lengths of x and y must be the same")
#   xind <- order(x)
#   x <- x[xind]
#   y <- y[xind]
#   if(missing(x0) && missing(lenx0))
#     stop("either x0 or lenx0 must be given")
#   x0 <- as.numeric(x0)
#   x0ind <- order(x0)
#   x0ord <- x0[x0ind]
#   nvals <- length(x0)
#   if(x0ord[1] < x[1] || x0ord[nvals] > x[lenx])
#     stop(paste("x0 values must be in the range", x[1], "to", x[lenx]))
#   y <- .C("lin_interp",
#           x,
#           y,
#           x0,
#           y = double(nvals),
#           as.integer(nvals))$y
#   list(x = x0, y = y[order(x0ind)])
# }
