#  File src/library/grDevices/R/convertColor.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/


## easyRGB scales Y=100 for white
## brucelindbloom uses XYZ in [0,1], so multiply by 100 to convert

## white points in xyY format (Y=1 omitted)
white.points <- cbind(A = c(x = 0.44757, y = 0.40745),
                      B = c(x = 0.34842, y = 0.35161),
                      C = c(x = 0.31006, y = 0.31616),
                      D50 = c(x = 0.34574, y = 0.35867),
                      D55 = c(x = 0.33250, y = 0.34761),
                      D65 = c(x = 0.3137, y = 0.3291),
                      E = c(x = 1/3, y = 1/3))
## converting these:
c2to3 <- function(col) c(col[1L]/col[2L], 1, (1 - sum(col))/col[2L])

## http://www.brucelindbloom.com/index.html?Equations.html


make.rgb <-
    function(red, green, blue, name = NULL, white = "D65", gamma = 2.2)
{
    whitexyz <- c2to3(white.points[, white])
    rgb <- rbind(c2to3(red),
                 c2to3(green),
                 c2to3(blue))
    S <- drop(whitexyz %*% solve(rgb))
    M <- S * rgb

    if (is.numeric(gamma) && length(gamma) == 1) {
        dogamma <- function(x) x %^% gamma
        ungamma <- function(x) x %^% (1/gamma)
    } else if (gamma == "sRGB") {
        dogamma <- function(x) ifelse(x < 0.04045,
                                      x/12.92,
                                      ((x+0.055)/1.055)^2.4)
        ungamma <- function(x) ifelse(x <= 0.0031308,
                                      12.92*x,
                                      1.055*x %^% (1/2.4)-0.055)
    } else stop("'gamma' must be a scalar or 'sRGB'")

    toXYZ <- function(rgb,...) { dogamma(rgb) %*% M }
    toRGB <- function(xyz,...) { ungamma(xyz %*% solve(M)) }

    if (is.null(name)) name <- deparse(sys.call())[1L]
    rval <- list(toXYZ = toXYZ, fromXYZ = toRGB, gamma = gamma,
                 reference.white = white, name = name)
    class(rval) <- c("RGBcolorConverter", "colorConverter")
    rval
}

print.colorConverter <- function(x,...) {
    cat(gettextf("Color space converter: %s", x$name), "\n", sep = "")
    if (!is.null(x$reference.white))
        cat(gettextf("Reference white: %s", x$reference.white), "\n", sep = "")
    invisible(x)
}

print.RGBcolorConverter <- function(x,...) {
    print.colorConverter(x, ...)
    if (!is.null(x$gamma))
        cat(gettextf("display gamma = %s", format(x$gamma)), "\n", sep = "")
    invisible(x)
}

chromaticAdaptation <- function(xyz, from, to) {
    ## bradford scaling algorithm
    Ma <- matrix(c( 0.40024, -0.22630, 0.,
                    0.70760,  1.16532, 0.,
                   -0.08081,  0.04570, 0.91822), nrow = 3L, byrow = TRUE)
    nWhite <- colnames(white.points)
    from <- c2to3(white.points[, match.arg(from, nWhite)])
    to   <- c2to3(white.points[, match.arg(to, nWhite)])
    from.cone <- drop(from %*% Ma)
    to.cone   <- drop(to %*% Ma)
    ## M <- Ma %*% diag(to.cone/from.cone) %*% solve(Ma)
    M <- (Ma * rep(to.cone/from.cone, each=3)) %*% solve(Ma)
    xyz %*% M
}


colorConverter <- function(toXYZ, fromXYZ, name, white = NULL) {
    rval <- list(toXYZ = toXYZ, fromXYZ = fromXYZ,
                 name = name, white = white)
    class(rval) <- "colorConverter"
    rval
}

colorspaces <-
    list("XYZ" =
         colorConverter(toXYZ = function(x,w) x,
                        fromXYZ = function(x,w) x,
                        white = NULL,name = "XYZ"),

         "Apple RGB" =
         make.rgb(red = c(0.6250,0.3400),
                  green = c(0.2800,0.5950),
                  blue = c(0.1550,0.0700),gamma = 1.8,
                  white = "D65", name = "Apple RGB"),

         "sRGB" =
         make.rgb(red = c(0.6400, 0.3300),
                  green = c(0.3000,0.6000),
                  blue = c(0.1500,0.0600), gamma = "sRGB",
                  white = "D65", name = "sRGB"),

         "CIE RGB" =
         make.rgb(red = c(0.7350,0.2650),
                  green = c(0.2740,0.7170),
                  blue = c(0.1670,0.0090), gamma = 2.2,
                  white = "E", name = "CIE RGB"),

         "Lab" =
         colorConverter(fromXYZ = function(XYZ, white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             xyzr <- XYZ/white
             fxyz <- ifelse(xyzr <= epsilon, (kappa*xyzr+16)/116, xyzr^(1/3))

             c(L = 116*fxyz[2L]-16,
               a = 500*(fxyz[1L]-fxyz[2L]),
               b = 200*(fxyz[2L]-fxyz[3L]))
         },
         toXYZ = function(Lab,white) {

             epsilon <- 216/24389
             kappa <- 24389/27

             yr <- ifelse(Lab[1L] < kappa*epsilon, Lab[1L]/kappa, ((Lab[1L]+16)/116)^3)
             fy <- ifelse(yr <= epsilon, (kappa*yr+16)/116, (Lab[1L]+16)/116)
             fx <- Lab[2L]/500+fy
             fz <- fy-Lab[3L]/200

             zr <- ifelse(fz^3 <= epsilon, (116*fz-16)/kappa, fz^3)
             xr <- ifelse(fx^3 <= epsilon, (116*fx-16)/kappa, fx^3)

             c(X = xr,Y = yr,Z = zr)*white

         }, name = "Lab", white = NULL),

         "Luv" =
         colorConverter(fromXYZ = function(XYZ, white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             yr <- XYZ[2L]/white[2L]

             denom <- sum(XYZ*c(1,15,3))
             wdenom <- sum(white*c(1,15,3))

             u1 <- ifelse(denom == 0, 1, 4*XYZ[1L]/denom)
             v1 <- ifelse(denom == 0, 1, 9*XYZ[2L]/denom)
             ur <- 4*white[1L]/wdenom
             vr <- 9*white[2L]/wdenom

             L <- ifelse(yr <= epsilon, kappa*yr, 116*(yr^(1/3))-16)
             c(L = L, u = 13*L*(u1-ur), v = 13*L*(v1-vr))
         }, toXYZ = function(Luv,white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             if(Luv[1L] == 0) return(c(0,0,0))

             u0 <- 4*white[1L]/(white[1L]+15*white[2L]+3*white[3L])
             v0 <- 9*white[2L]/(white[1L]+15*white[2L]+3*white[3L])

             Y <- ifelse(Luv[1L] <= kappa*epsilon,
                         Luv[1L]/kappa, ((Luv[1L]+16)/116)^3)
             a <- (52*Luv[1L]/(Luv[2L]+13*Luv[1L]*u0)-1)/3
             b <- -5*Y
             c <- -1/3
             d <- Y*(39*Luv[1L]/(Luv[3L]+13*Luv[1L]*v0)-5)

             X <- (d-b)/(a-c)
             Z <- X*a+b

             c(X = X,Y = Y,Z = Z)
         }, name = "Luv", white = NULL)

         ) # colorspaces


`%^%` <- function(a,b) {
  ifelse(a <= 0, -abs(a)^b, a^b)
}


convertColor <-
    function(color, from, to,
             from.ref.white = NULL, to.ref.white = NULL,
             scale.in = 1, scale.out = 1, clip = TRUE)
{
  if (is.character(from))
      from <- colorspaces[[match.arg(from, names(colorspaces))]]
  if (!inherits(from,"colorConverter"))
      stop("'from' must be a \"colorConverter\" object or a string")
  if (is.character(to))
      to <- colorspaces[[match.arg(to, names(colorspaces))]]
  if (!inherits(to,"colorConverter"))
      stop("'to' must be a \"colorConverter\" object or a string")

  ## Need a reference white. If both the definition and the argument
  ## specify one they must agree.

  if (is.null(from.ref.white))
      from.ref.white <- from$white
  else if (!is.null(from$white) && from.ref.white != from$white)
      stop(gettextf("'from.ref.white' disagrees with definition of %s",
                    from$name), domain = NA)

  if (is.null(to.ref.white))
      to.ref.white <- to$white
  else if (!is.null(to$white) && to.ref.white != to$white)
      stop(gettextf("'to.ref.white' disagrees with definition of %s",
                    to$name), domain = NA)

  if (is.null(to.ref.white) && is.null(from.ref.white))
      to.ref.white <- from.ref.white <- "D65"

  if (is.null(to.ref.white))
      to.ref.white <- from.ref.white
  if (is.null(from.ref.white))
      from.ref.white <- to.ref.white

  from.ref.white <- c2to3(white.points[, from.ref.white])
  to.ref.white   <- c2to3(white.points[, to.ref.white])

  if (is.null(nrow(color)))
    color <- matrix(color, nrow = 1L)

  if (!is.null(scale.in))
      color <- color/scale.in

  trim <- function(rgb) {
      rgb <- round(rgb,5)
      if (is.na(clip))
          rgb[rgb < 0 | rgb > 1] <- NaN
      else if(clip) {
          rgb[rgb < 0] <- 0
          rgb[rgb > 1] <- 1
      }
      rgb
  }

  xyz <- apply(color, 1L, from$toXYZ, from.ref.white)

  if (is.null(nrow(xyz)))
    xyz <- matrix(xyz, nrow = 1L)

  if (!isTRUE(all.equal(from.ref.white, to.ref.white))) {
      mc <- match.call()
      if (is.null(mc$from.ref.white) || is.null(mc$to.ref.white))
          warning("color spaces use different reference whites")
      xyz <- chromaticAdaptation(xyz, from.ref.white, to.ref.white)
  }

  rval <- apply(xyz, 2L, to$fromXYZ, to.ref.white)

  if (inherits(to,"RGBcolorConverter"))
      rval <- trim(rval)

  if (is.matrix(rval))
      rval <- t(rval)

  if (is.null(scale.out))
      rval
  else
      rval*scale.out
}

##' @title Modify a vector of colors by "screwing" any of (r,g,b,alpha)
##'   by multification by a factor
##' @param col vector of colors, in any format that col2rgb() accepts
##' @param alpha.f factor modifying the opacity alpha; typically in [0,1]
##' @param red.f   factor modifying "red"ness
##' @param green.f factor modifying "green"ness
##' @param blue.f  factor modifying "blue"ness
##' @return From rgb(), a color vector of the same length as 'col'.
##' @author Thomas Lumley, Luke Tierney, Martin Maechler, Duncan Murdoch...
adjustcolor <- function(col, alpha.f = 1, red.f = 1, green.f = 1,
                        blue.f = 1, offset = c(0,0,0,0),
                        transform = diag(c(red.f, green.f, blue.f, alpha.f)))
{
    stopifnot(length(offset) %% 4L == 0L,
              !is.null(d <- dim(transform)), d == c(4L, 4L))
    x <- col2rgb(col, alpha = TRUE)/255
    x[] <- pmax(0, pmin(1,
                        transform %*% x +
                        matrix(offset, nrow = 4L, ncol = ncol(x))))
    rgb(x[1L,], x[2L,], x[3L,], x[4L,])
}
