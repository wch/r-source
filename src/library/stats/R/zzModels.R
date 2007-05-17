###
###       Individual selfStarting nonlinear regression models
###
### Copyright 1997, 1999 Jose C. Pinheiro <jcp$research.bell-labs.com>,
###                      Douglas M. Bates <bates$stat.wisc.edu>
###
### This file is part of the nls library for R and related languages
### and was taken from the nlme library for S.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
### Boston, MA 02110-1301, USA

##*## SSasymp - asymptotic regression model

SSasymp <- # selfStart(~ Asym + (R0 - Asym) * exp(-exp(lrc) * input),
    selfStart(function(input, Asym, R0, lrc)
          {
              .expr1 <- R0 - Asym
              .expr2 <- exp(lrc)
              .expr5 <- exp((( - .expr2) * input))
              .value <- Asym + (.expr1 * .expr5)
              .actualArgs <- as.list(match.call()[c("Asym", "R0", "lrc")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3),
                                 list(NULL, c("Asym", "R0", "lrc")))
                  .grad[, "Asym"] <- 1 - .expr5
                  .grad[, "R0"] <- .expr5
                  .grad[, "lrc"] <-  -(.expr1*(.expr5*(.expr2*input)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit an asymptotic regression model")
              }
              if(nrow(xy) > 3) {
                  xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
                  xy <- data.frame(xy)
                  lrc <- log( - coef(lm(log(ydiff) ~ x, data = xy))[2])
                  names(lrc) <- NULL
                  ## This gives an estimate of the log (rate constant).  Use that
                  ## with a partially linear nls algorithm
                  pars <- coef(nls(y ~ cbind(1 - exp( - exp(lrc) * x),
                                             exp(- exp(lrc) * x)),
                                   data = xy,
                                   start = list(lrc = lrc),
                                   algorithm = "plinear"))
              }
              else {
                  ydiff <- diff(xy$y)
                  if(prod(ydiff) <= 0) {
                      stop("cannot fit an asymptotic regression model to these data")
                  }
                  avg.resp <- xy$y
                  frac <- (avg.resp[3] - avg.resp[1])/(avg.resp[2] - avg.resp[1])
                  xunique <- unique(xy$x)
                  xdiff <- diff(xunique)
                  if(xdiff[1] == xdiff[2]) {	# equal spacing - can use a shortcut
                      expmRd <- frac - 1
                      rc <-  - log(expmRd)/xdiff[1]
                      lrc <- log(rc)
                      expmRx1 <- exp( - rc * xunique[1])
                      bma <- ydiff[1]/(expmRx1 * (expmRd - 1))
                      Asym <- avg.resp[1] - bma * expmRx1
                      pars <- c(lrc = lrc, Asym = Asym, R0 = bma + Asym)
                  }
                  else {
                      stop("too few observations to fit an asymptotic regression model")
                  }
              }
              names(pars) <- NULL
              val <- list(pars[2], pars[3], pars[1])
              names(val) <- mCall[c("Asym", "R0", "lrc")]
              val
          },
              parameters = c("Asym", "R0", "lrc"))

##*## SSasympOff - alternate formulation of asymptotic regression model
##*## with an offset

SSasympOff <- # selfStart(~ Asym *( 1 - exp(-exp(lrc) * (input - c0) ) ),
    selfStart(
              function(input, Asym, lrc, c0)
          {
              .expr1 <- exp(lrc)
              .expr3 <- input - c0
              .expr5 <- exp((( - .expr1) * .expr3))
              .expr6 <- 1 - .expr5
              .value <- Asym * .expr6
              .actualArgs <- as.list(match.call()[c("Asym", "lrc", "c0")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3), list(NULL, c("Asym", "lrc", "c0")))
                  .grad[, "Asym"] <- .expr6
                  .grad[, "lrc"] <- Asym * (.expr5 * (.expr1 * .expr3))
                  .grad[, "c0"] <-  - (Asym * (.expr5 * .expr1))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 4) {
                  stop("too few distinct input values to fit the 'asympOff' model")
              }
              xy$ydiff <- abs(xy$y - NLSstRtAsymptote(xy))
              xy <- data.frame(xy)
              lrc <- log( - coef(lm(log(ydiff) ~ x, data = xy))[2]) # log( rate constant)
              pars <- as.vector(coef(nls(y ~ cbind(1, exp(- exp(lrc) * x)),
                                         data = xy, algorithm = "plinear",
                                         start = list(lrc = lrc))))
              val <- list(pars[2], pars[1], exp(-pars[1]) * log(-pars[3]/pars[2]))
              names(val) <- mCall[c("Asym", "lrc", "c0")]
              val
          }, parameters = c("Asym", "lrc", "c0"))

##*## SSasympOrig - exponential curve through the origin to an asymptote

SSasympOrig <- # selfStart(~ Asym * (1 - exp(-exp(lrc) * input)),
    selfStart(
              function(input, Asym, lrc)
          {
              .expr1 <- exp(lrc)
              .expr4 <- exp((( - .expr1) * input))
              .expr5 <- 1 - .expr4
              .value <- Asym * .expr5
              .actualArgs <- as.list(match.call()[c("Asym", "lrc")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 2), list(NULL, c("Asym", "lrc")))
                  .grad[, "Asym"] <- .expr5
                  .grad[, "lrc"] <- Asym * (.expr4 * (.expr1 * input))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit the 'asympOrig' model")
              }
              ## get a preliminary estimate for A
              A0 <- NLSstRtAsymptote(xy)
              ## get a least squares estimate for log of the rate constant
              lrc <- log(abs(mean(log(1 - xy$y/A0)/xy$x, na.rm = TRUE)))
              ## use the partially linear form to converge quickly
              xy <- data.frame(xy)
              pars <- as.vector(coef(nls(y ~ 1 - exp(-exp(lrc)*x),
                                         data = xy,
                                         start = list(lrc = lrc),
                                         algorithm = "plinear")))
              value <- c(pars[2], pars[1])
              names(value) <- mCall[c("Asym", "lrc")]
              value
          }, parameters = c("Asym", "lrc"))

##*## SSbiexp - linear combination of two exponentials

SSbiexp <-
                                        #  selfStart(~ A1 * exp(-exp(lrc1)*input) + A2 * exp(-exp(lrc2) * input),
    selfStart(
              function(input, A1, lrc1, A2, lrc2)
          {
              .expr1 <- exp(lrc1)
              .expr4 <- exp((( - .expr1) * input))
              .expr6 <- exp(lrc2)
              .expr9 <- exp((( - .expr6) * input))
              .value <- (A1 * .expr4) + (A2 * .expr9)
              .actualArgs <- as.list(match.call()[c("A1", "lrc1", "A2", "lrc2")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4),
                                 list(NULL, c("A1", "lrc1", "A2", "lrc2")))
                  .grad[, "A1"] <- .expr4
                  .grad[, "lrc1"] <-  - (A1 * (.expr4 * (.expr1 * input)))
                  .grad[, "A2"] <- .expr9
                  .grad[, "lrc2"] <-  - (A2 * (.expr9 * (.expr6 * input)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit a biexponential")
              }
              ndistinct <- nrow(xy)
              nlast <- max(3, round(ndistinct/2))		# take at least half the data
              dlast <- xy[(ndistinct + 1 - nlast):ndistinct, ]
              pars2 <- coef(lm(log(y) ~ x, data = dlast))
              lrc2 <- log(abs(pars2[2]))		# log of the slope
              xy[["res"]] <- xy[["y"]] - exp(pars2[1]) * exp(-exp(lrc2)*xy[["x"]])
              dfirst <- xy[1:(ndistinct - nlast), ]
              pars1 <- coef(lm(log(abs(res)) ~ x, data = dfirst))
              lrc1 <- log(abs(pars1[2]))
              pars <- coef(nls(y ~ cbind(exp(-exp(lrc1)*x), exp(-exp(lrc2)*x)),
                               data = xy,
                               start = list(lrc1 = lrc1, lrc2 = lrc2),
                               algorithm = "plinear"))
              value <- c(pars[3], pars[1], pars[4], pars[2])
              names(value) <- mCall[c("A1", "lrc1", "A2", "lrc2")]
              value
          }, parameters = c("A1", "lrc1", "A2", "lrc2"))

##*## SSfol - first order compartment model with the log of the rates
##*##         and the clearence
SSfol <-
                                        #  selfStart(~Dose * exp(lKe + lKa - lCl) * (exp(-exp(lKe) * input) -
                                        # exp(-exp(lKa) * input))/(exp(lKa) - exp(lKe)),
    selfStart(
              function(Dose, input, lKe, lKa, lCl)
          {
              .expr4 <- Dose * (exp(((lKe + lKa) - lCl)))
              .expr5 <- exp(lKe)
              .expr8 <- exp((( - .expr5) * input))
              .expr9 <- exp(lKa)
              .expr12 <- exp((( - .expr9) * input))
              .expr14 <- .expr4 * (.expr8 - .expr12)
              .expr15 <- .expr9 - .expr5
              .expr16 <- .expr14/.expr15
              .expr23 <- .expr15^2
              .value <- .expr16
              .actualArgs <- as.list(match.call()[c("lKe", "lKa", "lCl")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3), list(NULL, c("lKe", "lKa", "lCl")))
                  .grad[, "lKe"] <- ((.expr14 - (.expr4 * (.expr8 * (.expr5 * input))))/
                                     .expr15) + ((.expr14 * .expr5)/.expr23)
                  .grad[, "lKa"] <- ((.expr14 + (.expr4 * (.expr12 * (.expr9 * input))))/
                                     .expr15) - ((.expr14 * .expr9)/.expr23)
                  .grad[, "lCl"] <-  - .expr16
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              data <- data.frame(data)
              resp <- eval(LHS, data)
              input <- eval(mCall[["input"]], data)
              Dose <- eval(mCall[["Dose"]], data)
              n <- length(resp)
              if(length(input) != n) {
                  stop("must have length of response = length of second argument to 'SSfol'")
              }
              if(n < 4) {
                  stop("must have at least 4 observations to fit an 'SSfol' model")
              }
              rmaxind <- order(resp)[n]

              lresp <- log(resp)
              if(rmaxind == n) {
                  lKe <- -2.5
              } else {
                  lKe <- log((lresp[rmaxind] - lresp[n])/(input[n] - input[rmaxind]))
              }
              cond.lin <- nls(resp ~ (exp(-input * exp(lKe))-exp(-input * exp(lKa))) * Dose,
                              data = list(resp = resp, input = input, Dose = Dose, lKe = lKe),
                              start = list(lKa = lKe + 1),
                              algorithm = "plinear")
              pars <- coef(cond.lin)
              names(pars) <- NULL
              cond.lin <- nls(resp ~ (Dose * (exp(-input*exp(lKe))-
                                              exp(-input*exp(lKa))))/(exp(lKa) - exp(lKe)),
                              data = data.frame(list(resp = resp, input = input,
                              Dose = Dose)),
                              start = list(lKa = pars[1],lKe = lKe),
                              algorithm = "plinear")
              pars <- coef(cond.lin)
              names(pars) <- NULL
              lKa <- pars[1]
              lKe <- pars[2]
              Ka <- exp(lKa)
              Ke <- exp(lKe)
              value <- list(lKe, lKa, log((Ke * Ka)/(pars[3])))
              names(value) <- as.character(mCall)[4:6]
              value
          }, parameters = c("lKe", "lKa", "lCl"))

##*## SSfpl - four parameter logistic model

SSfpl <- # selfStart(~ A + (B - A)/(1 + exp((xmid - input)/scal)),
    selfStart(
              function(input, A, B, xmid, scal)
          {
              .expr1 <- B - A
              .expr2 <- xmid - input
              .expr4 <- exp((.expr2/scal))
              .expr5 <- 1 + .expr4
              .expr8 <- 1/.expr5
              .expr13 <- .expr5^2
              .value <- A + (.expr1/.expr5)
              .actualArgs <- as.list(match.call()[c("A", "B", "xmid", "scal")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4),
                                 list(NULL, c("A", "B", "xmid", "scal")))
                  .grad[, "A"] <- 1 - .expr8
                  .grad[, "B"] <- .expr8
                  .grad[, "xmid"] <-  - ((.expr1 * (.expr4 * (1/ scal)))/.expr13)
                  .grad[, "scal"] <- (.expr1 * (.expr4 * (.expr2/(scal^2))))/.expr13
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["input"]], LHS, data)
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit a four-parameter logistic")
              }
              ## convert the response to a proportion (i.e. contained in (0,1))
              rng <- range(xy$y); drng <- diff(rng)
              xy$prop <- (xy$y - rng[1] + 0.05 * drng)/(1.1 * drng)
              ## inverse regression of the x values on the proportion
              ir <- as.vector(coef(lm(x ~ I(log(prop/(1-prop))), data = xy)))
              pars <- as.vector(coef(nls(y ~ cbind(1, 1/(1 + exp((xmid - x)/
                                                                 exp(lscal)))),
                                         data = xy,
                                         start = list(xmid = ir[1],
                                                      lscal = log(abs(ir[2]))),
                                         algorithm = "plinear")))
              value <- c(pars[3], pars[3] + pars[4], pars[1], exp(pars[2]))
              names(value) <- mCall[c("A", "B", "xmid", "scal")]
              value
          }, parameters = c("A", "B", "xmid", "scal"))

##*## SSlogis - logistic model for nonlinear regression

SSlogis <- # selfStart(~ Asym/(1 + exp((xmid - input)/scal)),
    selfStart(
              function(input, Asym, xmid, scal)
          {
              .expr1 <- xmid - input
              .expr3 <- exp((.expr1/scal))
              .expr4 <- 1 + .expr3
              .expr10 <- .expr4^2
              .value <- Asym/.expr4
              .actualArgs <- as.list(match.call()[c("Asym", "xmid", "scal")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3), list(NULL, c("Asym", "xmid", "scal")))
                  .grad[, "Asym"] <- 1/.expr4
                  .grad[, "xmid"] <-  - ((Asym * (.expr3 * (1/scal)))/.expr10)
                  .grad[, "scal"] <- (Asym * (.expr3 * (.expr1/(scal^2))))/.expr10
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if(nrow(xy) < 4) {
                  stop("too few distinct input values to fit a logistic model")
              }
              z <- xy[["y"]]
              if (min(z) <= 0) { z <- z - 1.05 * min(z) } # avoid zeroes
              z <- z/(1.05 * max(z))		# scale to within unit height
              xy[["z"]] <- log(z/(1 - z))		# logit transformation
              aux <- coef(lm(x ~ z, xy))
              pars <- as.vector(coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)),
                                         data = xy,
                                         start = list(xmid = aux[1], scal = aux[2]),
                                         algorithm = "plinear")))
              value <- c(pars[3], pars[1], pars[2])
              names(value) <- mCall[c("Asym", "xmid", "scal")]
              value
          }, parameters = c("Asym", "xmid", "scal"))

##*## SSmicmen - Michaelis-Menten model for enzyme kinetics.

SSmicmen <- # selfStart(~ Vm * input/(K + input),
    selfStart(
              function(input, Vm, K)
          {
              .expr1 <- Vm * input
              .expr2 <- K + input
              .value <- .expr1/.expr2
              .actualArgs <- as.list(match.call()[c("Vm", "K")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 2), list(NULL, c("Vm", "K")))
                  .grad[, "Vm"] <- input/.expr2
                  .grad[, "K"] <-  - (.expr1/(.expr2^2))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if (nrow(xy) < 3) {
                  stop("too few distinct input values to fit a Michaelis-Menten model")
              }
              ## take the inverse transformation
              pars <- as.vector(coef(lm(1/y ~ I(1/x), data = xy)))
              ## use the partially linear form to converge quickly
              pars <- as.vector(coef(nls(y ~ x/(K + x),
                                         data = xy,
                                         start = list(K = abs(pars[2]/pars[1])),
                                         algorithm = "plinear")))
              value <- c(pars[2], pars[1])
              names(value) <- mCall[c("Vm", "K")]
              value
          }, parameters = c("Vm", "K"))

SSgompertz <- #    selfStart( ~ Asym * exp(-b2*b3^x),
    ## Gompertz model for growth curve data
    selfStart(function(x, Asym, b2, b3)
          {
              .expr2 <- b3^x
              .expr4 <- exp(-b2 * .expr2)
              .value <- Asym * .expr4
              .actualArgs <- as.list(match.call()[c("Asym", "b2", "b3")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 3),
                                 list(NULL, c("Asym", "b2", "b3")))
                  .grad[, "Asym"] <- .expr4
                  .grad[, "b2"] <- -Asym * (.expr4 * .expr2)
                  .grad[, "b3"] <- -Asym * (.expr4 * (b2 * (b3^(x - 1) * x)))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["x"]], LHS, data)
              if (nrow(xy) < 4) {
                  stop("too few distinct input values to fit the Gompertz model")
              }
              xyL <- xy
              xyL$y <- log(abs(xyL$y))
              pars <- NLSstAsymptotic(xyL)
              pars <- coef(nls(y ~ exp(-b2*b3^x),
                               data = xy,
                               algorithm = "plinear",
                               start = c(b2 = pars[["b1"]],
                               b3 = exp(-exp(pars[["lrc"]])))))
              val <- pars[c(3,1,2)]
              names(val) <- mCall[c("Asym", "b2", "b3")]
              val
          },
              c("Asym", "b2", "b3"))

SSweibull <- # selfStart( ~ Asym - Drop * exp(-exp(lrc)*x^pwr),
    ## Weibull model for growth curve data
    selfStart( function(x, Asym, Drop, lrc, pwr)
          {
              .expr1 <- exp(lrc)
              .expr3 <- x^pwr
              .expr5 <- exp(-.expr1 * .expr3)
              .value <- Asym - Drop * .expr5
              .actualArgs <- as.list(match.call()[c("Asym", "Drop", "lrc", "pwr")])
              if(all(unlist(lapply(.actualArgs, is.name))))
              {
                  .grad <- array(0, c(length(.value), 4),
                                 list(NULL, c("Asym", "Drop", "lrc", "pwr")))
                  .grad[, "Asym"] <- 1
                  .grad[, "Drop"] <- -.expr5
                  .grad[, "lrc"] <- Drop * (.expr5 * (.expr1 * .expr3))
                  .grad[, "pwr"] <- Drop * (.expr5 * (.expr1 * (.expr3 * log(x))))
                  dimnames(.grad) <- list(NULL, .actualArgs)
                  attr(.value, "gradient") <- .grad
              }
              .value
          },
              function(mCall, data, LHS)
          {
              xy <- sortedXyData(mCall[["x"]], LHS, data)
              if (nrow(xy) < 5) {
                  stop("too few distinct input values to fit the Weibull growth model")
              }
              if (any(xy[["x"]] < 0)) {
                  stop("all 'x' values must be non-negative to fit the Weibull growth model")
              }
              Rasym <- NLSstRtAsymptote(xy)
              Lasym <- NLSstLfAsymptote(xy)
              pars <- coef(lm(log(-log((Rasym - y)/(Rasym - Lasym))) ~ log(x),
                             data = xy, subset = x > 0))
              val <- coef(nls(y ~ cbind(1, -exp(-exp(lrc)*x^pwr)),
                               data = xy,
                               algorithm = "plinear",
                               start = c(lrc = pars[[1]], pwr = pars[[2]])))[
                                                         c(3,4,1,2)]
              names(val) <- mCall[c("Asym", "Drop", "lrc", "pwr")]
              val
          },
              c("Asym", "Drop", "lrc", "pwr"))


### Local variables:
### mode: S
### End:
