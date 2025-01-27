% File src/library/grDevices/man/axisTicks.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2024 R Core Team
% Distributed under GPL 2 or later

\name{axisTicks}
\alias{axisTicks}
\alias{.axisPars}
\title{Compute Pretty Axis Tick Scales}
\description{
  Compute pretty axis scales and tick mark locations, the same way as
  traditional \R graphics do it.  This is interesting particularly for
  log scale axes.
}
\usage{
axisTicks(usr, log, axp = NULL, nint = 5)
.axisPars(usr, log = FALSE,  nintLog = 5)
}
\arguments{
  \item{usr}{numeric vector of length 2, with \code{c(min, max)} axis
    extents.  Corresponds to traditional \pkg{graphics}'
    \code{\link[graphics]{par}("usr")}, and hence for \code{log = TRUE}
    needs to use \code{\link{log10}()} transformed coordinates.}
  \item{log}{logical indicating if a log scale is (thought to be) in
    use.}
  \item{axp}{numeric vector of length 3, \code{c(mi, ma, n.)}, with
    identical meaning to \code{\link{par}("?axp")} (where \code{?} is
    \code{x} or \code{y}), namely \dQuote{pretty} axis extents, and an
    integer \emph{code} \code{n.}.
    %% FIXME: Explain meaning -- different for log and "not-log"
  }
  \item{nint, nintLog}{positive integer value indicating
    (\emph{approximately}) the desired number of intervals.
    \code{nintLog} is used \bold{only} for the case \code{log = TRUE}.}
}
\details{
  \code{axisTicks(usr, *)} calls \code{.axisPars(usr, ..)} to set
  \code{axp} when that is missing or \code{NULL}.

  Apart from that, \code{axisTicks()} just calls the C function
  \code{CreateAtVector()} in \file{\var{R}/src/main/plot.c} which is also
  called by the base \pkg{graphics} package function
  \code{\link[graphics]{axis}(side, *)} when its argument \code{at} is not
  specified.

  Since \R 4.1.0, the underlying C \code{CreateAtVector()} has been tuned
  to provide a considerably more balanced (symmetric) set of tick locations.
}
\value{
  \code{axisTicks()} returns a numeric vector of potential axis tick
  locations in case of \code{log=TRUE} in the original scale, of length
  approximately \code{nint+1}.

  \code{.axisPars()} returns a \code{\link{list}} with components
  \item{axp}{numeric vector of length 2, \code{c(min., max.)}, of pretty
    axis extents.}
  \item{n}{integer (code), with the same meaning as
    \code{\link{par}("?axp")[3]}.}
}
\seealso{
  \code{\link[graphics]{axTicks}},
  \code{\link[graphics]{axis}}, and \code{\link[graphics]{par}} all from
  the \pkg{graphics} package.
}
\examples{
##--- Demonstrating correspondence between graphics'
##--- axis() and the graphics-engine agnostic  axisTicks() :

require("graphics")
plot(10*(0:10)); (pu <- par("usr"))
aX <- function(side, at, ...)
    axis(side, at = at, labels = FALSE, lwd.ticks = 2, col.ticks = 2,
         tck = 0.05, ...)
aX(1, print(xa <- axisTicks(pu[1:2], log = FALSE)))  # x axis
aX(2, print(ya <- axisTicks(pu[3:4], log = FALSE)))  # y axis

axisTicks(pu[3:4], log = FALSE, nint = 10)


## --------------------  Log Scale  --------------------------------
x <- c(10, 1000)
#         _____
axisTicks(log10(x), log = TRUE) #  10 20 50 .... 1000

plot(10*(0:10), log = "y"); (pu <- par("usr")) # ... ...  0.96 2.04
aX(2, print(ya <- axisTicks(pu[3:4], log = TRUE)))  # 10 20 50 100 (y axis)

plot(2^(0:9), log = "y"); (pu <- par("usr"))
aX(2, print(ya <- axisTicks(pu[3:4], log = TRUE)))  # y axis
## 'usr' corresponds to log10(<range>) :
stopifnot(ya == axisTicks(log10(c(1, 512)), log=TRUE))
}
\keyword{dplot}
