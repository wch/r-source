% File src/library/grDevices/man/axisTicks.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011-2 R Core Team
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
 .axisPars(usr, log = FALSE, nintLog = 5)
}
\arguments{
  \item{usr}{numeric vector of length 2, with \code{c(min, max)} axis
    extents.}
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
  \code{axisTicks(usr, *)} calls \code{.axisPars(usr, ..)} when
  \code{axp} is missing (or \code{NULL}).
% FIXME: say more
}
\value{
  \code{axisTicks()} returns a numeric vector of potential axis tick
  locations, of length approximately \code{nint+1}.

  \code{.axisPars()} returns a \code{\link{list}} with components
  \item{axp}{numeric vector of length 2, \code{c(min., max.)}, of pretty
    axis extents.}
  \item{n}{integer (code), with the same meaning as
    \code{\link{par}("?axp")[3]}.}
}
\seealso{
  \code{\link{axTicks}};
  \code{\link[graphics]{axis}}, and \code{\link[graphics]{par}} (from
  the \pkg{graphics} package).
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

axisTicks(pu[3:4], log = FALSE, n = 10)

plot(10*(0:10), log = "y"); (pu <- par("usr"))
aX(2, print(ya <- axisTicks(pu[3:4], log = TRUE)))  # y axis

plot(2^(0:9), log = "y"); (pu <- par("usr"))
aX(2, print(ya <- axisTicks(pu[3:4], log = TRUE)))  # y axis

}
\keyword{dplot}
