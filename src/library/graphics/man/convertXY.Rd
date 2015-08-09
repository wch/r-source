% File src/library/graphics/man/convertXY.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2014 R Core Team
% Distributed under GPL 2 or later

\name{convertXY}
\alias{grconvertX}
\alias{grconvertY}
\title{Convert between Graphics Coordinate Systems}
\description{
  Convert between graphics coordinate systems.
}
\usage{
grconvertX(x, from = "user", to = "user")
grconvertY(y, from = "user", to = "user")
}
\arguments{
  \item{x, y}{numeric vector of coordinates.}
  \item{from, to}{character strings giving the coordinate systems to
    convert between.}
}
\details{
  The coordinate systems are
  \describe{
    \item{\code{"user"}}{user coordinates.}
    \item{\code{"inches"}}{inches.}
    \item{\code{"device"}}{the device coordinate system.}
    \item{\code{"ndc"}}{normalized device coordinates.}
    \item{\code{"nfc"}}{normalized figure coordinates.}
    \item{\code{"npc"}}{normalized plot coordinates.}
    \item{\code{"nic"}}{normalized inner region coordinates.  (The
      \sQuote{inner region} is that inside the outer margins.)}
  }
  (These names can be partially matched.)  For the \sQuote{normalized}
  coordinate systems the lower left has value 0 and the top right
  value 1.

  Device coordinates are those in which the device works: they are
  usually in pixels where that makes sense and in big points (1/72 inch)
  otherwise (e.g., \code{\link{pdf}} and \code{\link{postscript}}).
}
\value{
  A numeric vector of the same length as the input.
}

\examples{
op <- par(omd=c(0.1, 0.9, 0.1, 0.9), mfrow = c(1, 2))
plot(1:4)
for(tp in c("in", "dev", "ndc", "nfc", "npc", "nic"))
    print(grconvertX(c(1.0, 4.0), "user", tp))
par(op)
}
\keyword{ dplot }
