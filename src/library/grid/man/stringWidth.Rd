% File src/library/grid/man/stringWidth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{stringWidth}
\alias{stringWidth}
\alias{stringHeight}
\alias{stringAscent}
\alias{stringDescent}
\title{Create a Unit Describing the Width and Height of a String or Math
  Expression}
\description{
  These functions create a unit object describing the width or height of
  a string.
}
\usage{
stringWidth(string)
stringHeight(string)
stringAscent(string)
stringDescent(string)
}
\arguments{
  \item{string}{A character vector or a language object (as used for
    \sQuote{\link{plotmath}} calls).}
}
\value{
  A \code{\link{unit}} object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}} and
  \code{\link{grobWidth}}

  \code{strwidth} in the \pkg{graphics} package for more details of the
  typographic concepts behind the computations.
}
\keyword{dplot}
