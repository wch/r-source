% File src/library/stats/man/print.power.htest.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{print.power.htest}
\alias{print.power.htest}
\title{Print method for power calculation object}
\description{
  Print object of class \code{"power.htest"} in nice layout.
}
\usage{
\method{print}{power.htest}(x, \dots)
}
\arguments{
  \item{x}{Object of class \code{"power.htest"}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  A \code{power.htest} object is just a named list of numbers and
  character strings, supplemented with \code{method} and \code{note}
  elements.  The \code{method} is displayed as a title, the \code{note}
  as a footnote, and the remaining elements are given in an aligned
  \sQuote{name = value} format.
}
\value{
  none
}
\author{Peter Dalgaard}

\seealso{
  \code{\link{power.t.test}},
  \code{\link{power.prop.test}}
}
\keyword{htest}
