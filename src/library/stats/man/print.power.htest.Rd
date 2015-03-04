% File src/library/stats/man/print.power.htest.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{print.power.htest}
\title{Print Methods for Hypothesis Tests and Power Calculation Objects}
\alias{print.htest}
\alias{print.power.htest}
\description{
  Printing objects of class \code{"htest"} or \code{"power.htest"},
  respectively, by simple \code{\link{print}} methods.
}
\usage{
\method{print}{htest}(x, digits = getOption("digits"), prefix = "\t", \dots)

\method{print}{power.htest}(x, digits = getOption("digits"), \dots)
}
\arguments{
  \item{x}{object of class \code{"htest"} or \code{"power.htest"}.}
  \item{digits}{number of significant digits to be used.}
  \item{prefix}{string, passed to \code{\link{strwrap}} for displaying
    the \code{method} component of the \code{htest} object.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  Both \code{\link{print}} methods traditionally have not obeyed the
  \code{digits} argument properly.  They now do, the \code{htest}
  method mostly in expressions like \code{max(1, digits - 2)}.

  A \code{power.htest} object is just a named list of numbers and
  character strings, supplemented with \code{method} and \code{note}
  elements.  The \code{method} is displayed as a title, the \code{note}
  as a footnote, and the remaining elements are given in an aligned
  \sQuote{name = value} format.
}
\value{
  the argument \code{x}, invisibly, as for all \code{\link{print}}
  methods.
}
\author{Peter Dalgaard}% and MMaechler for 'digits'

\seealso{
  \code{\link{power.t.test}},
  \code{\link{power.prop.test}}
}
\examples{
(ptt <- power.t.test(n = 20, delta = 1))
print(ptt, digits =  4) # using less digits than default
print(ptt, digits = 12) # using more  "       "     "
}
\keyword{htest}
