\name{print.htest.power}
\alias{print.htest.power}
\title{ Print method for power calculation object}
\usage{
print.htest.power(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{x}{ Object of class \code{htest.power} }
}
\description{
 Print \code{htest.power} object in nice layout
}
\details{
  An \code{htest.power} object is just a named list of numbers and
  strings, supplemented with \code{method} and \code{note} elements.
  The \code{method} is displayed as a title, the \code{note} as a
  footnote, and the remaining elements are given in an aligned `name =
  value' format.
}
\value{
  none
}
\author{Peter Dalgaard}

\seealso{\code{\link{t.test.power}}, \code{\link{prop.test.power}} }

\keyword{ ~keyword }%-- one or more ...
