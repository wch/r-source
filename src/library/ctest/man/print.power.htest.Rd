\name{print.power.htest}
\alias{print.power.htest}
\title{ Print method for power calculation object}
\usage{
print.power.htest(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{x}{ Object of class \code{power.htest} }
}
\description{
 Print \code{power.htest} object in nice layout
}
\details{
  A \code{power.htest} object is just a named list of numbers and
  strings, supplemented with \code{method} and \code{note} elements.
  The \code{method} is displayed as a title, the \code{note} as a
  footnote, and the remaining elements are given in an aligned `name =
  value' format.
}
\value{
  none
}
\author{Peter Dalgaard}

\seealso{\code{\link{power.t.test}}, \code{\link{power.prop.test}} }

\keyword{htest}
