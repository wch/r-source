\name{maxCol}
\alias{max.col}
\title{Find Maximum Position in Matrix}
\description{
  Find the maximum position for each row of a matrix, breaking ties at random.
}
\usage{
max.col(m)
}
\arguments{
  \item{m}{numerical matrix}
}
\value{index of a maximal value for each row, an integer vector of
  length \code{nrow(m)}.
}
\details{
  Ties are broken at random.  The determination of ``tie'' assumes that
  the entries are probabilities: there is a relative tolerance of
  \eqn{10^{-5}}{1e-5}, relative to the largest entry in the row.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  New York: Springer (4th ed).
}
\seealso{\code{\link{which.max}} for vectors.}
\examples{
data(swiss)
table(mc <- max.col(swiss))# mostly "1" and "5", 5 x "2" and once "4"
swiss[unique(print(mr <- max.col(t(swiss)))) , ] # 3 33 45 45 33 6
}
\keyword{utilities}
\keyword{array}
