% Originally file MASS/max.col.d
% copyright (C) 1999 W. N. Venables and B. D. Ripley
\name{max.col}
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
  the entries are probabilities.
}
\examples{
data(swiss)
table(mc <- max.col(swiss))# mostly "1" and "5", 5 x "2" and once "4"
}
\keyword{utilities}
