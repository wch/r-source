% File src/library/base/man/replace.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{replace}
\alias{replace}
\title{Replace Values in a Vector}
\description{
Replace vector elements at given indices by given \code{values}.
}
\usage{
replace(x, list, values)
}
\arguments{
  \item{x}{a \code{\link{vector}}.}
  \item{list}{an index vector, as for \code{\link{[<-}}.}
  \item{values}{replacement values, recycled if necessary.}
}
\value{
  A vector with the values replaced.
}
\references{
  \bibshow{R:Becker+Chambers+Wilks:1988}
}
\note{
  \code{x} is unchanged: remember to assign the result.
}
\examples{
x <- 1:5
replace(x, c(2, 5), c(8, 9))  # returns a modified copy
identical(x, 1:5)             # TRUE, x is not modified
}
\keyword{manip}
