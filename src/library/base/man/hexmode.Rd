\name{hexmode}
\alias{format.hexmode}
\alias{print.hexmode}
\alias{as.character.hexmode}
\alias{[.hexmode}
\title{Display Numbers in Hexadecimal}
\description{
  Convert or print integers in hexadecimal format, with as many digits
  as are needed to display the largest, using leading zeroes as necessary.
}
\usage{
\method{as.character}{hexmode}(x, \dots)

\method{format}{hexmode}(x, \dots)

\method{print}{hexmode}(x, \dots)
}
\arguments{
  \item{x}{An object inheriting from class \code{"hexmode"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"hexmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in hex.
}
\keyword{print}
