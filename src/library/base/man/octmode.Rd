\name{octmode}
\alias{format.octmode}
\alias{print.octmode}
\alias{as.character.octmode}
\alias{[.octmode}
\title{Display Numbers in Octal}
\description{
  Convert or print integers in octal format, with as many digits as are
  needed to display the largest, using leading zeroes as necessary.
}
\usage{
\method{as.character}{octmode}(x, \dots)

\method{format}{octmode}(x, \dots)

\method{print}{octmode}(x, \dots)
}
\arguments{
  \item{x}{An object inheriting from class \code{"octmode"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"octmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as
  \code{755}.
}
\seealso{
  These are auxiliary functions for \code{\link{file.info}}
}
\keyword{print}
