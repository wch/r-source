\name{octmode}
\alias{format.octmode}
\alias{print.octmode}
\alias{as.character.octmode}
\title{Display Numbers in Octal}
\description{
  Convert or print integers in octal format, with as many digits as are
  needed to display the largest, using leading zeroes as necessary.
}
\usage{
as.character.octmode(x, \dots)
format.octmode(x, \dots)
print.octmode(x, \dots)
}
\arguments{
  \item{x}{An object inheriting from class \code{"octmode"}.}
}
\details{
  Class \code{"octmode"} consists of integer vectors with that class
  atttribute, used merely to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as \code{755}.
}

\seealso{These are auxiliary functions for \code{\link{file.info}}}

\keyword{print}
