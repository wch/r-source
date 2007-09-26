% File src/library/base/man/octmode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{octmode}
\alias{as.octmode}
\alias{format.octmode}
\alias{print.octmode}
\alias{as.character.octmode}
\alias{[.octmode}
\alias{octmode}
\title{Display Numbers in Octal}
\description{
  Convert or print integers in octal format, with as many digits as are
  needed to display the largest, using leading zeroes as necessary.
}
\usage{
as.octmode(x)

\method{as.character}{octmode}(x, \dots)

\method{format}{octmode}(x, \dots)

\method{print}{octmode}(x, \dots)
}
\arguments{
  \item{x}{An object, for the methods inheriting from class \code{"octmode"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"octmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as
  \code{755}.  Subsetting (\code{\link{[}}) works too.

  \code{as.octmode} can convert integers (of \link{type} \code{"integer"} or
  \code{"double"}) and character strings to class \code{"octmode"}
}
\seealso{
  These are auxiliary functions for \code{\link{file.info}}.

  \code{\link{hexmode}}
}
\examples{
(on <- structure(c(16,32, 127:129),  class = "octmode")) #-> print.*()
##-> "020" "040" "177" "200" "201"
unclass(on[3:4]) # subsetting
}
\keyword{print}
