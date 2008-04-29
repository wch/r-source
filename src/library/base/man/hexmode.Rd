% File src/library/base/man/hexmode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{hexmode}
\alias{as.hexmode}
\alias{format.hexmode}
\alias{print.hexmode}
\alias{as.character.hexmode}
\alias{[.hexmode}
\alias{hexmode}
\title{Display Numbers in Hexadecimal}
\description{
  Convert or print integers in hexadecimal format, with as many digits
  as are needed to display the largest, using leading zeroes as
  necessary. 
}
\usage{
as.hexmode(x)

\method{as.character}{hexmode}(x, upper.case = FALSE, \dots)

\method{format}{hexmode}(x, upper.case = FALSE, \dots)

\method{print}{hexmode}(x, \dots)
}
\arguments{
  \item{x}{An object, for the methods inheriting from class
    \code{"hexmode"}.}
  \item{upper.case}{a logical indicating whether to use upper-case
    letters or lower-case letters (default).}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"hexmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in hex.
}
\seealso{
  \code{\link{octmode}}
}
\keyword{print}
