% File src/library/base/man/hexmode.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{hexmode}
\alias{as.hexmode}
\alias{format.hexmode}
\alias{print.hexmode}
\alias{as.character.hexmode}
\alias{[.hexmode}
\alias{!.hexmode}
\alias{|.hexmode}
\alias{&.hexmode}
\alias{xor.hexmode}
\alias{hexmode}
\title{Display Numbers in Hexadecimal}
\description{
  Convert or print integers in hexadecimal format, with as many digits
  as are needed to display the largest, using leading zeroes as
  necessary.
}
\usage{
as.hexmode(x)

\method{as.character}{hexmode}(x, \dots)

\method{format}{hexmode}(x, width = NULL, upper.case = FALSE, \dots)

\method{print}{hexmode}(x, \dots)
}
\arguments{
  \item{x}{An object, for the methods inheriting from class \code{"hexmode"}.}
  \item{width}{\code{NULL} or a positive integer specifying the minimum
    field width to be used, with padding by leading zeroes.}
  \item{upper.case}{a logical indicating whether to use upper-case
    letters or lower-case letters (default).}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"hexmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in hex.

  If \code{width = NULL} (the default), the output is padded with
  leading zeroes to the smallest width needed for all the non-missing
  elements.

  \code{as.hexmode} can convert integers (of \link{type} \code{"integer"} or
  \code{"double"}) and character vectors whose elements contain only
  \code{0-9}, \code{a-f}, \code{A-F} (or are \code{NA}) to class
  \code{"hexmode"}.

  There is a \code{\link{!}} method and \code{\link{|}}, \code{\link{&}} and
  \code{\link{xor}} methods: these recycle their arguments to the
  length of the longer and then apply the operators bitwise to each
  element.
}
\seealso{
  \code{\link{octmode}}, \code{\link{sprintf}} for other options in
  converting integers to hex, \code{\link{strtoi}} to convert hex
  strings to integers.
}
\examples{
i <- as.hexmode("7fffffff")
i; class(i)
identical(as.integer(i), .Machine$integer.max)

hm <- as.hexmode(c(NA, 1)); hm
as.integer(hm)
}
\keyword{utilities}
\keyword{print}
