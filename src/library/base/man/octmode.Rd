% File src/library/base/man/octmode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{octmode}
\alias{as.octmode}
\alias{format.octmode}
\alias{print.octmode}
\alias{as.character.octmode}
\alias{[.octmode}
\alias{!.octmode}
\alias{|.octmode}
\alias{&.octmode}
\alias{xor.octmode}
\alias{octmode}
\title{Display Numbers in Octal}
\description{
  Convert or print integers in octal format, with as many digits as are
  needed to display the largest, using leading zeroes as necessary.
}
\usage{
as.octmode(x)

\method{as.character}{octmode}(x, \dots)

\method{format}{octmode}(x, width = NULL, \dots)

\method{print}{octmode}(x, \dots)
}
\arguments{
  \item{x}{An object, for the methods inheriting from class \code{"octmode"}.}
  \item{width}{\code{NULL} or a positive integer specifying the minimum
    field width to be used, with padding by leading zeroes.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"octmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as
  \code{755}.  Subsetting (\code{\link{[}}) works too.

  If \code{width = NULL} (the default), the output is padded with
  leading zeroes to the smallest width needed for all the non-missing
  elements.

  \code{as.octmode} can convert integers (of \link{type} \code{"integer"} or
  \code{"double"}) and character vectors whose elements contain only
  digits \code{0-7} (or are \code{NA}) to class \code{"octmode"}.

  There is a \code{\link{!}} method and \code{\link{|}}, \code{\link{&}} and
  \code{\link{xor}} methods: these recycle their arguments to the
  length of the longer and then apply the operators bitwise to each
  element.
}
\seealso{
  These are auxiliary functions for \code{\link{file.info}}.

  \code{\link{hexmode}}, \code{\link{sprintf}} for other options in
  converting integers to octal, \code{\link{strtoi}} to convert octal
  strings to integers.
}
\examples{
(on <- as.octmode(c(16, 32, 127:129))) # "020" "040" "177" "200" "201"
unclass(on[3:4]) # subsetting

## manipulate file modes
fmode <- as.octmode("170")
(fmode | "644") & "755"
\donttest{
umask <- Sys.umask(NA) # depends on platform
c(fmode, "666", "755") & !umask
}}
\keyword{utilities}
\keyword{print}
