\name{character}
\alias{character}
\alias{as.character}
\alias{as.character.default}
\alias{as.character.factor}
\alias{is.character}
\title{Character Vectors}
\description{
  Create or test for objects of type \code{"character"}.
}
\usage{
character(length = 0)
as.character(x, \dots)
is.character(x)
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{as.character} and \code{is.character} are generic: you can
  write methods to handle specific classes of objects,
  see \link{InternalMethods}.
}
\value{
  \code{character} creates a character vector of the specified length.
  The elements of the vector are all equal to \code{""}.

  \code{as.character} attempts to coerce its argument to character type;
  like \code{\link{as.vector}} it strips attributes including names.

  \code{is.character} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of character type or not.
}
\note{
  \code{as.character} truncates components of language objects to 500
  characters (was about 70 before 1.3.1).
}
\seealso{
  \code{\link{paste}}, \code{\link{substr}} and \code{\link{strsplit}}
  for character concatenation and splitting,
  \code{\link{chartr}} for character translation and casefolding (e.g.,
  upper to lower case) and \code{\link{sub}}, \code{\link{grep}} etc for
  string matching and substitutions.  Note that
  \code{help.search(keyword = "character")} gives even more links.
%
  \code{\link{deparse}}, which is normally preferable to
  \code{as.character} for language objects.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
form <- y ~ a + b + c
as.character(form)  ## length 3
deparse(form)       ## like the input
}
\keyword{character}
\keyword{classes}
