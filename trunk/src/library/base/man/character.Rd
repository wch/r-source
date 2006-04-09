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

  \code{as.character} represents real and complex numbers to 15 decimal
  places (technically the compiler's setting of the ISO C constant
  \code{DBL_DIG}, which will be 15 on machines supporting IEC60559
  arithmetic according to the C99 standard).  This ensures that all the
  digits in the result will be reliable (and not the result of
  representation error), but does mean that coversion to character and
  back to numeric may change the number.  If you want to convert numbers
  to character with the maximum possible precision, use
  \code{\link{format}}.
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

a0 <- 11/999          # has a repeating decimal representation
(a1 <- as.character(a0))
format(a0, digits=16) # shows one more digit
a2 <- as.numeric(a1)
a2 - a0               # normally around -1e-17
as.character(a2)      # normally different from a1
print(c(a0, a2), digits = 16)
}
\keyword{character}
\keyword{classes}
