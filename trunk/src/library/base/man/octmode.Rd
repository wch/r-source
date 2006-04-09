\name{octmode}
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
\method{as.character}{octmode}(x, \dots)

\method{format}{octmode}(x, \dots)

\method{print}{octmode}(x, \dots)

%% x[...]  %- does not really display nicely
}
\arguments{
  \item{x}{An object inheriting from class \code{"octmode"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Class \code{"octmode"} consists of integer vectors with that class
  attribute, used merely to ensure that they are printed in octal
  notation, specifically for Unix-like file permissions such as
  \code{755}.  Subsetting (\code{\link{[}}) works too.
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
