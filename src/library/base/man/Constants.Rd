\name{Constants}
\alias{LETTERS}
\alias{letters}
\alias{month.abb}
\alias{month.name}
\alias{pi}
\title{Built-in Constants}
\description{Constants built into \R.}
\usage{
LETTERS
letters
month.abb
month.name
pi
}
\details{
  \R has a limited number of built-in constants (there is also a rather
  larger library of data sets which can be loaded with the function
  \code{\link{data}}).

  The following constants are available:
  \itemize{
    \item \code{LETTERS}: the 26 upper-case letters of the Roman
    alphabet;
    \item \code{letters}: the 26 lower-case letters of the Roman
    alphabet;
    \item \code{month.abb}: the three-letter abbreviations for the
    English month names;
    \item \code{month.name}: the English names for the months of the
    year;
    \item \code{pi}: the ratio of the circumference of a circle to its
    diameter.
  }
}
\seealso{
  \code{\link{data}}.
}
\examples{
all(nchar(letters) == 1)                    # TRUE
all(month.abb == substr(month.name, 1, 3))  # TRUE

eps <- .Machine$double.eps
all.equal(pi, 4*atan(1), tol= 2*eps)

# John Machin (1705) computed 100 decimals of pi :
all.equal(pi/4, 4*atan(1/5) - atan(1/239), 4*eps)

}
\keyword{sysdata}
