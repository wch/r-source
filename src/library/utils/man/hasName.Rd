% File src/library/utils/man/hasName.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016 R Core Team
% Distributed under GPL 2 or later

\name{hasName}
\alias{hasName}
\title{Check for name}
\description{
  \code{hasName} is a convenient way to test for one or more names
in an R object.
}
\usage{
hasName(x, name)
}
\arguments{
  \item{x}{Any object.}
  \item{name}{One or more character values to look for.}
}
\value{
  A logical vector of the same length as \code{name} containing
  \code{TRUE} if the corresponding entry is in \code{names(x)}.
}
\details{
  \code{hasName(x, name)} is defined to be equivalent to 
  \code{name \%in\% names(x)}, though it will evaluate slightly more
  quickly.  It is intended to replace the common idiom
  \code{!is.null(x$name)}.  The latter can be unreliable due to partial
  name matching; see the example below.
}
\seealso{
  \code{\link{\%in\%}}, \code{\link{exists}}
}
\examples{
x <- list(abc = 1)
!is.null(x$abc) # correct
!is.null(x$a)   # this is the wrong test!
hasName(x, "abc")
hasName(x, "a")
}
\keyword{manip}
\keyword{logic}
