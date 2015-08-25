% File src/library/methods/man/hasArg.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{hasArg}
\alias{hasArg}
\title{Look for an Argument in the Call}
\description{
  Returns \code{TRUE} if \code{name} corresponds to an argument in the
  call, either a formal argument to the function, or a component of
  \code{...}, and \code{FALSE} otherwise.
}
\usage{
hasArg(name)
}
\arguments{
  \item{name}{The name of a potential argument, as an unquoted name or
    character string.}
}
\details{
  The expression \code{hasArg(x)}, for example, is similar to
  \code{!missing(x)}, with two exceptions.  First,  \code{hasArg} will look for
  an argument named \code{x} in the call if \code{x} is not a formal
  argument to the calling function, but \code{...} is.  Second,
  \code{hasArg} never generates an error if given a name as an argument,
  whereas \code{missing(x)} generates an error if \code{x} is not a
  formal argument.
}
\value{
  Always \code{TRUE} or \code{FALSE} as described above.
}
\seealso{ \code{\link{missing}} }

\examples{
ftest <- function(x1, ...) c(hasArg(x1), hasArg("y2"))

ftest(1) ## c(TRUE, FALSE)
ftest(1, 2)  ## c(TRUE, FALSE)
ftest(y2 = 2)   ## c(FALSE, TRUE)
ftest(y = 2)    ## c(FALSE, FALSE) (no partial matching)
ftest(y2 = 2, x = 1)  ## c(TRUE, TRUE) partial match x1
}
\keyword{ programming }
