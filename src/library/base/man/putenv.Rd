\name{putenv}
\title{Set Environment Variables}
\usage{
putenv(\dots)
}
\alias{putenv}
\arguments{
  \item{\dots}{arguments in \code{name=value} form, with \code{value}
    coercible to a character string.}
}
\description{
  \code{putenv} sets environment variables (for other processes called
  from within \R or future calls to \code{\link{getenv}} from this
  \R process).
}
\value{
  A logical vector of the same length as \code{x}, with elements being
  true if setting the corresponding variable succeeded.
}
\details{
  Non-standard \R names must be quoted: see the Examples section.
}
\note{
  Not all systems need support \code{putenv}.
}
\seealso{
  \code{\link{getenv}},
  \code{\link{setwd}} for the working directory.
}
\examples{
print(putenv("R_TEST"="testit", ABC=123))
getenv("R_TEST")
}
\keyword{environment}
\keyword{utilities}
