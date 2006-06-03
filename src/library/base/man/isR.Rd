\name{is.R}
\title{Are we using R, rather than S?}
\usage{
is.R()
}
\alias{is.R}
\description{
  Test if running under \R.
}

\details{
  The function has been written such as to correctly run in all versions
  of \R, S and S-PLUS.
  In order for code to be runnable in both \R and S dialects previous to
  S-PLUS 8.0, your code must either define \code{is.R} or use it as

  \code{if (exists("is.R") && is.function(is.R) && is.R()) \{}\cr
  \code{    }\emph{\#\# R-specific code}\cr
  \code{\} else \{}\cr
  \code{    }\emph{\#\# S-version of code}\cr
  \code{\}}
}
\value{
  \code{is.R} returns \code{TRUE} if we are using \R and \code{FALSE}
  otherwise.
}
\seealso{\code{\link{R.version}}, \code{\link{system}}.}
\examples{
x <- runif(20); small <- x < 0.4
## In the early years of R, 'which()' only existed in R:
if(is.R()) which(small) else seq(along=small)[small]
}
\keyword{environment}
\keyword{utilities}
