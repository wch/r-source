\name{warning}
\alias{warning}
\title{Warning Messages}
\description{
  Generates a warning message that corresponds to its argument(s) and the
  expression or function from which it was called.
}
\usage{
warning(\dots)
}
\arguments{
  \item{\dots}{character vectors (which are pasted together with no
    separator) or \code{NULL}.}
}
\details{
  The result \emph{depends} on the value of
  \code{\link{options}("warn")}.

  If \code{warn} is negative warnings are ignored; if it is zero they
  are stored and printed after the top--level function has completed; if
  it is one they are printed as they occur and if it is 2 (or larger)
  warnings are turned into errors.

  If \code{warn} is zero (the default), a top-level variable
  \code{last.warning} is created.  It contains the warnings which can be
  printed via a call to \code{\link{warnings}}.
}
\seealso{
  \code{\link{stop}} for fatal errors,
  \code{\link{warnings}},
  and \code{\link{options}} with argument \code{warn=}.
}
\examples{
\testonly{eigenval <- 10 ^ -rnorm(1,m=6)
if(eigenval < 1.e-7) warning("system near singular")}
}
\keyword{programming}
\keyword{error}
