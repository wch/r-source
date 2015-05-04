\name{chkDots}
\alias{chkDots}
\title{Warn about extraneous arguments in the "..."	 (of its caller).}
\description{
Warn about extraneous arguments in the "..." (of its
caller).
}
\usage{
chkDots(..., which.call = -1, allowed = character(0))
}
\arguments{
  \item{...}{} \item{which.call}{passed to sys.call().
  A caller may use -2 if the message should mention
  *its* caller} \item{allowed}{not yet implemented:
  character vector of *named* elements in '...' which are
  \dQuote{allowed} and hence not warned about}
}
\author{Martin Maechler, first version outside base, June 2012.}
\seealso{
  \code{\link{warning}},  \code{\link{...}}.
}
\examples{
## used in, e.g.,
seq.default
seq(1,5, foo = "bar") # gives warning via chkDots()
}
\keyword{utilities}
