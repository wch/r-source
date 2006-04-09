\name{warnings}
\alias{warnings}
\title{Print Warning Messages}
\description{
  \code{warnings} prints the top-level variable \code{last.warning} in a
  pleasing form.
}
\usage{
warnings(\dots)
}
\arguments{
  \item{\dots}{arguments to be passed to \code{\link{cat}}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\details{
  See the decription of \code{\link{options}("warn")} for the
  circumstances under which there is a \code{last.warning} variable and
  \code{warnings()} is used.  In essence this is if \code{options(warn =
    0)} and \code{warning} has been called at least once.

  It is possible that \code{last.warning} refers to the last recorded
  warning (possibly even in a previous session with a saved workspace),
  and not to the last warning, for example if \code{options(warn)} has
  been changed or if a catastrophic error occurred.
}
\seealso{
  \code{\link{warning}}.
}
\examples{
## NB this example is intended to be pasted in,
##    rather than run by example()
ow <- options("warn")
for(w in -1:1) {
   options(warn = w); cat("\n warn =",w,"\n")
   for(i in 1:3) { cat(i,"..\n"); m <- matrix(1:7, 3,4) }
}
warnings()
options(ow) # reset
}
\keyword{programming}
\keyword{error}
