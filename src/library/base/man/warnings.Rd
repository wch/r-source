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
\seealso{\code{\link{warning}}.}
\examples{
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
