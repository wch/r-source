\name{warnings}
\title{Print Warning Messages}
\usage{
warnings(\dots)
}
\alias{warnings}
\description{
\code{warnings} prints the top--level variable \code{last.warning}
in a pleasing form.
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
