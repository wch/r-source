\name{demo}
\title{Demonstrations of R functions}
\usage{demo(topic, device = getOption("device"))}
\alias{demo}
\arguments{
  \item{topic}{The topic which should be demonstrated.
    If omitted, the list of available topics is displayed.}
  \item{device}{The graphics device to be used.}
}
\description{
  \code{demo} is a user-friendly interface to running some demonstration R
  scripts. \code{demo()} gives the list of available topics.}
\seealso{\code{\link{source}} which is called by \code{demo}.}
\examples{
demo(lm.glm)
}
\keyword{documentation}
\keyword{utilities}
