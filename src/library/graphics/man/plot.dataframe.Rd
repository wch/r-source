\name{plot.data.frame}
\alias{plot.data.frame}
\title{Plot Method for Data Frames}
\description{
  \code{plot.data.frame}, a method of the \code{\link{plot}} generic,
  uses \code{\link{stripchart}} for \emph{one} variable,
  \code{\link{plot.default}} (scatterplot) for \emph{two} variables, and
  \code{\link{pairs}} (scatterplot matrix) otherwise.
}
\usage{
\method{plot}{data.frame}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{\dots}{further arguments to \code{\link{stripchart}},
    \code{\link{plot.default}} or \code{\link{pairs}}.}
}
\details{
  This is intended for data frames with \emph{numeric} columns, and
  first calls \code{\link{data.matrix}} to convert the data frame to a
  numeric matrix.  That may well be inappropriate: for example numerical
  conversion of dates will lose their special meaning and a warning will
  be given.
}
\seealso{
  \code{\link{data.frame}}
}
\examples{
plot(OrchardSprays[1], method="jitter")
plot(OrchardSprays[c(4,1)])
plot(OrchardSprays)
}
\keyword{hplot}
\keyword{methods}
