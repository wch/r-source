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
\seealso{
  \code{\link{data.frame}}
}
\examples{
}
\keyword{hplot}
\keyword{methods}
