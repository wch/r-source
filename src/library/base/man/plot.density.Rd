\name{plot.density}
\alias{plot.density}
\title{Plot Method for Kernel Density Estimation}
\usage{
\method{plot}{density}(x, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE, \dots)
}
\arguments{
\item{x}{a \dQuote{density} object.}
\item{main, xlab, ylab, type}{plotting parameters with useful defaults.}
\item{\dots}{further plotting parameters.}
\item{zero.line}{logical; if \code{TRUE}, add a base line at \eqn{y = 0}}
}
\description{
  The \code{plot} method for density objects.
}
\value{
  None.
}
\references{
}
\seealso{
  \code{\link{density}}.
}
\keyword{dplot}
