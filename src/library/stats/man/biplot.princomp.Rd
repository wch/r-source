\name{biplot.princomp}
\alias{biplot.princomp}
\alias{biplot.prcomp}
\title{
Biplot for Principal Components
}
\description{
  Produces a biplot (in the strict sense) from the output of
  \code{\link{princomp}} or \code{\link{prcomp}}
}
\usage{
\method{biplot}{prcomp}(x, choices = 1:2, scale = 1, pc.biplot = FALSE, \dots)

\method{biplot}{princomp}(x, choices = 1:2, scale = 1, pc.biplot = FALSE, \dots)
}
\arguments{
  \item{x}{an object of class \code{"princomp"}.}
  \item{choices}{
    length 2 vector specifying the components to plot. Only the default
    is a biplot in the strict sense.
  }
  \item{scale}{
    The variables are scaled by \code{lambda ^ scale} and the
    observations are scaled by \code{lambda ^ (1-scale)} where
    \code{lambda} are the singular values as computed by
    \code{\link{princomp}}. Normally \code{0 <= scale <= 1}, and a warning 
    will be issued if the specified \code{scale} is outside this range.
  }
  \item{pc.biplot}{
    If true, use what Gabriel (1971) refers to as a "principal component
    biplot", with \code{lambda = 1} and observations scaled up by sqrt(n) and
    variables scaled down by sqrt(n).  Then inner products between
    variables approximate covariances and distances between observations
    approximate Mahalanobis distance.
  }
  \item{\dots}{optional arguments to be passed to
    \code{\link{biplot.default}}.}
}
\section{Side Effects}{
    a plot is produced on the current graphics device.
}
\details{
    This is a method for the generic function \code{biplot}.  There is
    considerable confusion over the precise definitions: those of the
    original paper, Gabriel (1971), are followed here.  Gabriel and
    Odoroff (1990) use the same definitions, but their plots actually
    correspond to \code{pc.biplot = TRUE}.
}
\references{
    Gabriel, K. R. (1971).
    The biplot graphical display of matrices with applications to
    principal component analysis.
    \emph{Biometrika}, \bold{58}, 453--467.

    Gabriel, K. R. and Odoroff, C. L. (1990).
    Biplots in biomedical research.
    \emph{Statistics in Medicine}, \bold{9}, 469--485.
}
\seealso{
    \code{\link{biplot}},
    \code{\link{princomp}}.
}
\examples{
biplot(princomp(USArrests))
}
\keyword{multivariate}
\keyword{hplot}
