\name{pairs}
\title{Scatterplot Matrices}
\alias{pairs}
\alias{pairs.default}
\usage{
pairs(x, \dots)
pairs.default(x, labels = dimnames(x)[[2]], panel = points, \dots)
}
\arguments{
  \item{x}{the coordinates of points given as columns of a matrix.}
  \item{labels}{the names of the variables.}
  \item{panel}{\code{\link{function}(x,y,\dots)} which is used to plot
    the contents of each panel of the display.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{plot}.}
}
\description{
  A matrix of scatterplots is produced.
}
\details{
  The \eqn{ij}th scatterplot contains \code{x[,i]} plotted against
  \code{x[,j]}.

  The graphical parameters \code{pch} and \code{col} can be used to
  specify a vector of plotting symbols and colors to be used in the
  plots.

  The graphical parameter \code{oma} will be set by \code{pairs.default}
  unless supplied as an argument.
}
\examples{
data(USJudgeRatings)
pairs(USJudgeRatings)
}
\keyword{hplot}
