\name{pairs}
\title{Scatterplot Matrices}
\usage{
pairs(x, labels=dimnames(x)[[2]], panel=points, \dots)
}
\alias{pairs}
\arguments{
\item{x}{the coordinates of points given as columns of a matrix.}
\item{labels}{the names of the variables.}
\item{panel}{the function which is used to plot the contents of
each panel of the display.}
\item{\dots}{graphical parameters can be given as arguments to \code{plot}.}
}
\description{
A matrix of scatterplots is produced.
The \eqn{ij}th scatterplot contains \code{x[,i]} plotted against
\code{x[,j]}.
The graphical parameters \code{pch} and \code{col} can be used to specify
a vector of plotting symbols and colors to be used in the plots.
}
\examples{
data(judges)
pairs(judges)
}
\keyword{hplot}
