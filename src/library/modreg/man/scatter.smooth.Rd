% file modreg/man/scatter.smooth.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{scatter.smooth}
\name{panel.smooth}
\name{loess.smooth}
\title{Scatter Plot with Smooth Curve Fitted by Loess}
\usage{
scatter.smooth(x, y, span=2/3, degree=1,
    family=c("symmetric", "gaussian"),
    xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
    evaluation=50, ...)
panel.smooth(x, y, span=2/3, degree=1,
    family=c("symmetric", "gaussian"), zero.line=F, evaluation=50, ...)
loess.smooth(x, y, span=2/3, degree=1,
    family=c("symmetric", "gaussian"), zero.line=F, evaluation=50, ...)
}
\alias{scatter.smooth}
\alias{panel.smooth}
\alias{loess.smooth}
\arguments{
 \item{x}{x coordinates for scatter plot.}
 \item{y}{y coordinates for scatter plot.}
 \item{span}{smoothness parameter for \code{loess}.}
 \item{degree}{degree of local polynomial used.}
 \item{family}{if \code{family="gaussian"} fitting is by least-squares,
   and if \code{family="symmetric"} a re-descending M estimator is used.}
 \item{xlab}{label for x axis.}
 \item{ylab}{label for y axis.}
 \item{zero.line}{logical for plotting a horizontal line at zero -- used
 for residual plots.}
 \item{evaluation}{number of points at which to evaluate the smooth curve.}
 \item{\ldots}{graphical parameters.}
}
\description{
 Plot (\code{scatter.smooth}) and add a smooth curve computed by
 \code{loess} to a scatter plot. \code{loess.smooth} is an auxiliary
 function used by the other two.
}
\value{
  None.
}
\author{B.D. Ripley}

\seealso{\code{\link{loess}}}

\examples{
data(cars)
attach(cars)
scatter.smooth(speed, dist)
detach()
}
\keyword{smooth}
