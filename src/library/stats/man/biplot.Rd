% File src/library/stats/man/biplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{biplot}
\alias{biplot}
\alias{biplot.default}
\title{Biplot of Multivariate Data}
\usage{
biplot(x, \dots)

\method{biplot}{default}(x, y, var.axes = TRUE, col, cex = rep(par("cex"), 2),
       xlabs = NULL, ylabs = NULL, expand = 1,
       xlim  = NULL, ylim  = NULL, arrow.len = 0.1,
       main = NULL, sub = NULL, xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{x}{The \code{biplot}, a fitted object. For \code{biplot.default},
    the first set of points (a two-column matrix), usually associated
    with observations.}
  \item{y}{The second set of points (a two-column matrix), usually associated
    with variables.}
  \item{var.axes}{If \code{TRUE} the second set of points have arrows
    representing them as (unscaled) axes.}
  \item{col}{A vector of length 2 giving the colours for the first and
    second set of points respectively (and the corresponding axes). If a
    single colour is specified it will be used for both sets.  If
    missing the default colour is looked for in the
    \code{\link{palette}}: if there it and the next colour as used,
    otherwise the first two colours of the palette are used.}
  \item{cex}{The character expansion factor used for labelling the
    points. The labels can be of different sizes for the two sets by
    supplying a vector of length two.}
  \item{xlabs}{A vector of character strings to label the first set of
    points: the default is to use the row dimname of \code{x}, or
    \code{1:n} if the dimname is \code{NULL}.}
  \item{ylabs}{A vector of character strings to label the second set of
    points: the default is to use the row dimname of \code{y}, or
    \code{1:n} if the dimname is \code{NULL}.}
  \item{expand}{An expansion factor to apply when plotting the second set
    of points relative to the first. This can be used to tweak the
    scaling of the two sets to a physically comparable scale.}
  \item{arrow.len}{The length of the arrow heads on the axes plotted in
    \code{var.axes} is true. The arrow head can be suppressed by
    \code{arrow.len = 0}.}
  \item{xlim, ylim}{Limits for the x and y axes in the units of the
    first set of variables.}
  \item{main, sub, xlab, ylab, \dots}{graphical parameters.}
}
\description{
  Plot a biplot on the current graphics device.
}
\details{
  A biplot is plot which aims to represent both the observations and
  variables of a matrix of multivariate data on the same plot. There are
  many variations on biplots (see the references) and perhaps the most
  widely used one is implemented by \code{\link{biplot.princomp}}.
  The function \code{biplot.default} merely provides the
  underlying code to plot two sets of variables on the same figure.

  Graphical parameters can also be given to \code{biplot}: the size of
  \code{xlabs} and \code{ylabs} is controlled by \code{cex}.
}
\section{Side Effects}{
  a plot is produced on the current graphics device.
}
\references{
  K. R. Gabriel (1971).  The biplot graphical display of matrices with
  application to principal component analysis. \emph{Biometrika}
  \bold{58}, 453--467.

  J.C. Gower and D. J. Hand (1996). \emph{Biplots}. Chapman & Hall.
}
\seealso{\code{\link{biplot.princomp}}, also for examples.}
\keyword{hplot}
\keyword{multivariate}
