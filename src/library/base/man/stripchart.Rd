\name{stripchart}
\title{1-D Scatter Plots}
\alias{stripchart}
\description{
  \code{stripchart} produces one dimensional scatter plots (or dot
  plots) of the given data.  These plots are are good alternative to 
  \code{\link{boxplot}}s when sample sizes are small.
}
\usage{
stripchart(x, method="overplot", jitter=0.1, offset=1/3,
           vertical=FALSE, group.names,
           xlim=NULL, ylim=NULL, main="", ylab="", xlab="",
           pch=0, col=par("fg"), cex=par("cex"))
}
\arguments{
  \item{x}{the data from which the plots are to be produced.  The data
    can be specified as a single vector, or as list of vectors, each
    corresponding to a component plot.  Alternatively a symbolic
    specification of the form \code{x ~ g} can be given, indicating the
    the observations in the vector \code{x} are to be grouped according
    to the levels of the factor \code{g}.
    \code{NA}s are allowed in the data.}
  \item{method}{the method to be used to separate coincident points.
    The default method \code{"overplot"} causes such points to be
    overplotted, but it is also possible to specify \code{"jitter"} to
    jitter the points, or \code{"stack"} have coincident points
    stacked.  The last method only makes sense for very granular data.}
  \item{jitter}{when jittering is used, \code{jitter} gives the amount
    of jittering applied.}
  \item{offset}{when stacking is used, points are stacked this many
    line-heights (symbol widths) apart.}
  \item{vertical}{when vertical is \code{TRUE} the plots are drawn
    vertically rather than the default horizontal.}
  \item{group.names}{group labels which will be printed alongside
    (or underneath) each plot.}
  \item{xlim, ylim, main, ylab, xlab, pch, col, cex}{Graphical parameters.}
}
\details{
  Extensive examples of the use of this kind of plot can be found in
  Box, Hunter and Hunter or Seber and Wild.
}
\examples{
x <- round(rnorm(50), 1)
stripchart(x)
}
\keyword{hplot}
