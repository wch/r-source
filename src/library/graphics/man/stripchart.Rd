\name{stripchart}
\title{1-D Scatter Plots}
\alias{stripchart}
\description{
  \code{stripchart} produces one dimensional scatter plots (or dot
  plots) of the given data.  These plots are a good alternative to
  \code{\link{boxplot}}s when sample sizes are small.
}
\usage{
stripchart(x, method = "overplot", jitter = 0.1, offset = 1/3,
           vertical = FALSE, group.names, add = FALSE,
           at = NULL, xlim = NULL, ylim = NULL,
           main = "", ylab = "", xlab = "",
           log = "", pch = 0, col = par("fg"), cex = par("cex"))
}
\arguments{
  \item{x}{the data from which the plots are to be produced.  The data
    can be specified as a single numeric vector, or as list of numeric
    vectors, each corresponding to a component plot.  Alternatively a
    symbolic specification of the form \code{x ~ g} can be given,
    indicating the observations in the vector \code{x} are to be
    grouped according to the levels of the factor \code{g}. \code{NA}s
    are allowed in the data.}
  \item{method}{the method to be used to separate coincident points.
    The default method \code{"overplot"} causes such points to be
    overplotted, but it is also possible to specify \code{"jitter"} to
    jitter the points, or \code{"stack"} have coincident points
    stacked.  The last method only makes sense for very granular data.}
  \item{jitter}{when \code{method="jitter"} is used, \code{jitter}
    gives the amount of jittering applied.}
  \item{offset}{when stacking is used, points are stacked this many
    line-heights (symbol widths) apart.}
  \item{vertical}{when vertical is \code{TRUE} the plots are drawn
    vertically rather than the default horizontal.}
  \item{group.names}{group labels which will be printed alongside
    (or underneath) each plot.}
  \item{add}{logical, if true \emph{add} the chart to the current plot.}
  \item{at}{numeric vector giving the locations where the charts should
    be drawn, particularly when \code{add = TRUE};
    defaults to \code{1:n} where \code{n} is the number of boxes.}
  \item{main, ylab, xlab}{labels: see \code{\link{title}}.}
  \item{xlim, ylim}{plot limits: see \code{\link{plot.window}}.}
  \item{log, pch, col, cex}{Graphical parameters: see \code{\link{par}}.}
}
\details{
  Extensive examples of the use of this kind of plot can be found in
  Box, Hunter and Hunter or Seber and Wild.
}
\examples{
x <- stats::rnorm(50)
xr <- round(x, 1)
stripchart(x) ; m <- mean(par("usr")[1:2])
text(m, 1.04, "stripchart(x, \"overplot\")")
stripchart(xr, method = "stack", add = TRUE, at = 1.2)
text(m, 1.35, "stripchart(round(x,1), \"stack\")")
stripchart(xr, method = "jitter", add = TRUE, at = 0.7)
text(m, 0.85, "stripchart(round(x,1), \"jitter\")")

with(OrchardSprays,
     stripchart(decrease ~ treatment,
                main = "stripchart(Orchardsprays)", ylab = "decrease",
                vertical = TRUE, log = "y"))
% Fixme: ylab should have a non-""  default !

with(OrchardSprays,
     stripchart(decrease ~ treatment, at = c(1:8)^2,
                main = "stripchart(Orchardsprays)", ylab = "decrease",
                vertical = TRUE, log = "y"))
}
\keyword{hplot}
