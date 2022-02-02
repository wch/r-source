% File src/library/graphics/man/stripchart.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{stripchart}
\title{1-D Scatter Plots}
\alias{stripchart}
\alias{stripchart.default}
\alias{stripchart.formula}

\description{
  \code{stripchart} produces one dimensional scatter plots (or dot
  plots) of the given data.  These plots are a good alternative to
  \code{\link{boxplot}}s when sample sizes are small.
}
\usage{
stripchart(x, \dots)

\method{stripchart}{formula}(x, data = NULL, dlab = NULL, \dots,
           subset, na.action = NULL)


\method{stripchart}{default}(x, method = "overplot", jitter = 0.1, offset = 1/3,
           vertical = FALSE, group.names, add = FALSE,
           at = NULL, xlim = NULL, ylim = NULL,
           ylab = NULL, xlab = NULL, dlab = "", glab = "",
           log = "", pch = 0, col = par("fg"), cex = par("cex"),
           axes = TRUE, frame.plot = axes, \dots)
}
\arguments{
  \item{x}{the data from which the plots are to be produced.  In the
    default method the data can be specified as a single numeric
    vector, or as list of numeric vectors, each corresponding to
    a component plot.  In the \code{formula} method, a symbolic
    specification of the form \code{y ~ g} can be given,
    indicating the observations in the vector \code{y} are to be
    grouped according to the levels of the factor
    \code{g}.  \code{NA}s are allowed in the data.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{x} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is to ignore missing
    values in either the response or the group.}
  \item{\dots}{additional parameters passed to the default method, or by
    it to \code{\link{plot.window}}, \code{\link{points}},
    \code{\link{axis}} and \code{\link{title}} to control the appearance
    of the plot.} 
  \item{method}{the method to be used to separate coincident points.
    The default method \code{"overplot"} causes such points to be
    overplotted, but it is also possible to specify \code{"jitter"} to
    jitter the points, or \code{"stack"} have coincident points
    stacked.  The last method only makes sense for very granular data.}
  \item{jitter}{when \code{method = "jitter"} is used, \code{jitter}
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
  \item{ylab, xlab}{labels: see \code{\link{title}}.}
  \item{dlab, glab}{alternate way to specify axis labels: see \sQuote{Details}.}
  \item{xlim, ylim}{plot limits: see \code{\link{plot.window}}.}
  \item{log}{on which axes to use a log scale:  see
    \code{\link{plot.default}}}
  \item{pch, col, cex}{Graphical parameters: see \code{\link{par}}.}
  \item{axes, frame.plot}{Axis control:  see \code{\link{plot.default}}.}
}
\details{
  Extensive examples of the use of this kind of plot can be found in
  Box, Hunter and Hunter or Seber and Wild.

  The \code{dlab} and \code{glab} labels may be used instead of \code{xlab}
  and \code{ylab} if those are not specified.  \code{dlab} applies to the
  continuous data axis (the X axis unless \code{vertical} is \code{TRUE}),
  \code{glab} to the group axis.
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

stripchart(decrease ~ treatment,
    main = "stripchart(OrchardSprays)",
    vertical = TRUE, log = "y", data = OrchardSprays)

stripchart(decrease ~ treatment, at = c(1:8)^2,
    main = "stripchart(OrchardSprays)",
    vertical = TRUE, log = "y", data = OrchardSprays)
}
\keyword{hplot}
