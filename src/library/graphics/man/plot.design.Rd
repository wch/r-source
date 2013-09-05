% File src/library/graphics/man/plot.design.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{plot.design}
\alias{plot.design}
\title{Plot Univariate Effects of a Design or Model}
\description{
  Plot univariate effects of one or more \code{\link{factor}}s,
  typically for a designed experiment as analyzed by \code{\link{aov}()}.
}
\usage{
plot.design(x, y = NULL, fun = mean, data = NULL, \dots,
            ylim = NULL, xlab = "Factors", ylab = NULL,
            main = NULL, ask = NULL, xaxt = par("xaxt"),
            axes = TRUE, xtick = FALSE)
}
\arguments{
  \item{x}{either a data frame containing the design factors and
    optionally the response, or a \code{\link{formula}} or
    \code{\link{terms}} object.}
  \item{y}{the response, if not given in x.}
  \item{fun}{a function (or name of one) to be applied to each
    subset.  It must return one number for a numeric (vector) input.}
  \item{data}{data frame containing the variables referenced by \code{x}
    when that is formula-like.}
  \item{\dots}{\link{graphical parameters} such as \code{col},
    see \code{\link{par}}.}
  \item{ylim}{range of y values, as in \code{\link{plot.default}}.}
  \item{xlab}{x axis label, see \code{\link{title}}.}
  \item{ylab}{y axis label with a \sQuote{smart} default.}
  \item{main}{main title, see \code{\link{title}}.}
  \item{ask}{logical indicating if the user should be asked before a new
    page is started -- in the case of multiple y's.}
  \item{xaxt}{character giving the type of x axis.}
  \item{axes}{logical indicating if axes should be drawn.}
  \item{xtick}{logical indicating if ticks (one per factor) should
    be drawn on the x axis.}
}
\details{
  The supplied function will be called once for each level of each
  factor in the design and the plot will show these summary values.  The
  levels of a particular factor are shown along a vertical line, and the
  overall value of \code{fun()} for the response is drawn as a
  horizontal line.
}
\references{
  Chambers, J. M. and Hastie, T. J. eds (1992)
  \emph{Statistical Models in S}.
  Chapman & Hall, London, \bold{the} \emph{white book}, pp. 546--7 (and 163--4).

  Freeny, A. E. and Landwehr, J. M. (1990)
  Displays for data from large designed experiments;
  Computer Science and Statistics: Proc.\ 22nd Symp\. Interface, 117--126,
  Springer Verlag.
}
\author{Roberto Frisullo and Martin Maechler}
\note{
  A big effort was taken to make this closely compatible to the S
  version.  However, \code{col} (and \code{fg}) specifications have
  different effects.

  In S this was a method of the \code{\link{plot}} generic function for
  \code{design} objects.
}
\seealso{\code{\link{interaction.plot}} for a \sQuote{standard graphic}
  of designed experiments.}
\examples{
require(stats)
plot.design(warpbreaks)  # automatic for data frame with one numeric var.

Form <- breaks ~ wool + tension
summary(fm1 <- aov(Form, data = warpbreaks))
plot.design(       Form, data = warpbreaks, col = 2)  # same as above

## More than one y :
utils::str(esoph)
plot.design(esoph) ## two plots; if interactive you are "ask"ed

## or rather, compare mean and median:
op <- par(mfcol = 1:2)
plot.design(ncases/ncontrols ~ ., data = esoph, ylim = c(0, 0.8))
plot.design(ncases/ncontrols ~ ., data = esoph, ylim = c(0, 0.8),
            fun = median)
par(op)
}
\keyword{hplot}

