% File src/library/graphics/man/hist.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{hist}
\title{Histograms}
\usage{
hist(x, \dots)

\method{hist}{default}(x, breaks = "Sturges",
     freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = NULL, border = NULL,
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, warn.unused = TRUE, \dots)
}
\alias{hist}
\alias{hist.default}
\arguments{
  \item{x}{a vector of values for which the histogram is desired.}
  \item{breaks}{one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a function to compute the vector of breakpoints,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the
      number of cells (see \sQuote{Details}),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only; as the
    breakpoints will be set to \code{\link{pretty}} values, the number
    is limited to \code{1e6} (with a warning if it was larger).  If
    \code{breaks} is a function, the \code{x} vector is supplied to it
    as the only argument (and the number of breaks is only limited by
    the amount of available memory).
  }
  \item{freq}{logical; if \code{TRUE}, the histogram graphic is a
    representation of frequencies, the \code{counts} component of
    the result; if \code{FALSE}, probability densities, component
    \code{density}, are plotted (so that the histogram has a total area
    of one).  Defaults to \code{TRUE} \emph{if and only if} \code{breaks} are
    equidistant (and \code{probability} is not specified).}
  \item{probability}{an \emph{alias} for \code{!freq}, for S compatibility.}
  \item{include.lowest}{logical; if \code{TRUE}, an \code{x[i]} equal to
    the \code{breaks} value will be included in the first (or last, for
    \code{right = FALSE}) bar.  This will be ignored (with a warning)
    unless \code{breaks} is a vector.}
  \item{right}{logical; if \code{TRUE}, the histogram cells are
    right-closed (left open) intervals.}

  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}
  \item{col}{a colour to be used to fill the bars.
    The default of \code{NULL} yields unfilled bars.}
  \item{border}{the color of the border around the bars.  The default
    is to use the standard foreground color.}
  \item{main, xlab, ylab}{main title and axis labels: these arguments to
    \code{\link{title}()} get \dQuote{smart} defaults here, e.g., the default
    \code{ylab} is \code{"Frequency"} iff \code{freq} is true.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.
    Note that \code{xlim} is \emph{not} used to define the histogram (breaks),
    but only for plotting (when \code{plot = TRUE}).}
  \item{axes}{logical.  If \code{TRUE} (default), axes are draw if the
    plot is drawn.}
  \item{plot}{logical.  If \code{TRUE} (default), a histogram is
    plotted, otherwise a list of breaks and counts is returned.  In the
    latter case, a warning is used if (typically graphical) arguments
    are specified that only apply to the \code{plot = TRUE} case.}
  \item{labels}{logical or character string.  Additionally draw labels on top
    of bars, if not \code{FALSE}; see \code{\link{plot.histogram}}.}
  \item{nclass}{numeric (integer).  For S(-PLUS) compatibility only,
    \code{nclass} is equivalent to \code{breaks} for a scalar or
    character argument.}
  \item{warn.unused}{logical.  If \code{plot = FALSE} and
    \code{warn.unused = TRUE}, a warning will be issued when graphical
    parameters are passed to \code{hist.default()}.}
  \item{\dots}{further arguments and \link{graphical parameters} passed to
    \code{\link{plot.histogram}} and thence to \code{\link{title}} and
    \code{\link{axis}} (if \code{plot = TRUE}).}
}
\description{
  The generic function \code{hist} computes a histogram of the given
  data values.  If \code{plot = TRUE}, the resulting object of
  \link{class} \code{"histogram"} is plotted by
  \code{\link{plot.histogram}}, before it is returned.
}
\details{
  The definition of \emph{histogram} differs by source (with
  country-specific biases).  \R's default with equi-spaced breaks (also
  the default) is to plot the counts in the cells defined by
  \code{breaks}.  Thus the height of a rectangle is proportional to
  the number of points falling into the cell, as is the area
  \emph{provided} the breaks are equally-spaced.

  The default with non-equi-spaced breaks is to give
  a plot of area one, in which the \emph{area} of the rectangles is the
  fraction of the data points falling in the cells.

  If \code{right = TRUE} (default), the histogram cells are intervals
  of the form \code{(a, b]}, i.e., they include their right-hand endpoint,
  but not their left one, with the exception of the first cell when
  \code{include.lowest} is \code{TRUE}.

  For \code{right = FALSE}, the intervals are of the form \code{[a, b)},
  and \code{include.lowest} means \sQuote{\emph{include highest}}.

  A numerical tolerance of \eqn{10^{-7}}{1e-7} times the median bin size
  (for more than four bins, otherwise the median is substituted) is
  applied when counting entries on the edges of bins.  This is not
  included in the reported \code{breaks} nor in the calculation of
  \code{density}.

  The default for \code{breaks} is \code{"Sturges"}: see
  \code{\link{nclass.Sturges}}.  Other names for which algorithms
  are supplied are \code{"Scott"} and \code{"FD"} /
  \code{"Freedman-Diaconis"} (with corresponding functions
  \code{\link{nclass.scott}} and \code{\link{nclass.FD}}).
  Case is ignored and partial matching is used.
  Alternatively, a function can be supplied which
  will compute the intended number of breaks or the actual breakpoints
  as a function of \code{x}.
}
\value{
  an object of class \code{"histogram"} which is a list with components:
  \item{breaks}{the \eqn{n+1} cell boundaries (= \code{breaks} if that
    was a vector). These are the nominal breaks, not with the boundary fuzz.}
  \item{counts}{\eqn{n} integers; for each cell, the number of
    \code{x[]} inside.}
  \item{density}{values \eqn{\hat f(x_i)}{f^(x[i])}, as estimated
    density values. If \code{all(diff(breaks) == 1)}, they are the
    relative frequencies \code{counts/n} and in general satisfy
    \eqn{\sum_i \hat f(x_i) (b_{i+1}-b_i) = 1}{sum[i; f^(x[i])
      (b[i+1]-b[i])] = 1}, where \eqn{b_i}{b[i]} = \code{breaks[i]}.}
  \item{mids}{the \eqn{n} cell midpoints.}
  \item{xname}{a character string with the actual \code{x} argument name.}
  \item{equidist}{logical, indicating if the distances between
    \code{breaks} are all the same.}
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Venables, W. N. and Ripley. B. D. (2002)
  \emph{Modern Applied Statistics with S}.  Springer.
}

\seealso{
  \code{\link{nclass.Sturges}}, \code{\link{stem}},
  \code{\link{density}},  \code{\link[MASS]{truehist}} in package
  \CRANpkg{MASS}.

  Typical plots with vertical bars are \emph{not} histograms.  Consider
  \code{\link{barplot}} or \code{\link{plot}(*, type = "h")}
  for such bar plots.
}

\examples{
op <- par(mfrow = c(2, 2))
hist(islands)
utils::str(hist(islands, col = "gray", labels = TRUE))

hist(sqrt(islands), breaks = 12, col = "lightblue", border = "pink")
##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
r <- hist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140),
          col = "blue1")
text(r$mids, r$density, r$counts, adj = c(.5, -.5), col = "blue3")
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
lines(r, lty = 3, border = "purple") # -> lines.histogram(*)
par(op)

require(utils) # for str
str(hist(islands, breaks = 12, plot =  FALSE)) #-> 10 (~= 12) breaks
str(hist(islands, breaks = c(12,20,36,80,200,1000,17000), plot = FALSE))

hist(islands, breaks = c(12,20,36,80,200,1000,17000), freq = TRUE,
     main = "WRONG histogram") # and warning
\donttest{% save 2 seconds
## Extreme outliers; the "FD" rule would take very large number of 'breaks':
XXL <- c(1:9, c(-1,1)*1e300)
hh <- hist(XXL, "FD") # did not work in R <= 3.4.1; now gives warning
## pretty() determines how many counts are used (platform dependently!):
length(hh$breaks) ## typically 1 million -- though 1e6 was "a suggestion only"
}
require(stats)
set.seed(14)
x <- rchisq(100, df = 4)
\dontshow{op <- par(mfrow = 2:1, mgp = c(1.5, 0.6, 0), mar = .1 + c(3,3:1))}
## Comparing data with a model distribution should be done with qqplot()!
qqplot(x, qchisq(ppoints(x), df = 4)); abline(0, 1, col = 2, lty = 2)

## if you really insist on using hist() ... :
hist(x, freq = FALSE, ylim = c(0, 0.2))
curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)
\dontshow{par(op)}
}
\keyword{dplot}
\keyword{hplot}
\keyword{distribution}
