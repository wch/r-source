\name{hist}
\title{Histograms}
\usage{
hist(x, \dots)

\method{hist}{default}(x, breaks = "Sturges", freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = NULL, border = NULL,
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, \dots)
}
\alias{hist}
\alias{hist.default}
\arguments{
  \item{x}{a vector of values for which the histogram is desired.}
  \item{breaks}{one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the
      number of cells (see Details),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only.
  }
  \item{freq}{logical; if \code{TRUE}, the histogram graphic is a
    representation of frequencies, the \code{counts} component of
    the result; if \code{FALSE}, \emph{relative} frequencies
    (\dQuote{probabilities}), component \code{density},
    are plotted.   Defaults to \code{TRUE} \emph{iff} \code{breaks} are
    equidistant (and \code{probability} is not specified).}
  \item{probability}{an \emph{alias} for \code{!freq}, for S compatibility.}
  \item{include.lowest}{logical; if \code{TRUE}, an \code{x[i]} equal to
    the \code{breaks} value will be included in the first (or last, for
    \code{right = FALSE}) bar.  This will be ignored (with a warning)
    unless \code{breaks} is a vector.}
  \item{right}{logical; if \code{TRUE}, the histograms cells are
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
  \item{main, xlab, ylab}{these arguments to \code{title} have useful
    defaults here.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.
    Note that \code{xlim} is \emph{not} used to define the histogram (breaks),
    but only for plotting (when \code{plot = TRUE}).}
  \item{axes}{logical.  If \code{TRUE} (default), axes are draw if the
    plot is drawn.}
  \item{plot}{logical.  If \code{TRUE} (default), a histogram is
    plotted, otherwise a list of breaks and counts is returned.}
  \item{labels}{logical or character.  Additionally draw labels on top
    of bars, if not \code{FALSE}; see \code{\link{plot.histogram}}.}
  \item{nclass}{numeric (integer).  For S(-PLUS) compatibility only,
    \code{nclass} is equivalent to \code{breaks} for a scalar or
    character argument.}
  \item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\description{
  The generic function \code{hist} computes a histogram of the given
  data values.  If \code{plot=TRUE}, the resulting object of
  \code{\link[base]{class} "histogram"} is plotted by
  \code{\link{plot.histogram}}, before it is returned.
}
\details{
  The definition of \dQuote{histogram} differs by source (with
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
  and \code{include.lowest} really has the meaning of
  \dQuote{\emph{include highest}}.

  A numerical tolerance of \eqn{10^{-7}}{1e-7} times the median bin size
  is applied when counting entries on the edges of bins.

  The default for \code{breaks} is \code{"Sturges"}: see
  \code{\link{nclass.Sturges}}.  Other names for which algorithms
  are supplied are \code{"Scott"} and \code{"FD"} /
  \code{"Friedman-Diaconis"} (with corresponding functions
  \code{\link{nclass.scott}} and \code{\link{nclass.FD}}).
  Case is ignored and partial matching is used.
  Alternatively, a function can be supplied which
  will compute the intended number of breaks as a function of \code{x}.
}
\value{
  an object of class \code{"histogram"} which is a list with components:
  \item{breaks}{the \eqn{n+1} cell boundaries (= \code{breaks} if that
  was a vector).}
  \item{counts}{\eqn{n} integers; for each cell, the number of
    \code{x[]} inside.}
  \item{density}{values \eqn{\hat f(x_i)}{f^(x[i])}, as estimated
    density values. If \code{all(diff(breaks) == 1)}, they are the
    relative frequencies \code{counts/n} and in general satisfy
    \eqn{\sum_i \hat f(x_i) (b_{i+1}-b_i) = 1}{sum[i; f^(x[i])
      (b[i+1]-b[i])] = 1}, where \eqn{b_i}{b[i]} = \code{breaks[i]}.}
  \item{intensities}{same as \code{density}. Deprecated, but retained
    for compatibility.}
  \item{mids}{the \eqn{n} cell midpoints.}
  \item{xname}{a character string with the actual \code{x} argument name.}
  \item{equidist}{logical, indicating if the distances between
    \code{breaks} are all the same.}
}
\note{
  The resulting value does \emph{not} depend on the values of
  the arguments \code{freq} (or \code{probability})
  or \code{plot}.  This is intentionally different from S.

  Prior to \R 1.7.0, the element \code{breaks} of the result was
  adjusted for numerical tolerances.  The nominal values are now
  returned even though tolerances are still used when counting.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Venables, W. N. and Ripley. B. D. (2002)
  \emph{Modern Applied Statistics with S}.  Springer.
}

\seealso{
  \code{\link{nclass.Sturges}}, \code{\link{stem}},
  \code{\link{density}},  \code{\link[MASS]{truehist}}.
}

\examples{
op <- par(mfrow=c(2, 2))
hist(islands)
utils::str(hist(islands, col="gray", labels = TRUE))

hist(sqrt(islands), br = 12, col="lightblue", border="pink")
##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
r <- hist(sqrt(islands), br = c(4*0:5, 10*3:5, 70, 100, 140), col='blue1')
text(r$mids, r$density, r$counts, adj=c(.5, -.5), col='blue3')
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
lines(r, lty = 3, border = "purple") # -> lines.histogram(*)
par(op)

utils::str(hist(islands, br=12, plot= FALSE)) #-> 10 (~= 12) breaks
utils::str(hist(islands, br=c(12,20,36,80,200,1000,17000), plot = FALSE))

hist(islands, br=c(12,20,36,80,200,1000,17000), freq = TRUE,
     main = "WRONG histogram") # and warning
}
\keyword{dplot}
\keyword{hplot}
\keyword{distribution}
