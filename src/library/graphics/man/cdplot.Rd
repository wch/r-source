\name{cdplot}
\alias{cdplot}
\alias{cdplot.default}
\alias{cdplot.formula}
\title{Conditional Density Plots}
\description{
  Computes and plots conditional densities describing how the
  conditional distribution of a categorical variable \code{y} changes over a
  numerical variable \code{x}.
}
\usage{
cdplot(x, \dots)

\method{cdplot}{default}(x, y,
  plot = TRUE, tol.ylab = 0.05,
  bw = "nrd0", n = 512, from = NULL, to = NULL,
  col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
  yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), \dots)

\method{cdplot}{formula}(formula, data = list(),
  plot = TRUE, tol.ylab = 0.05,
  bw = "nrd0", n = 512, from = NULL, to = NULL,
  col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
  yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), \dots,
  subset = NULL)
}
\arguments{
  \item{x}{an object, the default method expects either a single numerical variable.}
  \item{y}{a \code{"factor"} interpreted to be the dependent variable}
  \item{formula}{a \code{"formula"} of type \code{y ~ x} with a single dependent
    \code{"factor"} and a single numerical explanatory variable.}    
  \item{data}{an optional data frame.}
  \item{plot}{logical. Should the computed conditional densities be plotted?}
  \item{tol.ylab}{convenience tolerance parameter for y-axis annotation.
    If the distance between two labels drops under this threshold, they are
    plotted equidistantly.}
  \item{bw, n, from, to, \dots}{arguments passed to \code{\link{density}}}
  \item{col}{a vector of fill colors of the same length as \code{levels(y)}.
    The default is to call \code{\link{gray.colors}}.}  
  \item{border}{border color of shaded polygons.}  
  \item{main, xlab, ylab}{character strings for annotation}
  \item{yaxlabels}{character vector for annotation of y axis, defaults to
    \code{levels(y)}.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
}
\details{
  \code{cdplot} computes the conditional densities of \code{x} given
  the levels of \code{y} weighted by the marginal distribution of \code{y}.
  The densities are derived cumulatively over the levels of \code{y}.

  This visualization technique is similar to spinograms (see \code{\link{spineplot}})
  and plots \eqn{P(y | x)} against \eqn{x}. The conditional probabilities
  are not derived by discretization (as in the spinogram), but using a smoothing
  approach via \code{\link{density}}.
  
  Note, that the estimates of the conditional densities are more reliable for 
  high-density regions of \eqn{x}. Conversely, the are less reliable in regions
  with only few \eqn{x} observations.
}
\value{
  The conditional density functions (cumulative over the levels of \code{y})
  are returned invisibly.
}
\seealso{
  \code{\link{spineplot}}, \code{\link{density}}
}
\references{
  Hofmann, H., Theus, M. (2005), \emph{Interactive graphics for visualizing
  conditional distributions}, Unpublished Manuscript.
}
\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}
}
\examples{
## NASA space shuttle o-ring failures
fail <- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1),
               levels = 1:2, labels = c("no", "yes"))
temperature <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 72, 73, 75,
                 75, 76, 76, 78, 79, 81)

## CD plot
cdplot(fail ~ temperature)
cdplot(fail ~ temperature, bw = 2)
cdplot(fail ~ temperature, bw = "SJ")

## compare with spinogram
(spineplot(fail ~ temperature, breaks = 3))

## scatter plot with conditional density
cdens <- cdplot(fail ~ temperature, plot = FALSE)
plot(I(as.numeric(fail) - 1) ~ jitter(temperature, factor = 2),
     xlab = "Temperature", ylab = "Conditional failure probability")
lines(53:81, 1 - cdens[[1]](53:81), col = 2)
}
\keyword{hplot}
