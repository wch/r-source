\name{boxplot.default}
\alias{boxplot.default}
\title{Box Plots}
\usage{
boxplot.default(x, \dots, range = 1.5, width = NULL, varwidth = FALSE,
        notch = FALSE, names.x, data = sys.frame(sys.parent()),
        plot = TRUE, border = par("fg"), col = NULL, log = "",
        pars = NULL)
}
\arguments{
  \item{x, \dots}{the data from which the boxplots are to be produced.
    The data can be specified as separate vectors, each
    corresponding to a component boxplot, or as a single list
    containing such vectors.  Alternatively a symbolic specification
    of the form \code{x ~ g} can be given, indicating that the
    observations in the vector \code{x} are to be grouped according
    to the levels of the \code{\link{factor}} \code{g}.  In this
    case the argument \code{data} can be used to provide values for
    the variables in the specification. \code{\link{NA}}s are
    allowed in the data.}
  \item{range}{this determines how far the plot whiskers extend out
    from the box.  If \code{range} is positive, the whiskers extend
    to the most extreme data point which is no more than
    \code{range} times the interquartile range from the box. A value
    of zero causes the whiskers to extend to the data extremes.}
  \item{width}{a vector giving the relative widths of the boxes making
    up the plot.}
  \item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are
    drawn with widths proportional to the square-roots of the number
    of observations in the groups.}
  \item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in
    each side of the boxes.  If the notches of two plots do not
    overlap then the medians are significantly different at the 5
    percent level.}
  \item{names}{group labels which will be printed under each boxplot.}
  \item{data}{\code{\link{data.frame}}, \code{\link{list}}, or
    \code{\link{environment}} in which variable names are evaluated
    when \code{x} is a formula.}
  \item{plot}{if \code{TRUE} (the default) then a boxplot is
    produced.  If not, the summaries which the boxplots are based on
    are returned.}
  \item{border}{an optional vector of colors for the outlines of the
    boxplots.  The values in \code{border} are recycled if the
    length of \code{border} is less than the number of plots.}
  \item{col}{if \code{col} is non-null it is assumed to contain colors
    to be used to col the bodies of the box plots.}
  \item{log}{character indicating if x or y or both coordinates should
    be plotted in log scale.}
  \item{pars, \dots}{graphical parameters can also be passed as
    arguments to \code{boxplot}.}
}
\description{
  Produce box-and-whisker plot(s) of the given (grouped) values.
}
\value{
  List with one component corresponding to each plot.  The
  components are themselves lists with named components as follows:
  \item{stats}{a vector containing the extreme of the lower whisker,
    the lower hinge, the median, the upper hinge and the extreme of
    the upper whisker.}
  \item{n}{the number of observations in the sample.}
  \item{conf}{the lower and upper extremes of the notch.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers.}
}
\seealso{
  \code{\link{boxplot.stats}} which does the computation,
  \code{\link{bxp}} for the plotting, and \code{\link{stripplot}} for an
  alternative (with small data sets).

  \code{\link{plot.default}} for some high-level plotting parameters.
}
\examples{
data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

data(OrchardSprays)
boxplot(decrease ~ treatment, data = OrchardSprays, log = "y", col="bisque")

## boxplot(.) for a matrix:
mat <- cbind(Uni05= (1:100)/21, Norm= rnorm(100),
             T5 = rt(100,df=5), Gam2= rgamma(100, shape=2))
boxplot(data.frame(mat), main= "boxplot(data.frame(mat), main=...)")
}
\keyword{hplot}
