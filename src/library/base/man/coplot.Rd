\name{coplot}
\title{Conditioning Plots}
\usage{
coplot(formula, data, given.values, panel = points, rows, columns,
       show.given = TRUE, col = par("fg"), pch = par("pch"), ...)
co.intervals(x, number = 6, overlap = 0.5)
}
\alias{coplot}
\alias{co.intervals}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | a} indicates that plots of
    \code{y} versus \code{x} should be produced conditional on the
    variable \code{a}.  A formula of the form \code{y ~ x| a * b}
    indicates that plots of \code{y} versus \code{x} should be produced
    conditional on the two variables \code{a} and \code{b}.

    \code{x} and lang(y) must be numeric, but \code{a} and \code{b} may
    be either numeric or factors.}
  \item{data}{a data frame containing values for any variables in the
    formula.  By default the environment where \code{coplot} was called
    from is used.}
  \item{given.values}{a value or list of two values which determine how
    the conditioning on \code{a} and \code{b} is to take place.  Usually
    this is a matrix with two columns each row of which gives an
    interval, to be conditioned on, but is can also be a single vector
    of numbers or a set of factor levels (if the variable being
    conditioned on is a factor).

    The result of \code{co.intervals(..)} can be used directly as
    \code{given.values} argument.}
  \item{panel}{a function which gives the action to be carried out in
    each panel of the display.  The default is \code{points}.}
  \item{rows}{the panels of the plot are laid out in a \code{rows} by
    \code{columns} array.  \code{rows} gives the number of rows in the
    array.}
  \item{columns}{the number of columns in the panel layout array.}
  \item{show.given}{should conditioning plots be shown for the
    conditioning variables (default \code{TRUE})}.
  \item{col}{a vector of colors to be used to plot the points.  If too
    short, the values are recycled.}
  \item{pch}{a vector of plotting symbols or characters.  If too short,
    the values are recycyled.}
  \item{...}{additional arguments to the panel function.}
  \item{number}{the number conditioning intervals.}
  \item{overlap}{the fraction of overlap of the conditioning variables.}
}
\description{
  This function produces two variants of the conditioning plots
  discussed in the reference below.
}
\references{
  Cleveland, W. S. (1993).
  \emph{Visualizing Data}.
  New Jersey: Summit Press.
}
\seealso{
  \code{\link{pairs}},
  \code{\link{panel.smooth}},
  \code{\link{points}}.
}
\examples{
## Tonga Trench Earthquakes
data(quakes)
coplot(long ~ lat | depth, data = quakes)
coplot(long ~ lat | depth * mag, data = quakes)
}
\keyword{hplot}
\keyword{aplot}
