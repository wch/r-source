\name{coplot}
\title{Conditioning Plots}
\usage{
coplot(formula, data, given.values, panel = points, rows, columns,
       show.given = TRUE, col = par("fg"), pch = par("pch"), 
       xlab = c(x.name, paste("Given :", a.name)),
       ylab = c(y.name, paste("Given :", b.name)),
       subscripts = FALSE,
       number = 6, overlap = 0.5, xlim, ylim, \dots) 
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

    \code{x} and \code{y} must be numeric, but \code{a} and \code{b} may
    be either numeric or factors.}
  \item{data}{a data frame containing values for any variables in the
    formula.  By default the environment where \code{coplot} was called
    from is used.}
  \item{given.values}{a value or list of two values which determine how
    the conditioning on \code{a} and \code{b} is to take place.

    When there is no \code{b} (i.e., conditioning only on \code{a}),
    usually this is a matrix with two columns each row of which gives an
    interval, to be conditioned on, but is can also be a single vector
    of numbers or a set of factor levels (if the variable being
    conditioned on is a factor). In this case (no \code{b}),
    the result of \code{co.intervals(..)} can be used directly as
    \code{given.values} argument.
  }
  \item{panel}{a \code{\link{function}(x, y, col, pch, \dots)}
      which gives the action to be carried out in
      each panel of the display.  The default is \code{points}.}
  \item{rows}{the panels of the plot are laid out in a \code{rows} by
    \code{columns} array.  \code{rows} gives the number of rows in the
    array.}
  \item{columns}{the number of columns in the panel layout array.}
  \item{show.given}{logical (possibly of length 2 for 2 conditioning
      variables): should conditioning plots be shown for the
    corresponding conditioning variables (default \code{TRUE})}.
  \item{col}{a vector of colors to be used to plot the points.  If too
    short, the values are recycled.}
  \item{pch}{a vector of plotting symbols or characters.  If too short,
    the values are recycled.}
  \item{xlab}{character; labels to use for the x axis and the
    first conditioning variable.  If only one label is given, it is used
    for the x axis and the default label is used for the conditioning
    variable.}
  \item{ylab}{character; labels to use for the y axis and any
    second conditioning variable.}
  \item{subscripts}{logical: if true the panel function is given an
    additional (third) argument \code{subscripts} giving the subscripts
    of the data passed to that panel.}
  \item{number}{integer; the number of conditioning intervals,
    possibly of length 2 for x and y direction.}
  \item{overlap}{numeric < 1; the fraction of overlap of the
    conditioning variables, possibly of length 2 for x and y
    direction.  When overlap < 0, there will be \emph{gaps} between
    the data slices.}
  \item{xlim}{the range for the x axis.}
  \item{ylim}{the range for the y axis.}
  \item{\dots}{additional arguments to the panel function.}
}
\description{
  This function produces two variants of the \bold{co}nditioning plots
  discussed in the reference below.
}
\details{
  In the case of a single conditioning variable \code{a}, when both \code{rows}
  and \code{columns} are unspecified, a ``close to square'' layout is
  chosen with \code{columns >= rows}.

  In the case of multiple \code{rows}, the \emph{order} of the panel
  plots is from the bottom and from the left (corresponding to
  increasing \code{a}, typically).
}
\value{
  \code{co.intervals(., number, .)} returns a (\code{number} \eqn{\times}{x}
  2) \code{\link{matrix}}, say \code{ci},  where \code{ci[k,]} is
  the \code{\link{range}} of \code{x} values for the \code{k}-th interval.
}
\references{
  Cleveland, W. S. (1993) \emph{Visualizing Data.} New Jersey: Summit Press.
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
given.depth <- co.intervals(quakes$depth, number=4, overlap=.1)
coplot(long ~ lat | depth, data = quakes, given.v=given.depth, rows=1)

## Conditioning on 2 variables:
ll.dm <- long ~ lat | depth * mag
coplot(ll.dm, data = quakes)
coplot(ll.dm, data = quakes, number=c(4,7), show.given = c(T,F))
coplot(ll.dm, data = quakes, number=c(3,7),
       overlap=c(-.5,.1)) # negative overlap DROPS values

data(warpbreaks)
## given two factors
coplot(breaks ~ 1:54 | wool * tension, data = warpbreaks, col = 'red')

## Example with empty panels:
data(state)
attach(data.frame(state.x77))#> don't need `data' arg. below
coplot(Life.Exp ~ Income | Illiteracy * state.region, number = 3,
       panel = function(x, y, ...) panel.smooth(x, y, span = .8, ...))
detach() # data.frame(state.x77)
}
\keyword{hplot}
\keyword{aplot}
