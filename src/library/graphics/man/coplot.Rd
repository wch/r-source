\name{coplot}
\title{Conditioning Plots}
\usage{
coplot(formula, data, given.values, panel = points, rows, columns,
       show.given = TRUE, col = par("fg"), pch = par("pch"), 
       bar.bg = c(num = gray(0.8), fac = gray(0.95)),
       xlab = c(x.name, paste("Given :", a.name)),
       ylab = c(y.name, paste("Given :", b.name)),
       subscripts = FALSE,
       axlabels = function(f) abbreviate(levels(f)),
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

    All three or four variables may be either numeric or factors.
    When \code{x} or \code{y} are factors, the result is almost as if
    \code{as.numeric()} was applied, whereas for factor
    \code{a} or \code{b}, the conditioning (and its graphics if
    \code{show.given} is true) are adapted.
  }
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
    the result of \code{co.intervals} can be used directly as
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
  \item{bar.bg}{a named vector with components \code{"num"} and
    \code{"fac"} giving the background colors for the (shingle) bars,
    for \bold{num}eric and \bold{fac}tor conditioning variables respectively.}
  \item{xlab}{character; labels to use for the x axis and the
    first conditioning variable.  If only one label is given, it is used
    for the x axis and the default label is used for the conditioning
    variable.}
  \item{ylab}{character; labels to use for the y axis and any
    second conditioning variable.}
  \item{subscripts}{logical: if true the panel function is given an
    additional (third) argument \code{subscripts} giving the subscripts
    of the data passed to that panel.}
  \item{axlabels}{function for creating axis (tick) labels when x or y
      are factors.}
  \item{number}{integer; the number of conditioning intervals,
    for a and b, possibly of length 2.  It is only used if the
    corresponding conditioning variable is not a \code{\link{factor}}.}
  \item{overlap}{numeric < 1; the fraction of overlap of the
    conditioning variables, possibly of length 2 for x and y
    direction.  When overlap < 0, there will be \emph{gaps} between
    the data slices.}
  \item{xlim}{the range for the x axis.}
  \item{ylim}{the range for the y axis.}
  \item{\dots}{additional arguments to the panel function.}

  \item{x}{a numeric vector.}
}
\description{
  This function produces two variants of the \bold{co}nditioning plots
  discussed in the reference below.
}
\details{
  In the case of a single conditioning variable \code{a}, when both
  \code{rows} and \code{columns} are unspecified, a \dQuote{close to
    square} layout is chosen with \code{columns >= rows}.

  In the case of multiple \code{rows}, the \emph{order} of the panel
  plots is from the bottom and from the left (corresponding to
  increasing \code{a}, typically).

  A panel function should not attempt to start a new plot, but just plot
  within a given coordinate system: thus \code{plot} and \code{boxplot}
  are not panel functions.

  The rendering of arguments \code{xlab} and
  \code{ylab} is not controlled by \code{\link{par}} arguments
  \code{cex.lab} and \code{font.lab} even though they are plotted by
  \code{\link{mtext}} rather than \code{\link{title}}.
}
\value{
  \code{co.intervals(., number, .)} returns a (\code{number} \eqn{\times}{x}
  2) \code{\link{matrix}}, say \code{ci},  where \code{ci[k,]} is
  the \code{\link{range}} of \code{x} values for the \code{k}-th interval.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

  Cleveland, W. S. (1993) \emph{Visualizing Data.} New Jersey: Summit Press.
}
\seealso{
  \code{\link{pairs}},
  \code{\link{panel.smooth}},
  \code{\link{points}}.
}
\examples{
## Tonga Trench Earthquakes
coplot(lat ~ long | depth, data = quakes)
given.depth <- co.intervals(quakes$depth, number=4, overlap=.1)
coplot(lat ~ long | depth, data = quakes, given.v=given.depth, rows=1)

## Conditioning on 2 variables:
ll.dm <- lat ~ long | depth * mag
coplot(ll.dm, data = quakes)
coplot(ll.dm, data = quakes, number=c(4,7), show.given=c(TRUE,FALSE))
coplot(ll.dm, data = quakes, number=c(3,7),
       overlap=c(-.5,.1)) # negative overlap DROPS values

## given two factors
Index <- seq(length=nrow(warpbreaks)) # to get nicer default labels
coplot(breaks ~ Index | wool * tension, data = warpbreaks, show.given = 0:1)
coplot(breaks ~ Index | wool * tension, data = warpbreaks,
       col = "red", bg = "pink", pch = 21, bar.bg = c(fac = "light blue"))

## Example with empty panels:
with(data.frame(state.x77), {
coplot(Life.Exp ~ Income | Illiteracy * state.region, number = 3,
       panel = function(x, y, ...) panel.smooth(x, y, span = .8, ...))
## y ~ factor -- not really sensical, but 'show off':
coplot(Life.Exp ~ state.region | Income * state.division,
       panel = panel.smooth)
})
}
\keyword{hplot}
\keyword{aplot}
