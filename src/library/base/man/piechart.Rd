\name{piechart}
\title{Pie Charts}
\alias{piechart}
\description{
  Draw a pie chart.
}
\usage{
piechart(x, labels = names(x), edges = 200, radius = 0.8, col = NULL,
         main = NULL, \dots)
}
\arguments{
  \item{x}{a vector of positive quantities.
    The values in \code{x} are displayed as the areas of pie slices.}
  \item{labels}{a vector of character strings giving names for the
    slices.}
  \item{edges}{the circular outline of the pie is approximated by a
    polygon with this many edges.}
  \item{radius}{the pie is drawn centered in a square box whose sides
    range from \eqn{-1} to \eqn{1}.  If the character strings labeling
    the slices are long it may be necessary to use a smaller radius.}
  \item{col}{a vector of colors to be used in filling the slices.}
  \item{main}{an overall title for the plot.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{piechart}.}
}
\note{
  Pie charts are a very bad way of displaying information.
  The eye is good at judging linear measures and bad at judging
  relative areas.

  A bar chart or dot chart is a preferable way of displaying this
  type of data.
}
\seealso{
  \code{\link{dotchart}}.
}
\examples{
piechart(rep(1,24), col=rainbow(24), radius=0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")
piechart(pie.sales,
    col=c("purple", "violetred1", "green3",
    "cornsilk", "cyan", "white"))
piechart(pie.sales,
    col=gray(seq(0.4,1.0,length=6)))
}
\keyword{hplot}
