\name{pie}
\title{Pie Charts}
\alias{pie}
\alias{old-piechart}
\description{
  Draw a pie chart.
}
\usage{
pie(x, labels = names(x), edges = 200, radius = 0.8,
    density = NULL, angle = 45, col = NULL,
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
  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of `density' also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}
  \item{col}{a vector of colors to be used in filling or shading
    the slices. If missing a set of 6 pastel colours is used,
    unless \code{density} is specified when \code{par("fg")} is used.}
  \item{main}{an overall title for the plot.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{pie}.}
}
\note{
  Pie charts are a very bad way of displaying information.
  The eye is good at judging linear measures and bad at judging
  relative areas.

  A bar chart or dot chart is a preferable way of displaying this
  type of data.

  Prior to \R 1.5.0 this was known as \code{piechart}, which is the name
  of a Trellis function, so the name was changed to be compatible with S.
}
\seealso{
  \code{\link{dotchart}}.
}
\examples{
pie(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours
pie(pie.sales,
    col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))
pie(pie.sales, density = 10, angle = 15 + 10 * 1:6)
}
\keyword{hplot}
