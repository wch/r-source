\name{pie}
\title{Pie Charts}
\alias{pie}
\description{
  Draw a pie chart.
}
\usage{
pie(x, labels = names(x), edges = 200, radius = 0.8,
    clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
    density = NULL, angle = 45, col = NULL, border = NULL,
    lty = NULL, main = NULL, \dots)
}
\arguments{
  \item{x}{a vector of non-negative numerical quantities.
    The values in \code{x} are displayed as the areas of pie slices.}
  \item{labels}{one or more expressions or character strings giving
    names for the slices.  Other objects are coerced by
    \code{\link{as.graphicsAnnot}}.  For empty or \code{NA}
    (after coercion to character) labels, no label nor pointing line
    is drawn.}
  \item{edges}{the circular outline of the pie is approximated by a
    polygon with this many edges.}
  \item{radius}{the pie is drawn centered in a square box whose sides
    range from \eqn{-1} to \eqn{1}.  If the character strings labeling
    the slices are long it may be necessary to use a smaller radius.}
  \item{clockwise}{logical indicating if slices are drawn clockwise or
    counter clockwise (i.e., mathematically positive direction), the
    latter is default.}
  \item{init.angle}{number specifying the \emph{starting angle} (in
    degrees) for the slices. Defaults to 0 (i.e., \sQuote{3 o'clock})
    unless \code{clockwise} is true where \code{init.angle}
    defaults to 90 (degrees), (i.e., \sQuote{12 o'clock}).}
  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}
  \item{col}{a vector of colors to be used in filling or shading
    the slices. If missing a set of 6 pastel colours is used,
    unless \code{density} is specified when \code{par("fg")} is used.}
  \item{border, lty}{(possibly vectors) arguments passed to
    \code{\link{polygon}} which draws each slice.}
  \item{main}{an overall title for the plot.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{pie}.  They will affect the main title and labels only.}
}
\note{
  Pie charts are a very bad way of displaying information.
  The eye is good at judging linear measures and bad at judging
  relative areas.  A bar chart or dot chart is a preferable way of
  displaying this type of data.

  Cleveland (1985), page 264: \dQuote{Data that can be shown by pie charts
    always can be shown by a dot chart.  This means that judgements of
    position along a common scale can be made instead of the less
    accurate angle judgements.}
  This statement is based on the empirical investigations of Cleveland
  and McGill as well as investigations by perceptual psychologists.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Cleveland, W. S. (1985)
  \emph{The elements of graphing data}.
  Wadsworth: Monterey, CA, USA.
}
\seealso{
  \code{\link{dotchart}}.
}
\examples{
require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours
pie(pie.sales,
    col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))
pie(pie.sales, density = 10, angle = 15 + 10 * 1:6)
pie(pie.sales, clockwise=TRUE, main="pie(*, clockwise=TRUE)")
segments(0,0, 0,1, col= "red", lwd = 2)
text(0,1, "init.angle = 90", col= "red")

n <- 200
pie(rep(1,n), labels="", col=rainbow(n), border=NA,
    main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")
}
\keyword{hplot}
