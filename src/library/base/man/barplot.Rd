\name{barplot}
\alias{barplot}
\alias{barplot.default}
\title{Bar Plots}
\usage{
barplot(height, width = 1, space = NULL, names.arg = NULL,
        legend.text = NULL, beside = FALSE, horiz = FALSE,
        col = heat.colors(NR), border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
	xlim = NULL, ylim = NULL,
	axes = TRUE, axisnames = TRUE, inside = TRUE, plot = TRUE, \dots)
}
\arguments{
  \item{height}{either a vector or matrix of values describing the
    bars which make up the plot.  If \code{height} is a vector, the
    plot consists of a sequence of rectangular bars with heights
    given by the values in the vector.  If \code{height} is a matrix
    and \code{beside} is \code{FALSE} then each bar of the plot
    corresponds to a column of \code{height}, with the values in the
    column giving the heights of stacked ``sub-bars'' making up the
    bar.  If \code{height} is a matrix and \code{beside} is
    \code{TRUE}, then the values in each column are juxtaposed
    rather than stacked.}
  \item{width}{optional vector of bar widths.}
  \item{space}{the amount of space (as a fraction of the average bar
    width) left before each bar.  May be given as a single number or
    one number per bar.  If \code{height} is a matrix and
    \code{beside} is \code{TRUE}, \code{space} may be specified by
    two numbers, where the first is the space between bars in the
    same group, and the second the space between the groups.  If not
    given explicitly, it defaults to \code{c(0,1)} if \code{height}
    is a matrix and \code{beside} is \code{TRUE}, and to 0.2
    otherwise.}
  \item{names.arg}{a vector of names to be plotted below each bar or
    group of bars.  If this argument is omitted, then the names are
    taken from the \code{names} attribute of \code{height} if this
    is a vector, or the column names if it is a matrix.}
  \item{legend.text}{a vector of text used to construct a legend for
    the plot.  This is only useful when \code{height} is a matrix.
    In that case the legend labels should correspond to the rows of
    \code{height}.}
  \item{beside}{a logical value.  If \code{FALSE}, the columns of
    \code{height} are portrayed as stacked bars, and if \code{TRUE}
    the columns are portrayed as juxtaposed bars.}
  \item{horiz}{a logical value.  If \code{FALSE}, the bars are drawn
    vertically with the first bar to the left.  If \code{TRUE}, the
    bars are drawn horizontally with the first at the bottom.}
  \item{col}{a vector of colors for the bars or bar components.}
  \item{border}{the color to be used for the border of the bars.}
  \item{main,sub}{overall and sub title for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{xlim}{limits for the x axis.}
  \item{ylim}{limits for the y axis.}
  \item{axes}{logical.  If \code{TRUE}, a vertical (or horizontal, if
    \code{horiz} is true) axis is drawn.}
  \item{axisnames}{logical.  If \code{TRUE}, and if there are
    \code{names.arg} (see above), the
    other axis is drawn (with \code{lty=0}) and labeled.}
  \item{plot}{logical.  If \code{FALSE}, nothing is plotted.}
  \item{\dots}{further graphical parameters (\code{\link{par}}) are
    passed to \code{\link{plot.window}()} and \code{\link{title}()}.}
}
\description{
  Creates a bar plot with vertical or horizontal bars.
}
\details{
  This is a generic function, it currently only has a default method.
  A formula interface may be added eventually.
}
\value{
  A numeric vector (or matrix, when \code{beside = TRUE}), say
  \code{mp}, giving the coordinates of \emph{all} the bar midpoints
  drawn, useful for adding to the graph.

  If \code{beside} is true, use \code{apply(mp, 2, mean)} for the
  midpoints of each \emph{group} of bars, see example.
}
\seealso{
  \code{\link{plot}(\dots, type="h")}, \code{\link{dotplot}},
  \code{\link{hist}}.
}
\examples{
tN <- table(Ni <- rpois(100, lambda=5))
r <- barplot(tN, col='gray')
lines(r, tN, type='h', col='red', lwd=2)#- type = "h" plotting *is* `bar'plot

barplot(tN, space = 1.5, axisnames=FALSE,
        sub = "barplot(...., space= 1.5, axisnames = FALSE)")

data(VADeaths, package = "base")
barplot(VADeaths, plot = FALSE)
barplot(VADeaths, plot = FALSE, beside = TRUE)

mp <- barplot(VADeaths) # default
tot <- apply(VADeaths, 2, sum)
text(mp, tot + 3, format(tot), xpd = NA, col = "blue")
barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

hh <- t(VADeaths)[, 5:1]
mybarcol <- "gray20"
mp <- barplot(hh, beside = TRUE,
        col = c("lightblue", "mistyrose",
                "lightcyan", "lavender"),
        legend = colnames(VADeaths), ylim= c(0,100),
        main = "Death Rates in Virginia", font.main = 4,
        sub = "Faked upper 2*sigma error bars", col.sub = mybarcol)
segments(mp, hh, mp, hh+ 2*sqrt(1000*hh/100), col=mybarcol, lwd=1.5)
all(dim(mp) == dim(hh))# corresponding matrices
mtext(side=1, at=apply(mp,2,mean), line = -2,
      text=paste("Mean",formatC(apply(hh,2,mean))),col="red")
}
\keyword{hplot}
