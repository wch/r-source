\name{barplot}
\title{Bar Plots}
\usage{
barplot(height, width = 1, space = NULL, names.arg = NULL,
        legend.text = NULL, beside = FALSE, horiz = FALSE,
        col = heat.colors(NR), border = par("fg"), main = NULL,
        xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
        axes = TRUE, ...)
}
\alias{barplot}
\arguments{
  \item{height}{either a vector or matrix of values describing the bars
    which make up the plot.  If \code{height} is a vector, the plot
    consists of a sequence of rectangular bars with heights given by the
    values in the vector.  If \code{height} is a matrix and
    \code{beside} is \code{FALSE} then each bar of the plot corresponds
    to a column of \code{height}, with the values in the column giving
    the heights of stacked ``sub-bars'' making up the bar. If
    \code{height} is a matrix and \code{beside} is \code{TRUE}, then the
    values in each column are juxtaposed rather than stacked.}
  \item{width}{optional vector of bar widths.}
  \item{space}{the amount of space (as a fraction of the average bar
    width) left before each bar.  May be given as a single number or one
    number per bar.  If \code{height} is a matrix and \code{beside} is
    \code{TRUE}, \code{space} may be specified by two numbers, where the
    first is the space between bars in the same group, and the second
    the space between the groups.  If not given explicitly, it defaults
    to \code{c(0,1)} if \code{height} is a matrix and \code{beside} is
    \code{TRUE}, and to 0.2 otherwise.}
  \item{names.arg}{a vector of names to be plotted below each bar or
    group of bars.  If this argument is omitted, then the names are
    taken from the \code{names} attribute of \code{height} if this is a
    vector, or the column names of \code{height} if this is a matrix.}
  \item{legend.text}{a vector of text used to construct a legend for the
    plot.  This is only useful when \code{height} is a matrix.  In that
    case the legend labels should correspond to the rows of
    \code{height}.}
  \item{beside}{a logical value.  If \code{FALSE}, the columns of
    \code{height} are portrayed as stacked bars, and if \code{TRUE} the
    columns are portrayed as juxtaposed bars.}
  \item{horiz}{a logical value.  If \code{FALSE}, the bars are drawn
    vertically with the first bar to the left.  If \code{TRUE}, the bars
    are drwan horizontally with the first at the bottom.}
  \item{col}{a vector of colors for the bars or bar components.}
  \item{border}{the color to be used for the border of the bars.}
  \item{main}{an overall title for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{xlim}{limits for the x axis.}
  \item{ylim}{limits for the y axis.}
  \item{axes}{logical.  If \code{TRUE}, a vertical axis is drawn.}
  \item{\dots}{further arguments are passed to \code{title(.)}.}
}
\value{
  A numeric vector, giving the coordinates of the bar midpoints drawn,
  useful for adding to the graph.
}
\seealso{
  \code{\link{dotplot}}, \code{\link{hist}}.
}
\examples{
data(deaths)
mp <- barplot(deaths) # default
tot <- apply(deaths,2,sum); text(mp, tot + 3, format(tot))
barplot(deaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(deaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

barplot(t(deaths)[, 5:1], beside = TRUE,
        col = c("lightblue", "mistyrose",
                "lightcyan", "lavender"),
        legend = colnames(deaths), ylim = c(0,80),
        main = "Death Rates in Virginia", font.main = 4)
        ## instead of title(.)
}
\keyword{hplot}
