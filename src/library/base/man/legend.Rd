\name{legend}
\title{Add Legends to Plots}
\usage{
legend(x, y, legend, fill, col = "black", lty, pch, bty = "o",
       bg = par("bg"), cex = 1, xjust = 0, yjust = 1,
       x.intersp = NULL, y.intersp = NULL, text.width = NULL,
       merge = FALSE)
}
\alias{legend}
\arguments{
  \item{x}{the x location of the legend.}
  \item{y}{the y location of the legend.}
  \item{legend}{a vector of text values or an \code{\link{expression}}
    of length \eqn{\ge 1}{>= 1} to appear in the legend.}
  \item{fill}{if specied, this argument will cause boxes filled
    with the specified colors to appear beside the legend text.}
  \item{col}{the color of points or lines appearing in the legend.}
  \item{lty}{the line types for lines appearing in the legend. \emph{Must}
    be specified for line drawing.}
  \item{pch}{the plotting symbols appearing in the legend, either as
    vector of 1-character strings, or one (multi character)
    string. \emph{Must} be specified for symbol drawing.}
  \item{bty}{the type of box to be drawn around the legend.}
  \item{bg}{the background color for the legend box.}
  \item{cex}{character expansion factor \bold{relative} to current
    \code{par("cex")}.}
  \item{xjust}{how the legend is to be justified relative to the legend
    x location.  A value of 0 means left justified, 0.5 means centered
    and 1 means right justified.}
  \item{yjust}{the same as \code{xjust} for the legend y location.}
  \item{x.intersp}{character interspacing factor for horizontal (x)
    spacing.}
  \item{y.intersp}{the same as \code{x.intersp} for vertical (y) line
    distances.}
  \item{text.width}{the width of the legend text in x (\code{"user"})
    coordinates.
    Defaults to the proper value computed by \code{\link{strwidth}(legend)}.}
  \item{merge}{logical; if \code{TRUE}, ``merge'' points and lines but
    not filled boxes.}
}
\description{
  This function can be used to add legends to plots.  Note that a call
  to the function \code{locator} can be used in place of the \code{x}
  and \code{y} arguments.

  The default values for \code{x.intersp} and \code{y.intersp} are
  computed from \code{Cx <- cex * par('cex')}.

  ``Attribute'' arguments such as \code{col}, \code{lty}, \code{pch},
  etc, are recycled if necessary.
}
\seealso{
  \code{\link{plot}}, \code{\link{barplot}} which uses \code{legend()}, and
  \code{\link{text}} for more examples of math expressions.
}
\examples{
# Run the example in `?matplot' or the following:
matplot(c(1,8), c(0,4.5), type = "n", xlab = "Length", ylab = "Width",
        main = "Petal and Sepal Dimensions in Iris Blossoms")
leg.txt <- c("    Setosa Petals", "    Setosa Sepals",
             "Versicolor Petals", "Versicolor Sepals")
y.leg <- c( 4, 3, 2.2, 1.5, .9)
cexv  <- c( 1, .8, .6, .4, .25)
for (i in seq(cexv))
  legend(1, y.leg[i], leg.txt, pch = "sSvV", col = c(1, 3), cex = cexv[i])

# `merge = TRUE' for merging lines & points:
x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type = "l", ylim = c(-1.2, 1.8), col = 3, lty = 2)
points(x, cos(x), pch = 3, col = 4)
lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
title("legend(..... lty = c(2, -1, 1), pch = c(-1,3,4), merge = TRUE)",
      cex.main = 1.1)
legend(-1, 1.9, c("sin", "cos", "tan"), col = c(3,4,6),
       lty = c(2, -1, 1), pch = c(-1, 3, 4), merge = TRUE, bg='gray90')

##-- Math expressions:
plot(x, sin(x), type="l", col = 2,xlab=expression(phi),ylab=expression(f(phi)))
abline(h=-1:1, v=pi/2*(-6:6), col="gray90")
lines(x, cos(x), col = 3, lty = 2)
ex.cs1 <- expression(plain(sin) * phi,  paste("cos", phi))# 2 ways
legend(-3, .9, ex.cs1, lty=1:2, col=2:3)

x <- rexp(100, rate = .5)
hist(x, main = "Mean and Median of a Skewed Distribution")
abline(v = mean(x),   col=2, lty=2, lwd=2)
abline(v = median(x), col=3, lty=3, lwd=2)
ex12 <- expression(bar(x) == sum(over(x[i], n), i==1, n),
    		   hat(x) == median(x[i], i==1,n))
legend(4,30, ex12, col = 2:3, lty=2:3) # lwd = 2 does not yet work in PS
}
\keyword{aplot}
