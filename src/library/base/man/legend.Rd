\name{legend}
\title{Add Legends to Plots}
\usage{
legend(x, y, legend, fill, col = "black", lty, lwd, pch,
       bty = "o", bg = par("bg"), cex = 1, xjust = 0, yjust = 1,
       x.intersp = 1, y.intersp = 1, adj = 0,
       text.width = NULL, merge = do.lines && has.pch, trace = FALSE,
       ncol = 1, horiz = FALSE)
}
\alias{legend}
\description{
  This function can be used to add legends to plots.  Note that a call
  to the function \code{\link{locator}} can be used in place of the \code{x}
  and \code{y} arguments.
}
\arguments{
  \item{x,y}{the x and y location of the legend.
    \code{x} can be a list with \code{x} and \code{y} components.}
%% if argument \code{y} is missing.
%S: location of the rectangle in which to put the legend.
%S: If x & y are length 1, they determine the top left corner of the rectangle;
%S: if .. length 2 vectors, they give opposite corners of the rectangular area.
%S: A list containing x and y values may be supplied.
  \item{legend}{a vector of text values or an \code{\link{expression}}
    of length \eqn{\ge 1}{>= 1} to appear in the legend.}
  \item{fill}{if specified, this argument will cause boxes filled
    with the specified colors to appear beside the legend text.}
  \item{col}{the color of points or lines appearing in the legend.}
  \item{lty,lwd}{the line types and widths for lines appearing in the
    legend.  One of these two \emph{must} be specified for line drawing.}
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
  \item{x.intersp}{character interspacing factor for horizontal (x) spacing.}
  \item{y.intersp}{the same for vertical (y) line distances.}
  \item{adj}{numeric of length 1 or 2; the string adjustment for legend
      text.  Useful for y-adjustment when \code{labels} are
      \link{plotmath} expressions.}
  \item{text.width}{the width of the legend text in x (\code{"user"})
    coordinates.
    Defaults to the proper value computed by \code{\link{strwidth}(legend)}.}
  \item{merge}{logical; if \code{TRUE}, ``merge'' points and lines but
    not filled boxes.  Defaults to \code{TRUE} if there are points and lines.}
  \item{trace}{logical; if \code{TRUE}, shows how \code{legend} does all
    its magical computations.}
  \item{ncol}{the number of columns in which to set the legend items
    (default is 1, a vertical legend).}
  \item{horiz}{logical; if \code{TRUE}, set the legend horizontally rather than
     vertically (specifying \code{horiz} overrides the \code{ncol}
     specification).}
}
\details{
 ``Attribute'' arguments such as \code{col}, \code{pch}, \code{lty},
  etc, are recycled if necessary.  \code{merge} is not.
}
\value{
  A (\code{\link{invisible}}) list with list components
  %   \code{rect} and \code{text}.
  %
  %   \code{$ rect} has components
  %   \item{w,h}{positive numbers giving \bold{w}idth and \bold{h}eight of
  %     the legend's box.}
  %   \item{left,top}{x and y coordinates of upper left corner of the box.}
  %
  %   \code{$ text} has components
  %   \item{x,y}{numeric vectors of length \code{length(legend)}, giving
  %     the x and y coordinates of the legend's text(s).}
  \item{rect}{a list with components
    \itemize{
      \item{w,h}{positive numbers giving \bold{w}idth and \bold{h}eight of
	the legend's box.}
      \item{left,top}{x and y coordinates of upper left corner of the box.}
    }
  }
  \item{text}{a list with components
    \itemize{
      \item{x,y}{numeric vectors of length \code{length(legend)}, giving
	the x and y coordinates of the legend's text(s).}
    }
  }
}
\seealso{
  \code{\link{plot}}, \code{\link{barplot}} which uses \code{legend()}, and
  \code{\link{text}} for more examples of math expressions.
}
\examples{
## Run the example in `?matplot' or the following:
leg.txt <- c("Setosa     Petals", "Setosa     Sepals",
             "Versicolor Petals", "Versicolor Sepals")
y.leg <- c(4.5, 3, 2.1, 1.4, .7)
cexv  <- c(1.2, 1, 4/5, 2/3, 1/2)
matplot(c(1,8), c(0,4.5), type = "n", xlab = "Length", ylab = "Width",
        main = "Petal and Sepal Dimensions in Iris Blossoms")
for (i in seq(cexv)) {
  text  (1, y.leg[i]-.1, paste("cex=",formatC(cexv[i])), cex=.8, adj = 0)
  legend(3, y.leg[i], leg.txt, pch = "sSvV", col = c(1, 3), cex = cexv[i])
}
## `merge = TRUE' for merging lines & points:
x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type = "l", ylim = c(-1.2, 1.8), col = 3, lty = 2)
points(x, cos(x), pch = 3, col = 4)
lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
title("legend(..... lty = c(2, -1, 1), pch = c(-1,3,4), merge = TRUE)",
      cex.main = 1.1)
legend(-1, 1.9, c("sin", "cos", "tan"), col = c(3,4,6),
       lty = c(2, -1, 1), pch = c(-1, 3, 4), merge = TRUE, bg='gray90')

##--- log scaled Examples ------------------------------
leg.txt <- c("a one", "a two")

par(mfrow = c(2,2))
for(ll in c("","x","y","xy")) {
  plot(2:10, log=ll, main=paste("log = '",ll,"'", sep=""))
  abline(1,1)
  lines(2:3,3:4, col=2) #
  points(2,2, col=3)    #
  rect(2,3,3,2, col=4)
  text(c(3,3),2:3, c("rect(2,3,3,2, col=4)",
                     "text(c(3,3),2:3,\"c(rect(...)\")"), adj = c(0,.3))
  legend(list(x=2,y=8), legend = leg.txt, col=2:3, pch=1:2,
         lty=1, merge=T)#, trace=T)
}
par(mfrow=c(1,1))

##-- Math expressions:  ------------------------------
plot(x, sin(x), type="l", col = 2,xlab=expression(phi),ylab=expression(f(phi)))
abline(h=-1:1, v=pi/2*(-6:6), col="gray90")
lines(x, cos(x), col = 3, lty = 2)
ex.cs1 <- expression(plain(sin) * phi,  paste("cos", phi))# 2 ways
str(legend(-3, .9, ex.cs1, lty=1:2, col=2:3, adj = c(0, .6)))# adj y !

x <- rexp(100, rate = .5)
hist(x, main = "Mean and Median of a Skewed Distribution")
abline(v = mean(x),   col=2, lty=2, lwd=2)
abline(v = median(x), col=3, lty=3, lwd=2)
ex12 <- expression(bar(x) == sum(over(x[i], n), i==1, n),
                   hat(x) == median(x[i], i==1,n))
str(legend(4.1, 30, ex12, col = 2:3, lty=2:3, lwd=2))

## Using `ncol' :
x <- 0:64/64
matplot(x, outer(x, 1:7, function(x, k) sin(k * pi * x)),
        type = "o", col = 1:7, ylim = c(-1, 1.5), pch = "*")
legend(0, 1.5, paste("sin(",1:7,"pi * x)"), col=1:7, lty=1:7, pch = "*",
       ncol = 4, cex=.8)
legend(.8,1.2, paste("sin(",1:7,"pi * x)"), col=1:7, lty=1:7, pch = "*",cex=.8)
legend(0, -.1, paste("sin(",1:4,"pi * x)"), col=1:4, lty=1:4, ncol=2, cex=.8)
legend(0, -.7, paste("sin(",5:7,"pi * x)"), col=5:7, pch="*", ncol=2, cex=.8)
}
\keyword{aplot}
