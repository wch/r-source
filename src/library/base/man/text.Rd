\name{text}
\title{Add Text to a Plot}
\usage{
text (x, ...)
text.default (x, y = NULL, labels = seq(along = x), adj = NULL, \dots)
}
\alias{text}
\alias{text.default}
\arguments{
  \item{x, y}{numeric vectors of coordinates where the text
    \code{labels} should be written.}
  \item{labels}{character or expression with the \emph{text} to be
    written.}
  \item{adj}{character or expression with the \emph{text} to be
    written.}
  \item{\dots}{further graphical parameters (from \code{\link{par}}),
    such as \code{cex}, etc.}
}
\description{
  \code{text} draws the strings given in the vector \code{labels} at the
  coordinates given by \code{x} and \code{y}.
  \code{y} may be missing since \code{\link{xy.coords}(x,y)} is used for
  construction of the coordinates.
}
\details{
  \code{labels} must be of type \code{\link{character}} or
  \code{\link{expression}}.  In the latter case, quite a bit of
  mathematical notation is available such as sub- and superscripts,
  greek letters, fractions, etc.

  \code{adj} allows \emph{adj}ustment of the text with respect to \code{(x,y)}.
}
\seealso{
  \code{\link{mtext}}, \code{\link{title}}.
}
\examples{
plot(-1:1,-1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)

plot(1:10, 1:10, main = "text(..) examples")
points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
text(6, 2, "the text is CENTERED around (x,y) = (6,2) by default",
     cex = .8)
text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by `adj = c(0,0)'",
     adj = c(0,0))
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .6)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))", cex = .6)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                            plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})))
}
\keyword{aplot}
