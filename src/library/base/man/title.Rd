\name{title}
\title{Plot Annotation}
\usage{
title(main = "", sub = "", xlab = "", ylab = "", \dots)
}
\alias{title}
\arguments{
  \item{main}{The main title (on top) using font and size (character
    expansion) \code{par("font.main")} and color
    \code{par("col.main")}.}
  \item{sub}{Sub-title (at bottom) using font and size
    \code{par("font.sub")} and color \code{par("col.sub")}.}
  \item{xlab}{X axis label using font and character expansion
    \code{par("font.axis")} and color \code{par("col.axis")}.}
  \item{ylab}{Y axis label, same font attributes as \code{xlab}.}
  \item{\dots}{further graphical parameters (from \code{\link{par}}).}
}
\description{
  This function can be used to add labels to a plot.  Its first four
  principal arguments can also be used as arguments in most high-level
  plotting functions.  They must be of type \code{\link{character}} or
  \code{\link{expression}}. In the latter case, quite a bit of
  mathematical notation is available such as sub- and superscripts,
  greek letters, fractions, etc.
}
\seealso{
  \code{\link{mtext}}, \code{\link{text}};
  \code{\link{plotmath}} for details on mathematical annotation.
}
\examples{
\testonly{data(cars)}
plot(cars, main = "")
title(main = "Stopping Distance versus Speed")

x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     lab = expression(-pi, -pi/2, 0, pi/2, pi))
abline(h = 0, v = pi/2 * c(-1,1), lty = 2, lwd = .1, col = "gray70")
}
\keyword{aplot}
