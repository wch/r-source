\name{text}
\title{Add Text to a Plot}
\usage{
text (x, ...)
text.default (x, y = NULL, labels = seq(along = x), adj = NULL,
              pos = NULL, offset = 0.5, \dots)
}
\alias{text}
\alias{text.default}
\arguments{
  \item{x, y}{numeric vectors of coordinates where the text
    \code{labels} should be written.  If the length of \code{x} and
    \code{y} differs, the shorter one is recycled.}
  \item{labels}{character or expression with the \emph{text} to be
    written.}
  \item{adj}{one or two values in \eqn{[0,1]} which specify the x
    (and optionally y) adjustment of the labels.}
  \item{pos}{a position specifier for the text.  If specified this
    overrides any \code{adj} value given.  Values of \code{1},
    \code{2}, \code{3} and \code{4}, respectively indicate
    positions below, to the left of, above and to the right of
    the specified coordinates.}
  \item{offset}{when \code{pos} is specified, this value gives the
    offset of the label from the specified coordinate in fractions
    of a character width.}
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
  Values of 0, 0.5, and 1 specify left/bottom, middle and
  right/top, respectively.  The default is for centered text, i.e.,
  \code{adj = c(0.5, 0.5)}. Accurate vertical centering needs
  character metric information on individual characters, which is
  only available on some devices.

  The \code{pos} and \code{offset} arguments can be used in conjunction
  with values returned by \code{identify} to recreate an interactively
  labelled plot.

  Text can be rotated by using graphical parameters \code{srt} (see
  \code{\link{par}}); this rotates about the centre set by \code{adj}.
}
\seealso{
  \code{\link{mtext}}, \code{\link{title}}.
}
\examples{
plot(-1:1,-1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)

plot(1:10, 1:10, main = "text(..) examples\n~~~~~~~~~~~~~~",
     sub = "R is GNU ©, but not ® ..")
mtext("«ISO-accents»: ± éè øØ å<Å æ<Æ", side=3)
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

text(5,10.2,"Le français, c'est façile: Règles, Liberté, Egalité, Fraternité..")
text(5,9.8, "Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)")
}
\keyword{aplot}




