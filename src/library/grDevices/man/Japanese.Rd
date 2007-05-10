\name{Japanese}
\alias{Japanese}
\title{Japanese characters in R}
\description{
  The implementation of Hershey vector fonts provides a large number of
  Japanese characters (Hiragana, Katakana, and Kanji).
}
\details{
  Without keyboard support for typing Japanese characters, the only way
  to produce these characters is to use special escape sequences: see
  \code{\link{Hershey}}.

  For example, the Hiragana character for the sound "ka" is produced
  by \code{\\\\#J242b} and the Katakana character for this sound is
  produced by \code{\\\\#J252b}.  The Kanji ideograph for "one" is
  produced by \code{\\\\#J306c} or \code{\\\\#N0001}.

  The output from \code{demo(Japanese)} shows tables of the escape
  sequences for the available Japanese characters.
}
\seealso{
  \code{demo(Japanese)},
  \code{\link{Hershey}},
  \code{\link{text}},
  \code{\link{contour}}
}
\references{
  \url{http://www.gnu.org/software/plotutils/plotutils.html}
}
\examples{
require(graphics)

plot(1:9, type="n", axes=FALSE, frame=TRUE, ylab="",
     main= "example(Japanese)", xlab= "using Hershey fonts")
par(cex=3)
Vf <- c("serif", "plain")
% doubled "\" -> can't execute here!
text(4, 2, "\\\\#J2438\\\\#J2421\\\\#J2451\\\\#J2473", vfont = Vf)
text(4, 4, "\\\\#J2538\\\\#J2521\\\\#J2551\\\\#J2573", vfont = Vf)
text(4, 6, "\\\\#J467c\\\\#J4b5c", vfont = Vf)
text(4, 8, "Japan", vfont = Vf)
par(cex=1)
text(8, 2, "Hiragana")
text(8, 4, "Katakana")
text(8, 6, "Kanji")
text(8, 8, "English")
}
\keyword{aplot}
