% File src/library/grDevices/man/Japanese.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

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
  by \samp{\\\\#J242b} and the Katakana character for this sound is
  produced by \samp{\\\\#J252b}.  The Kanji ideograph for "one" is
  produced by \samp{\\\\#J306c} or \samp{\\\\#N0001}.

  The output from \code{\link{demo}(Japanese)} shows tables of the escape
  sequences for the available Japanese characters.
}
\seealso{
  \code{\link{demo}(Japanese)}, \code{\link{Hershey}}, \code{\link{text}}
}
\references{
  \url{http://www.gnu.org/software/plotutils/plotutils.html}
}
\examples{
require(graphics)

plot(1:9, type = "n", axes = FALSE, frame = TRUE, ylab = "",
     main = "example(Japanese)", xlab = "using Hershey fonts")
par(cex = 3)
Vf <- c("serif", "plain")
% doubled "\" -> can't execute here!
text(4, 2, "\\\\#J244b\\\\#J245b\\\\#J2473", vfont = Vf)
text(4, 4, "\\\\#J2538\\\\#J2563\\\\#J2551\\\\#J2573", vfont = Vf)
text(4, 6, "\\\\#J467c\\\\#J4b5c", vfont = Vf)
text(4, 8, "Japan", vfont = Vf)
par(cex = 1)
text(8, 2, "Hiragana")
text(8, 4, "Katakana")
text(8, 6, "Kanji")
text(8, 8, "English")
}
\keyword{aplot}
