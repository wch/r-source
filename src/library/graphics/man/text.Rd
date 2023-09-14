% File src/library/graphics/man/text.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{text}
\title{Add Text to a Plot}
\encoding{UTF-8}
\usage{
text(x, \dots)

\method{text}{default} (x, y = NULL, labels = seq_along(x$x), adj = NULL,
     pos = NULL, offset = 0.5, vfont = NULL,
     cex = 1, col = NULL, font = NULL, \dots)
}
\alias{text}
\alias{text.default}
\arguments{
  \item{x, y}{numeric vectors of coordinates where the text
    \code{labels} should be written.  If the length of \code{x} and
    \code{y} differs, the shorter one is recycled.}
  \item{labels}{a character vector or \link{expression} specifying
    the \emph{text} to be written.  An attempt is made to coerce other
    language objects (names and calls) to expressions, and vectors and
    other classed objects to character vectors by \code{\link{as.character}}.
    If \code{labels} is longer than \code{x} and
    \code{y}, the coordinates are recycled to the length of \code{labels}.}
  \item{adj}{one or two values in \eqn{[0, 1]} which specify the x
    (and optionally y) adjustment (\sQuote{justification}) of the
    labels, with 0 for left/bottom, 1 for right/top, and 0.5 for
    centered.  On most devices values outside \eqn{[0, 1]} will also
    work.  See below.}
   \item{pos}{a position specifier for the text.  If specified this
    overrides any \code{adj} value given.  Values of \code{1},
    \code{2}, \code{3} and \code{4}, respectively indicate
    positions below, to the left of, above and to the right of
    the specified \code{(x,y)} coordinates.}
  \item{offset}{when \code{pos} is specified, this value controls the
    distance (\sQuote{offset}) of the text label from the specified
    coordinate in fractions of a character width.}
  \item{vfont}{\code{NULL} for the current font family, or a character
    vector of length 2 for \code{\link[grDevices]{Hershey}} vector
    fonts.  The first element of
    the vector selects a typeface and the second element selects a
    style.  Ignored if \code{labels} is an expression.}
  \item{cex}{numeric \bold{c}haracter \bold{ex}pansion factor; multiplied
    by \code{\link{par}("cex")} yields the final character size.
    \code{NULL} and \code{NA} are equivalent to \code{1.0}.}
  \item{col, font}{the color and (if \code{vfont = NULL}) font to be
    used, possibly vectors.  These default to the values of the global
    \link{graphical parameters} in \code{\link{par}()}.}
  \item{\dots}{further \link{graphical parameters} (from \code{\link{par}}),
    such as \code{srt}, \code{family} and \code{xpd}.}
}
\description{
  \code{text} draws the strings given in the vector \code{labels} at the
  coordinates given by \code{x} and \code{y}.
  \code{y} may be missing since \code{\link{xy.coords}(x, y)} is used for
  construction of the coordinates.
}
\details{
  \code{labels} must be of type \code{\link{character}} or
  \code{\link{expression}} (or be coercible to such a type).
  In the latter case, quite a bit of
  mathematical notation is available such as sub- and superscripts,
  greek letters, fractions, etc.

  \code{adj} allows \emph{adj}ustment of the text position with respect to
  \code{(x, y)}.
  Values of 0, 0.5, and 1 specify that \code{(x, y)} should align with
  the left/bottom, middle and
  right/top of the text, respectively.
  A value of \code{NA} means \dQuote{centre}, which is the same as 0.5
  for horizontal justification, but includes descenders for vertical
  justification (where 0.5 does not).
  The default is for centered text, although the default horizontal
  justification is taken from \code{par(adj)}, i.e., the default is
  \code{adj = c(par("adj"), NA)}.
  If only one value is provided, it is applied
  to adjust \code{x} \emph{only}, i.e., when \code{length(adj) == 1L},
  \code{adj} is applied as \code{adj = c(adj, NA)}.
  Accurate vertical centering needs
  character metric information on individual characters which is
  only available on some devices.  Vertical alignment is done slightly
  differently for character strings and for expressions:
  \code{adj = c(0,0)} means to left-justify and to align on the baseline
  for strings but on the bottom of the bounding box for expressions.
  This also affects vertical centering: for strings the centering
  excludes any descenders whereas for expressions it includes them.

  The \code{pos} and \code{offset} arguments can be used in conjunction
  with values returned by \code{identify} to recreate an interactively
  labelled plot.

  Text can be rotated by using \link{graphical parameters} \code{srt}
  (see \code{\link{par}}).  When \code{adj} is specified, a non-zero
  \code{srt} rotates the label about \code{(x, y)}.  If \code{pos} is
  specified, \code{srt} rotates the text about the point on its bounding
  box which is closest to \code{(x, y)}: top center for \code{pos = 1},
  right center for \code{pos = 2}, bottom center for \code{pos = 3}, and
  left center for \code{pos = 4}.  The \code{pos} interface is not as
  useful for rotated text because the result is no longer centered
  vertically or horizontally with respect to \code{(x, y)}.  At present
  there is no interface in the \pkg{graphics} package for directly
  rotating text about its center which is achievable however by fiddling
  with \code{adj} and \code{srt} simultaneously.

  Graphical parameters \code{col}, \code{cex} and \code{font} can be
  vectors and will then be applied cyclically to the \code{labels} (and
  extra values will be ignored).  \code{NA} values of \code{font} are
  replaced by \code{par("font")}, and similarly for \code{col}.

  Labels whose \code{x}, \code{y} or \code{labels} value is \code{NA}
  are omitted from the plot.

  What happens when \code{font = 5} (the symbol font) is selected can be
  both device- and locale-dependent.  Most often \code{labels} will be
  interpreted in the Adobe symbol encoding, so e.g.\sspace{}\code{"d"}
  is delta, and \code{"\300"} is aleph.
}
\section{Euro symbol}{
  The Euro symbol may not be available in older fonts.  In current
  versions of Adobe symbol fonts it is character 160, so \code{text(x,
  y, "\xA0", font = 5)} may work. People using Western European locales
  on Unix-alikes can probably select ISO-8895-15 (Latin-9) which has the
  Euro as character 165: this can also be used for
  \code{\link{postscript}} and \code{\link{pdf}}. It is \samp{\u20ac} in
  Unicode, which can be used in UTF-8 locales.
#ifdef unix

  The Euro should be rendered correctly by \code{\link{X11}} in UTF-8
  locales, but the corresponding single-byte encoding in
  \code{\link{postscript}} and \code{\link{pdf}} will need to be selected
  as \code{ISOLatin9.enc} (and the font will need to contain the Euro
  glyph, which for example older printers may not).
#endif
#ifdef windows

  In all the European Windows encodings the Euro is symbol 128 and
  \samp{\u20ac} will work in all locales: however not all fonts will
  include it.  It is not in the symbol font used for
  \code{\link{windows}} and related devices, including the Windows printer.
#endif
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{text.formula}} for the formula method;
  \code{\link{mtext}}, \code{\link{title}},
  \code{\link{Hershey}} for details on Hershey vector fonts,
  \code{\link{plotmath}} for details and more examples on
  mathematical annotation.
}
\examples{
plot(-1:1, -1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)

## The following two examples use latin1 characters: these may not
## appear correctly (or be omitted entirely).
plot(1:10, 1:10, main = "text(...) examples\n~~~~~~~~~~~~~~",
     sub = "R is GNU ©, but not ® ...")
mtext("«Latin-1 accented chars»: éè øØ å<Å æ<Æ", side = 3)
points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
text(6, 2, "the text is CENTERED around (x,y) = (6,2) by default",
     cex = .8)
text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by 'adj = c(0,0)'",
     adj = c(0,0))
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
     cex = .75)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))

## Two more latin1 examples
text(5, 10.2,
     "Le français, c'est facile: Règles, Liberté, Egalité, Fraternité...")
text(5, 9.8,
     "Jetz no chli züritüütsch: (noch ein bißchen Zürcher deutsch)")
}
\keyword{aplot}
