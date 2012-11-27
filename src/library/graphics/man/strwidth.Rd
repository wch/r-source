% File src/library/graphics/man/strwidth.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{strwidth}
\title{Plotting Dimensions of Character Strings and Math Expressions}
\description{
  These functions compute the width or height, respectively, of the
  given strings or mathematical expressions \code{s[i]} on
  the current plotting device in \emph{user} coordinates, \emph{inches}
  or as fraction of the figure width \code{par("fin")}.
}
\usage{
strwidth(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...)
strheight(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...)
}
\alias{strwidth}
\alias{strheight}
\arguments{
  \item{s}{a character or \link{expression} vector whose dimensions
    are to be determined.  Other objects are coerced by
    \code{\link{as.graphicsAnnot}}.}
  \item{units}{character indicating in which units \code{s} is measured;
    should be one of \code{"user"}, \code{"inches"}, \code{"figure"};
    partial matching is performed.}
  \item{cex}{numeric \bold{c}haracter \bold{ex}pansion factor; multiplied
    by \code{\link{par}("cex")} yields the final character size; the
    default \code{NULL} is equivalent to \code{1}.}
  \item{font, vfont, \dots}{additional information about the font,
    possibly including the graphics parameter \code{"family"}:
    see \code{\link{text}}.}
}
\details{
  Note that the \sQuote{height} of a string is determined only by the
  number of linefeeds (\code{"\n"}) it contains: it is the (number of
  linefeeds - 1) times the line spacing plus the height of \code{"M"} in
  the selected font.  For an expression it is the height of the
  bounding box as computed by \link{plotmath}.  Thus in both cases it is
  an estimate of how far \bold{above} the final baseline the typeset
  object extends.  (It may also extend below the baseline.)  The
  inter-line spacing is controlled by \code{cex},
  \code{\link{par}("lheight")} and the \sQuote{point size} (but not the
  actual font in use).

  Measurements in \code{"user"} units (the default) are only available
  after \code{\link{plot.new}} has been called -- otherwise an error is
  thrown.
}
\value{
  Numeric vector with the same length as \code{s}, giving the estimate of
  width or height for each \code{s[i]}.  \code{NA} strings are given
  width and height 0 (as they are not plotted).
}
\seealso{
  \code{\link{text}}, \code{\link{nchar}}
}
\examples{
str.ex <- c("W","w","I",".","WwI.")
op <- par(pty = "s"); plot(1:100, 1:100, type = "n")
sw <- strwidth(str.ex); sw
all.equal(sum(sw[1:4]), sw[5])
#- since the last string contains the others

sw.i <- strwidth(str.ex, "inches"); 25.4 * sw.i  # width in [mm]
unique(sw / sw.i)
# constant factor: 1 value
mean(sw.i / strwidth(str.ex, "fig")) / par('fin')[1]  # = 1: are the same

## See how letters fall in classes
##  -- depending on graphics device and font!
all.lett <- c(letters, LETTERS)
shL <- strheight(all.lett, units = "inches") * 72  # 'big points'
table(shL)  # all have same heights ...
mean(shL)/par("cin")[2] # around 0.6

(swL <- strwidth(all.lett, units = "inches") * 72)  # 'big points'
split(all.lett, factor(round(swL, 2)))

sumex <- expression(sum(x[i], i=1,n), e^{i * pi} == -1)
strwidth(sumex)
strheight(sumex)

par(op)  #- reset to previous setting
}
\keyword{dplot}
\keyword{character}
