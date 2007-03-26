\name{strwidth}
\title{Plotting Dimensions of Character Strings and Math Expressions}
\description{
  These functions compute the width or height, respectively, of the
  given strings or mathematical expressions \code{s[i]} on
  the current plotting device in \emph{user} coordinates, \emph{inches}
  or as fraction of the figure width \code{par("fin")}.
}
\usage{
strwidth(s, units = "user", cex = NULL)
strheight(s, units = "user", cex = NULL)
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
}
\value{
  Numeric vector with the same length as \code{s}, giving the
  width or height for each \code{s[i]}.  \code{NA} strings are given
  width and height 0 (as they are not plotted).
}
\seealso{
  \code{\link{text}}, \code{\link{nchar}}
}
\examples{
str.ex <- c("W","w","I",".","WwI.")
op <- par(pty='s'); plot(1:100,1:100, type="n")
sw <- strwidth(str.ex); sw
all.equal(sum(sw[1:4]), sw[5])#- since the last string contains the others

sw.i <- strwidth(str.ex, "inches"); 25.4 * sw.i # width in [mm]
unique(sw / sw.i)
# constant factor: 1 value
mean(sw.i / strwidth(str.ex, "fig")) / par('fin')[1] # = 1: are the same

## See how letters fall in classes -- depending on graphics device and font!
all.lett <- c(letters, LETTERS)
shL <- strheight(all.lett, units = "inches") * 72 # 'big points'
table(shL) # all have same heights ...
mean(shL)/par("cin")[2] # around 0.6

(swL <- strwidth(all.lett, units="inches") * 72) # 'big points'
split(all.lett, factor(round(swL, 2)))

sumex <- expression(sum(x[i], i=1,n), e^{i * pi} == -1)
strwidth(sumex)
strheight(sumex)

par(op)#- reset to previous setting
}
\keyword{dplot}
\keyword{character}
