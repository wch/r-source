\name{strwidth}
\title{Plotting Dimensions of Character Strings and Math Expressions}
\usage{
strwidth(s, units = "user", cex = NULL)
strheight(s, units = "user", cex = NULL)
}
\alias{strwidth}
\alias{strheight}
\arguments{
  \item{s}{character vector or \code{\link{expression}}s whose string
    widths in plotting units are to be determined.}
  \item{units}{character indicating in which units \code{s} is measured;
    must be one of \code{"user"}, \code{"inches"}, \code{"figure"} .}
  \item{cex}{character expansion to which is applies. Per default, the
    current \code{par("cex")} is used.}
}
\description{
  These functions compute the width or height, respectively, of the
  given strings or mathematical expressions \code{s[i]} on
  the current plotting device in \emph{user} coordinates, \emph{inches}
  or as fraction of the figure width \code{par("fin")}.
}
\value{
  integer vector with the same length as \code{s}, giving the
  width for each \code{s[i]}.
}
\seealso{
  \code{\link{text}}, \code{\link{nchar}}
}
\examples{
str.ex <- c("W","w","I",".","WwI.")
op <- par(pty='s'); plot(1:100,1:100); par('usr')
sw <- strwidth(str.ex); sw
sum(sw[1:4] == sw[5])#- since the last string contains the others
sw / strwidth(str.ex, cex = .5)
# between 1.5 and 4.2 (!),  font dependent

sw.i <- strwidth(str.ex, "inches"); 25.4 * sw.i # width in [mm]
unique(sw / sw.i)
# constant factor: 1 value
mean(sw.i / strwidth(str.ex, "fig")) / par('fin')[1] # = 1: are the same

## See how letters fall in classes -- depending on graphics device and font!
all.lett <- c(letters, LETTERS)
shL <- strheight(all.lett, units = "inches")
table(shL)# all have same heights ..
mean(shL) / par("cin")[2] # should be 1 (exactly?)

swL <- strwidth(all.lett)
swL <- 3 * swL / min(swL)
all(swL == round(swL))#- TRUE !
swL <- as.integer(swL)
n.classes <- length(tL <- table(swL)); tL
iL <- order(swL)
structure(swL[iL], names = all.lett[iL])
lett.classes <- structure(vector("list", n.classes), names= names(tL))
for(i in 1:n.classes)
  lett.classes[[i]] <- all.lett[swL == as.numeric(names(tL)[i])]
lett.classes

sumex <- expression(sum(x[i], i=1,n), e^{i * pi} == -1)
strwidth(sumex)
strheight(sumex)

rm(sumex); par(op)#- reset to previous setting
}
\keyword{dplot}
\keyword{character}
