\name{col2rgb}
\alias{col2rgb}
\title{Color to RGB Conversion}
\description{
  \dQuote{Any \R color} to RGB (red/green/blue) conversion.
}
\usage{
col2rgb(col)
}
\arguments{
  \item{col}{vector of any of the three kind of \R colors, i.e., either
    a color name (an element of \code{\link{colors}()}), a hexadecimal
    string of the form \code{"#rrggbb"}, or an integer \code{i} meaning
    \code{palette()[i]}.}
}
\details{
  For integer colors, \code{0} is shorthand for the current
  \code{par("bg")}, and \code{\link{NA}} means \dQuote{nothing} which
  effectively does not draw the corresponding item.

  For character colors, \code{"NA"} is equivalent to \code{NA} above.
}
\value{
  an integer matrix with three rows and number of columns the length
  (and names if any) as \code{col}.
}
\author{Martin Maechler}
\seealso{\code{\link{rgb}}, \code{\link{colors}}, \code{\link{palette}}, etc.}
\examples{
col2rgb("peachpuff")
col2rgb(c(blu = "royalblue", reddish = "tomato")) # names kept

col2rgb(1:8)# the ones from the palette() :

col2rgb(paste("gold", 1:4, sep=""))

col2rgb("#08a0ff")
## all three kind of colors mixed :
col2rgb(c(red="red", palette= 1:3, hex="#abcdef"))

##-- NON-INTRODUCTORY examples --

grC <- col2rgb(paste("gray",0:100,sep=""))
table(print(diff(grC["red",])))# '2' or '3': almost equidistant
## The 'named' grays are in between {"slate gray" is not gray, strictly}
col2rgb(c(g66="gray66", darkg= "dark gray", g67="gray67",
          g74="gray74", gray =      "gray", g75="gray75",
          g82="gray82", light="light gray", g83="gray83"))

crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
t(crgb)## The whole table

ccodes <- c(256^(2:0) \%*\% crgb)## = internal codes
## How many names are 'aliases' of each other:
table(tcc <- table(ccodes))
length(uc <- unique(sort(ccodes))) # 502
## All the multiply named colors:
mult <- uc[tcc >= 2]
cl <- lapply(mult, function(m) cc[ccodes == m])
names(cl) <- apply(col2rgb(sapply(cl, function(x)x[1])),
                   2, function(n)paste(n, collapse=","))
utils::str(cl)
\dontrun{
 if(require(xgobi)) { ## Look at the color cube dynamically :
   tc <- t(crgb[, !duplicated(ccodes)])
   table(is.gray <- tc[,1] == tc[,2] & tc[,2] == tc[,3])# (397, 105)
   xgobi(tc, color = c("gold", "gray")[1 + is.gray])
 }
}
}
\keyword{color}
\keyword{dplot}
