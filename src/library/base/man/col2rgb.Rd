\name{col2rgb}
\alias{col2rgb}
\title{Color to RGB Conversion}
\description{
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
\value{
  an integer matrix with three rows and number of columns the length
  (and names if any) as \code{col}.
}
\author{Martin Maechler}
\seealso{\code{\link{rgb}}, \code{\link{colors}}, \code{\link{palette}}, etc.}
\examples{
col2rgb("peachpuff")
col2rgb(c(blu = "royalblue", reddish = "tomato")) # names kept

col2rgb(1:7)# the ones from the palette()
stopifnot(col2rgb(0) ==  # col = 0 is the background color:
    print(col2rgb(par("bg"))))

col2rgb(paste("gold", 1:4, sep=""))

stopifnot(col2rgb("#08a0ff") == c(8, 160, 255))
## all three kind of colors mixed :
col2rgb(c(red="red", palette= 1:3, hex="#abcdef"))

crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
t(crgb)## The whole table

## Internal ("unsigned int") codes:  FIXME{MM}: There's something wrong
ccodes <- c(256^(2:0) \%*\% crgb)
## How many names are `aliases' of each other:
(ttc <- table(tc <- table(ccodes)))
length(uc <- unique(ccodes)) # 502
mult <- uc[tc >= 2]
cl <- lapply(mult, function(m) cc[ccodes == m])
}
\keyword{color}
\keyword{dplot}
