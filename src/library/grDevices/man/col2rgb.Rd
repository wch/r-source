% File src/library/grDevices/man/col2rgb.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{col2rgb}
\alias{col2rgb}
\title{Color to RGB Conversion}
\description{
  \R color to RGB (red/green/blue) conversion.
}
\usage{
col2rgb(col, alpha = FALSE)
}
\arguments{
  \item{col}{vector of any of the three kinds of \R color specifications,
    i.e., either a color name (as listed by \code{\link{colors}()}), a
    hexadecimal string of the form \code{"#rrggbb"} or
    \code{"#rrggbbaa"} (see \code{\link{rgb}}), or a positive
    integer \code{i} meaning \code{\link{palette}()[i]} or \code{0} (see
    \sQuote{Details}).  Non-character values are coerced to integers.}
  \item{alpha}{logical value indicating whether alpha channel (opacity)
    values should be returned.}
}
\details{
  For integer colors, \code{0} is shorthand for the current
  \code{\link{par}("bg")} (and hence is only relevant to base graphics
  and not grid graphics, and is an error if \pkg{graphics} is not
  loaded: like \code{par} it will open a device if none is open).  Its
  used is deprecated.

  \code{\link{NA}} (as integer or character) and \code{"NA"} mean transparent.
}
\value{
  An integer matrix with three or four (for\code{alpha = TRUE}) rows
  and number of columns the length of \code{col}.  If \code{col} has
  names these are used as the column names of the return value.
}
\author{
  Martin Maechler and the R core team.
}
\seealso{
  \code{\link{rgb}}, \code{\link{colors}}, \code{\link{palette}}, etc.
}
\examples{
col2rgb("peachpuff")
col2rgb(c(blu = "royalblue", reddish = "tomato"))  # note: colnames

col2rgb(1:8)  # the ones from the palette() (if the default)

col2rgb(paste("gold", 1:4, sep = ""))

col2rgb("#08a0ff")
## all three kinds of color specifications:
col2rgb(c(red = "red", hex = "#abcdef"))
col2rgb(c(palette = 1:3))

##-- NON-INTRODUCTORY examples --

grC <- col2rgb(paste("gray", 0:100, sep = ""))
table(print(diff(grC["red",])))  # '2' or '3': almost equidistant
## The 'named' grays are in between {"slate gray" is not gray, strictly}
col2rgb(c(g66 = "gray66", darkg =  "dark gray", g67 = "gray67",
          g74 = "gray74", gray  =       "gray", g75 = "gray75",
          g82 = "gray82", light = "light gray", g83 = "gray83"))

crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
t(crgb)  # The whole table

ccodes <- c(256^(2:0) \%*\% crgb)  # = internal codes
## How many names are 'aliases' of each other:
table(tcc <- table(ccodes))
length(uc <- unique(sort(ccodes))) # 502
## All the multiply named colors:
mult <- uc[tcc >= 2]
cl <- lapply(mult, function(m) cc[ccodes == m])
names(cl) <- apply(col2rgb(sapply(cl, function(x)x[1])),
                   2, function(n)paste(n, collapse = ","))
utils::str(cl)
\dontrun{
 if(require(xgobi)) { ## Look at the color cube dynamically :
   tc <- t(crgb[, !duplicated(ccodes)])
   table(is.gray <- tc[,1] == tc[,2] & tc[,2] == tc[,3])  # (397, 105)
   xgobi(tc, color = c("gold", "gray")[1 + is.gray])
 }
}
}
\keyword{color}
\keyword{dplot}
