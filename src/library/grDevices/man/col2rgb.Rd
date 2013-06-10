% File src/library/grDevices/man/col2rgb.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
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
    \code{"#rrggbbaa"} (see \code{\link{rgb}}), or a positive integer
    \code{i} meaning \code{\link{palette}()[i]}.}
  \item{alpha}{logical value indicating whether the alpha channel (opacity)
    values should be returned.}
}
\details{
  \code{\link{NA}} (as integer or character) and \code{"NA"} mean transparent.
  
  Values of \code{col} not of one of these types are coerced: real
  vectors are coerced to integer and other types to character.  (Prior to
  \R 3.0.2 factors were coerced to their integer codes: in all other
  cases the class is still ignored when doing the coercion.)
  
  Prior to \R 3.0.0, \code{col = 0} could be used to refer to the
  background color of the current device in base graphics: this is now
  an error.

  Before \R 3.0.0 negative integer values were accepted and wrapped
  around to colors in the palette: this was not how either the base or
  grid graphics systems usually resolved such values.  They are now
  always an error.
}
\value{
  An integer matrix with three or four (for \code{alpha = TRUE}) rows
  and number of columns the length of \code{col}.  If \code{col} has
  names these are used as the column names of the return value.
}
\author{
  Martin Maechler and the R core team.
}
\seealso{
  \code{\link{rgb}}, \code{\link{colors}}, \code{\link{palette}}, etc.

  The newer, more flexible interface, \code{\link{convertColor}()}.
}
\examples{
col2rgb("peachpuff")
col2rgb(c(blu = "royalblue", reddish = "tomato"))  # note: colnames

col2rgb(1:8)  # the ones from the palette() (if the default)

col2rgb(paste0("gold", 1:4))

col2rgb("#08a0ff")
## all three kinds of color specifications:
col2rgb(c(red = "red", hex = "#abcdef"))
col2rgb(c(palette = 1:3))

##-- NON-INTRODUCTORY examples --

grC <- col2rgb(paste0("gray", 0:100))
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
