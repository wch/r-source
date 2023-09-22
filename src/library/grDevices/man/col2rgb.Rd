% File src/library/grDevices/man/col2rgb.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
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
    hexadecimal string (see Details),
    or a positive integer \code{i} meaning \code{\link{palette}()[i]}.}
  \item{alpha}{logical value indicating whether the alpha channel (opacity)
    values should be returned.}
}
\details{
  \code{\link{NA}} (as integer or character) and \code{"NA"} mean
  transparent, which can also be specified as \code{"transparent"}.

  Values of \code{col} not of one of these types are coerced: real
  vectors are coerced to integer and other types to character.  (factors
  are coerced to character: in all other cases the class is
  ignored when doing the coercion.)

  Hexadecimal string colors can be in the long hexadecimal form (e.g., 
  \code{"#rrggbb"} or \code{"#rrggbbaa"}) or the short form (e.g, \code{"#rgb"}
  or \code{"#rgba"}). The short form is expanded to the long form by 
  replicating digits (not by adding zeroes), e.g., \code{"#rgb"} becomes
  \code{"#rrggbb"}. 

  Zero and negative values of \code{col} are an error.
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

# long and short form of hexadecimal notation
col2rgb(c(long = "#559955", short = "#595"))
# with alpha
col2rgb(c(long = "#559955BB", short = "#595B"), alpha = TRUE)

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

## How many names are 'aliases' of each other?
ccodes <- c(256^(2:0) \%*\% crgb)% = internal codes
cl <- split(cc, ccodes)
length(cl) # 502 distinct colors
table(tcc <- lengths(cl))
## All the multiply named colors:
clmult <- cl[tcc >= 2]
names(clmult) <- sapply(clmult, function(x) paste(crgb[,x[1]], collapse = ","))
utils::str(clmult)

## Look at the color cube:
tc <- t(crgb[, !duplicated(ccodes)])
cNms <- rownames(tc)
if(requireNamespace("lattice", quietly = TRUE))
    lattice::cloud(blue ~ red + green, data = as.data.frame(tc), col = cNms)
## The 8 corners of the color cube:
isC <- rowSums(tc == 0 | tc == 255) == 3
cNms[isC] # "white" "black" "blue" "cyan" "green" "magenta" "red" "yellow"
\dontshow{stopifnot(setequal(cNms[isC],
     c("white","black","blue","cyan","green","magenta","red","yellow")))}
table(is.gray <- tc[,1] == tc[,2] & tc[,2] == tc[,3])  # (397, 105)

\dontrun{## Look at the color cube dynamically:% only interactively %
 if(require("rgl")) {
   open3d(windowRect = c(50,50, 950, 950)) # large, so we see details
   plot3d (tc, col = cNms, size = 11) # --> rotate w/ mouse; enlarged corners:
   points3d(tc[isC,], col = cNms[isC], size=22)
   bg3d("darkgray") # (to "see more"); rotate around gray-axis:
   play3d(spin3d(axis = c(1, 1, 1), rpm = 2), duration = 30)
   if(FALSE) # add all names {zoom in with 2nd mouse button!}
     text3d(tc[!is.gray,], texts = cNms[!is.gray],
                             col = cNms[!is.gray], adj=-1/4, cex = 1/2)
   if(FALSE) { ## next version of {rgl}
     hover3d(tc, labels = cNms)
     message("Move mouse over plot to identify points.")
   } else { ##  click on blob to see colors()' name:
     identify3d(tc, labels=cNms)
   }
 }
}
}
\keyword{color}
\keyword{dplot}
