\name{hcl}
\alias{hcl}
\encoding{latin1}
\title{HCL Color Specification}
\description{
  Create a vector of colors from vectors specifying hue,
  chroma and luminance.
}
\usage{
hcl(h = 0, c = 35, l = 85, alpha, fixup = TRUE)
}
\arguments{
  \item{h}{The hue of the color specified as an angle in the range
      [0,360].  0 yields red, 120 yields green 240 yields blue, etc.}
  \item{c}{The chroma of the color.  The upper bound for chroma depends
      on hue and luminance.}
  \item{l}{A value in the range [0,100] giving the luminance of the
        colour.  For a given combination of hue and chroma, only
        a subset of this range is possible.}
  \item{alpha}{numeric value in the range \code{[0,1]} for
    alpha transparency channel (0 means transparent and 1 means opaque).}
  \item{fixup}{a logical value which indicates whether the resulting
    RGB values should be corrected to ensure that a real color results.
    if \code{fixup} is \code{FALSE} RGB components lying outside the
    range [0,1] will result in an \code{NA} value.}
}
\details{
This function corresponds to polar coordinates in the CIE-LUV
color space. Steps of equal size in this space correspond to
approximately equal peceptual changes in color.  Thus, \code{hcl}
can be thought of as a perceptually based version of \code{hsv}.

The function is primarily intended as a way of computing
colors for filling areas in plots where area corresponds to a
numerical value (pie charts, bar charts, mosaic plots, histograms,
etc).  Choosing colors which have equal chroma and luminance
provides a way of minimising the irradiation illusion which
would otherwise produce a misleading impression of how large
the areas are.

The default values of chroma and luminance make it possible
to generate a full range of hues and have a relatively pleasant
pastel appearance.

The RGB values produced by this function correspond to the sRGB
color space used on most PC computer displays.  There are other
packages which provide more general color space facilities.
}
\value{
A vector of character strings which can be used as color
specifications by R graphics functions.
}
\references{
Ihaka, R. (2003).
Colour for Presentation Graphics, Proceedings of the 3rd International
Workshop on Distributed Statistical Computing (DSC 2003), March 20-22,
2003, Technische Universität Wien, Vienna, Austria.
\url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003}.
}
\author{Ross Ihaka}
\note{
At present there is no guarantee that the colours rendered by R
graphics devices will correspond to their sRGB description.
It is planned to adopt sRGB as the standard R color description
in future.
}
\seealso{
\code{\link{hsv}},
\code{\link{rgb}}.
}
\examples{
# The Foley and Van Dam PhD Data.
csd <- matrix(c( 4,2,4,6, 4,3,1,4, 4,7,7,1,
                 0,7,3,2, 4,5,3,2, 5,4,2,2,
                 3,1,3,0, 4,4,6,7, 1,10,8,7,
                 1,5,3,2, 1,5,2,1, 4,1,4,3,
                 0,3,0,6, 2,1,5,5), nr=4)

csphd =
function(colors)
barplot(csd, col = colors, ylim = c(0,30),
        names = 72:85, xlab = "Year", ylab = "Students",
        legend = c("Winter", "Spring", "Summer", "Fall"),
        main = "Computer Science PhD Graduates", las = 1)

# The Original (Metaphorical) Colors (Ouch!)
csphd(c("blue", "green", "yellow", "orange"))

# A Color Tetrad (Maximal Color Differences)
csphd(hcl(h = c(30, 120, 210, 300)))

# Same, but lighter and less colorful
# Turn of automatic correction to make sure
# that we have defined real colors.
csphd(hcl(h = c(30, 120, 210, 300),
          c = 20, l = 90, fixup = FALSE))

# Analogous Colors
# Good for those with red/green color confusion
csphd(hcl(h = seq(60, 240, by = 60)))

# Metaphorical Colors
csphd(hcl(h = seq(210, 60, length = 4)))

# Cool Colors
csphd(hcl(h = seq(120, 0, length = 4) + 150))

# Warm Colors
csphd(hcl(h = seq(120, 0, length = 4) - 30))

# Single Color
hist(rnorm(1000), col = hcl(240))
}
\keyword{color}
\keyword{dplot}
