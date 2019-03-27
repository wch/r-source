% File src/library/grDevices/man/palettes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{Palettes}
\alias{rainbow}
\alias{heat.colors}
\alias{terrain.colors}
\alias{topo.colors}
\alias{cm.colors}
\alias{hcl.colors}
\alias{hcl.pals}
\title{Color Palettes}
\encoding{UTF-8}
\description{
  Create a vector of \code{n} contiguous colors.
}
\usage{
hcl.colors(n, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)
hcl.pals(type = NULL)

rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n,
        alpha = 1, rev = FALSE)
heat.colors(n, alpha = 1, rev = FALSE)
terrain.colors(n, alpha = 1, rev = FALSE)
topo.colors(n, alpha = 1, rev = FALSE)
cm.colors(n, alpha = 1, rev = FALSE)
}
\arguments{
  \item{n}{the number of colors (\eqn{\ge 1}) to be in the
    palette.}
  \item{palette}{the name of the palette to generate colors from.
    The name is matched to the list of available palettes (listed in the
    details), ignoring upper vs. lower case, spaces, dashes, etc. in the
    matching.}
  \item{alpha}{the alpha transparency, a number in [0,1], see argument
    \code{alpha} in \code{\link{hsv}} and \code{\link{hcl}}, respectively.}
  \item{rev}{logical indicating whether the ordering of the colors should
    be reversed.}
  \item{fixup}{logical indicating whether the resulting color should be
    corrected to RGB coordinates in [0,1], see \code{\link{hcl}}.}
  \item{type}{the type of palettes to list: \code{"qualitative"},
    \code{"sequential"}, \code{"diverging"}, or \code{"divergingx"}.
    \code{NULL} lists all palettes.}
  \item{s, v}{the \sQuote{saturation} and \sQuote{value} to be used
    to complete the HSV color descriptions.}
  \item{start}{the (corrected) hue in [0,1] at which the rainbow
    begins.}
  \item{end}{the (corrected) hue in [0,1] at which the rainbow ends.}
}
\details{
All of these functions (except the helper function \code{hcl.pals}) create a
vector of \code{n} contiguous colors, either based on the HSV color space
(rainbow, heat, terrain, topography, and cyan-magenta colors) or the
perceptually-based HCL color space.

HSV (hue-saturation-value) is a simple transformation of the RGB
(red-green-blue) space which was therefore a convenient choice for color
palettes in many software systems (see also \code{\link{hsv}}). However, HSV
colors capture the perceptual properties hue, colorfulness/saturation/chroma,
and lightness/brightness/luminance/value only poorly and consequently
the corresponding palettes are typically not a good choice for statistical
graphics and data visualization.

In contrast, HCL (hue-chroma-luminance) colors are much more suitable for
capturing human color perception (see also \code{\link{hcl}} and better color
palettes can be derived based on HCL coordinates.
Conceptually, three types of palettes are often distinguished:
\itemize{
  \item Qualitative: For coding categorical information, i.e., where no
    particular ordering of categories is available and every color should
    receive the same perceptual weight.
  \item Sequential: For coding ordered/numeric information, i.e., where colors
    go from high to low (or vice versa).
  \item Diverging: Designed for coding numeric information around a central
    neutral value, i.e., where colors diverge from neutral to two extremes.
}
The \code{hcl.colors} function provides a basic and lean implementation of
the pre-specified palettes in the \pkg{colorspace} package. In addition to the
types above, the functions distinguish \dQuote{diverging} palettes where the two
arms are restricted to be rather balanced as opposed to flexible
\dQuote{divergingx} palettes that combine two sequential palettes without any
restrictions. The latter group also includes the cividis palette as it is based
on two different hues (blue and yellow) but it is actually a sequential palette
(going from dark to light).

The names of all available HCL palettes can be queried with the \code{hcl.pals}
function and they are also visualized by color swatches in the examples. Many of
the palettes closely approximate palettes of the same name from various other
packages (including \pkg{RColorBrewer}, \pkg{rcartocolor}, \pkg{viridis},
\pkg{scico}, among others). 

The default HCL palette is the widely used viridis palette which is a sequential
palette with relatively high chroma throughout so that it also works reasonably
well as a qualitative palette. However, while viridis is a rather robust default
palette, more suitable HCL palettes are available for most visualizations.

Note that the \code{rainbow} function implements the (in-)famous rainbow (or
jet) color palette that was used very frequently in many software packages but
has been widely criticized for its many perceptual problems.
It is specified by a \code{start} and \code{end} hue with red = 0,
yellow = \eqn{\frac 1 6}{1/6}, green = \eqn{\frac 2 6}{2/6},
cyan = \eqn{\frac 3 6}{3/6}, blue = \eqn{\frac 4 6}{4/6}, and
magenta = \eqn{\frac 5 6}{5/6}. However, these are very flashy and unbalanced
with respect to both chroma and luminance which can lead to various optical
illusions. Also, the hues that are equispaced in RGB space tend to cluster at
the red, green, and blue primaries. Therefore, it is recommended to use a
suitable palette from \code{hcl.colors} instead of
\code{rainbow}.
}
\value{A character vector \code{cv} containing either palette names (for
  \code{hcl.pals}) or \code{n} hex color codes (for all other functions).
  The latter can be used either to create a user-defined color palette for
  subsequent graphics by \code{\link{palette}(cv)}, a \code{col =} specification
  in graphics functions or in \code{par}.
}
\references{
  Wikipedia (2019).
  HCL color space -- Wikipedia, The Free Encyclopedia.
  \url{https://en.wikipedia.org/w/index.php?title=HCL_color_space&oldid=883465135}.
  Accessed March 26, 2019.

  Zeileis, A., Fisher, J. C., Hornik, K., Ihaka, R., McWhite, C. D., Murrell, P., Stauffer, R. and Wilke, C. O. (2019).
  \dQuote{ccolorspace: A toolbox for manipulating and assessing colors and palettes.}
  arXiv:1903.06490, arXiv.org E-Print Archive.
  \url{http://arxiv.org/abs/1903.06490}.

  Ihaka, R. (2003).
  \dQuote{Colour for presentation graphics.}
  Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003),
  March 20-22, 2003, Technische Universität Wien, Vienna, Austria.
  \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003}.

  Zeileis, A., Hornik, K. and Murrell, P. (2009).
  Escaping RGBland: Selecting colors for statistical graphics.
  \emph{Computational Statistics & Data Analysis}, \bold{53}, 3259--3270.
  \doi{10.1016/j.csda.2008.11.033}.
}
\seealso{
  \code{\link{colors}}, \code{\link{palette}},
  \code{\link{gray.colors}},
  \code{\link{hsv}},
  \code{\link{hcl}}, \code{\link{rgb}}, \code{\link{gray}} and
  \code{\link{col2rgb}} for translating to RGB numbers.
}
\examples{
require("graphics")

# color wheels in RGB/HSV and HCL space
par(mfrow = c(2, 2))
pie(rep(1, 12), col = rainbow(12), main = "RGB/HSV")
pie(rep(1, 12), col = hcl.colors(12, "Set 2"), main = "HCL")
par(mfrow = c(1, 1))

## color swatches for RGB/HSV palettes
demo.pal <-
  function(n, border = if (n < 32) "light gray" else NA,
           main = paste("color palettes;  n=", n),
           ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)",
                      "cm.colors(n)"))
{
    nt <- length(ch.col)
    i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
    plot(i, i+d, type = "n", yaxt = "n", ylab = "", main = main)
    for (k in 1:nt) {
        rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
             col = eval(parse(text = ch.col[k])), border = border)
        text(2*j,  k * j + dy/4, ch.col[k])
    }
}
demo.pal(16)

## color swatches for HCL palettes
hcl.swatch <- function(type = NULL, n = 5, nrow = 11,
  border = if (n < 15) "black" else NA) {
    palette <- hcl.pals(type)
    cols <- sapply(palette, hcl.colors, n = n)
    ncol <- ncol(cols)
    nswatch <- min(ncol, nrow)

    par(mar = rep(0.1, 4),
        mfrow = c(1, min(5, ncol \%/\% nrow + 1)),
        pin = c(1, 0.5 * nswatch),
        cex = 0.7)

    while (length(palette)) {
        subset <- 1:min(nrow, ncol(cols))
        plot.new()
        plot.window(c(0, n), c(0, nrow + 1))
        text(0, rev(subset) + 0.1, palette[subset], adj = c(0, 0))
        y <- rep(subset, each = n)
        rect(rep(0:(n-1), n), rev(y), rep(1:n, n), rev(y) - 0.5,
             col = cols[, subset], border = border)
        palette <- palette[-subset]
        cols <- cols[, -subset]
    }

    par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), cex = 1)
}
hcl.swatch()
hcl.swatch("qualitative")
hcl.swatch("sequential")
hcl.swatch("diverging")
hcl.swatch("divergingx")

## heat maps with sequential HCL palette (purple)
image(volcano, col = hcl.colors(11, "purples", rev = TRUE))
filled.contour(volcano, nlevels = 10,
               color.palette = function(n, ...)
                   hcl.colors(n, "purples", rev = TRUE, ...))

## list available HCL color palettes
hcl.pals("qualitative")
hcl.pals("sequential")
hcl.pals("diverging")
hcl.pals("divergingx")
}
\keyword{color}
\keyword{dplot}
