% File src/library/grDevices/man/cairoSymbolFont.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{cairoSymbolFont}
\alias{cairoSymbolFont}
\title{Specify a Symbol Font}
\description{
  Specify a symbol font for a Cairo-based graphics device.
  This function provides the opportunity to specify whether the font
  supports Private Use Area code points.
}
\usage{
cairoSymbolFont(family, usePUA = TRUE)
}
\arguments{
  \item{family}{A character vector giving the symbol font family name. }
  \item{usePUA}{Does the font support Private Use Area code points?}
}
\value{
  An object of class \code{"CairoSymbolFont"}.
}
\details{
  On Cairo-based graphics devices, when drawing with a symbol font
  (e.g., \link{plotmath}),
  Adobe Symbol Encoding characters are converted to UTF-8 code points.
  This conversion can use Private Use Area code points or not.
  It is useful to be able to specify this option because some fonts
  (e.g., the \I{OpenSymbol} font that is included in LibreOffice)
  have glyphs mapped to the Private Use Area and some fonts 
  (e.g., Nimbus Sans L, the URW Fonts equivalent of Helvetica) do not.
}
\seealso{
  \code{\link{cairo_pdf}}.
}
\examples{
\dontrun{
## If a font uses PUA, we can just specify the font name ...
cairo_pdf(symbolfamily="OpenSymbol")
dev.off()
## ... or equivalently ...
cairo_pdf(symbolfamily=cairoSymbolFont("OpenSymbol"))
dev.off()

## If a font does not use PUA, we must indicate that ...
cairo_pdf(symbolfamily=cairoSymbolFont("Nimbus Sans", usePUA=FALSE))
dev.off()
}
}
\keyword{device}
