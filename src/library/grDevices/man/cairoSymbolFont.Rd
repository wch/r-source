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
\details{
  On Cairo-based graphics devices, when drawing with a symbol font
  (e.g., \link{plotmath}),
  Adobe Symbol Encoding characters are converted to UTF8 code points.
  This conversion can use Private Use Area code points or not.
  It is useful to be able to specify this option because some fonts
  (e.g., OpenSymbol)
  have glyphs mapped to the Private Use Area and some fonts 
  (e.g., NimbusSans) do not.
}

\keyword{device}
