% File src/library/grDevices/man/dev2bitmap.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{dev2bitmap}
\alias{dev2bitmap}
\alias{bitmap}
\title{Graphics Device for Bitmap Files via GhostScript}
\usage{
bitmap(file, type = "png16m", height = 7, width = 7, res = 72,
       units = "in", pointsize, taa = NA, gaa = NA, \dots)

dev2bitmap(file, type = "png16m", height = 7, width = 7, res = 72,
           units = "in", pointsize, \dots,
           method = c("postscript", "pdf"), taa = NA, gaa = NA)
}
\arguments{
  \item{file}{The output file name, with an appropriate extension.}
  \item{type}{The type of bitmap. the default is \code{"png256"}.}
  \item{width, height}{Dimensions of the display region.}
  \item{res}{Resolution, in dots per inch.}
  \item{units}{The units in which \code{height} and \code{width} are
    given.  Can be \code{in} (inches), \code{px} (pixels), \code{cm} or
    \code{mm}.}
  \item{pointsize}{The pointsize to be used for text: defaults to
    something reasonable given the width and height}
  \item{\dots}{Other parameters passed to \code{\link{postscript}} or
    \code{\link{pdf}}.}
  \item{method}{Should the plot be done by \code{\link{postscript}} or
    \code{\link{pdf}}?}
  \item{taa, gaa}{Number of bits of antialiasing for text and for graphics
    respectively.  Usually 4 (for best effect) or 2.  Not supported on
    all types.}
}
\description{
  \code{bitmap} generates a graphics file.
  \code{dev2bitmap} copies the current graphics device to a file in a
  graphics format.
}
\details{
  \code{dev2bitmap} works by copying the current device to a
  \code{\link{postscript}} or \code{\link{pdf}} device, and
  post-processing the output file using \code{ghostscript}.
  \code{bitmap} works in the same way using a \code{postscript} device
  and post-processing the output as \sQuote{printing}.

  You will need \code{ghostscript}: the full path to the executable can
  be set by the environment variable \env{R_GSCMD}.
#ifdef unix
  (If this is unset the command \code{"gs"} is used, which will work if
  it is in your path.)
#endif  
#ifdef windows
  (If this is unset the setting of \env{GSC} is used, otherwise command
  \code{"gswin32c.exe"}, which will work if it is in your \env{PATH}.)
#endif

  The types available will depend on the version of \code{ghostscript},
  but are likely to include
  \code{"pcxmono"}, \code{"pcxgray"}, \code{"pcx16"}, \code{"pcx256"},
  \code{"pcx24b"}, \code{"pcxcmyk"}, \code{"pbm"}, \code{"pbmraw"},
  \code{"pgm"}, \code{"pgmraw"}, \code{"pgnm"}, \code{"pgnmraw"},
  \code{"pnm"}, \code{"pnmraw"}, \code{"ppm"}, \code{"ppmraw"},
  \code{"pkm"}, \code{"pkmraw"}, \code{"tiffcrle"}, \code{"tiffg3"},
  \code{"tiffg32d"}, \code{"tiffg4"}, \code{"tifflzw"},
  \code{"tiffpack"}, \code{"tiff12nc"}, \code{"tiff24nc"},
  \code{"psmono"}, \code{"psgray"}, \code{"psrgb"}, \code{"bit"},
  \code{"bitrgb"}, \code{"bitcmyk"}, \code{"pngmono"}, \code{"pnggray"},
  \code{"pngalpha"}, \code{"png16"}, \code{"png256"}, \code{"png16m"},
  \code{"png48"}, \code{"jpeg"}, \code{"jpeggray"}, \code{"pdfwrite"}.
  
  The default type, \code{"png16m"} supports 24-bit colour and
  anti-aliasing.  Versions of \R prior to 2.7.0 defaulted to
  \code{"png256"}, which uses a palette of 256 colours and could be a
  more compact representation.  Monochrome graphs can use
  \code{"pngmono"}, or \code{"pnggray"} if anti-aliasing is desired.

  Note that for a colour TIFF image you probably want \code{"tiff24nc"},
  which is 8-bit per channel RGB (the most common TIFF format).  None of
  the listed TIFF types support transparency.
  
  For formats which contain a single image, a file specification like
  \code{Rplots\%03d.png} can be used: this is interpreted by GhostScript.

  For \code{dev2bitmap} if just one of \code{width} and \code{height} is
  specified, the other is chosen to preserve aspect ratio of the
  device being copied.  The main reason to prefer \code{method = "pdf"}
  over the default would be to allow semi-transparent colours to be used.

  For graphics parameters such as \code{"cra"} that need to work in
  pixels, the default resolution of 72dpi is always used.

#ifdef windows
  Paths for \code{file} and \env{R_GSCMD} which contain spaces are
  mapped to short names \emph{via} \code{\link{shortPathName}}.
#endif
}
\value{
  None.
}

#ifdef windows
\note{
  Use of \code{bitmap} will leave a temporary file (with file name
  starting \code{Rbit}).  Use of the \code{\link{pdf}}, \code{\link{bmp}},
  \code{\link{png}} and \code{\link{jpeg}} devices may be preferred to
  using these functions.
}
#endif

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.  These
  devices follow the underlying device, so when viewed at the stated
  \code{res}:

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is (for the standard GhostScript
    setup) URW Nimbus Sans.
    \item Line widths are as a multiple of 1/96 inch, with no minimum. 
    \item Circle of any radius are allowed.
    \item Colours are interpreted by the viewing/printing application.
  }
}

\seealso{
  \code{\link{savePlot}}, which for \code{windows} and
  \code{X11(type = "Cairo")} provides a simple way to record a PNG
  record of the current plot.
  
  \code{\link{postscript}}, \code{\link{pdf}}, \code{\link{png}} and
  \code{\link{jpeg}} and on Windows \code{bmp}.

  To display an array of data, see \code{\link{image}}.
}

\keyword{utilities}
