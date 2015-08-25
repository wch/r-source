% File src/library/grDevices/man/dev2bitmap.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{dev2bitmap}
\alias{dev2bitmap}
\alias{bitmap}

\title{Graphics Device for Bitmap Files via Ghostscript}
\usage{
bitmap(file, type = "png16m", height = 7, width = 7, res = 72,
       units = "in", pointsize, taa = NA, gaa = NA, \dots)

dev2bitmap(file, type = "png16m", height = 7, width = 7, res = 72,
           units = "in", pointsize, \dots,
           method = c("postscript", "pdf"), taa = NA, gaa = NA)
}
\arguments{
  \item{file}{The output file name, with an appropriate extension.}
  \item{type}{The type of bitmap.}
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
  be set by the environment variable \env{R_GSCMD}. If this is unset, a
  GhostScript executable will be looked for by name on your path: on a
  Unix alike \code{"gs"} is used, and on Windows the setting of the
  environment variable \env{GSC} is used, otherwise commands
  \code{"gswi64c.exe"} then \code{"gswin32c.exe"} are tried.

  The types available will depend on the version of \code{ghostscript},
  but are likely to include
  \code{"jpeg"}, \code{"jpegcmyk"}, \code{"jpeggray"},
  \code{"tiffcrle"}, \code{"tiffg3"},
  \code{"tiffg32d"}, \code{"tiffg4"}, \code{"tiffgray"}, \code{"tifflzw"},
  \code{"tiffpack"}, \code{"tiff12nc"}, \code{"tiff24nc"}, \code{"tiff32nc"}
  \code{"png16"}, \code{"png16m"}, \code{"png256"}, \code{"png48"},
  \code{"pngmono"}, \code{"pnggray"}, \code{"pngalpha"},
  \code{"bmp16"}, \code{"bmp16m"} \code{"bmp256"}, \code{"bmp32b"},
  \code{"bmpgray"}, \code{"bmpmono"}.

  The default type, \code{"png16m"}, supports 24-bit colour and
  anti-aliasing.  Type \code{"png256"} uses a palette of 256 colours and
  could give a more compact representation.  Monochrome graphs can use
  \code{"pngmono"}, or \code{"pnggray"} if anti-aliasing is desired.
  Plots with a transparent background and varying degrees of
  transparency should use \code{"pngalpha"}.

  Note that for a colour TIFF image you probably want \code{"tiff24nc"},
  which is 8-bit per channel RGB (the most common TIFF format).  None of
  the listed TIFF types support transparency.  \code{"tiff32nc"} uses
  8-bit per channel CMYK, which printers might require.

  For formats which contain a single image, a file specification like
  \code{Rplots\%03d.png} can be used: this is interpreted by Ghostscript.

  For \code{dev2bitmap} if just one of \code{width} and \code{height} is
  specified, the other is chosen to preserve the aspect ratio of the
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

\note{
#ifdef windows
  Use of \code{bitmap} will leave a temporary file (with file name
  starting \code{Rbit}).

#endif
  Although using \code{type = "pdfwrite"} will work for simple plots, it
  is not recommended.  Either use \code{\link{pdf}} to produce PDF
  directly, or call \command{ps2pdf -dAutoRotatePages=/None} on the
  output of \code{\link{postscript}}: that command is optimized to do
  the conversion to PDF in ways that these functions are not.
}

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.  These
  devices follow the underlying device, so when viewed at the stated
  \code{res}:

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is (for the standard Ghostscript
    setup) URW Nimbus Sans.
    \item Line widths are as a multiple of 1/96 inch, with no minimum.
    \item Circle of any radius are allowed.
    \item Colours are interpreted by the viewing/printing application.
  }
}

\seealso{
  \code{\link{savePlot}}, which for \code{windows} and
  \code{X11(type = "cairo")} provides a simple way to record a PNG
  record of the current plot.

  \code{\link{postscript}}, \code{\link{pdf}}, \code{\link{png}},
  \code{\link{jpeg}}, \code{\link{tiff}} and  \code{bmp}.

  To display an array of data, see \code{\link{image}}.
}

\keyword{utilities}
