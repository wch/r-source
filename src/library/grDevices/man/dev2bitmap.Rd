\name{dev2bitmap}
\alias{dev2bitmap}
\alias{bitmap}
\title{Graphics Device for Bitmap Files via GhostScript}
\usage{
bitmap(file, type = "png256", height = 6, width = 6, res = 72,
       pointsize, \dots)
dev2bitmap(file, type = "png256", height = 6, width = 6, res = 72,
           pointsize, \dots)
}
\arguments{
  \item{file}{The output file name, with an appropriate extension.}
  \item{type}{The type of bitmap. the default is \code{"png256"}.}
  \item{height}{The plot height, in inches.}
  \item{width}{The plot width, in inches.}
  \item{res}{Resolution, in dots per inch.}
  \item{pointsize}{The pointsize to be used for text: defaults to
    something reasonable given the width and height}
  \item{\dots}{Other parameters passed to \code{\link{postscript}}.}
}
\description{
  \code{bitmap} generates a graphics file.
  \code{dev2bitmap} copies the current graphics device to a file in a
  graphics format.
}
\details{
  \code{dev2bitmap} works by copying the current device to a
  \code{\link{postscript}} device, and post-processing the output file
  using \code{ghostscript}.  \code{bitmap} works in the same way using a
  \code{postscript} device and postprocessing the output as
  \dQuote{printing}.

  You will need a version of \code{ghostscript} (5.10 and
  later have been tested): the full path to the executable can be set by
  the environment variable \env{R\_GSCMD}.

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
  \code{"png16"}, \code{"png256"}, \code{"png16m"}, \code{"jpeg"},
  \code{"jpeggray"}, \code{"pdfwrite"}.

  Note: despite the name of the functions they can produce PDF \emph{via}
  \code{type = "pdfwrite"}, and the PDF produced is not bitmapped.

  For formats which contain a single image, a file specification like
  \code{Rplots\%03d.png} can be used: this is interpreted by GhostScript.

  For \code{dev2bitmap} if just one of \code{width} and \code{height} is
  specified, the other is chosen to preserve aspect ratio of the
  device being copied.
}
\value{
  None.
}

#ifdef windows
\note{
  Use of \code{bitmap} will leave a temporary file (with file name
  starting \code{Rbit}).  Use of the \code{\link{pdf}}, \code{\link{bmp}},
  \code{\link{png}} and \code{\link{jpeg}} devices is preferable to
  using these functions.
}
#endif

\seealso{
  \code{\link{postscript}}, \code{\link{png}} and
  \code{\link{jpeg}} and on Windows \code{bmp}.

  \code{\link{pdf}} generate PDF directly.

  To display an array of data, see \code{\link{image}}.
}

\keyword{utilities}
