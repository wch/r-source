\name{dev2bitmap}
\alias{dev2bitmap}
\title{Copy Graphics Device to Bitmap File}
\usage{
dev2bitmap(file, type = "png256", height = 6, width = 6, res = 72,
           pointsize, \dots)
}
\arguments{
 \item{file}{The putput file name, with an appropriate extension.}
 \item{type}{The type of bitmap. the default is \code{"png256"}.}
 \item{height}{The plot height, in inches.}
 \item{width}{The plot width, in inches.}
 \item{res}{Resolution, in dots per inch.}
 \item{pointsize}{The pointsize to be used for text: defaults to
     something reasonable given the width and height}
 \item{\dots}{Other parameters passes to \code{\link{postscript}}.}
}
\description{
  Copy the current graphics device to a file in a bitmap graphics
  format.
}
\details{
  \code{dev2bitmap} works by copying the current device to a
  \code{\link{postscript}} device, and post-processing the output file
  using \code{ghostscript}.  You will need a recent version of
  \code{ghostscript} (5.10 and later have been tested): the full path to
  the executable can be set by the environment variable \code{"R_GSCMD"}.

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
}
\value{
    None.
}
\author{B. D. Ripley}

\section{Warning}{This is experimental and may be withdrawn in favour of
an equivalent device in a later version.}

\seealso{\code{\link{postscript}}}

\keyword{utilities}
