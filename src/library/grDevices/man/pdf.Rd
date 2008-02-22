% File src/library/grDevices/man/pdf.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{pdf}
\alias{pdf}
\encoding{latin1}

\title{PDF Graphics Device}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../main/devPS.c
\description{
  \code{pdf} starts the graphics device driver for producing PDF
  graphics.
}
\usage{
pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot\%03d.pdf"),
    width, height, onefile, family, title, fonts, version,
    paper, encoding, bg, fg, pointsize, pagecentre)
}
\arguments{
  \item{file}{a character string giving the name of the file.
    For use with \code{onefile=FALSE} give a C integer format such
    as \code{"Rplot\%03d.pdf"} (the default in that case).
    (See \code{\link{postscript}} for further details.)
  }
  \item{width, height}{the width and height of the graphics region in
    inches.  The default values are \code{7}.}
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file.  If false, generate a file name containing the page
    number for each page.  Defaults to  \code{TRUE}.}
  \item{family}{the font family to be used, see
    \code{\link{postscript}}.  Defaults to \code{"Helvetica"}. }
  \item{title}{title string to embed as the \code{/Title} field in the
    file.  Defaults to \code{"R Graphics Output"}.}
  \item{fonts}{a character vector specifying \R graphics font family
    names for fonts which will be included in the PDF file.  Defaults to
    \code{NULL}.}
  \item{version}{a string describing the PDF version that will be
    required to view the output.  This is a minimum, and will be
    increased (with a warning) if necessary.  Defaults to \code{"1.1"}. }
  \item{paper}{the target paper size.  The choices are
    \code{"a4"}, \code{"letter"}, \code{"legal"} (or \code{"us"}) and
    \code{"executive"} (and these can be capitalized), or \code{"a4r"}
    and \code{"USr"} for rotated (\sQuote{landscape}).
    The default is \code{"special"}, which means that the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}; if this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and as \code{"a4"} if it is unset or empty.
    Defaults \code{"special"}.
  }
  \item{encoding}{the name of an encoding file.  See
    \code{\link{postscript}} for details.  Defaults to \code{"default"}.}
  \item{bg}{the default background color to be used.  Defaults to
    \code{"transparent"}.}
  \item{fg}{the default foreground color to be used.  Defaults to
   \code{"black"}.}
  \item{pointsize}{the default point size to be used.  Strictly
    speaking, in bp, that is 1/72 of an inch, but approximately in
    points.  Defaults to \code{12}.}
  \item{pagecentre}{logical: should the device region be centred on the
    page? -- is only relevant for \code{paper != "special"}.  Defaults
    to true.}
}
\details{
  All arguments except \code{file} default to values given by
  \code{\link{pdf.options}()}.  The ultimate defaults are quoted in the
  arguments section.
    
  \code{pdf()} opens the file \code{file} and the PDF commands needed to
  plot any graphics requested are sent to that file.

  The \code{file} argument is interpreted as a C integer format as used
  by \code{\link{sprintf}}, with integer argument the page number.
  The default gives files \file{Rplot001.pdf}, \dots, \file{Rplot999.pdf},
  \file{Rplot1000.pdf}, \dots.
  
  The \code{family} argument can be used to specify a PDF-specific
  font family as the initial/default font for the device.

  If a device-independent \R graphics font family is specified (e.g., via
  \code{par(family=)} in the graphics package), the PDF device makes use
  of the PostScript font mappings to convert the \R graphics font family
  to a PDF-specific font family description.  (See the
  documentation for \code{\link{pdfFonts}}.)

  \R does \emph{not} embed fonts in the PDF file, so it is only
  straightforward to use mappings to the font families that can be
  assumed to be available in any PDF viewer: \code{"Times"}
  (equivalently \code{"serif"}), \code{"Helvetica"} (equivalently
  \code{"sans"}), \code{"Courier"} (equivalently \code{"mono"}) and
  \code{"Symbol"} (equivalently \code{"symbol"}).  Other families may be
  specified, but it is the user's responsibility to ensure that these
  fonts are available on the system and third-party software, e.g.,
  Ghostscript, may be required to embed the fonts so that the PDF can be
  included in other documents (e.g., LaTeX): see
  \code{\link{embedFonts}}.  The URW-based families described for
  \code{\link{postscript}} can be used with viewers
#ifdef unix
  set up to use URW fonts, which is usual with those based on
  \code{xpdf} or Ghostscript.
#endif
#ifdef windows
  such as GSView which utilise URW fonts.
#endif
  Since \code{\link{embedFonts}} makes use of Ghostscript, it should be
  able to embed the URW-based families for use with other viewers.
  
  See \code{\link{postscript}} for details of encodings, as the internal
  code is shared between the drivers.  The native PDF encoding is given
  in file \file{PDFDoc.enc}.

  \code{pdf} writes uncompressed PDF.  It is primarily intended for
  producing PDF graphics for inclusion in other documents, and
  PDF-includers such as \code{pdftex} are usually able to handle
  compression.

  The PDF produced is fairly simple, with each page being represented
  as a single stream.  The \R graphics model does not distinguish
  graphics objects at the level of the driver interface.

  The \code{version} argument declares the version of PDF that gets
  produced.  The version must be at least 1.4 for semi-transparent
  output to be understood, and at least 1.3 if CID fonts are to be used:
  if these features are used the version number will be increased (with
  a warning).  Specifying a low version number (as the default) is
  useful if you want to produce PDF output that can be viewed on older
  or non-Adobe PDF viewers.  (PDF 1.4 requires Acrobat 5 or later.)

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  1/96 inch.  Multiples less than 1 are allowed.  \code{pch="."} with
  \code{cex = 1} corresponds to a square of side 1/72 inch, which is
  also the \sQuote{pixel} size assumed for graphics parameters such as
  \code{"cra"}.

  The \code{paper} argument sets the \code{/MediaBox} entry in the file,
  which defaults to \code{width} by \code{height}.  If it is set to
  something other than \code{"special"}, a device region of the
  specified size is (by default) centred on the rectangle given by the
  paper size: if either \code{width} or \code{height} is less
  than \code{0.1} or too large to give a total margin of 0.5 inch, it is
  reset to the corresponding paper dimension minus 0.5.  Thus if you
  want the default behaviour of \code{\link{postscript}} use
  \code{pdf(paper="a4r", width=0, height=0)} to centre the device region
  on a landscape A4 page with 0.25 inch margins.
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths are as a multiple of 1/96 inch, with no minimum. 
    \item Circle of any radius are allowed.  Opaque circles of less than 10
    points radius are rendered using char 108 in the Dingbats font:
    all semi-transparent and larger circles using a \enc{Bézier}{Bezier}
    curve for each quadrant.
    \item Colours are interpreted by the viewing/printing application.
  }
}
\note{
  Acrobat Reader does not use the fonts specified but rather emulates
  them from multiple-master fonts.  This can be seen in imprecise
  centering of characters, for example the multiply and divide signs in
  Helvetica.  This can be circumvented by embedding fonts where possible.

  Acrobat Reader 5.x and later can be extended by support for Asian and
  (so-called) Central European fonts (the latter only for 7.x and
  later, part of the \sQuote{Extended} pack for 8.x), and this will be
  needed for the full use of encodings other than Latin-1.  See
  \url{http://www.adobe.com/products/acrobat/acrrasianfontpack.html}.
}
\seealso{
  \code{\link{pdfFonts}}, \code{\link{pdf.options}},
  \code{\link{embedFonts}},
  \code{\link{Devices}},
  \code{\link{postscript}}

  More details of font families and encodings and especially handling
  text in a non-Latin-1 encoding and embedding fonts can be found in

  Paul Murrell and Brian Ripley (2006) Non-standard fonts in PostScript
  and PDF graphics. \emph{R News}, 6(2):41--47.
  \url{http://cran.r-project.org/doc/Rnews/Rnews_2006-2.pdf}.
}
\examples{
\dontrun{
## Test function for encodings
TestChars <- function(encoding="ISOLatin1", ...)
{
    pdf(encoding=encoding, ...)
    par(pty="s")
    plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="",
         xaxs="i", yaxs="i")
         title(paste("Centred chars in encoding", encoding))
    grid(17, 17, lty=1)
    for(i in c(32:255)) {
	x <- i \%\% 16
	y <- i \%/\% 16
	points(x, y, pch=i)
    }
    dev.off()
}
## there will be many warnings.
TestChars("ISOLatin2")
## this does not view properly in older viewers.
TestChars("ISOLatin2", family="URWHelvetica")
## works well for viewing in gs-based viewers, and often in xpdf.
}}
\keyword{device}
