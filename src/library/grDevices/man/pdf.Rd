\name{pdf}
\alias{pdf}
\title{PDF Graphics Device}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../main/devPS.c
\description{
  \code{pdf} starts the graphics device driver for producing PDF
  graphics.
}
\synopsis{
pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot\%03d.pdf"),
    width = 6, height = 6, onefile = TRUE, family,
    title = "R Graphics Output", fonts = NULL, version = "1.1",
    paper = "special", \dots)
}
\usage{
pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot\%03d.pdf"),
    width = 6, height = 6, onefile = TRUE, family = "Helvetica",
    title = "R Graphics Output", fonts = NULL, version = "1.1",
    paper, encoding, bg, fg, pointsize)
}
\arguments{
  \item{file}{a character string giving the name of the file.}
  \item{width, height}{the width and height of the graphics region in
    inches.}
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file.  If false, generate a file name containing the page
    number.}
  \item{family}{the font family to be used, one of \code{"AvantGarde"},
    \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
    \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
    \code{"Palatino"} or \code{"Times"}.  \bold{Note} the other
    specifications allowed for \code{\link{postscript}} are \bold{not}
    available.}
  \item{title}{title string to embed in the file.}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"}, \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    The default is \code{"special"}, which means that the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}; if this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and to \code{"a4"} if it is unset or empty.}
  \item{encoding}{the name of an encoding file.  Defaults to
#ifdef unix
\code{"ISOLatin1.enc"}
#endif
#ifdef windows
\code{"WinAnsi.enc"}
#endif
    in the \file{R\_HOME/afm} directory, which is used if the path does
    not contain a path separator.  An extension \code{".enc"} can be
    omitted.
#ifdef unix
    In a UTF-8 locale only \code{"ISOLatin1.enc"} is allowed.
#endif
  }
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.}
  \item{fg}{the default foreground color to be used.}
  \item{fonts}{a character vector specifying device-independent
    R graphics font family names for fonts
    which will be included in the PDF file.}
  \item{version}{a string describing the PDF version that will be
    used to produce output.}
}
\details{
  \code{pdf()} opens the file \code{file} and the PDF commands needed to
  plot any graphics requested are sent to that file.

  The \code{family} argument can be used to specify either a
  device-independent R graphics font family (see
  \code{postscriptFonts}) or a PDF-specific
  font family as the initial/default font for the device.

  If a device-independent R graphics font family is specified
  (e.g., via \code{par(family=)} in the graphics package),
  the PDF device makes use of the PostScript font mappings to
  convert the R graphics font family
  to a PDF-specific font family description.  R does \emph{not}
  embed fonts in the PDF file though, so it is only possible to
  use mappings to the font families that are
  assumed to be available in a PDF viewer: \code{"Times"} or
  \code{"Times New Roman"},
  \code{"Helvetica"} or \code{"Arial"}, \code{"Courier"},
  \code{"Symbol"}, and \code{"ZapfDingbats"}.
  
  See \code{\link{postscript}} for details of encodings, as the internal
  code is shared between the drivers.  The native PDF encoding is given
  in file \file{PDFDoc.enc}.

  \code{pdf} writes uncompressed PDF.  It is primarily intended for
  producing PDF graphics for inclusion in other documents, and
  PDF-includers such as \code{pdftex} are usually able to handle
  compression.

  At present the PDF is fairly simple, with each page being represented
  as a single stream.  The \R graphics model does not distinguish
  graphics objects at the level of the driver interface.

  The \code{version} argument modifies the sort of PDF code that
  gets produced.  At the moment this only concerns the production of
  transparent output.  The version must be greater than 1.4 for
  transparent output to be produced.  Specifying a lower version
  number may be useful if you want to produce PDF output that can be
  viewed on older PDF viewers.
}
\note{
  Acrobat Reader does not use the fonts specified but rather emulates
  them from multiple-master fonts.  This can be seen in imprecise
  centering of characters, for example the multiply and divide signs in
  Helvetica.
}
\seealso{
  \code{\link{postscriptFonts}},
  \code{\link{Devices}},
  \code{\link{postscript}}
}
\examples{
\dontrun{
## Test function for encodings
TestChars <- function(encoding="ISOLatin1", ...)
{
    pdf(encoding=encoding, ...)
    par(pty="s")
    plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="", xaxs="i", yaxs="i")
    title(paste("Centred chars in encoding", encoding))
    grid(17, 17, lty=1)
    for(i in c(32:255)) {
	x <- i %% 16
	y <- i %/% 16
	points(x, y, pch=i)
    }
    dev.off()
}
## there will be many warnings.
TestChars("ISOLatin2")
## doesn't view properly in US-spec Acrobat 5.05, but gs7.04 works.
## Lots of characters are not centred.
}}
\keyword{device}
