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
    width = 6, height = 6, onefile = TRUE, title = "R Graphics Output", \dots)
}
\usage{
pdf(file = ifelse(onefile, "Rplots.pdf", "Rplot\%03d.pdf"),
    width = 6, height = 6, onefile = TRUE, family = "Helvetica",
    title = "R Graphics Output", encoding, bg, fg, pointsize)
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
    \code{"Palatino"} or \code{"Times"}. }
  \item{title}{title string to embed in the file.}
  \item{encoding}{the name of an encoding file.  Defaults to
#ifdef unix
\code{"ISOLatin1.enc"}
#endif
#ifdef windows
\code{"WinAnsi.enc"}
#endif
    in the \file{R\_HOME/afm} directory, which is used if the path does
    not contain a path separator.  An extension \code{".enc"} can be
    omitted.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.}
  \item{fg}{the default foreground color to be used.}
}
\details{
  \code{pdf()} opens the file \code{file} and the PDF commands needed to
  plot any graphics requested are sent to that file.

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
}
\note{
  Acrobat Reader does not use the fonts specified but rather emulates
  them from multiple-master fonts.  This can be seen in imprecise
  centering of characters, for example the multiply and divide signs in
  Helvetica.
}
\seealso{
  \code{\link{Devices}},
  \code{\link{postscript}}
}
\examples{
\dontrun{
## Test function for encodings
TestChars <- function(encoding="ISOLatin1")
{
    pdf(encoding=encoding)
    par(pty="s")
    plot(c(0,15), c(0,15), type="n", xlab="", ylab="")
    title(paste("Centred chars in encoding", encoding))
    grid(15, 15, lty=1)
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
