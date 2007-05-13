\name{postscript}
\alias{postscript}
\alias{.ps.prolog}
\concept{encoding}
\title{PostScript Graphics}
\description{
  \code{postscript} starts the graphics device driver for producing
  PostScript graphics.
}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../unix/devPS.c
\usage{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE, family,
           title = "R Graphics Output", fonts = NULL,
           encoding, bg, fg,
           width, height, horizontal, pointsize,
           paper, pagecentre, print.it, command, colormodel)
}
\arguments{
  \item{file}{a character string giving the name of the file.  If it is
    \code{""}, the output is piped to the command given by the argument
    \code{command}.
#ifdef unix
    If it is of the form \code{"|cmd"}, the output is piped to the
    command given by \file{cmd}.
#endif

    For use with \code{onefile = FALSE} give a \code{printf} format such
    as \code{"Rplot\%03d.ps"} (the default in that case).
  }
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file.  If false, generate a file name containing the page
    number and use an EPSF header and no \code{DocumentMedia} comment.}
  \item{family}{the initial font family to be used, normally as a
    character string.  See the section \sQuote{Families}.  This defaults
    to \code{"Helvetica"}.}
  \item{title}{title string to embed in the file.}
  \item{fonts}{a character vector specifying additional \R graphics font
    family names for font families whose declarations will be included
    in the PostScript file and are available for use with the device.
    See Details.}
  \item{encoding}{the name of an encoding file.  Defaults to
#ifdef unix
    \file{"ISOLatin1.enc"} unless the locale is recognized as
    corresponding to a language using ISO 8859-{5,7,13,15} or KOI8-{R,U},
#endif
#ifdef windows
    \file{"CP1250.enc"} (East European), \code{"CP1251.enc"} (Cyrillic),
    \code{"CP1253.enc"} (Greek) or \code{"CP1257.enc"} (Baltic) if one
    of those codepages is in use, otherwise \file{"WinAnsi.enc"}
    (codepage 1252),
#endif
    in the \file{enc} directory of package \pkg{grDevices}, which is used
    if the path does not contain a path separator.  An extension
    \code{".enc"} can be omitted.
  }
  \item{bg}{the default background color to be used.
    If \code{"transparent"} (or an equivalent specification), no
    background is painted.}
  \item{fg}{the default foreground color to be used.}
  \item{width, height}{the width and height of the graphics region in
    inches.  The default is to use the entire page less a 0.25 inch
    border on each side.}
  \item{horizontal}{the orientation of the printed image, a
    logical.  Defaults to true, that is landscape orientation on paper
    sizes with width less than height.}
  \item{pointsize}{the default point size to be used.  Strictly
    speaking, in bp, that is 1/72 of an inch, but approximately in points.}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"} (or \code{"us"}), \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    Also, \code{"special"} can be used, when the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}, which is the default.  If this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and to \code{"a4"} if it is unset or empty.}
  \item{pagecentre}{logical: should the device region be centred on the
    page? -- defaults to true.}
  \item{print.it}{logical: should the file be printed when the device is
    closed?  (This only applies if \code{file} is a real file name.)}
  \item{command}{the command to be used for \dQuote{printing}.  Defaults to
    option \code{"printcmd"}; this can also be selected as
    \code{"default"}. The length limit is \code{2*PATH_MAX},
#ifdef unix
    typically 8096 bytes.
#endif
#ifdef windows
    520 bytes.
#endif
  }
  \item{colormodel}{a character string describing the color model:
    currently allowed values as \code{"rgb"} (the default and the only
    model prior to \R 2.5.0), \code{"rgb-nogray"}, \code{"gray"}
    and \code{"cmyk"}.}
}
\details{
  \code{postscript} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are written to that
  file.  This file can then be printed on a suitable device to obtain
  hard copy.

  A postscript plot can be printed via \code{postscript} in two ways.
  \enumerate{
    \item Setting \code{print.it = TRUE} causes the command given in
    argument \code{command} to be called with argument \code{"file"}
    when the device is closed.  Note that the plot file is not deleted
    unless \code{command} arranges to delete it.

    \item \code{file=""} or \code{file="|cmd"} can be used to print
    using a pipe on systems that support \file{popen}.  Failure to open the
    command will probably be reported to the terminal but not to
    \file{popen}, in which case close the device by \code{dev.off}
    immediately.
  }
#ifdef windows
  Only the first of these will work on Windows, and the default
  \code{"printcmd"} is empty and will give an error if
  \code{print.it=TRUE} is used.  Suitable commands to spool a PostScript
  file to a printer can be found in \file{RedMon} suite available from
  \url{http://www.cs.wisc.edu/~ghost/index.html}.  The command will be
  run in a minimized window.   GSView 4.x provides \file{gsprint.exe}
  which may be even more convenient (it requires GhostScript version 6.0
  or later).
#endif

  The \code{file} argument is interpreted as a C integer format as used
  by \code{\link{sprintf}}, with integer argument the page number.
  The default gives files \file{Rplot001.ps}, \dots, \file{Rplot999.ps},
  \file{Rplot1000.ps}, \dots.

  The postscript produced for a single \R plot is EPS (\emph{Encapsulated
    PostScript}) compatible, and can be included into other documents,
  e.g., into LaTeX, using \code{\includegraphics{<filename>}}.  For use
  in this way you will probably want to set \code{horizontal = FALSE,
    onefile = FALSE, paper = "special"}.  Note that the bounding box is
  for the device region: if you find the white space around the plot
  region excessive, reduce the margins of the figure region via
  \code{\link{par}(mar=)}.

  Most of the PostScript prologue used is taken from the \R character
  vector \code{.ps.prolog}.  This is marked in the output, and can be
  changed by changing that vector.  (This is only advisable for
  PostScript experts: the standard version is in
  \code{namespace:grDevices}.)

  A PostScript device has a default family, which can be set by the user
  via \code{family}.  If other font families are to be used when drawing
  to the PostScript device, these must be declared when the device is
  created via \code{fonts};  the font family names for this argument are
  \R graphics font family names (see the documentation for
  \code{\link{postscriptFonts}}).

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  1/96 inch: multiples less than 1 are allowed.  \code{pch="."} with
  \code{cex = 1} corresponds to a square of side 1/72 inch, which is
  also the \sQuote{pixel} size assumed for graphics parameters such as
  \code{"cra"}.
}

\section{Families}{
  Font families are collections of fonts covering the five font faces,
  (conventionally plain, bold, italic, bold-italic and symbol) selected
  by the graphics parameter \code{\link{par}(font=)} or the grid
  parameter \code{\link{gpar}(fontface=)}.  Font families can be
  specified either as an an initial/default font family for the device
  via the \code{family} argument or after the device is opened by
  the graphics parameter \code{\link{par}(family=)} or the grid
  parameter \code{\link{gpar}(fontfamily=)}.  Families which will be
  used in addition to the initial family must be specified in the
  \code{fonts} argument when the device is opened.

  Font families are declared via a call to \code{\link{postscriptFonts}}.
  
  The argument \code{family} specifies the initial/default font family
  to be used.  In normal use it is one of \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} or \code{"Times"}, and refers to the standard Adobe
  PostScript fonts families of those names which are included (or
  cloned) in all common PostScript devices.

  Many PostScript emulators (including those based on
  \code{ghostscript}) use the URW equivalents of these fonts, which are
  \code{"URWGothic"}, \code{"URWBookman"}, \code{"NimbusMon"},
  \code{"NimbusSan"}, \code{"NimbusSanCond"}, \code{"CenturySch"},
  \code{"URWPalladio"} and \code{"NimbusRom"} respectively.  If your
  PostScript device is using URW fonts, you will obtain access to more
  characters and more appropriate metrics by using these names.  To make
  these easier to remember, \code{"URWHelvetica" == "NimbusSan"} and
  \code{"URWTimes" == "NimbusRom"} are also supported.

  Another type of family makes use of CID-keyed fonts for East Asian
  languages -- see \code{\link{postscriptFonts}}.

  The \code{family} argument is normally a character string naming a
  font family, but family objects generated by \code{\link{Type1Font}}
  and \code{\link{CIDFont}} are also accepted.
  For compatibility with earlier versions of \R, the initial family can
  also be specified as a vector of four or five afm files.

  Note that \R does not embed the font(s) used in the PostScript output:
  see \code{\link{embedFonts}} for a utility to help do so.
}

\section{Encodings}{
  Encodings describe which glyphs are used to display the character codes
  (in the range 0--255).  Most commonly \R uses ISOLatin1 encoding, and
  the examples for \code{\link{text}} are in that encoding.  However,
  the encoding used on machines running \R may well be different, and by
  using the \code{encoding} argument the glyphs can be matched to
  encoding in use.  This suffices for European and Cyrillic languages,
  but not for CJK languages.  For the latter, composite CID fonts are
  used.  These fonts are useful for other languages: for example they
  may contain Greek glyphs. (The rest of this section applies only when CID
  fonts are not used.)

  None of this will matter if only ASCII characters (codes 32--126) are
  used as all the encodings (except \code{"TeXtext"}) agree over that
  range.  Some encodings are supersets of ISOLatin1, too.  However, if
  accented and special characters do not come out as you expect, you may
  need to change the encoding.  Some other encodings are supplied with
  \R: \code{"WinAnsi.enc"} and \code{"MacRoman.enc"} correspond to the
  encodings normally used on Windows and Classic MacOS (at least by
  Adobe), and \code{"PDFDoc.enc"} is the first 256 characters of the
  Unicode encoding, the standard for PDF.  There are also encodings
  \code{"ISOLatin2.enc"}, \code{"CP1250.enc"}, \code{"ISOLatin7.enc"}
  (ISO 8859-13), \code{"CP1257.enc"}, and \code{"ISOLatin9.enc"} (ISO
  8859-15), \code{"Cyrillic.enc"} (ISO 8859-5), \code{"KOI8-R.enc"},
  \code{"KOI8-U.enc"}, \code{"CP1251.enc"}, \code{"Greek.enc"} (ISO
  8859-7) and \code{"CP1253.enc"}.  Note that many glyphs in these
  encodings are not in the fonts corresponding to the standard families.
  (The Adobe ones for all but Courier, Helvetica and Times cover little
  more than Latin-1, whereas the URW ones also cover Latin-2, Latin-7,
  Latin-9 and Cyrillic but no Greek.  The Adobe exceptions cover the
  Latin character sets, but not the Euro.)

  If you specify the encoding, it is your responsibility to ensure that
  the PostScript font contains the glyphs used.  One issue here is the Euro
  symbol which is in the WinAnsi and MacRoman encodings but may well not
  be in the PostScript fonts.  (It is in the URW variants; it is not in
  the supplied Adobe Font Metric files.)

  There is an exception.  Character 45 (\code{"-"}) is always set
  as minus (its value in Adobe ISOLatin1) even though it is hyphen in
  the other encodings.  Hyphen is available as character 173 (octal
  0255) in all the Latin encodings, Cyrillic and Greek.  There are some
  discrepancies in accounts of glyphs 39 and 96: the supplied encodings
  (except CP1250 and CP1251) treat these as \sQuote{quoteright} and
  \sQuote{quoteleft} (rather than \sQuote{quotesingle}/\sQuote{acute}
  and \sQuote{grave} respectively), as they are in the Adobe
  documentation.
}
\section{TeX fonts}{
  TeX has traditionally made use of fonts such as Computer Modern which
  are encoded rather differently, in a 7-bit encoding.  This encoding
  can be specified by \code{encoding = "TeXtext.enc"}, taking care that
  the ASCII characters \code{< > \\ _ \{ \}} are not available in those
  fonts.

  There are supplied families \code{"ComputerModern"} and
  \code{"ComputerModernItalic"} which use this encoding, and which are
  only supported for \code{postscript} (and not \code{pdf}).  They are
  intended to use with the Type 1 versions of the TeX CM fonts.  It will
  normally be possible to include such output in TeX or LaTeX provided
  it is processed with \code{dvips -Ppfb -j0} or the equivalent on your
  system. (\code{-j0} turns off font subsetting.)  When \code{family =
    "ComputerModern"} is used, the italic/bold-italic fonts used are
  slanted fonts (\code{cmsl10} and \code{cmbxsl10}).  To use text italic
  fonts instead, set \code{family = "ComputerModernItalic"}.

  These families use the TeX math italic and  symbol fonts for a
  comprehensive but incomplete coverage of the glyphs covered by the
  Adobe symbol font in other families.  This is achieved by
  special-casing the postscript code generated from the supplied
  \file{CM\_symbol\_10.afm}.
}
\section{Color models}{
  The default color model is RGB, with pure gray colors expressed as
  greyscales.  Color model \code{"rgb-nogray"} uses only  RGB, model
  \code{"cmyk"} only CMYK, and model \code{"gray"} only greyscales (and
  selecting any other colour is an error).  Nothing in \R specifies the
  interpretation of the RGB or CMYK color spaces, and the simplest
  possible conversion to CMYK is used
  (\url{http://en.wikipedia.org/wiki/CMYK_color_model#Mapping_RGB_to_CMYK}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{postscriptFonts}},
  \code{\link{Devices}},
  {\code{\link{check.options}} which is called from both
    \code{\link{ps.options}} and \code{postscript}}.

  More details of font families and encodings and especially handling
  text in a non-Latin-1 encoding and embedding fonts can be found in

  Paul Murrell and Brian Ripley (2006) Non-standard fonts in PostScript
  and PDF graphics. \emph{R News}, 6(2):41--47.
  \url{http://cran.r-project.org/doc/Rnews/Rnews_2006-2.pdf}.
}
\author{
  Support for Computer Modern fonts is based on a contribution by
  Brian D'Urso \email{durso@hussle.harvard.edu}.
}
\examples{
require(graphics)
\dontrun{
# open the file "foo.ps" for graphics output
postscript("foo.ps")
# produce the desired graph(s)
dev.off()              # turn off the postscript device
#ifdef unix
postscript("|lp -dlw")
# produce the desired graph(s)
dev.off()              # plot will appear on printer
#endif
#ifdef windows
options(printcmd='redpr -P"\\\\markov\\lw"')
postscript(file=tempfile("Rps."), print.it=TRUE)
# produce the desired graph(s)
dev.off()              # send plot file to the printer
## alternative using GSView 4.x
options(printcmd='/GhostGum/gsview/gsprint -query')
#endif

# for URW PostScript devices
postscript("foo.ps", family = "NimbusSan")

## for inclusion in Computer Modern TeX documents, perhaps
postscript("cm_test.eps", width = 4.0, height = 3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "ComputerModern", encoding = "TeXtext.enc")
## The resultant postscript file can be used by dvips -Ppfb -j0.

## To test out encodings, you can use
TestChars <- function(encoding="ISOLatin1", family="URWHelvetica")
{
    postscript(encoding=encoding, family=family)
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
## there will be many warnings.  We use URW to get a complete enough
## set of font metrics.
TestChars()
TestChars("ISOLatin2")
TestChars("WinAnsi")
}
\dontshow{
xx <- seq(0, 7, length=701)
yy <- sin(xx)/xx; yy[1] <- 1
plot(xx,yy)                     # produce the desired graph(s)
}
}
\keyword{device}
