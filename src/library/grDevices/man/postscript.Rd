\name{postscript}
\alias{postscript}
\alias{ps.options}
%\alias{.PostScript.Options}
\alias{.ps.prolog}
\concept{encoding}
\title{PostScript Graphics}
\description{
  \code{postscript} starts the graphics device driver for producing
  PostScript graphics.

  The auxiliary function \code{ps.options} can be used to set and view
  (if called without arguments) default values for the arguments to
  \code{postscript}.
}
\synopsis{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE, family, title = "R Graphics Output",
           fonts = NULL, \dots)
ps.options(\dots, reset = FALSE, override.check = FALSE)
}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../unix/devPS.c
\usage{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE,
           paper, family, encoding, bg, fg,
           width, height, horizontal, pointsize,
           pagecentre, print.it, command,
           title = "R Graphics Output", fonts = NULL)

% paper, horizontal, width, height, family, pointsize, bg, fg)
ps.options(paper, horizontal, width, height, family, encoding,
           pointsize, bg, fg,
           onefile = TRUE, print.it = FALSE, append = FALSE,
           reset = FALSE, override.check = FALSE)
}
\arguments{
  \item{file}{a character string giving the name of the file.  If it is
    \code{""}, the output is piped to the command given by
    the argument \code{command}.
#ifdef unix
    If it is \code{"|cmd"}, the output is piped to the command given
    by \file{cmd}.
#endif

    For use with \code{onefile=FALSE} give a \code{printf} format such
    as \code{"Rplot\%03d.ps"} (the default in that case).
  }
%  \item{\dots}{further options for \code{postscript()}.}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"}, \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    Also, \code{"special"} can be used, when the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}, which is the default.  If this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and to \code{"a4"} if it is unset or empty.}
  \item{horizontal}{the orientation of the printed image, a
    logical. Defaults to true, that is landscape orientation on paper
    sizes with width less than height.}
  \item{width, height}{the width and height of the graphics region in
    inches.  The default is to use the entire page less a 0.25 inch
    border on each side.}
  \item{family}{the font family to be used.  EITHER a single character
    string OR a character vector of length
    four or five.  See the section \sQuote{Families}.}
  \item{encoding}{the name of an encoding file. Defaults to
#ifdef unix
"ISOLatin1.enc"
#endif
#ifdef windows
"WinAnsi.enc"
#endif
    in the \file{R\_HOME/afm} directory, which is used
    if the path does not contain a path separator.  An extension
    \code{".enc"} can be omitted.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.
    If \code{"transparent"} (or an equivalent specification), no
    background is painted.}
  \item{fg}{the default foreground color to be used.}
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file.  If false, generate a file name containing the page
    number and use an EPSF header and no \code{DocumentMedia} comment.}
  \item{pagecentre}{logical: should the device region be centred on the
    page: defaults to true.}
  \item{print.it}{logical: should the file be printed when the device is
    closed?  (This only applies if \code{file} is a real file name.)}
  \item{command}{the command to be used for \dQuote{printing}.  Defaults to
    option \code{"printcmd"}; this can also be selected as
    \code{"default"}.}
  \item{append}{logical; currently \bold{disregarded}; just there for
    compatibility reasons.}
  \item{reset, override.check}{logical arguments passed to
    \code{\link{check.options}}.  See the Examples.}
  \item{title}{title string to embed in the file.}
  \item{fonts}{a character vector specifying font family names for fonts
    which must be 
    included in the PostScript file.}
}
\details{
  \code{postscript} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are stored in that
  file.  This file can then be printed on a suitable device to obtain
  hard copy.

  A postscript plot can be printed via \code{postscript} in two ways.
  \enumerate{
    \item Setting \code{print.it = TRUE} causes the command given in
    argument \code{command} to be called with argument \code{"file"}
    when the device is closed.
    Note that the plot file is not deleted unless \code{command} arranges to
    delete it.

    \item \code{file=""} or \code{file="|cmd"} can be used to print
    using a pipe on systems that support \file{popen}.  Failure to open the
    command will probably be reported to the terminal but not to
    \file{popen}, in which case close the device by \code{dev.off}
    immediately.
  }
#ifdef windows
  Only the first of these will work on Windows, and the default
  \code{"printcmd"} is empty and will give an error if \code{print.it=TRUE}
  is used.  Suitable commands to spool a PostScript file to a printer
  can be found in \file{RedMon} suite available from
  \url{http://www.cs.wisc.edu/~ghost/index.html}.  The command will be run
  in a minimized window.   GSView 4.x provides
  \file{gsprint.exe} which may be even more convenient (it requires
  GhostScript version 6.0 or later).
#endif

  The postscript produced by \R is EPS (\emph{Encapsulated PostScript})
  compatible, and can be included into other documents, e.g., into
  LaTeX, using \code{\includegraphics{<filename>}}.  For use in this way
  you will probably want to set \code{horizontal = FALSE, onefile = FALSE,
    paper = "special"}.

  Most of the PostScript prologue used is taken from the \R character
  vector \code{.ps.prolog}.  This is marked in the output, and can be
  changed by changing that vector.  (This is only advisable for
  PostScript experts: the standard version is in
  \code{namespace:graphics}.)

  \code{ps.options} needs to be called before calling \code{postscript},
  and the default values it sets can be overridden by supplying
  arguments to \code{postscript}.

  A PostScript device has a default font, which can be set by the user
  via \code{family}.  If other fonts are to be used when drawing to
  the PostScript device, these must be declared when the
  device is created via \code{fonts};  the font family names for this
  argument are generic R graphics font family names (see the
  documentation for \code{postscriptFonts}).
}

\section{Families}{
  The argument \code{family} specifies the font family to be used.
  In normal use it is one of \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} or \code{"Times"}, and refers to the standard
  Adobe PostScript fonts of those names which are included (or cloned)
  in all common PostScript devices.

  Many PostScript emulators (including those based on
  \code{ghostscript}) use the URW equivalents of these fonts, which are
  \code{"URWGothic"}, \code{"URWBookman"}, \code{"NimbusMon"},
  \code{"NimbusSan"}, \code{"NimbusSanCond"}, \code{"CenturySch"},
  \code{"URWPalladio"} and \code{"NimbusRom"} respectively.  If your
  PostScript device is using URW fonts, you will obtain access to more
  characters and more appropriate metrics by using these names.  To make
  these easier to remember, \code{"URWHelvetica" == "NimbusSan"} and
  \code{"URWTimes" == "NimbusRom"} are also supported.

  It is also possible to specify \code{family="ComputerModern"}. This
  is intended to use with the Type 1 versions of the TeX CM fonts.  It
  will normally be possible to include such output in TeX or LaTeX
  provided it is processed with \code{dvips -Ppfb -j0} or the equivalent
  on your system. (\code{-j0} turns off font subsetting.)
  
  If the second form of argument \code{"family"} is used, it should be a
  character vector of four or five paths to Adobe Font Metric files for
  the regular, bold, italic, bold italic and (optionally) symbol fonts to
  be used.  If these paths do not contain the file separator, they are
  taken to refer to files in the \R directory \file{R\_HOME/afm}.  Thus the
  default Helvetica family can be specified by \code{family =
    c("hv______.afm",
    "hvb_____.afm", "hvo_____.afm", "hvbo____.afm", "sy______.afm")}.
  It is the user's responsibility to check that suitable fonts are made
  available, and that they contain the needed characters when
  re-encoded.  The fontnames used are taken from the \code{FontName}
  fields of the afm files.  The software including the PostScript plot
  file should either embed the font outlines (usually from \file{.pfb}
  or \file{.pfa} files) or use DSC comments to instruct the print
  spooler to do so.

  The \code{.afm} files for the first four fonts do not need to be in
  the correct encoding, but that for the symbol font must be.

  When \code{family="ComputerModern"} is used, the italic/bold-italic
  fonts used are slanted fonts (\code{cmsl10} and \code{cmbxsl10}).  To
  use text italic fonts instead, use
  \code{family = c("CM_regular_10.afm", "CM_boldx_10.afm", "cmti10.afm",
    "cmbxti10.afm", "CM_symbol_10.afm")}.
}

\section{Encodings}{
  Encodings describe which glyphs are used to display the character codes
  (in the range 0--255).  By default \R uses ISOLatin1 encoding, and
  the examples for \code{\link{text}} are in that encoding.  However,
  the encoding used on machines running \R may well be different, and by
  using the \code{encoding} argument the glyphs can be matched to
  encoding in use.

  None of this will matter if only ASCII characters (codes 32--126) are
  used as all the encodings agree over that range.  Some encodings are
  supersets of ISOLatin1, too.  However, if accented and special
  characters do not come out as you expect, you may need to change the
  encoding.  Three other encodings are supplied with \R:
  \code{"WinAnsi.enc"} and \code{"MacRoman.enc"} correspond to the
  encodings normally used on Windows and MacOS (at least by Adobe), and
  \code{"PDFDoc.enc"} is the first 256 characters of the Unicode
  encoding, the standard for PDF.

  If you change the encoding, it is your responsibility to ensure that
  the PostScript font contains the glyphs used.  One issue here is the Euro
  symbol which is in the WinAnsi and MacRoman encodings but may well not
  be in the PostScript fonts.  (It is in the URW variants; it is not in
  the supplied Adobe Font Metric files.)

  There is one exception.  Character 45 (\code{"-"}) is always set
  as minus (its value in Adobe ISOLatin1) even though it is hyphen in
  the other encodings.  Hyphen is available as character 173 (octal
  0255) in ISOLatin1.
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
    \code{ps.options} and \code{postscript}}.
}
\author{
  Support for Computer Modern fonts is based on a contribution by
  Brian D'Urso \email{durso@hussle.harvard.edu}.
}
\examples{
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
#endif

# for URW PostScript devices
postscript("foo.ps", family = "NimbusSan")

## for inclusion in Computer Modern TeX documents, perhaps
postscript("cm_test.eps", width = 4.0, height = 3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "ComputerModern")
## The resultant postscript file can be used by dvips -Ppfb -j0.

## To test out encodings, you can use
TestChars <- function(encoding="ISOLatin1", family="URWHelvetica")
{
    postscript(encoding=encoding, family=family)
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
## there will be many warnings. We use URW to get a complete enough
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

ps.options(bg = "pink")
utils::str(ps.options(reset = TRUE))

### ---- error checking of arguments: ----
ps.options(width=0:12, onefile=0, bg=pi)
# override the check for 'onefile', but not the others:
utils::str(ps.options(width=0:12, onefile=1, bg=pi,
                      override.check = c(FALSE,TRUE,FALSE)))
}
\keyword{device}
