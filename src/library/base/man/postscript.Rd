\name{postscript}
\title{PostScript Graphics}
\synopsis{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE, family, \dots)
ps.options(\dots, reset = FALSE, override.check = FALSE)
}
\usage{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE,
           paper, family, encoding, bg, fg,
           width, height, horizontal, pointsize,
           pagecentre, print.it, command)

%  paper, horizontal, width, height, family, pointsize, bg, fg)
ps.options(paper, horizontal, width, height, family, encoding,
           pointsize, bg, fg,
           onefile = TRUE, print.it = FALSE, append = FALSE,
           reset = FALSE, override.check = FALSE)
.PostScript.Options
}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../unix/devPS.c
\description{
  \code{postscript} starts the graphics device driver for producing
  PostScript graphics.

  The auxiliary function \code{ps.options} can be used to set and view
  (if called without arguments)
  default values for the arguments to \code{postscript}.
}
\alias{postscript}
\alias{ps.options}
\alias{.PostScript.Options}
\alias{.ps.prolog}
\arguments{
  \item{file}{a character string giving the name of the file.  If it is
    \code{""}, the output is piped to the command given by
    the argument \code{command}.
#ifdef unix
    If it is \code{"|cmd"}, the output is piped to the command given
    by \file{cmd}.
#endif

    For use with \code{onefile=FALSE} give a
    \code{printf} format such as \code{"Rplot\%d.ps"} (the default in that
    case).
  }
  \item{\dots}{further options for \code{postscript()} :}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"}, \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    Also, \code{"special"} can be used, when the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}, which is the default.  If this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and to \code{"a4"} if it is unset or empty.}
  \item{horizontal}{the orientation of the printed image, a
    logical. Defaults to true, that is landscape orientation.}
  \item{width, height}{the width and height of the graphics region in inches.
    The default is to use the entire page less a 0.25 inch border on
    each side.}
  \item{family}{the font family to be used. EITHER a single character string
    which must be one of \code{"AvantGarde"},
    \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
    \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
    \code{"Palatino"} or \code{"Times"}, OR a character vector of length
    four. }
  \item{encoding}{the name of an encoding file. Defaults to
#ifdef unix
"ISOLatin1.enc"
#endif
#ifdef windows
"WinAnsi.enc"
#endif
#ifdef mac
"MacRoman.enc"
#endif
    in the \file{R\_HOME/afm} directory, which is used
    if the path does not contain a path separator.  An extension
    \code{".enc"} can be omitted.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.}
  \item{fg}{the default foreground color to be used.}
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file. If false, generate a
    file name containing the page number and give EPSF header
    and no \code{DocumentMedia} comment.}
  \item{pagecentre}{logical: should the device region be centred on the page:
    defaults to true.}
  \item{print.it}{logical: should the file be printed when the device is
    closed?  (This only applies if \code{file} is a real file name.)}
  \item{command}{the command to be used for ``printing''. Defaults to
    option \code{"printcmd"}; this can also be selected as \code{"default"}.}
  \item{append}{logical; currently \bold{disregarded}; just there for
    compatibility reasons.}
}
\details{
  \code{postscript(..)} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are stored in that file.
  This file can then be printed on a suitable device to obtain hard
  copy.

  A postscript plot can be printed via \code{postscript} in two ways.
  \enumerate{
    \item Setting \code{print.it = TRUE} causes the command given in
    argument \code{command} to be called with argument \code{"file"}
    when the device is closed.
    Note that the plot file is not deleted unless command arranges to
    delete it.

    \item \code{file=""} or \code{file="|cmd"} can be used to print
    using a pipe on systems that support \file{popen}.
  }
#ifdef windows
  Only the first of these will work on Windows, and the default
  \code{"printcmd"} is empty and will give an error if \code{print.it=TRUE}
  is used.  Suitable commands to spool a PostScript file to a printer
  can be found in \file{RedMon} suite available from
  \url{http://www.cs.wisc.edu/~ghost/rjl.html}.  The command will be run
  in a minimized window.
#endif

  The postscript produced by \R is EPS (\emph{Encapsulated PostScript})
  compatible, and can be included into other documents, e.g. into LaTeX,
  using \code{\includegraphics{<filename>}}.  For use in this way you will
  probably want to set \code{horizontal=FALSE, onefile=FALSE, paper="special"}.

  Most of the PostScript prologue used is taken from the \R character
  vector \code{.ps.prolog}.  This is marked in the output, and can be
  changed by changing that vector.  (This is only advisable for
  PostScript experts.)

  If the second form of argument \code{"family"} is used, it should be a
  set of four paths to Adobe Font Metric files for the regular, bold,
  italic and bold italic fonts to be used.  If these paths do not
  contain the file separator, they are taken to refer to files in the
  \R directory \file{R\_HOME/afm}.  Thus the default Helvetica family
  can be specified by \code{family = c("hv______.afm",
    "hvb_____.afm", "hvo_____.afm", "hvbo____.afm")}.

  It is the user's responsibility to check that suitable fonts are made
  available, and that they contain the needed characters when
  re-encoded.  The fontnames used are taken from the
  \code{FontName} fields of the \code{afm} files.  The software
  including the PostScript plot file should either embed the font
  outlines (usually from \code{.pfb} or \code{.pfa} files) or
  use DSC comments to instruct the print spooler to do so.

  As ISOLatin1 encoding is used, \code{-} is set as a minus and not
  as a hyphen.  Supply a hyphen (character 173) if that is what you need.

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
  the PostScript font contains the glyphs used .  One issue here is the Euro
  symbol which is in the WinAnsi and MacRoman encodings but may well not
  be in the PostScript fonts.  (It is in the URW variants; it is not in
  the supplied Adobe Font Metric files.)

  There is one exception.  Character 45 (\code{"-"}) is always set
  as minus (its value in Adobe ISOLatin1) even though it is hyphen in
  the other encodings.  Hyphen is available as character 173 (octal
  0255) in ISOLatin1.
}
\seealso{
    \code{\link{Devices}},
    {\code{\link{check.options}} which is called from both
        \code{ps.options} and \code{postscript}}.
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
postscript(file=tempfile("R.ps"), print.it=TRUE)
# produce the desired graph(s)
dev.off()              # send plot file to the printer
#endif
}
\testonly{
xx <- seq(0, 7, length=701)
yy <- sin(xx)/xx; yy[1] <- 1
plot(xx,yy)                     # produce the desired graph(s)
}

stopifnot(unlist(ps.options()) == unlist(.PostScript.Options))
ps.options(bg = "pink")
str(ps.options(reset = TRUE))

### ---- error checking of arguments: ----
ps.options(width=0:12, onefile=0, bg=pi)
# override the check for 'onefile', but not the others:
str(ps.options(width=0:12, onefile=1, bg=pi, override.check = c(F,T,F)))

\dontrun{###  ---- Use TeX's Computer Modern fonts --- 
## Only use alphanumeric chars here.
postscript(family=paste("/myfonts/afm/",
   c("cmr10", "cmbx10", "cmsl10", "cmbxsl10"), ".afm", sep=""))
## The resultant postscript file can be used by dvips provided
## font subsetting is disabled (by flag -j0)
}}
\keyword{device}
