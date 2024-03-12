% File src/library/grDevices/man/postscript.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

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
%%      ../../../main/devices.c  &   ../../../unix/devPS.c
\usage{
postscript(file = if(onefile) "Rplots.ps" else "Rplot\%03d.ps",
           onefile, family, title, fonts, encoding, bg, fg,
           width, height, horizontal, pointsize,
           paper, pagecentre, print.it, command,
           colormodel, useKerning, fillOddEven)
}
\arguments{
  \item{file}{a character string giving the file path.  If it is
    \code{""}, the output is piped to the command given by the argument
    \code{command}.
    If it is of the form \code{"|cmd"}, the output is piped to the
    command given by \command{cmd}.

    For use with \code{onefile = FALSE}, give a C integer format such as
    \code{"Rplot\%03d.ps"} (the default in that case).  The string
    should not otherwise contain a \code{\%}: if it is really necessary,
    use \code{\%\%} in the string for \code{\%} in the file name.  A
    single integer format matching the \link{regular expression}
    \code{"\%[#0 +=-]*[0-9.]*[diouxX]"} is allowed.

    Tilde expansion (see \code{\link{path.expand}}) is done. An input with
    a marked encoding is converted to the native encoding or an error is
    given.

    See also section \sQuote{File specifications} in the help
    for \code{\link{pdf}} for further details.
  }
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file.  If false, generate a file name containing the page
    number for each page and use an \abbr{EPSF} header and no
    \code{DocumentMedia} comment.  Defaults to \code{TRUE}.}
 \item{family}{the initial font family to be used, see the section
    \sQuote{Families} in \code{\link{pdf}}.  Defaults to \code{"Helvetica"}.}
  \item{title}{title string to embed as the \code{Title} comment in the
    file.  Defaults to \code{"R Graphics Output"}.}
  \item{fonts}{a character vector specifying additional \R graphics font
    family names for font families whose declarations will be included
    in the PostScript file and are available for use with the device.
    See \sQuote{Families} below.  Defaults to \code{NULL}.}
  \item{encoding}{the name of an encoding file.  See
    \code{\link{pdf}} for details.  Defaults to \code{"default"}.}
  \item{bg}{the initial background color to be used.  If
    \code{"transparent"} (or any other non-opaque colour), no background
    is painted.  Defaults to \code{"transparent"}.}
  \item{fg}{the initial foreground color to be used.  Defaults to
    \code{"black"}.}
  \item{width, height}{the width and height of the graphics region in
    inches.  Default to \code{0}.

    If \code{paper != "special"} and \code{width} or \code{height} is less
    than \code{0.1} or too large to give a total margin of 0.5 inch, the
    graphics region is reset to the corresponding paper dimension minus 0.5.
  }
  \item{horizontal}{the orientation of the printed image, a logical.
    Defaults to true, that is landscape orientation on paper sizes
    with width less than height.}
  \item{pointsize}{the default point size to be used.  Strictly
    speaking, in \abbr{bp}, that is 1/72 of an inch, but approximately in
    points.  Defaults to \code{12}.}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"} (or \code{"us"}), \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    Also, \code{"special"} can be used, when arguments \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"} (the default): If this is selected, the paper size
    is taken from the option \code{"papersize"} if that is set and to
    \code{"a4"} if it is unset or empty.}
  \item{pagecentre}{logical: should the device region be centred on the
    page?  Defaults to true.}
  \item{print.it}{logical: should the file be printed when the device is
    closed?  (This only applies if \code{file} is a real file name.)
    Defaults to false.}
  \item{command}{the command to be used for \sQuote{printing}.  Defaults
    to \code{"default"}, the value of option \code{"printcmd"}.  The
    length limit is \code{2*PATH_MAX},
    typically 8096 bytes on Unix-alikes and 520 bytes on Windows. Recent
    Windows systems may be configured to use long paths, raising this
    limit currently to 10000.
  }
  \item{colormodel}{a character string describing the color model:
    currently allowed values as \code{"srgb"}, \code{"srgb+gray"},
    \code{"rgb"}, \code{"rgb-nogray"}, \code{"gray"} (or \code{"grey")} and
    \code{"cmyk"}.  Defaults to \code{"srgb"}.  See section
    \sQuote{Color models}.}
  \item{useKerning}{logical.  Should kerning corrections be included in
    setting text and calculating string widths?  Defaults to \code{TRUE}.}
  \item{fillOddEven}{logical controlling the polygon fill mode:  see
      \code{\link{polygon}} for details.  Default \code{FALSE}.}
}
\details{
  All arguments except \code{file} default to values given by
  \code{\link{ps.options}()}.  The ultimate defaults are quoted in the
  arguments section.

  \code{postscript} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are written to that
  file.  This file can then be printed on a suitable device to obtain
  hard copy.

  The \code{file} argument is interpreted as a C integer format as used
  by \code{\link{sprintf}}, with integer argument the page number.
  The default gives files \file{Rplot001.ps}, \dots, \file{Rplot999.ps},
  \file{Rplot1000.ps}, \dots.

  The postscript produced for a single \R plot is EPS (\emph{Encapsulated
    PostScript}) compatible, and can be included into other documents,
  e.g., into LaTeX, using \samp{\includegraphics{<filename>}}.  For use
  in this way you will probably want to use \code{\link{setEPS}()} to
  set the defaults as \code{horizontal = FALSE, onefile = FALSE, paper =
    "special"}.  Note that the bounding box is for the \emph{device}
  region: if you find the white space around the plot region excessive,
  reduce the margins of the figure region via \code{\link{par}(mar = )}.

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

  Line widths as controlled by \code{par(lwd = )} are in multiples of
  1/96 inch: multiples less than 1 are allowed.  \code{pch = "."} with
  \code{cex = 1} corresponds to a square of side 1/72 inch, which is
  also the \sQuote{pixel} size assumed for graphics parameters such as
  \code{"cra"}.

  When the background colour is fully transparent (as is the initial
  default value), the PostScript produced does not paint the
  background. Almost all PostScript viewers will use a white canvas so
  the visual effect is if the background were white.  This will not be
  the case when printing onto coloured paper, though.
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
  \file{CM_symbol_10.afm}.
}

\section{Color models}{
  The default color model (\code{"srgb"}) is \abbr{sRGB}.

  The alternative \code{"srgb+gray"} uses \abbr{sRGB} for colors, but with pure
  gray colors (including black and white) expressed as greyscales (which
  results in smaller files and can be advantageous with some printer
  drivers).  Conversely, its files can be rendered much slower on some
  viewers, and there can be a noticeable discontinuity in color
  gradients involving gray or white.

  Other possibilities are \code{"gray"} (or \code{"grey"}) which used
  only greyscales (and converts other colours to a luminance), and
  \code{"cmyk"}.  The simplest possible conversion from \abbr{sRGB} to CMYK is
  used
  (\url{https://en.wikipedia.org/wiki/CMYK_color_model#Mapping_RGB_to_CMYK}),
  and raster images are output in RGB.

  Color models provided for backwards compatibility are \code{"rgb"}
  (which is RGB+gray) and \code{"rgb-nogray"} which use uncalibrated RGB
  (as used in \R prior to 2.13.0).  These result in slightly smaller
  files which may render faster, but do rely on the viewer being
  properly calibrated.
}

\section{Printing}{
  A postscript plot can be printed via \code{postscript} in two ways.
  \enumerate{
    \item Setting \code{print.it = TRUE} causes the command given in
    argument \code{command} to be called with argument \code{"file"}
    when the device is closed.  Note that the plot file is not deleted
    unless \code{command} arranges to delete it.

    \item \code{file = ""} or \code{file = "|cmd"} can be used to print
    using a pipe.  Failure to open the command will probably be reported
    to the terminal but not to \R, in which case close the
    device by \code{dev.off} immediately.
  }

  On Windows the default
  \code{"printcmd"} is empty and will give an error if
  \code{print.it = TRUE} is used.  Suitable commands to spool a PostScript
  file to a printer can be found in \file{RedMon} suite available from
  \url{http://pages.cs.wisc.edu/~ghost/index.html}.  The command will be
  run in a minimized window.  \I{GSView} 4.x provides \file{gsprint.exe}
  which may be more convenient (it requires Ghostscript version 6.50
  or later).
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \sQuote{R Internals} manual.

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths are as a multiple of 1/96 inch, with a minimum
    of 0.01 enforced.
    \item Circle of any radius are allowed.
    \item Colours are by default specified as \abbr{sRGB}.
  }

  At very small line widths, the line type may be forced to solid.

  Raster images are currently limited to opaque colours.
}

\note{
  If you see problems with postscript output, do remember that the
  problem is much more likely to be in your viewer than in \R.  Try
  another viewer if possible.  Symptoms for which the viewer has been at
  fault are apparent grids on image plots (turn off graphics
  anti-aliasing in your viewer if you can) and missing or incorrect glyphs
  in text (viewers silently doing font substitution).

  Unfortunately the default viewers on most Linux and macOS systems
  have these problems, and no obvious way to turn off graphics anti-aliasing.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}

\seealso{
  \code{\link{postscriptFonts}},
  \code{\link{Devices}},
  and \code{\link{check.options}} which is called from both
  \code{\link{ps.options}} and \code{postscript}.

  \code{\link{cairo_ps}} for another device that can produce PostScript.

  More details of font families and encodings and especially handling
  text in a non-Latin-1 encoding and embedding fonts can be found in

  \bibshow{
  Paul Murrell and Brian Ripley (2006).
  \dQuote{Non-standard fonts in PostScript and PDF graphics.}
  \emph{R News}, \bold{6}(2), 41--47.
  \url{https://www.r-project.org/doc/Rnews/Rnews_2006-2.pdf}.}
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

## On Unix-alikes only:
postscript("|lp -dlw")
# produce the desired graph(s)
dev.off()              # plot will appear on printer

## On Windows:
options(printcmd = 'redpr -P"\\\\printhost\\lw"')
postscript(file = tempfile("Rps."), print.it = TRUE)
# produce the desired graph(s)
dev.off()              # send plot file to the printer
## alternative using GSView 4.x :
options(printcmd = '/GhostGum/gsview/gsprint -query')


# for URW PostScript devices
postscript("foo.ps", family = "NimbusSan")

## for inclusion in Computer Modern TeX documents, perhaps
postscript("cm_test.eps", width = 4.0, height = 3.0,
           horizontal = FALSE, onefile = FALSE, paper = "special",
           family = "ComputerModern", encoding = "TeXtext.enc")
## The resultant postscript file can be used by dvips -Ppfb -j0.

## To test out encodings, you can use
TestChars <- function(encoding = "ISOLatin1", family = "URWHelvetica")
{
    postscript(encoding = encoding, family = family)
    par(pty = "s")
    plot(c(-1,16), c(-1,16), type = "n", xlab = "", ylab = "",
         xaxs = "i", yaxs = "i")
    title(paste("Centred chars in encoding", encoding))
    grid(17, 17, lty = 1)
    for(i in c(32:255)) {
        x <- i \%\% 16
        y <- i \%/\% 16
        points(x, y, pch = i)
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
xx <- seq(0, 7, length = 701)
yy <- sin(xx)/xx; yy[1] <- 1
plot(xx,yy)                     # produce the desired graph(s)
}
}
\keyword{device}
