\name{postscript}
\title{PostScript Graphics}
\usage{
postscript(file = "Rplots.ps", \dots)
%  paper, horizontal, width, height, family, pointsize, bg, fg)
ps.options(\dots, reset = FALSE, override.check = FALSE)
% (paper, horizontal, width, height, family, pointsize, bg, fg,
%            onefile = TRUE, print.it = FALSE, append = FALSE,
.PostScript.Options
}
\alias{postscript}
\alias{ps.options}
\alias{.PostScript.Options}
\arguments{
\item{file}{a character string giving the name of the file to print
  to. If this is \code{""}, the output is piped to the command given by
  \code{R_PRINTCMD} when \R was built.}
\item{\dots}{further options for \code{postscript()}.}
\item{paper}{the size of paper in the printer.  The choices are
  \code{"a4"}, \code{"letter"}, \code{"legal"} and
  \code{"executive"} (and these can be capitalized).
  Also, \code{"special"} can be used, when the \code{width}
  and \code{height} specify the paper size.  A further choice is
  \code{"default"}, which is the default.  If this is selected, the
  papersize is taken from the environment variable \code{"R_PAPERSIZE"}
  if that is set (which it usually is on Unix) and to \code{"a4"}
  if it is unset or empty.}
\item{horizontal}{the orientation of the printed image, a
    logical. Defaults to true, that is landscape orientation.}
\item{width, height}{the width and height of the graphics region in inches.
  The default is to use the entire page less a 0.25 inch border.}
\item{family}{the font family to be used.
  This must be one of \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} or \code{"Times"}.}
\item{pointsize}{the default point size to be used.}
\item{bg}{the default background color to be used.}
\item{fg}{the default foreground color to be used.}
\item{onefile}{logical: if true (the default) allow multiple figures
    in one file. If false, assume only one figure and give EPSF header
    and no \code{DocumentMedia} comment.}
\item{pagecentre}{logical: should the device region be centred on the page:
    defaults to true.}
\item{print.it, append}{logical; are currently \bold{disregarded};
    just there for compatibility reasons.}
}
%%>>> REAL DOC in SOURCE:    ../../../main/devices.c  &   ../../../unix/devPS.c
\description{
\code{postscript} starts the graphics device driver for producing
PostScript graphics.

The auxiliary function \code{ps.options} can be used to set and view
(if called without arguments)
default values for the arguments to \code{postscript}.
}

\details{
  \code{postscript(..)} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are stored in that file.
  This file can then be printed on a suitable device to obtain hard copy.
  Alternatively, \code{file= ""} can be used to print directly to the
  default printer (if the print command is set, which it usually is on
  Unix and is not on Windows).

  The postscript produced by \R is EPS (\emph{Encapsulated PostScript})
  compatible, and can be included into other documents, e.g. into LaTeX,
  using \code{\includegraphics{<filename>}}. For use in this way you will
  probably want to set \code{horizontal=FALSE, onefile=FALSE, paper="special"}.
}
\seealso{
    \code{\link{Devices}}, 
    {\code{\link{check.options}} which is called from both
        \code{ps.options} and \code{postscript}.}
}
\examples{
\dontrun{
 # open the file "foo.ps" for graphics output
 postscript("foo.ps")
}
\testonly{
xx <- seq(0, 7, length=701)
yy <- sin(xx)/xx; yy[1] <- 1
plot(xx,yy)                     # produce the desired graph(s)
}
\dontrun{dev.off()              # turn off the postscript device}

all(unlist(ps.options()) == unlist(.PostScript.Options))
ps.options(bg = "pink")
str(ps.options(reset = TRUE))

##- error checking of arguments:
ps.options(width=0:12, onefile=0, bg=pi)

## override the check for 'onefile', but not the others:
str(ps.options(width=0:12, onefile=1, bg=pi, override.check = c(F,T,F)))
}
\keyword{device}
