\name{postscript}
\title{PostScript Graphics}
\usage{
postscript(file = "Rplots.ps", paper, horizontal, width, height,
           family, pointsize, bg, fg)
ps.options(paper, horizontal, width, height, family, pointsize, bg, fg,
           onefile = TRUE, print.it = FALSE, append = FALSE)
.PostScript.Options
}
\alias{postscript}
\alias{ps.options}
\alias{.PostScript.Options}
\arguments{
\item{file}{a character string giving the name of a file to print to.}
\item{paper}{the size of paper in the printer.  The choices are
\code{"a4"}, \code{"letter"}, \code{"legal"} and \code{"executive"}.}
\item{horizontal}{the orientation of the printed image, a logical.}
\item{width,height}{the width, and height of the graphics region in inches.
The default is to use the entire page.}
\item{family}{the font family to be used.
  This must be one of \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} or \code{"Times"}.}
\item{pointsize}{the default point size to be used.}
\item{bg}{the default background color to be used.}
\item{fg}{the default foreground color to be used.}
}
\description{
\code{postscript} start the graphics device driver for producing
PostScript graphics.  The file \code{file} is opened and the
PostScript commands needed to plot any graphics requested are
stored in that file.
This file can then be printed on a suitable device to obtain hard copy.

The auxiliary function \code{ps.options} can be used to set and view
(if called without arguments)
default values for the arguments to \code{postscript}.

The postscript produced by \R is EPS (\emph{Encapsulated PostScript})
compatible, and can be included into other documents, e.g. into LaTeX, using
\code{\epsfig{file=<filename>}}.
}
\seealso{
\code{\link{x11}}, \code{\link{macintosh}}, \code{\link{Devices}}.
}
\examples{
# open the file "foo.ps" for graphics output
postscript("foo.ps")

plot(xx,yy)# produce the desired graph(s)
## dev.off()# turn off the postscript device

all(unlist(ps.options()) == unlist(.PostScript.Options))
str(ps.options(reset = TRUE))

##- error checking of arguments:
ps.options(width=0:12, onefile=1, bg=pi)
%% If  override.check  was an argument to ps.options:
%%> str(ps.options(width=0:12, onefile=1, bg=pi, override.check = c(F,T,F)))
}
\keyword{device}
