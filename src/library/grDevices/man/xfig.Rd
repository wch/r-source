\name{xfig}
\alias{xfig}
\title{XFig Graphics Device}
\description{
  \code{xfig} starts the graphics device driver for producing XFig
  (version 3.2) graphics.

  The auxiliary function \code{ps.options} can be used to set and view
  (if called without arguments) default values for the arguments to
  \code{xfig} and \code{postscript}.
}
\usage{
xfig(file = ifelse(onefile, "Rplots.fig", "Rplot\%03d.fig"),
     onefile = FALSE, \dots)
}
\arguments{
  \item{file}{a character string giving the name of the file.  If it is
    \code{""}, the output is piped to the command given by the argument
    \code{command}.  For use with \code{onefile = FALSE} give a
    \code{printf} format such as \code{"Rplot\%d.fig"} (the default in
    that case).}
  \item{onefile}{logical: if true allow multiple figures
    in one file. If false, assume only one page per file and generate a
    file number containing the page number.}
  \item{\dots}{further arguments to \code{\link{ps.options}} accepted by
    \code{xfig()}:
    \describe{
      \item{\code{paper}}{the size of paper in the printer.  The choices
	are \code{"A4"}, \code{"Letter"} and \code{"Legal"} (and these
	can be lowercase).  A further choice is \code{"default"}, which
	is the default.  If this is selected, the papersize is taken
	from the option \code{"papersize"} if that is set and to
	\code{"A4"} if it is unset or empty.}
      \item{\code{horizontal}}{the orientation of the printed image, a
	logical.  Defaults to true, that is landscape orientation.}
      \item{\code{width}, \code{height}}{the width and height of the
	graphics region in inches.  The default is to use the entire
	page less a 0.25 inch border.}
      \item{\code{family}}{the font family to be used.  This must be one
	of \code{"AvantGarde"}, \code{"Bookman"}, \code{"Courier"},
	\code{"Helvetica"}, \code{"Helvetica-Narrow"},
	\code{"NewCenturySchoolbook"}, \code{"Palatino"} or
	\code{"Times"}.}
      \item{\code{pointsize}}{the default point size to be used.}
      \item{\code{bg}}{the default background color to be used.}
      \item{\code{fg}}{the default foreground color to be used.}
      \item{\code{pagecentre}}{logical: should the device region be
	centred on the page: defaults to \code{TRUE}.}
    }
  }
}
%%>>> REAL DOC in SOURCE:    ../../../main/devices.c  &   ../../../unix/devPS.c
\details{
  Although \code{xfig} can produce multiple plots in one file, the XFig
  format does not say how to separate or view them.  So
  \code{onefile = FALSE} is the default.

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  5/6*1/72 inch.  Multiples less than 1 are allowed.  \code{pch="."}
  corresponds to a square of side 1/72 inch.  
}
\note{
  Only some line textures (\code{0 <= lty < 4}) are used.  Eventually this
  will be partially remedied, but the XFig file format does not allow as
  general line textures as the \R model.  Unimplemented line textures
  are displayed as \emph{dash-double-dotted}.

  There is a limit of 512 colours (plus white and black) per file.
}
\seealso{
  \code{\link{Devices}},
  \code{\link{postscript}},
  \code{\link{ps.options}}.
}
\keyword{device}
