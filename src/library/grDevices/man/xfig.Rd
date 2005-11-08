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
     onefile = FALSE, encoding = "none", \dots)
}
\arguments{
  \item{file}{a character string giving the name of the file.   For use
    with \code{onefile = FALSE} give a C integer format such as
    \code{"Rplot\%03d.fig"} (the default in that case).
    (See \code{\link{postscript}} for further details.)
  }
  \item{onefile}{logical: if true allow multiple figures
    in one file. If false, assume only one page per file and generate a
    file number containing the page number.}
  \item{encoding}{The encoding in which to write text strings.  The
    default is not to re-encode.  This can be any encoding recognized by
    \code{\link{iconv}}: in a Western UTF-8 locale you probably want to
    select an 8-bit encoding such as \code{latin1}, and in an East Asian
    locale an \code{EUC} encoding.  If re-encoding fails, the text
    strings will be written in the current encoding with a warning.}
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
	\code{"Times"}.  Any other value is replaced by
	\code{"Helvetica"} with a warning.}
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

  The \code{file} argument is interpreted as a C integer format as used
  by \code{\link{sprintf}}, with integer argument the page number.
  The default gives files \file{Rplot001.fig}, \dots, \file{Rplot999.fig},
  \file{Rplot1000.fig}, \dots.  

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  5/6*1/72 inch.  Multiples less than 1 are allowed.  \code{pch="."} with
  \code{cex = 1} corresponds to a square of side 1/72 inch.  
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
