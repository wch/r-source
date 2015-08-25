% File src/library/grDevices/man/xfig.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

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
     onefile = FALSE, encoding = "none",
     paper = "default", horizontal = TRUE,
     width = 0, height = 0, family = "Helvetica",
     pointsize = 12, bg = "transparent", fg = "black",
     pagecentre = TRUE, defaultfont = FALSE, textspecial = FALSE)
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
  \item{paper}{the size of paper region.  The choices
    are \code{"A4"}, \code{"Letter"} and \code{"Legal"} (and these
    can be lowercase).  A further choice is \code{"default"}, which
    is the default.  If this is selected, the papersize is taken
    from the option \code{"papersize"} if that is set to a non-empty
    value, otherwise \code{"A4"}.}
  \item{horizontal}{the orientation of the printed image, a
    logical.  Defaults to true, that is landscape orientation.}
  \item{width, height}{the width and height of the
    graphics region in inches.  The default is to use the entire
    page less a 0.5 inch overall margin in each direction.  (See
    \code{\link{postscript}} for further details.)}
  \item{family}{the font family to be used.  This must be one
    of \code{"AvantGarde"}, \code{"Bookman"}, \code{"Courier"},
    \code{"Helvetica"} (the default), \code{"Helvetica-Narrow"},
    \code{"NewCenturySchoolbook"}, \code{"Palatino"} or
    \code{"Times"}.  Any other value is replaced by
    \code{"Helvetica"}, with a warning.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the initial background color to be used.}
  \item{fg}{the initial foreground color to be used.}
  \item{pagecentre}{logical: should the device region be centred on the
    page?}
  \item{defaultfont}{logical: should the device use xfig's default
    font?}
  \item{textspecial}{logical: should the device set the textspecial flag
  for all text elements. This is useful when generating pstex from xfig
  figures.}
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

  Line widths as controlled by \code{par(lwd =)} are in multiples of
  5/6*1/72 inch.  Multiples less than 1 are allowed.  \code{pch = "."} with
  \code{cex = 1} corresponds to a square of side 1/72 inch.

  Windows users can make use of WinFIG
  (\url{http://www.schmidt-web-berlin.de/WinFIG.htm}, shareware), or
  XFig under Cygwin.
}
\note{
  Only some line textures (\code{0 <= lty < 4}) are used.  Eventually this
  may be partially remedied, but the XFig file format does not allow as
  general line textures as the \R model.  Unimplemented line textures
  are displayed as \emph{dash-double-dotted}.

  There is a limit of 512 colours (plus white and black) per file.
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is the paper size with a 0.25 inch
    border on all sides.
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths are integers, multiples of 5/432 inch.
    \item Circle radii are multiples of 1/1200 inch.
    \item Colours are interpreted by the viewing/printing application.
  }
}
\author{
  Brian Ripley.  Support for \code{defaultFont} and \code{textSpecial}
  contributed by Sebastian Fischmeister.
}
\seealso{
  \code{\link{Devices}},
  \code{\link{postscript}},
  \code{\link{ps.options}}.
}
\keyword{device}
