\name{pdf}
\alias{pdf}
\title{PDF Graphics Device}
\synopsis{
pdf(file = "Rplots.pdf", width=6, height = 6, \dots)
}
\usage{
pdf(file = "Rplots.pdf",  width=6, height = 6,
    family = "Helvetica", encoding, bg, fg, pointsize)
}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../unix/devPS.c
\description{
  \code{pdf} starts the graphics device driver for producing
  PDF graphics.
}
\arguments{
  \item{file}{a character string giving the name of the file.}
  \item{width, height}{the width and height of the graphics region in inches.}
  \item{family}{the font family to be used, one of \code{"AvantGarde"},
    \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
    \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
    \code{"Palatino"} or \code{"Times"}. }
  \item{encoding}{the name of an encoding file. Defaults to
    "ISOLatin1.enc" in the \file{R\_HOME/afm} directory.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.}
  \item{fg}{the default foreground color to be used.}
}
\details{
  \code{pdf(..)} opens the file \code{file} and the PDF
  commands needed to plot any graphics requested are sent to that file.

  See \code{\link{postscript}} for details of encodings, as the
  internal code is shared between the drivers.
}
\seealso{
    \code{\link{Devices}}, \code{\link{postscript}}
}
\keyword{device}
