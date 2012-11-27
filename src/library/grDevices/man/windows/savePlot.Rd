% File src/library/grDevices/man/windows/savePlot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{savePlot}
\alias{savePlot}
\title{Save Windows Plot to a File}
\usage{
savePlot(filename = "Rplot",
         type = c("wmf", "emf", "png", "jpg", "jpeg", "bmp",
                  "tif", "tiff", "ps", "eps", "pdf"),
         device = dev.cur(),
         restoreConsole = TRUE)
}
\arguments{
  \item{filename}{The filename under which to save the plot.
    Tilde-expansion (see \code{\link{path.expand}} is supported.}
  \item{type}{The type of plot, Windows metafile, PNG, JPEG, BMP
    (Windows bitmap format), TIFF, PostScript or PDF.}
  \item{device}{A device number of a \code{windows} device, by default
    the current device.}
  \item{restoreConsole}{See the \sQuote{Details} section
    of \code{\link{windows}}.}
}
\description{
  Saves the current plot on a \code{windows} device to a file.
}
\details{
  This is equivalent to selecting the \sQuote{Save as} menu item on the
  \sQuote{File} menu of a \code{\link{windows}} device.

  If \code{filename} does not include a dot (\sQuote{.}), savePlot
  will add the file type as an extension; that is, the filename will
  be set to \code{paste(filename, type, sep = ".")}. If a dot is present in
  \code{filename}, the filename is assumed to include an extension and
  is used without change.

  Using \code{filename} as \code{"clipboard"} or \code{""} with
  \code{type = "wmf"} will copy to the clipboard.

  Types \code{"eps"} and \code{"ps"} are the same thing apart from the
  extension for the default \code{filename}.  Similarly
  \code{"wmf"}/\code{"emf"}, \code{"jpeg"}/\code{"jpg"} and
  \code{"tiff"}/\code{"tif"}.

  JPEG quality is 75\%, and TIFF is saved without compression.
}
\value{
  None, but a plot file will be created.
}

\note{
  There is a similar function of the same name but fewer types for
  cairo-based \code{X11} devices on Unix-alikes
}

\seealso{
  \code{\link{png}}, \code{\link{dev.print}}
}

\keyword{misc}
