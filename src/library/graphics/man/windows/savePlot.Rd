\name{savePlot}
\alias{savePlot}
\title{Save Windows Plot to a File}
\usage{
savePlot(filename="Rplot",
         type=c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),
         device=dev.cur())
}
\arguments{
  \item{filename}{The filename under which to save the plot, without
    the extension.}
  \item{type}{The type of plot, Windows metafile, PNG, JPEG, BMP
    (Windows bitmap format), PostScript or PDF.}
  \item{device}{A device number of a \code{windows} device, by default
    the current device.}
}
\description{
  Saves the current plot on a \code{windows} device to a file.
}
\details{
  This is equivalent to selecting the \sQuote{Save as} menu item on the
  \sQuote{File} menu of a \code{\link{windows}} device.

  Using \code{filename} as \code{"clipboard"} or \code{""} with
  \code{type = "wmf"} will copy to the clipboard.
}
\value{
  None, but a plot file will be created.
}

\seealso{
  \code{\link{windows}}, \code{\link{dev.print}}
}

\keyword{misc}
