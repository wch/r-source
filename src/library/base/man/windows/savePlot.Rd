\name{savePlot}
\alias{savePlot}
\title{Save Windows Plot to a File}
\usage{
savePlot(filename="Rplot",
         type=c("wmf", "png", "jpeg", "jpg", "bmp", "ps"),
         device=dev.cur())
}
\arguments{
    \item{filename}{The filename under which to save the plot, without
	the extension.}
    \item{type}{The type of plot, Windows metafile, PNG, JPEG, BMP
      (Windows bitmap format)  or PostScript.}
    \item{device}{A device number of a \code{windows} device, by default
    the current device.}
}
\description{
    Saves the current plot on a \code{windows} device to a file.
}
\details{
    This is equivalent to selecting the `Save as' menu item on the
    `File' menu of a devga device.
}
\value{
  None, but a plot file will be created.
}
\author{Guido Masarotto, B. D. Ripley}

\seealso{\code{\link{dev.print}}}

\keyword{misc}
