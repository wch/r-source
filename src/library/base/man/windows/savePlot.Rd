\name{savePlot}
\alias{savePlot}
\title{Save Windows Plot to a File}
\usage{
savePlot(filename="Rplot", type=c("wmf", "gif", "ps"),
         device=dev.cur())
}
\arguments{
    \item{filename}{The filename under which to save the plot, without
	the extension.}
    \item{type}{The type of plot, Windows metafile, GIF or PostScript.}
    \item{device}{A device number of a \code{windows} device, by default
    the current device.}
}
\description{
    Saves the current plot on a \code{windows} device to a file.
}
\details{
    This is equivalent to selecting the `Save as' menu item on the
    `File' menu of a devga device. Although \code{\link{dev.print}} can
    be used for metafile and PostScript plots, this is the only way to
    obtain a GIF file (directly).
}
\value{
  None, but a plot file will be created.
}
\author{Guido Masarotto, B. D. Ripley}

\seealso{\code{\link{dev.print}}}

\keyword{misc}
