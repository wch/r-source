% File src/library/utils/man/winProgressBar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2018 R Core Team
% Distributed under GPL 2 or later

\name{winProgressBar}
\alias{winProgressBar}
\alias{getWinProgressBar}
\alias{setWinProgressBar}
\alias{close.winProgressBar}
\title{Progress Bars under MS Windows}
\description{
  Put up a Windows progress bar widget, update and access it.
}
\usage{
winProgressBar(title = "R progress bar", label = "",
               min = 0, max = 1, initial = 0, width = 300)

getWinProgressBar(pb)
setWinProgressBar(pb, value, title = NULL, label = NULL)
\method{close}{winProgressBar}(con, \dots)
}
\arguments{
  \item{title, label}{character strings, giving the window title and the
    label on the dialog box respectively.}
  \item{min, max}{(finite) numeric values for the extremes of the
    progress bar.}
  \item{initial, value}{initial or new value for the progress bar.}
  \item{width}{the width of the progress bar in pixels: the dialog box
    will be 40 pixels wider (plus frame).}
  \item{pb, con}{an object of class \code{"winProgressBar"}.}
  \item{\dots}{for consistency with the generic.}
}
\details{
  \code{winProgressBar} will display a progress bar centred on the
  screen.  Space will be allocated for the label only if it is non-empty.

  \code{setWinProgessBar} will update the value and for non-\code{NULL}
  values, the title and label (provided there was one when the widget
  was created).  Missing (\code{\link{NA}}) and out-of-range values of
  \code{value} will be (silently) ignored.

  The progress bar should be \code{close}d when finished with, but it
  will be garbage-collected once no \R object refers to it.
}
\value{
  For \code{winProgressBar} an object of class \code{"winProgressBar"}.

  For \code{getWinProgressBar} and \code{setWinProgressBar}, a
  length-one numeric vector giving the previous value (invisibly for
  \code{setWinProgressBar}).
}
\note{
  These functions are only available on Windows.% incl RStudio, not yet in ESS
}
\seealso{
  On all platforms,
  \code{\link{txtProgressBar}}, \code{\link{tkProgressBar}}
}
\examples{\donttest{% popups are irritating
pb <- winProgressBar("test progress bar", "Some information in \%",
                     0, 100, 50)
Sys.sleep(0.5)
u <- c(0, sort(runif(20, 0, 100)), 100)
for(i in u) {
    Sys.sleep(0.1)
    info <- sprintf("\%d\%\% done", round(i))
    setWinProgressBar(pb, i, sprintf("test (\%s)", info), info)
}
Sys.sleep(5)
close(pb)
}}
\keyword{utilities}
