% File src/library/tcltk/man/tkProgressBar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008 R Core Team
% Distributed under GPL 2 or later

\name{tkProgressBar}
\alias{tkProgressBar}
\alias{getTkProgressBar}
\alias{setTkProgressBar}
\alias{close.tkProgressBar}
\title{Progress Bars via Tk}
\description{
  Put up a Tk progress bar widget.
}
\usage{
tkProgressBar(title = "R progress bar", label = "",
              min = 0, max = 1, initial = 0, width = 300)

getTkProgressBar(pb)
setTkProgressBar(pb, value, title = NULL, label = NULL)
\method{close}{tkProgressBar}(con, \dots)
}
\arguments{
  \item{title, label}{character strings, giving the window title and the
    label on the dialog box respectively.}
  \item{min, max}{(finite) numeric values for the extremes of the
    progress bar.}
  \item{initial, value}{initial or new value for the progress bar.}
  \item{width}{the width of the progress bar in pixels: the dialog box
    will be 40 pixels wider (plus frame).}
  \item{pb, con}{an object of class \code{"tkProgressBar"}.}
  \item{\dots}{for consistency with the generic.}
}
\details{
  \code{tkProgressBar} will display a widget containing a label and
  progress bar.

  \code{setTkProgessBar} will update the value and for non-\code{NULL}
  values, the title and label (provided there was one when the widget
  was created).  Missing (\code{\link{NA}}) and out-of-range values of
  \code{value} will be (silently) ignored.

  The progress bar should be \code{close}d when finished with.

  This will use the \code{ttk::progressbar} widget for Tk version 8.5 or
  later, otherwise \R's copy of BWidget's \code{progressbar}.
}
\value{
  For \code{tkProgressBar} an object of class \code{"tkProgressBar"}.

  For \code{getTkProgressBar} and \code{setTkProgressBar}, a
  length-one numeric vector giving the previous value (invisibly for
  \code{setTkProgressBar}).
}

\seealso{
  \code{\link{txtProgressBar}}
#ifdef windows

  \code{\link{winProgressBar}} for a version using Windows native
  controls (which this also does for Tk >= 8.5).
#endif
}

\examples{\donttest{% popups are irritating
pb <- tkProgressBar("test progress bar", "Some information in \%",
                    0, 100, 50)
Sys.sleep(0.5)
u <- c(0, sort(runif(20, 0, 100)), 100)
for(i in u) {
    Sys.sleep(0.1)
    info <- sprintf("\%d\%\% done", round(i))
    setTkProgressBar(pb, i, sprintf("test (\%s)", info), info)
}
Sys.sleep(5)
close(pb)
}}
\keyword{utilities}
