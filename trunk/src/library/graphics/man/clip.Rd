% File src/library/graphics/man/clip.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2011 R Core Team
% Distributed under GPL 2 or later

\name{clip}
\alias{clip}
\title{Set Clipping Region}
\description{
  Set clipping region in user coordinates
}
\usage{
clip(x1, x2, y1, y2)
}
\arguments{
  \item{x1, x2, y1, y2}{user coordinates of clipping rectangle}
}
\details{
  How the clipping rectangle is set depends on the setting of
  \code{\link{par}("xpd")}: this function changes the current setting
  until the next high-level plotting command resets it.

  Clipping of lines, rectangles and polygons is done in the graphics
  engine, but clipping of text is if possible done in the device, so the
  effect of clipping text is device-dependent (and may result in text
  not wholly within the clipping region being omitted entirely).

  Exactly when the clipping region will be reset can be hard to
  predict.  \code{\link{plot.new}} always resets it.
  Functions such as \code{\link{lines}} and \code{\link{text}} only
  reset it if \code{\link{par}("xpd")} has been changed.  However,
  functions such as \code{\link{box}}, \code{\link{mtext}},
  \code{\link{title}} and \code{\link{plot.dendrogram}} can manipulate
  the \code{xpd} setting.
}
\seealso{
  \code{\link{par}}
}
\examples{
x <- rnorm(1000)
hist(x, xlim = c(-4,4))
usr <- par("usr")
clip(usr[1], -2, usr[3], usr[4])
hist(x, col = 'red', add = TRUE)
clip(2, usr[2], usr[3], usr[4])
hist(x, col = 'blue', add = TRUE)
do.call("clip", as.list(usr))  # reset to plot region
}
\keyword{dplot}
