\name{box}
\title{Draw a Box around a Plot}
\usage{
box(\dots)
}
\alias{box}
\arguments{
\item{\dots}{the graphical parameters \code{bty},
\code{lty} and \code{col} can be specified as arguments
to this function.}
}
\value{
This function is invoked for its side effect which is to
draws a box around the current plot in the given color
and linetype.  The \code{bty} parameter determines
the type of box drawn.  See \code{par} for details.
}
\seealso{
\code{\link{rect}} for drawing of arbitrary rectangles.
}
\examples{
plot(1:7,abs(rnorm(7)), type='h', axes = F)
axis(1, labels = letters[1:7])
box(lty='137', col = 'red')
}
\keyword{aplot}
