\name{colors}
\title{Color Names}
\usage{
colors()
}
\alias{colors}
\alias{colours}
\value{
This function returns the built-in color names which \R knows about.
These names can be used with a \code{col=} specification in graphics
functions.

An even wider variety of colors can be created with primitives \code{rgb} and
\code{hsv} or the derived \code{rainbow}, \code{heat.colors}, etc.
}
\seealso{
  \code{\link{palette}} for setting the ``palette'' of colors for
  \code{par(col=}\emph{<num>}\code{)};
  \code{\link{rgb}}, \code{\link{hsv}}, \code{\link{gray}};
  \code{\link{rainbow}} for a nice example;
  and \code{\link{heat.colors}}, \code{\link{topo.colors}} for images.
}
\examples{
str(colors())
}
\keyword{color}
\keyword{dplot}
\keyword{sysdata}

