\name{colors}
\alias{colors}
\alias{colours}
\title{Color Names}
\description{
  Returns the built-in color names which \R knows about.
}
\usage{
colors()
colours()
}
\details{
  These color names can be used with a \code{col=} specification in
  graphics functions.

  An even wider variety of colors can be created with primitives
  \code{rgb} and \code{hsv} or the derived \code{rainbow},
  \code{heat.colors}, etc.
}
\value{
  A character vector containing all the built-in color names.
}
\seealso{
  \code{\link{palette}} for setting the \dQuote{palette} of colors for
  \code{par(col=}\emph{<num>}\code{)};
  \code{\link{rgb}}, \code{\link{hsv}}, \code{\link{hcl}}, \code{\link{gray}};
  \code{\link{rainbow}} for a nice example;
  and \code{\link{heat.colors}}, \code{\link{topo.colors}} for images.

  \code{\link{col2rgb}} for translating to RGB numbers and extended
  examples.
}
\examples{
cl <- colors()
length(cl); cl[1:20]
}
\keyword{color}
\keyword{dplot}
\keyword{sysdata}
