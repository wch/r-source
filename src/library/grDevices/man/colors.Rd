% File src/library/grDevices/man/colors.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Copyright 2002-2012 The R Foundation
% Distributed under GPL 2 or later

\name{colors}
\alias{colors}
\alias{colours}
\title{Color Names}
\description{
  Returns the built-in color names which \R knows about.
}
\usage{
colors (distinct = FALSE)
colours(distinct = FALSE)
}
\arguments{
  \item{distinct}{logical indicating if the colors returned should all be
    distinct; e.g., \code{"snow"} and \code{"snow1"} are effectively the
    same point in the \eqn{(0:255)^3} RGB space.}
}
\details{
  These color names can be used with a \code{col=} specification in
  graphics functions.

  An even wider variety of colors can be created with primitives
  \code{rgb}, \code{hsv} and \code{hcl}, or the derived \code{rainbow},
  \code{heat.colors}, etc.
}
\value{
  A character vector containing all the built-in color names.
}
\seealso{
  \code{\link{palette}} for setting the \sQuote{palette} of colors for
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

length(cl. <- colors(TRUE))
## only 502 of the 657 named ones

## ----------- Show all named colors and more:
demo("colors")
## -----------
}
\keyword{color}
\keyword{dplot}
\keyword{sysdata}
