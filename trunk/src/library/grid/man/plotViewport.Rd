\name{plotViewport}
\alias{plotViewport}
\title{ Create a Viewport with a Standard Plot Layout }
\description{
  This is a convenience function for producing a viewport with
  the common S-style plot layout -- i.e., a central
  plot region surrounded by margins given in terms of a
  number of lines of text.
}
\usage{
plotViewport(margins=c(5.1, 4.1, 4.1, 2.1), ...)
}
\arguments{
  \item{margins}{ A numeric vector interpreted in the same way
    as \code{par(mar)} in base graphics. }
  \item{\dots}{ All other arguments will be passed to a call to
  the \code{viewport()} function. }
}
\value{
  A grid viewport object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{viewport}} and
  \code{\link{dataViewport}}.
}
\keyword{dplot}
