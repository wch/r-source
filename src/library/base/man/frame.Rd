\name{frame}
\title{Create / Start a New Plot Frame}
\usage{
plot.new(ask = NA)
frame(ask = NA)
}
\alias{plot.new}
\alias{frame}
\arguments{
\item{ask}{logical, defaulting to \code{par("ask")}.}
}
\description{
The function
causes the completion of plotting in the current plot
and an advance to a new graphics frame.
This is used in all high-level plotting functions and also useful for
skipping plots when a multi-figure region is in use.
}
\seealso{
\code{\link{plot.window}}, \code{\link{plot.default}}.
}
\keyword{iplot}
\keyword{aplot}
