\name{frame}
\title{Create / Start a New Plot Frame}
\usage{
plot.new()
frame()
}
\alias{plot.new}
\alias{frame}
\description{
  This function (\code{frame} is a \code{\link{.Alias}} for
  \code{plot.new}) causes the completion of plotting in the current plot
  (if there is one) and an advance to a new graphics frame.  This is
  used in all high-level plotting functions and also useful for skipping
  plots when a multi-figure region is in use.
}
\seealso{
  \code{\link{plot.window}}, \code{\link{plot.default}}.
}
\keyword{iplot}
\keyword{aplot}
