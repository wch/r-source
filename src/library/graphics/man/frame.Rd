\name{frame}
\title{Create / Start a New Plot Frame}
\usage{
plot.new()
frame()
}
\alias{plot.new}
\alias{frame}
\description{
  This function (\code{frame} is an alias for
  \code{plot.new}) causes the completion of plotting in the current plot
  (if there is one) and an advance to a new graphics frame.  This is
  used in all high-level plotting functions and also useful for skipping
  plots when a multi-figure region is in use.
}
\details{
  There is a hook called \code{"plot.new"} (see \code{\link{setHook}})
  called immediately after advancing the frame, which is used in the
  testing code to annotate the new page.  The hook function(s) are
  called with no argument.  (If the value is a character string,
  \code{get} is called on it from within the \pkg{graphics} namespace.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (\code{frame}.)
}
\seealso{
  \code{\link{plot.window}}, \code{\link{plot.default}}.
}
\keyword{iplot}
\keyword{aplot}
