\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\usage{
.Deprecated(new)

dnchisq(.)
pnchisq(.)
qnchisq(.)
print.anova.lm(.)
print.anova.glm(.)
print.tabular(.)
print.plot(.)
save.plot(.)
}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{print.anova.lm}
\alias{print.anova.glm}
\alias{print.tabular}
\alias{print.plot}
\alias{save.plot}
\alias{Version}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.

  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  The new function \code{dev.print()} should now be used for saving
  plots to a file or printing them.

  \code{.Deprecated("<new name>")} is called from deprecated functions.
}
\seealso{\code{\link{Defunct}}}
\examples{
save.plot # to see how the ``Deprecated'' reference is made automagically
}
\keyword{documentation}
\keyword{utilities}
