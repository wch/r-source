\name{Defunct}
\title{Defunct Functions}
\alias{Defunct}
\alias{.Defunct}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{Version}
\alias{provide}
\alias{.Provided}
\alias{category}
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{rnchisq}
\alias{print.anova.glm}
\alias{print.anova.lm}
\alias{print.tabular}
\alias{print.plot}
\alias{save.plot}
\alias{system.test}
\alias{dotplot}
\alias{stripplot}
\alias{getenv}
%
\description{
  The functions listed here are no longer part of \R as they are not
  needed (any more).
}
\usage{
.Defunct()

%--- oldfun(.)
Version()
provide(package)
% Removed in 1.2.0
.Provided
category(\dots)
dnchisq(.)
pnchisq(.)
qnchisq(.)
rnchisq(.)
print.anova.glm(.)
print.anova.lm(.)
print.tabular(.)
print.plot(.)
save.plot(.)
system.test(.)
% </Removed 1.2.0>
% Removed in 1.3.0
dotplot(\dots)
stripplot(\dots)
getenv(\dots)
% </Removed 1.3.0>
}
\details{
  \code{.Defunct} is the function to which defunct functions are set.
  
  \code{category} has been an old-S function before there were factors;
  should be replaced by \code{\link{factor}} throughout!

  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  The new function \code{dev.print()} should now be used for saving
  plots to a file or printing them.

  \code{provide} and its object \code{.Provided} have been removed.
  They were never used for their intended purpose, to allow one
  package to subsume another.

  \code{dotplot} and \code{stripplot} have been renamed to
  \code{\link{dotchart}} and \code{\link{stripchart}}, respectively.

  \code{getenv} has been replaced by \code{\link{Sys.getenv}}.
}
\seealso{\code{\link{Deprecated}}}
\keyword{documentation}
\keyword{utilities}
