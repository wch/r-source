\name{Defunct}
\title{Defunct Functions}
\alias{Defunct}
\alias{.Defunct}
\usage{
.Defunct()

%--- oldfun(.)
category(x, levels, labels, ordered, exclude)
dnchisq(.)
pnchisq(.)
qnchisq(.)
rnchisq(.)
print.anova.lm(.)
print.anova.glm(.)
print.tabular(.)
print.plot(.)
save.plot(.)
}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{category}
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{rnchisq}
\alias{print.anova.lm}
\alias{print.anova.glm}
\alias{print.tabular}
\alias{print.plot}
\alias{save.plot}
\alias{Version}
\alias{system.test}
%
\description{
  The functions listed here are no longer part of \R as they are not
  needed (any more).
}
\details{
  \code{category} has been an old-S function before there were factors;
  should be replaced by \code{\link{factor}} throughout!

  The \code{*chisq()} functions now take an optional non-centrality
  argument, so the \code{*nchisq()} functions are no longer needed.

  The new function \code{dev.print()} should now be used for saving
  plots to a file or printing them.

  \code{.Defunct} is the function to which defunct functions are set.
}
\seealso{\code{\link{Deprecated}}}
\keyword{documentation}
\keyword{utilities}

