\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{La.eigen}
\alias{tetragamma}
\alias{pentagamma}
\alias{package.description}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
.Deprecated(new, package=NULL)

% Deprecated in 1.9.0
La.eigen(x, symmetric, only.values = FALSE, method = c("dsyevr", "dsyev"))
tetragamma(x)
pentagamma(x)
package.description(pkg, lib.loc = NULL, fields = NULL)
% </Deprecated 1.9.0>
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package should be listed in
  \code{help("pkg-deprecated")}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{documentation}
\keyword{utilities}
