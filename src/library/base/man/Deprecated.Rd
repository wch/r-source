\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{piechart}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release. 
}
\usage{
.Deprecated(new)
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).

  \code{piechart} is the old name for \code{pie}, but clashed with usage
  in Trellis.
}
\seealso{
  \code{\link{Defunct}},
}
\keyword{documentation}
\keyword{utilities}
