\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{.Alias}
\alias{reshapeWide}
\alias{reshapeLong}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release. 
}
\usage{
.Deprecated(new)
.Alias(expr)
reshapeWide(x, i = reshape.i,
             j = reshape.j,
             val = reshape.v,
             jnames = levels(j))

reshapeLong(x,jvars,  ilev = row.names(x),
             jlev = names(x)[jvars],
             iname = "reshape.i",
             jname = "reshape.j",
             vname = "reshape.v")
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is available at
  \code{help("oldName-deprecated")} (note the quotes).

  \code{.Alias} provided an unreliable way to create duplicate
  references to the same object. There is no direct replacement. Where
  multiple references to a single object are required for semantic
  reasons consider using environments or external pointers. There are
  some notes on \url{http://developer.r-project.org}.

  \code{reshape*}, which were experimental, are replace by
  \code{\link{reshape}}. This has a different syntax and allows multiple
  time-varying variables.  See \code{\link{reshapeLong-deprecated}} and
  \code{\link{reshapeWide-deprecated}} for the earlier syntax.

}
\seealso{\code{\link{Defunct}},
  \code{\link{.Alias-deprecated}},\code{\link{reshapeWide-deprecated}}, \code{\link{reshapeLong-deprecated}}}
\keyword{documentation}
\keyword{utilities}
