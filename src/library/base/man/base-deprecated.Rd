\name{base-deprecated}
\alias{base-deprecated}
\alias{write.table0}
\alias{format.char}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
%% put usage here.
format.char(x, width = NULL, flag = "-")
}
\arguments{
  \item{x}{a character vector.}
  \item{width, flag}{see \code{\link{formatC}}.}
}
\details{
  \code{write.table0} is an \R-level version of \code{write.table}
  provided for temporary back-compatibility.

  \code{format.char} was an auxiliary function for \code{\link{formatC}}
  whose functionality (and more) is provided by \code{\link{format.default}}.
  
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
