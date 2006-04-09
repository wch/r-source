\name{stats-deprecated}
\alias{stats-deprecated}
\alias{mauchley.test}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Stats package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
mauchley.test(object, Sigma = diag(nrow = p),
   T = Thin.row(proj(M) - proj(X)), M = diag(nrow = p), X = ~0,
   idata = data.frame(index = seq(length = p)), ...)
}
\arguments{
  \item{object}{object of class \code{SSD} or \code{mlm}}
  \item{Sigma}{Matrix to be proportional to}
  \item{T}{Transformation matrix. By default computed from \code{M} and
    \code{X}}
  \item{M}{Formula or matrix describing the outer projection}
  \item{X}{Formula or matrix describing the inner projection}
  \item{idata}{Data frame describing intra-block design}
  \item{\dots}{For consistency with generic}
}
\details{  
  \code{mauchley.test} was a misspelling of Mauchly's name, corrected
  by the introduction of \code{\link{mauchly.test}}.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
