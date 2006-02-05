\name{base-deprecated}
\alias{base-deprecated}
\alias{La.chol}
\alias{La.chol2inv}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
%% put usage here.
La.chol(x)
La.chol2inv(x, size = ncol(x))
}
\arguments{
  \item{x}{a suitable matrix}
  \item{size}{the number of columns of \code{x} containing the
    Choleski decomposition.}
}
\details{  
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{La.chol} and \code{La.chol2inv} are pre-1.7.0 interfaces to the
  same code as the default options of \code{\link{chol}} and
  \code{\link{chol2inv}} respectively.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
