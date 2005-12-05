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
%% put usage here.
## S3 methods for class 'SSD' or 'mlm'
mauchley.test(object, Sigma = diag(nrow = p),
   T = Thin.row(proj(M) - proj(X)), M = diag(nrow = p), X = ~0,
   idata = data.frame(index = seq(length = p)), ...)
}
\details{  
	\code{mauchley.test} was a misspelling of Mauchly's name, corrected
 	by the introduction of \code{mauchly.test}.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
