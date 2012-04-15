% File src/library/base/man/dataframeHelpers.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{dataframeHelpers}
\title{Data Frame Auxiliary Functions}
\alias{xpdrows.data.frame}
\description{
  Internal auxiliary functions for use with data frames.
}
\usage{
xpdrows.data.frame(x, old.rows, new.rows)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{old.rows, new.rows}{row names for old and new rows.}
}
\details{
  \code{xpdrows.data.frame} is an auxiliary function which expands the
  rows of a data frame.  It is used by the data frame methods of
  \code{[<-} and \code{[[<-} (which perform subscripted assignments on a
  data frame), and not intended to be called directly.
}
\seealso{
  \code{\link{[.data.frame}}
}
\keyword{internal}
