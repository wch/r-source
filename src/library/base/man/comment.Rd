% File src/library/base/man/comment.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{comment}
\title{Query or Set a \code{"comment"} Attribute}
\usage{
comment(x)
comment(x) <- value
}
\alias{comment}
\alias{comment<-}
\arguments{
 \item{x}{any \R object}
 \item{value}{a \code{character} vector, or \code{NULL}.}
}
\description{
  These functions set and query a \emph{comment}
  attribute for any \R objects.  This is typically useful for
  \code{\link{data.frame}}s or model fits.

  Contrary to other \code{\link{attributes}}, the \code{comment} is not
  printed (by \code{\link{print}} or \code{\link{print.default}}).

  Assigning \code{NULL} or a zero-length character vector removes the
  comment.
}
\seealso{
  \code{\link{attributes}} and \code{\link{attr}} for other attributes.
}
\examples{
x <- matrix(1:12, 3, 4)
comment(x) <- c("This is my very important data from experiment #0234",
                "Jun 5, 1998")
x
comment(x)
}
\keyword{attribute}
