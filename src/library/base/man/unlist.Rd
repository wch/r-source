\name{unlist}
\title{Flatten Lists}
\usage{
unlist(x, recursive = TRUE, use.names = TRUE)
}
\alias{unlist}
\arguments{
  \item{x}{A list.}
  \item{recursive}{logical. Should unlisting be applied to list
    components of \code{x}?}
  \item{use.names}{logical. Should names be preserved?}
}
\description{
Given a list structure \code{x}, \code{unlist}
produces a vector which contains all the atomic components
which occur in \code{x}.
}
\details{
If \code{recursive=FALSE}, the function will not
recurse beyond the first level items in \code{x}.

By default, \code{unlist} tries to retain the naming
information present in \code{x}.
If \code{use.names = FALSE} all naming information is dropped.
}
\seealso{
  \code{\link{c}}, \code{\link{as.list}}.
}
\examples{
unlist(options())
unlist(unlist(options(), use.names=F))# works for vectors or lists
l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
unlist(l.ex, rec = F)
unlist(l.ex, rec = T)
}
\keyword{list}
\keyword{manip}
