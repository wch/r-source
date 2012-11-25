% File src/library/base/man/unname.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{unname}
\alias{unname}
\title{Remove \code{names} or \code{dimnames}}
\description{
  Remove the \code{\link{names}} or \code{\link{dimnames}} attribute of
  an \R object.
}
\usage{
unname(obj, force = FALSE)
}
\arguments{
 \item{obj}{an \R object.}
 \item{force}{logical; if true, the \code{dimnames} (names and row
   names) are removed even from \code{\link{data.frame}}s.}
}
\value{
  Object as \code{obj} but without \code{\link{names}} or
  \code{\link{dimnames}}.
}
\examples{
require(graphics); require(stats)

## Answering a question on R-help (14 Oct 1999):
col3 <- 750+ 100*rt(1500, df = 3)
breaks <- factor(cut(col3, breaks = 360+5*(0:155)))
z <- table(breaks)
z[1:5] # The names are larger than the data ...
barplot(unname(z), axes = FALSE)
}
\keyword{utilities}
