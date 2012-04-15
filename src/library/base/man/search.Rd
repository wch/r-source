% File src/library/base/man/search.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{search}
\title{Give Search Path for R Objects}
\usage{
search()
searchpaths()
}
\alias{search}
\alias{searchpaths}
\description{
  Gives a list of \code{\link{attach}}ed \emph{packages}
  (see \code{\link{library}}), and \R objects, usually
  \code{\link{data.frame}s}.
}
\value{
  A character vector, starting with \code{"\link{.GlobalEnv}"}, and
  ending with \code{"package:base"} which is \R's \pkg{base} package
  required always.

  \code{searchpaths} gives a similar character vector, with the
  entries for packages being the path to the package used to load the
  code.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.  (\code{search}.)

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer.  (\code{searchPaths}.)
}
\seealso{
  \code{\link{.packages}} to list just the packages on search path.

  \code{\link{loadedNamespaces}} to list loaded namespaces.

  \code{\link{attach}} and \code{\link{detach}} to change the
  search path, \code{\link{objects}} to find \R objects in there.
}
\examples{
search()
searchpaths()
}
\keyword{data}

