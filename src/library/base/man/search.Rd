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
  A character vector, starting with
  \code{"\link{.GlobalEnv}"}, and ending with \code{"package:base"}
  which is \R's \emph{base} package required always.

  \code{searchpaths} gives a similar character vector, with the
  entries for packages being the path to the package used to load the
  code.
}
\seealso{\code{\link{attach}} and \code{\link{detach}} to change the
  search ``path'', \code{\link{objects}} to find \R objects in there.
}
\examples{
search()
searchpaths()
}
\keyword{data}

