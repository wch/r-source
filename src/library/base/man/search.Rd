\name{search}
\title{Give Search Path for R Objects}
\usage{
search()
}
\alias{search}
\description{
  Gives basically a list of \code{\link{attach}}ed \emph{packages}
  (see \code{\link{library}}), and \R objects, usually
  \code{\link{data.frame}s}.
}
\value{
  A \code{character} vector, usually starting with
  \code{"\link{.GlobalEnv}"}, and containing \code{"package:base"}
  which is \R's \emph{base} package required always.

}
\seealso{\code{\link{attach}} and \code{\link{detach}} to change the
  search ``path'', \code{\link{objects}} to find \R objects in there.
}
\examples{
search()
}
\keyword{data}

