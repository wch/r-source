\name{page}
\alias{page}
\title{Invoke a Pager on an R Object}
\usage{
page(x, method = c("dput", "print"), \dots)
}
\arguments{
  \item{x}{the name of an \R object.}
  \item{method}{The default method is to dump the object \emph{via}
    \code{\link{dput}}. An alternative is to print to a file.}
  \item{\dots}{additional arguments for \code{\link{file.show}}.
    Intended for setting \code{pager} as \code{title} and
    \code{delete.file} are already used.}
}
\description{
  Displays a representation of the object named by \code{x} in a pager.
}
\seealso{
  \code{\link{file.show}}, \code{\link{edit}}, \code{\link{fix}}.

  To go to a new page when graphing, see \code{\link{frame}}.
}
\keyword{utilities}
