\name{page}
\alias{page}
\title{Invoke a Pager on an R Object}
\description{
  Displays a representation of the object named by \code{x} in a pager
  \emph{via} \code{\link{file.show}}.
}
\usage{
page(x, method = c("dput", "print"), \dots)
}
\arguments{
  \item{x}{An \R object, or a character string naming an object.}
  \item{method}{The default method is to dump the object \emph{via}
    \code{\link{dput}}.  An alternative is to use \code{print} and
    capture the output to be shown in the pager.}
  \item{\dots}{additional arguments for \code{\link{dput}},
    \code{\link{print}} or \code{\link{file.show}} (such as \code{title}).}
}
\details{
  If \code{x} is a length-one character vector, it is used as the name
  of an object to look up in the environment from which \code{page} is
  called.   All other objects are displayed directly.

  A default value of \code{title} is passed to \code{file.show} if one
  is not supplied in \code{\dots}.
}
\seealso{
  \code{\link{file.show}}, \code{\link{edit}}, \code{\link{fix}}.

  To go to a new page when graphing, see \code{\link{frame}}.
}
\examples{\dontrun{## four ways to look at the code of 'page'
page(page)             # as an object
page("page")           # a character string
v <- "page"; page(v)   # a length-one character vector
page(utils::page)      # a call
}}
\keyword{utilities}
