\name{attach}
\title{Attach Set of \R Objects to Search Path}
\usage{
attach(what, pos = 2, name = deparse(substitute(what)))
}
\alias{attach}
\arguments{
  \item{what}{``database'' to attach. Typically a
    \code{\link{data.frame}}.}
  \item{pos}{integer specifying position in \code{\link{search}()} where
    to attach.}
  \item{name}{alternative way to specify the database to be attached.}
}
\description{
  A new database is attached. This means that its objects are made
  available to \R.

  ------ should say more here ------
}
\value{
  The \code{\link{environment}} is returned invisibly with a
  \code{"name"} attribute.
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{environment}}.
}
\examples{
data(women)
attach(women)
summary(height)   #- which belongs to `women'
}
\keyword{data}
