\name{detach}
\title{Detach Objects from the Search Path}
\usage{
detach(name, pos = 2)
}
\alias{detach}
\arguments{
\item{name}{The object to detach.  Defaults to \code{search()[pos]}.}
\item{pos}{Index position in \code{\link{search}()} of database to
  detach.  When \code{name} is \code{\link{numeric}}, \code{pos = name}
  is used.}
}
\description{
Detach a database, i.e., remove it from the \code{\link{search}()} patch
of available \R objects. Usually, this either a \code{\link{data.frame}}
which has been \code{\link{attach}}ed or a package which was required
previously.
}
\value{
The attached database is returned invisibly, either as
\code{\link{data.frame}} or as \code{\link{list}}.
}
\seealso{
\code{\link{attach}}, \code{\link{library}}, \code{\link{search}},
\code{\link{objects}}.
}
\examples{
require(eda)#package
detach(package:eda)
library(mva)
detach(2)# `pos' used for `name'
}
\keyword{data}
