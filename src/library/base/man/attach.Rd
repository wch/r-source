\name{attach}
\title{Attach Set of R Objects to Search Path}
\usage{
attach(what, pos = 2, name = deparse(substitute(what)))
}
\alias{attach}
\arguments{
  \item{what}{``database''. This may currently be a \code{data.frame} or
    \code{list} or a \R data file created with \code{\link{save}}.}
  \item{pos}{integer specifying position in \code{\link{search}()} where
    to attach.}
  \item{name}{alternative way to specify the database to be attached.}
}
\description{
  The database is attached to the \R search path.  This means that the
  database is searched by \R when evaluating a variable, so objects in
  the database can be accessed by simply giving their names.}

\details{When evaluating a variable or function name \R searches for
  that name in the databases listed by \code{\link{search}}. The first
  name of the appropriate type is used.

  By attaching a data frame to the search path it is possible to refer
  to the variables in the data frame by their names alone, rather than
  as components of the data frame (eg in the example below,
  \code{height} rather than \code{women$height}).

  By default the database is attached in position 2 in the search path,
  immediately after the \R global environment and before all previously
  loaded packages and previously attached databases. This can be altered
  with the \code{pos} option.

  Note that by default assignment is not performed in an attached
  database. Attempting to modify a variable or function in an attached
  database will actually create a modified version in the \R global
  environment. For this reason \code{attach} can lead to confusion.
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
summary(women$height) ## refers to variable `height' in the dataframe
attach(women)
summary(height)      ## The same variable now available by name
height<-height*2.54  ## Don't do this. It creates a new variable
detach("women")
summary(height)      ## The new variable created by modifying `height'
rm(height)
}
\keyword{data}
