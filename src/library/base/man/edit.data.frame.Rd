\name{edit.data.frame}
\alias{edit.data.frame}
\title{ Edit data frames}
\description{
  Use data editor on data frame contents.
}
\usage{
edit.data.frame(name, factor.mode=c("numeric", "character"))
}
\arguments{
  \item{name}{A data frame.}
  \item{factor.mode}{How to handle factors (as integers or using
    character levels).}
}
\details{
  At present, this only works on simple data frames containing numeric or
  character vectors and factors. Factors are represented in the
  spreadsheet as either numeric vectors (which is more suitable for data
  entry) or character vectors (better for browsing). After editing,
  vectors are padded with \code{NA} to have the same length and
  factor attributes are restored. The set of factor levels can not be
  changed by the editing; invalid levels are changed to NA and a warning
  is issued.
}
\value{
  The edited data frame.
}
\author{ Peter Dalgaard }
\note{\code{fix(dataframe)} works for in-place editing by calling this
  function.

  If the data editor is not available, a dump of the object is presented
  for editing using the default method of \code{edit}.
}
\seealso{\code{\link{data.entry}}, \code{\link{edit}}}
\examples{
\dontrun{
data(airquality)
edit(airquality)
edit(airquality, factor.mode="character")
}
}

\keyword{utilities}
