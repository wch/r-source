\name{edit.data.frame}
\alias{edit.data.frame}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Edit data frames}
\description{
 Use data editor on data frame contents.
}
\usage{
edit.data.frame(name, factor.mode=c("numeric", "character"))
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{name}{ A data frame}
 \item{factor.mode}{ How to handle factors (as integers or using character levels) }
}
\details{
 Presently, this only works on simple data frames containing numeric or
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
  function.}
\seealso{\code{\link{data.entry}}}
\examples{
\dontrun{
data(airquality)
edit(airquality)
edit(airquality, factor.mode="character")
}
}

\keyword{ utilities }%-- one or more ...
