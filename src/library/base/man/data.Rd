\name{data}
\title{Data Sets}
\usage{
data(..., list = character(0), package = .packages(),
     lib.loc = .lib.loc)
}
\alias{data}
\arguments{
 \item{\dots}{a sequence of \code{name}s or character strings}
 \item{list}{a character vector}
 \item{package}{a name or character vector giving the packages to look
  into for data sets.
  By default, all packages in the search path are used.}
 \item{lib.loc}{a character vector of directory names of \R libraries.
  Defaults to all libraries currently known.}}
\description{
\code{data} loads or lists available data sets.

The data sets to be loaded can be specified as a sequence of \code{name}s
or character strings, or as the character vector \code{list}, or as both.

If no data sets are specified, the available data sets are displayed.
}
\value{
A character vector of all data sets specified for loading.
}
\seealso{
\code{\link{help}} for obtaining documentation on data sets.
}
\examples{
data()                       # list all available data sets
data(package = base)         # list the data sets in the base package
data(crimes, "deaths")       # load the data sets `crimes' and `deaths'
help(crimes)                 # give information on data set `crimes'
}
\keyword{datasets}
\keyword{documentation}
