\name{data}
\title{Data Sets}
\usage{
data(\dots, list = character(0), package = c(.packages(),.Autoloaded),
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

Currently, four formats of data files are supported:
\enumerate{
  \item files ending \code{".RData"} or \code{".rda"} are
  \code{\link{load}()ed}. 
  \item files ending \code{".R"} are \code{\link{source}()d} in.
  \item files ending \code{".tab"} or \code{".txt"} are read using
    \code{\link{read.table}(\dots, header = TRUE)}, and hence result in
    a \code{\link{data.frame}}.
  \item files ending \code{".csv"} are read using
    \code{\link{read.table}(\dots, header = TRUE, sep=";")},
    and also result in a \code{\link{data.frame}}.
}
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
\keyword{documentation}
\keyword{datasets}
