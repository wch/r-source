\name{data}
\alias{data}
\alias{show.data}
\title{Data Sets}
\description{
  \code{data} loads a data set or lists (via \code{show.data})
  the available data sets.
}
\usage{
data(\dots, list = character(0), package = c(.packages(),.Autoloaded),
     lib.loc = .lib.loc)
show.data(package = c(.packages(),.Autoloaded), lib.loc = .lib.loc)
}
\arguments{
  \item{\dots}{a sequence of \code{name}s or character strings}
  \item{list}{a character vector}
  \item{package}{a name or character vector giving the packages to
    look into for data sets.  By default, all packages in the search
    path are used.} 
  \item{lib.loc}{a character vector of directory names of \R
    libraries.  Defaults to all libraries currently known.}}
\value{
  \code{data()} returns a character vector of all data sets specified,
  an empty character vector if none were specified.
}
\details{
  Currently, four formats of data files are supported:

  \enumerate{
    \item files ending \file{.RData} or \file{.rda} are
    \code{\link{load}()}ed.

    \item files ending \file{.R} or \file{.r} are
    \code{\link{source}()}d in, with the \R working directory changed
    temporarily to the directory containing the respective file.

    \item files ending \file{.tab} or \file{.txt} are read using
    \code{\link{read.table}(\dots, header = TRUE)}, and hence result in
    a data frame.

    \item files ending \file{.csv} are read using
    \code{\link{read.table}(\dots, header = TRUE, sep = ";")},
    and also result in a data frame.
  }

  The data sets to be loaded can be specified as a sequence of
  \code{name}s or character strings, or as the character vector
  \code{list}, or as both.  If no data sets are specified or
  \code{show.data} is called directly, the available data sets are
  displayed.
 
  If no data sets are specified, \code{data} calls \code{show.data}.
  \code{show.data} looks for a file \file{00Index} (failing that the
  deprecated convention of \file{index.doc}) in a \file{data}
  directory of each specified package, and uses these files to prepare
  a listing. If there is a \file{data} area but no index a warning is
  given: such packages are incomplete. 
}

\seealso{
  \code{\link{help}} for obtaining documentation on data sets.
}
\examples{
data()                       # list all available data sets
data(package = base)         # list the data sets in the base package
data(USArrests, "VADeaths")  # load the data sets `USArrests and `VADeaths'
help(USArrests)              # give information on data set `USArrests'
}
\keyword{documentation}
\keyword{datasets}
