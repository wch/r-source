\name{data}
\alias{data}
\alias{show.data}
\title{Data Sets}
\description{
  \code{data} loads a data set or lists (via \code{show.data})
  the available data sets.
}
\usage{
data(\dots, list = character(0), package = .packages(),
     lib.loc = .lib.loc, verbose = getOption("verbose"))
show.data(package = .packages(), lib.loc = .lib.loc)
}
\arguments{
  \item{\dots}{a sequence of names or character strings.}
  \item{list}{a character vector.}
  \item{package}{a name or character vector giving the packages to
    look into for data sets.  By default, all packages in the search
    path are used, then the \code{data} directory (if present) of the
    current working directory.} 
  \item{lib.loc}{a character vector of directory names of \R
    libraries.  Defaults to all libraries currently known.  If the
    default is used, the loaded packages are searched before the
    libraries.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
}
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

  The data sets to be loaded can be specified as a sequence of names or
  character strings, or as the character vector \code{list}, or as both.
  If no data sets are specified or \code{show.data} is called directly,
  the available data sets are displayed.
 
  If no data sets are specified, \code{data} calls \code{show.data}.
  \code{show.data} looks for a file \file{00Index} in a \file{data}
  directory of each specified package, and uses these files to prepare
  a listing.  If there is a \file{data} area but no index a warning is
  given: such packages are incomplete.

  If \code{lib.loc} is not specified,  the datasets are searched for
  amongst those packages already loaded, followed by the \file{data}
  directory (if any) of the current working directory and then packages
  in the specified libraries.  If \code{lib.loc} \emph{is}
  specified, packages are searched for in the specified libraries, even if
  they are already loaded from another library.

  To just look in the \file{data} directory of the current working
  directory, set \code{package = NULL}.
}
\note{
  The data files can be many small files.  On some file systems it is
  desirable to save space, and the files in the \file{data} directory of
  an installed package can be zipped up as a zip archive
  \file{Rdata.zip}.  You will need to provide a single-column file 
  \file{filelist} of file names in that directory.

  One can take advantage of the search order and the fact that a
  \file{.R} file will change directory.  If raw data are stored in
  \file{mydata.txt} then one can set up \file{mydata.R} to read
  \file{mydata.txt} and pre-process it, e.g. using \code{transform}. For
  instance one can convert numeric vectors to factors with the
  appropriate labels.
  Thus, the \file{.R} file can effectively contain a metadata
  specification for the plaintext formats.
}
\seealso{
  \code{\link{help}} for obtaining documentation on data sets.
}
\examples{
data()                       # list all available data sets
data(package = base)         # list the data sets in the base package
data(USArrests, "VADeaths")  # load the data sets `USArrests' and `VADeaths'
help(USArrests)              # give information on data set `USArrests'
}
\keyword{documentation}
\keyword{datasets}
