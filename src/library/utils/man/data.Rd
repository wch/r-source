\name{data}
\alias{data}
\alias{print.packageIQR}
\title{Data Sets}
\description{
  Loads specified data sets, or list the available data sets.
}
\usage{
data(\dots, list = character(0), package = NULL,
     lib.loc = NULL, verbose = getOption("verbose"),
     envir = .GlobalEnv)
}
\arguments{
  \item{\dots}{a sequence of names or literal character strings.}
  \item{list}{a character vector.}
  \item{package}{a character vector giving the package(s) to look
    in for data sets, or \code{NULL}.

    By default, all packages in the search path are used, then
    the \file{data} subdirectory (if present) of the current working
    directory.
  }
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{envir}{the \link{environment} where the data should be loaded.}
}
\value{
  a character vector of all data sets specified, or information about
  all available data sets in an object of class \code{"packageIQR"} if
  none were specified.
}
\details{
  Currently, four formats of data files are supported:

  \enumerate{
    \item files ending \file{.R} or \file{.r} are
    \code{\link{source}()}d in, with the \R working directory changed
    temporarily to the directory containing the respective file.

    \item files ending \file{.RData} or \file{.rda} are
    \code{\link{load}()}ed.

    \item files ending \file{.tab}, \file{.txt} or \file{.TXT} are read
    using \code{\link{read.table}(\dots, header = TRUE)}, and hence
    result in a data frame.

    \item files ending \file{.csv} or \file{.CSV} are read using
    \code{\link{read.table}(\dots, header = TRUE, sep = ";")},
    and also result in a data frame.
  }
  If more than one matching file name is found, the first on this list
  is used.
  
  The data sets to be loaded can be specified as a sequence of names or
  character strings, or as the character vector \code{list}, or as both.

  For each given data set, the first two types (\file{.R} or \file{.r},
  and \file{.RData} or \file{.rda} files) can create several variables
  in the load environment, which might all be named differently from the
  data set.  The second two (\file{.tab}, \file{.txt}, or \file{.TXT},
  and \file{.csv} or \file{.CSV} files) will always result in the
  creation of a single variable with the same name as the data set. 

  If no data sets are specified, \code{data} lists the available data
  sets.  It looks for a new-style data index in the \file{Meta} or, if
  this is not found, an old-style \file{00Index} file in the \file{data}
  directory of each specified package, and uses these files to prepare a
  listing.  If there is a \file{data} area but no index, available data
  files for loading are computed and included in the listing, and a
  warning is given: such packages are incomplete.  The information about
  available data sets is returned in an object of class
  \code{"packageIQR"}.  The structure of this class is experimental.
  Where the datasets have a different name from the argument that should
  be used to retrieve them the index will have an entry like
  \code{beaver1 (beavers)} which tells us that dataset \code{beaver1}
  can be retrieved by the call \code{data(beaver)}.

  If \code{lib.loc} and \code{package} are both \code{NULL} (the
  default), the data sets are searched for in all the currently loaded
  packages then in the \file{data} directory (if any) of the current
  working directory.

  If \code{lib.loc = NULL} but \code{package} is specified as a
  character vector, the specified package(s) are searched for first
  amongst loaded packages and then in the default library/ies
  (see \code{\link{.libPaths}}).

  If \code{lib.loc} \emph{is} specified (and not \code{NULL}), packages
  are searched for in the specified library/ies, even if they are
  already loaded from another library.

  To just look in the \file{data} directory of the current working
  directory, set \code{package = character(0)} (and \code{lib.loc =
    NULL}, the default).
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
  \file{mydata.txt} and pre-process it, e.g., using \code{transform}.
  For instance one can convert numeric vectors to factors with the
  appropriate labels.  Thus, the \file{.R} file can effectively contain
  a metadata specification for the plaintext formats.
}
\seealso{
  \code{\link{help}} for obtaining documentation on data sets,
  \code{\link{save}} for \emph{creating} the second (\file{.rda}) kind
  of data, typically the most efficient one.
}
\examples{
require(utils)
data()                       # list all available data sets
try(data(package = "rpart") )# list the data sets in the rpart package
data(USArrests, "VADeaths")  # load the data sets 'USArrests' and 'VADeaths'
help(USArrests)              # give information on data set 'USArrests'
}
\keyword{documentation}
\keyword{datasets}
