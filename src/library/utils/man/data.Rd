% File src/library/utils/man/data.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{data}
\alias{data}
\alias{print.packageIQR}
\title{Data Sets}
\description{
  Loads specified data sets, or list the available data sets.
}
\usage{
data(\dots, list = character(), package = NULL, lib.loc = NULL,
     verbose = getOption("verbose"), envir = .GlobalEnv,
     overwrite = FALSE)
}
\arguments{
  \item{\dots}{literal character strings or names.}
  \item{list}{a character vector.}
  \item{package}{
    a character vector giving the package(s) to look
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
  \item{overwrite}{logical: should existing objects of the same name in
    \env{envir} be replaced?}
}
\details{
  Currently, four formats of data files are supported:

  \enumerate{
    \item files ending \file{.R} or \file{.r} are
    \code{\link{source}()}d in, with the \R working directory changed
    temporarily to the directory containing the respective file.
    (\code{data} ensures that the \pkg{utils} package is attached, in
    case it had been run \emph{via} \code{utils::data}.)

    \item files ending \file{.RData} or \file{.rda} are
    \code{\link{load}()}ed.

    \item files ending \file{.tab}, \file{.txt} or \file{.TXT} are read
    using \code{\link{read.table}(\dots, header = TRUE, as.is=FALSE)},
    and hence
    result in a data frame.

    \item files ending \file{.csv} or \file{.CSV} are read using
    \code{\link{read.table}(\dots, header = TRUE, sep = ";", as.is=FALSE)},
    and also result in a data frame.
  }
  If more than one matching file name is found, the first on this list
  is used.  (Files with extensions \file{.txt}, \file{.tab} or
  \file{.csv} can be compressed, with or without further extension
  \file{.gz}, \file{.bz2} or \file{.xz}.)

  The data sets to be loaded can be specified as a set of character
  strings or names, or as the character vector \code{list}, or as both.

  For each given data set, the first two types (\file{.R} or \file{.r},
  and \file{.RData} or \file{.rda} files) can create several variables
  in the load environment, which might all be named differently from the
  data set.  The third and fourth types will always result in the
  creation of a single variable with the same name (without extension)
  as the data set.

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
  directory, set \code{package = character(0)}
  (and \code{lib.loc = NULL}, the default).
}
\value{
  A character vector of all data sets specified, or information about
  all available data sets in an object of class \code{"packageIQR"} if
  none were specified.
}
\section{Good practice}{
  \code{data()} was originally intended to allow users to load datasets
  from packages for use in their examples, and as such it loaded the
  datasets into the workspace \code{\link{.GlobalEnv}}.  This avoided
  having large datasets in memory when not in use.  That need has been
  almost entirely superseded by lazy-loading of datasets.

  The ability to specify a dataset by name (without quotes) is a
  convenience: in programming the datasets should be specified by
  character strings (with quotes).

  Use of \code{data} within a function without an \code{envir} argument
  has the almost always undesirable side-effect of putting an object in
  the user's workspace (and indeed, of replacing any object of that name
  already there).  It would almost always be better to put the object in
  the current evaluation environment by
  \code{data(\dots, envir = environment())}.
  However, two alternatives are usually preferable,
  both described in the \sQuote{Writing R Extensions} manual.
  \itemize{
    \item For sets of data, set up a package to use lazy-loading of data.
    \item For objects which are system data, for example lookup tables
    used in calculations within the function, use a file
    \file{R/sysdata.rda} in the package sources or create the objects by
    \R code at package installation time.
  }
  A sometimes important distinction is that the second approach places
  objects in the namespace but the first does not.  So if it is important
  that the function sees \code{mytable} as an object from the package,
  it is system data and the second approach should be used.  In the
  unusual case that a package uses a lazy-loaded dataset as a default
  argument to a function, that needs to be specified by \code{\link{::}},
  e.g., \code{survival::survexp.us}.
}
\note{
  One can take advantage of the search order and the fact that a
  \file{.R} file will change directory.  If raw data are stored in
  \file{mydata.txt} then one can set up \file{mydata.R} to read
  \file{mydata.txt} and pre-process it, e.g., using \code{transform}.
  For instance one can convert numeric vectors to factors with the
  appropriate labels.  Thus, the \file{.R} file can effectively contain
  a metadata specification for the plaintext formats.
}
\section{Warning}{
  This function creates objects in the \code{envir} environment (by
  default the user's workspace) replacing any which already
  existed. \code{data("foo")} can silently create objects other than
  \code{foo}: there have been instances in published  packages where it
  created/replaced \code{\link{.Random.seed}} and hence change the seed
  for the session.
}
\seealso{
  \code{\link{help}} for obtaining documentation on data sets,
  \code{\link{save}} for \emph{creating} the second (\file{.rda}) kind
  of data, typically the most efficient one.

  The \sQuote{Writing R Extensions} for considerations in preparing the
  \file{data} directory of a package.
}
\examples{
require(utils)
data()                         # list all available data sets
try(data(package = "rpart") )  # list the data sets in the rpart package
data(USArrests, "VADeaths")    # load the data sets 'USArrests' and 'VADeaths'
\dontrun{## Alternatively
ds <- c("USArrests", "VADeaths"); data(list = ds)}
help(USArrests)                # give information on data set 'USArrests'
}
\keyword{documentation}
\keyword{datasets}
