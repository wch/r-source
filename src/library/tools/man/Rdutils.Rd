% File src/library/tools/man/Rdutils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007=9 R Core Development Team
% Distributed under GPL 2 or later

\name{Rdutils}
\alias{Rd_db}
\alias{Rd_parse}
\title{Rd Utilities}
\description{Utilities for computing on the information in Rd objects.}
\usage{
Rd_db(package, dir, lib.loc = NULL)
Rd_parse(file, text = NULL)
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \file{man}
    with \R documentation sources (in Rd format).  Only used if
    \code{package} is not given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
  \item{file}{a connection, or a character string giving the name of a
    file or a URL to read documentation in Rd format from.}
  \item{text}{character vector with documentation in Rd format.
    Elements are treated as if they were lines of a file.}
}
\details{
  \code{Rd_db} builds a simple database of all Rd sources in a
  package, as a list of character vectors with the lines of the Rd files
  in the package.  This is particularly useful for working on installed
  packages, where the individual Rd files in the sources are no longer
  available.

  \code{Rd_parse} is a simple legacy top-level Rd parser/analyzer which
  is now deprecated in favor of the full \code{\link{parse_Rd}} parser,
  and will be removed in \R 2.10.0.  It returns a list with components
  \describe{
    \item{\code{meta}}{a list containing the Rd metadata (aliases,
      concepts, keywords, and documentation type);
    }
    \item{\code{data}}{a data frame with the names (\code{tags}) and
      corresponding text (\code{vals}) of the top-level sections in the
      R documentation object;
    }
    \item{\code{rest}}{top-level text not accounted for (currently,
      silently discarded by Rdconv, and hence usually the indication of
      a problem).
    }
  }
  Note that at least for the time being, only the top-level structure is
  analyzed.
}
\section{Warning}{
  These functions are still experimental.  Names, interfaces and values
  might change in future versions.
}
\seealso{
  \code{\link{parse_Rd}}.
}
\examples{
## Build the Rd db for the (installed) base package.
db <- Rd_db("base")
}
\keyword{utilities}
\keyword{documentation}
