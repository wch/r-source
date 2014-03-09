% File src/library/utils/man/citation.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{citation}
\alias{CITATION}
\alias{citation}
\alias{readCitationFile}
\title{Citing R and R Packages in Publications}
\description{
  How to cite \R and \R packages in publications.
}
\usage{
citation(package = "base", lib.loc = NULL, auto = NULL)
readCitationFile(file, meta = NULL)
}
\arguments{
  \item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of \R libraries, or
    \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{auto}{a logical indicating whether the default citation
    auto-generated from the package \file{DESCRIPTION} metadata should
    be used or not, or \code{NULL} (default), indicating that a
    \file{CITATION} file is used if it exists, or an object of class
    \code{"\link{packageDescription}"} with package metadata (see
    below).}
  \item{file}{a file name.}
  \item{meta}{a list of package metadata as obtained by
    \code{\link{packageDescription}}, or \code{NULL} (the default).}
}
\details{
  The \R core development team and the very active community of package
  authors have invested a lot of time and effort in creating \R as it is
  today.  Please give credit where credit is due and cite \R and \R
  packages when you use them for data analysis.

  Execute function \code{citation()} for information on how to cite the
  base R system in publications.  If the name of a non-base package is
  given, the function either returns the information contained in the
  \file{CITATION} file of the package or auto-generates citation
  information.  In the latter case the package \file{DESCRIPTION} file
  is parsed, the resulting citation object may be arbitrarily bad, but
  is quite useful (at least as a starting point) in most cases.

  In \R >= 2.14.0, one can use a \samp{Authors@R} field in
  \file{DESCRIPTION} to provide (\R code giving) a
  \code{\link{person}} object with a refined, machine-readable
  description of the package \dQuote{authors} (in particular specifying
  their precise roles).  Only those with an author role will be
  included in the auto-generated citation.

  If only one reference is given, the print method for the object
  returned by \code{citation()} shows both a text version and a BibTeX
  entry for it, if a package has more than one reference then only the
  text versions are shown.  The BibTeX versions can be obtained using
  function \code{toBibtex()} (see the examples below).

  The \file{CITATION} file of an R package should be placed in the
  \file{inst} subdirectory of the package source.  The file is an R
  source file and may contain arbitrary R commands including
  conditionals and computations.  Function \code{readCitationFile()} is
  used by \code{citation()} to extract the information in
  \file{CITATION} files.  The file is \code{source()}ed by the R
  parser in a temporary environment and all resulting bibliographic
  objects (specifically, of class \code{"\link{bibentry}"}) are
  collected.

  Traditionally, the \file{CITATION} file contained zero or more calls
  to \code{citHeader}, then one or more calls to \code{\link{citEntry}},
  and finally zero or more calls to \code{citFooter}, where in fact
  \code{citHeader} and \code{citFooter} are simply wrappers to
  \code{\link{paste}}, with their \code{\dots} argument passed on to
  \code{\link{paste}} as is.  The \code{"\link{bibentry}"} class
  makes for improved representation and manipulation of bibliographic
  information (in fact, the old mechanism is implemented using the new
  one), and one can write \file{CITATION} files using the unified
  \code{\link{bibentry}} interface.

  In \R >= 2.14.0, one can include an auto-generated package citation in
  the \file{CITATION} file via \code{citation(auto = meta)}.

  \code{readCitationFile} makes use of the \code{Encoding} element (if
  any) of \code{meta} to determine the encoding of the file.
}
\value{
  An object inheriting from class \code{"\link{bibentry}"}.
}
\seealso{
  \code{\link{bibentry}}
}
\keyword{misc}
\examples{
## the basic R reference
citation()

## references for a package -- might not have these installed
if(nchar(system.file(package = "lattice"))) citation("lattice")
if(nchar(system.file(package = "foreign"))) citation("foreign")

## extract the bibtex entry from the return value
x <- citation()
toBibtex(x)
}
