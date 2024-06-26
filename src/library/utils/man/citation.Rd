% File src/library/utils/man/citation.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{citation}
\alias{CITATION}
\alias{citation}
\alias{readCitationFile}
\alias{citHeader}
\alias{citFooter}
\title{Citing R and R Packages in Publications}
\description{
  How to cite \R and \R packages in publications.
}
\usage{
citation(package = "base", lib.loc = NULL, auto = NULL)

readCitationFile(file, meta = NULL)
citHeader(...)
citFooter(...)
}
\arguments{
  \item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of \R libraries, or
    the directory containing the source for \code{package}, or
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
  \item{...}{character strings (which will be \code{\link{paste}}d).}
}
\details{
  The \R core development team and the very active community of package
  authors have invested a lot of time and effort in creating \R as it is
  today.  Please give credit where credit is due and cite \R and \R
  packages when you use them for data analysis.

  Use \code{citation()} (without arguments) for information on how to
  cite the base R system in publications.

  If \code{citation()} is called with \code{package} the name of a
  non-base package, as controlled by the \code{auto} argument it either
  returns the information contained in the package \file{CITATION} file
  or auto-generates citation information from the package
  \file{DESCRIPTION} file.  By default (\code{auto = NULL}), the
  \file{CITATION} file is used if it exists, in which case it is read
  via \code{readCitationFile} with \code{meta} equal to
  \code{packageDescription(package, lib.loc)}.  One can force
  auto-generation via \code{auto = TRUE}.

  The auto-generated citation includes \abbr{URL}s for packages
  installed from the standard repositories CRAN and Bioconductor and
  from development platforms such as \I{GitHub}, \I{GitLab}, or
  \I{R-Forge}.  In case of CRAN and Bioconductor, \abbr{DOI}s are
  included as well.
  
  Packages can use an \samp{Authors@R} field in their
  \file{DESCRIPTION} to provide (\R code giving) a
  \code{\link{person}} object with a refined, machine-readable
  description of the package \dQuote{authors} (in particular specifying
  their precise roles).  Only those with an author role will be
  included in the auto-generated citation.

  If the object returned by \code{citation()} contains only one reference,
  the associated print method shows both a text version and a BibTeX
  entry for it. If a package has more than one reference then only the
  text versions are shown. This threshold is controlled by
  \code{\link{options}("citation.bibtex.max")}.
  The BibTeX versions can also be obtained using
  function \code{toBibtex()} (see the examples below).

  The \file{CITATION} file of an R package should be placed in the
  \file{inst} subdirectory of the package source.  The file is an R
  source file and may contain arbitrary R commands including
  conditionals and computations.  Function \code{readCitationFile()} is
  used by \code{citation()} to extract the information in
  \file{CITATION} files.  The file is \code{source()}ed by the R
  parser in a temporary environment and all resulting bibliographic
  objects (specifically, inheriting from \code{"\link{bibentry}"}) are
  collected.
  These are typically produced by one or more \code{\link{bibentry}()}
  calls, optionally preceded by a \code{citHeader()} and followed
  by a \code{citFooter()} call.
  One can include an auto-generated package citation in the
  \file{CITATION} file via \code{citation(auto = meta)}.

  \code{readCitationFile} makes use of the \code{Encoding} element (if
  any) of \code{meta} to determine the encoding of the file.
}
\value{
  An object of class \code{"citation"}, inheriting from class
  \code{"\link{bibentry}"}; see there, notably for the
  \code{\link{print}} and \code{\link{format}} methods.

  \code{citHeader} and \code{citFooter} return an empty
  \code{"\link{bibentry}"} storing \dQuote{outer} header/footer text
  for the package citation.
}
\seealso{
  \code{\link{bibentry}}
}
\keyword{misc}
\examples{
## the basic R reference
citation()

## extract the BibTeX entry from the return value
x <- citation()
toBibtex(x)
\donttest{% requires recommended packages and gives volatile output
## references for a package
citation("lattice")
citation("lattice", auto = TRUE)  # request the Manual-type reference
citation("foreign")

## a CITATION file with more than one bibentry:
file.show(system.file("CITATION", package="mgcv"))
cm <- citation("mgcv")
cm # header, text references, plus "reminder" about getting BibTeX
print(cm, bibtex = TRUE) # each showing its bibtex code

## a CITATION file including citation(auto = meta)
file.show(system.file("CITATION", package="nlme"))
citation("nlme")
}%dont
}
