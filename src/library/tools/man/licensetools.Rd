% File src/library/tools/man/licensetools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2025 R Core Team
% Distributed under GPL 2 or later

\name{licensetools}
\alias{analyze_license}
\title{License Tools}
\description{
  Tools for computing on license specifications.
}
\usage{
analyze_license(x)
}
\arguments{
  \item{x}{a character string.}
}
\details{
  R packages use the \samp{License} field in their \file{DESCRIPTION}
  file to specify their license in a standardized form described in
  section \sQuote{Licenses} of the \sQuote{Writing R Extensions} manual.
  This uses alternatives of individual specifications
  (\sQuote{components}) which can use version restrictions or give
  pointers to \file{LICENSE} (or \file{LICENCE}) files either
  restricting the base license or giving a full license.

  Function \code{analyze_license()} checks if a given character string
  provides such a standardized license spec (or something recognized as
  a historic exception which can be transformed to a standardized spec).
  If standardizable, it extracts the individual components and where
  these give version ranges the corresponding expansions into components
  with fixed versions.  It also indicates whether the license can be
  verified to be \abbr{FOSS}
  (\url{https://en.wikipedia.org/wiki/Free_and_open-source_software})
  licenses.
}
\value{
  a list with elements including
  \item{is_canonical}{a logical indicating whether the given string 
    is a standardized license spec.}
  \item{is_standardizable}{a logical indicating whether the given string
    is a standardized license spec, or can be transformed to one.}
  \item{standardization}{a character string giving the standardized
    license spec corresponding to the given string if this is
  standardizable.}
  \item{components}{the components of the standardized license spec.}
  \item{expansions}{the expansions of the components of the standardized
    license spec.}
  \item{is_verified}{a logical indicating whether the given string can
    be verified to specify a \abbr{FOSS} license.}
}
\examples{
## Examples from section 'Licenses' of 'Writing R Extensions':
analyze_license("GPL-2")
analyze_license("LGPL (>= 2.0, < 3) | Mozilla Public License")
analyze_license("GPL-2 | file LICENCE")
analyze_license("GPL (>= 2) | BSD_3_clause + file LICENSE")
analyze_license("Artistic-2.0 | AGPL-3 + file LICENSE")
}
