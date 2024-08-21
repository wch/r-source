% File src/library/tools/man/basetools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2024 R Core Team
% Distributed under GPL 2 or later

\name{basetools}
\title{Base Package Tools}
\alias{base_aliases_db}
\alias{base_rdxrefs_db}
\description{
  Tools for obtaining information about base packages.
}
\usage{
base_aliases_db()
base_rdxrefs_db()
}
\details{
  \code{base_aliases_db()} returns the Rd aliases in the base
  packages, as a nested per-package named list of per-Rd-file named
  lists with the aliases.

  \code{base_rdxrefs_db()} returns the Rd cross-references in the
  base packages, as a per-package list of matrices with columns 
  \code{"Target"}, \code{"Anchor"} and \code{"Source"}.
}
\seealso{
  \code{\link{CRAN_aliases_db}()} and \code{\link{CRAN_rdxrefs_db}()}
  for getting the Rd aliases and cross-references in the current CRAN
  packages.
}
